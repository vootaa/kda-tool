{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Types.TxInputs where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Error
import           Data.Aeson as A
import qualified Data.Aeson.Key as K
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as LB
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Pact.ApiReq
import qualified Pact.JSON.Encode as J
import           Pact.Types.Lang
import           Pact.Types.RPC
------------------------------------------------------------------------------

data PactTxType = PttExec | PttCont
  deriving (Eq,Ord,Show,Read)

ptt2text :: PactTxType -> Text
ptt2text PttExec = "exec"
ptt2text PttCont = "cont"

instance ToJSON PactTxType where
  toJSON = String . ptt2text

instance FromJSON PactTxType where
  parseJSON = withText "PactTxType" $ \t ->
    case t of
      "exec" -> pure PttExec
      "cont" -> pure PttCont
      _ -> fail ("Error: " <> T.unpack t <> " not a valid PactTxType")

data ExecInputs = ExecInputs
  { _execInputs_codeOrFile :: Either Text FilePath
  , _execInputs_dataOrFile :: Either Value FilePath
  } deriving (Eq,Show)

execInputsPairs :: ExecInputs -> [Pair]
execInputsPairs ei =
  [ either ("code" .=) ("codeFile" .=) $ _execInputs_codeOrFile ei
  , either ("data" .=) ("dataFile" .=) $ _execInputs_dataOrFile ei
  ]

-- | This type makes the following (backwards-compatible) changes from ApiReq in
-- Pact:
--
-- The "networkId" field is now mandatory
--
-- The "publicMeta" field is changed to "meta" to match the name that is used in
--   the final commands that are submitted to the blockchain but the FromJSON
--   instance still accepts "publicMeta".
--
-- This same publicMeta/meta field is now mandatory
data TxInputs = TxInputs
  { _txInputs_type :: PactTxType
  , _txInputs_payload :: Either ContMsg ExecInputs
  , _txInputs_signers :: Maybe [ApiSigner]
  , _txInputs_nonce :: Maybe Text
  , _txInputs_meta :: ApiPublicMeta
  , _txInputs_networkId :: NetworkId
  } deriving (Eq,Show)

txInputsToApiReq :: TxInputs -> IO ApiReq
txInputsToApiReq txi = do
  let t = ptt2text $ _txInputs_type txi
      n = Just $ _txInputs_networkId txi
  case _txInputs_payload txi of
    Left c -> pure $ ApiReq
      { _ylType = Just t
      , _ylPactTxHash = let PactId pid = _cmPactId c in hush $ fromText' pid
      , _ylStep = Just $ _cmStep c
      , _ylRollback = Just $ _cmRollback c
      , _ylData = legacyToAeson (_cmData c)
      , _ylProof = _cmProof c
      , _ylDataFile = Nothing
      , _ylCode = Nothing
      , _ylCodeFile = Nothing
      , _ylKeyPairs = Nothing
      , _ylSigners = Just $ fromMaybe [] $ _txInputs_signers txi
      , _ylVerifiers = Nothing
      , _ylNonce = _txInputs_nonce txi
      , _ylPublicMeta = Just $ _txInputs_meta txi
      , _ylNetworkId = n
      }
    Right ei -> do
      d <- getOrReadFile (eitherDecode . LB.fromStrict . T.encodeUtf8) $ _execInputs_dataOrFile ei
      c <- getOrReadFile Right $ _execInputs_codeOrFile ei
      pure $ ApiReq
        { _ylType = Just t
        , _ylPactTxHash = Nothing
        , _ylStep = Nothing
        , _ylRollback = Nothing
        , _ylData = hush d
        , _ylProof = Nothing
        , _ylDataFile = Nothing
        , _ylCode = hush c
        , _ylCodeFile = Nothing
        , _ylKeyPairs = Nothing
        , _ylSigners = Just $ fromMaybe [] $ _txInputs_signers txi
        , _ylVerifiers = Nothing
        , _ylNonce = _txInputs_nonce txi
        , _ylPublicMeta = Just $ _txInputs_meta txi
        , _ylNetworkId = n
        }

legacyToAeson :: A.ToJSON a => a -> Maybe Value
legacyToAeson = A.decode . A.encode

getOrReadFile :: (Text -> Either String a) -> Either a FilePath -> IO (Either String a)
getOrReadFile _ (Left a) = pure $ Right a
getOrReadFile parser (Right fp) = do
  t <- T.readFile fp
  pure $ parser t

instance ToJSON TxInputs where
  toJSON ti = object $ payloadPairs ++
    [ "type" .= _txInputs_type ti
    , "signers" .= maybe [] (map J.toJsonViaEncode) (_txInputs_signers ti)
    , "nonce" .= _txInputs_nonce ti

    -- TODO Not sure if this should be "meta" or "publicMeta". I think it should
    -- be "meta" because we want to move people towards the key used in the
    -- actual Pact API and people shouldn't be consuming the output of this
    -- function with a legacy pact command line executable.
    , "meta" .= J.toJsonViaEncode (_txInputs_meta ti)

    , "networkId" .= J.toJsonViaEncode (_txInputs_networkId ti)
    ]
    where
      payloadPairs = either contMsgJsonPairs execInputsPairs $ _txInputs_payload ti

contMsgJsonPairs :: ContMsg -> [Pair]
contMsgJsonPairs c =
  [ "pactTxHash" .= pid
  , "step" .= _cmStep c
  , "rollback" .= _cmRollback c
  , "data" .= legacyToAeson (_cmData c)
  , "proof" .= fmap J.toJsonViaEncode (_cmProof c)
  ]
  where
    PactId pid = _cmPactId c


instance FromJSON TxInputs where
  parseJSON v = withObject "TxInputs" func v
    where
      func o = do
        t <- pure . fromMaybe PttExec =<< o .:? "type"
        p <- case t of
          PttCont -> Left <$> parseJSON v
          PttExec -> do
            mc :: Maybe (Either Text FilePath) <- parseMaybePair o "code"
            md :: Maybe (Either Value FilePath) <- parseMaybePair o "data"
            c <- maybe (fail "Must have exec or cont fields") pure mc
            let d = fromMaybe (Left $ object []) md
            pure $ Right $ ExecInputs c d

        -- We want to allow both "meta" and "publicMeta" here to make this tool
        -- usable in as many situations as possible. We can consider removing
        -- the legacy support for "publicMeta" somewhere down the line after
        -- sufficient adoption.
        m <- (o .: "meta") <|> (o .: "publicMeta")

        TxInputs
          <$> pure t
          <*> pure p
          <*> o .:? "signers"
          <*> o .:? "nonce"
          <*> pure m
          <*> o .: "networkId"

parseMaybePair
  :: FromJSON a
  => A.Object
  -> Text
  -> Parser (Maybe (Either a FilePath))
parseMaybePair o name = do
  mn <- o .:? K.fromText name
  let nameFile = name <> "File"
  mf <- o .:? K.fromText nameFile
  case (mn,mf) of
    (Nothing,Nothing) -> pure Nothing
    (Just n,Nothing) -> pure $ Just $ Left n
    (Nothing,Just f) -> pure $ Just $ Right f
    (Just _,Just _) -> fail $ T.unpack ("Cannot have both " <> name <> " and " <> nameFile)
