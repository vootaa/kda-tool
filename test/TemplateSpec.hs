{-# LANGUAGE OverloadedStrings #-}

module TemplateSpec where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Either
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import           Test.Hspec
import           Test.Hspec.Golden
import           Text.Mustache
import           Text.Printf
------------------------------------------------------------------------------
import           Commands.GenTx
import           TxTemplate
------------------------------------------------------------------------------

templateSpec :: Spec
templateSpec = do
    tcText <- runIO $ T.readFile "test/golden/transfer-create.ktpl"
    let tcTmpl = mkTmpl tcText
    it "data reader doesn't have decimal imprecision" $ do
      case readVars "foo: 0.0000000001" of
        Left err -> expectationFailure err
        Right m -> case M.lookup "foo" m of
          Just (Number n) ->
            toRealFloat n `shouldSatisfy` \x ->
              abs (x - (1e-10 :: Double)) < 1e-20
          Just v -> expectationFailure $ "Expected numeric value for key foo, got: " <> show v
          Nothing -> expectationFailure "Expected key foo in parsed vars"
    it "enforceEqualArrayLens ignores singleton arrays" $ do
      eealTest varMap `shouldBe` Right (Just 2)
    let nm = "fillValueVars-transfer-create"
    it nm $ do
      defaultGolden nm $ either show (T.unpack . T.unlines) $ fillValueVars tcTmpl transferCreateVarMap
  where
fooTmplText :: String -> String -> Text
fooTmplText hole1 hole2 = T.unlines
  [ "someText: |-"
  , T.pack $ printf "  This is a %s test" hole1
  , T.pack $ printf "num: %s" hole2
  ]

mkTmpl :: Text -> Template
mkTmpl t = fst $ fromRight (error "mkTmpl error") $
  parseAndGetVars t

fillValArr :: [Text]
fillValArr = ["alpha", "bravo"]

fillValNum :: Scientific
fillValNum = 5.00000000001

transferCreateVarMap :: Map Text Value
transferCreateVarMap = M.fromList
    [ ("amount", Number 5)
    , ("chain", mkArr ["0", "1"])
    , ("from-acct", String "alice")
    , ("from-key", String "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    , ("network", String "testnet04")
    , ("to-acct", String "bob")
    , ("to-keys", mkArr [mkArr ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                        ,"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                        ,"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                        ]])
    ]

varMap :: Map Text Value
varMap = M.fromList
    [ ("foo", mkArr [mkArr $ map String fillValArr])
    , ("bar", Number fillValNum)
    , ("baz", mkArr $ map String fillValArr)
    ]

mkArr :: [Value] -> Value
mkArr = Array . V.fromList

fooFilledText :: Text
fooFilledText = fooTmplText (show fillValArr) (show fillValNum)

eealTest :: Map Text Value -> Either [String] (Maybe Int)
eealTest vm = enforceEqualArrayLens vs
  where
    vs = snd $ partitionEithers $ map parseValueValue (M.toList vm)
