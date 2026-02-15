{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Keygen
  ( keygenCommand
  ) where

------------------------------------------------------------------------------
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Pact.Types.Crypto
import           Pact.Types.Util (toB16Text)
------------------------------------------------------------------------------
import           Keys
import           Types.KeyType
import           Utils
------------------------------------------------------------------------------

keygenCommand :: KeyType -> IO ()
keygenCommand kt = do
  case kt of
    Plain -> do
      (pub, sec) <- genKeyPair
      putStrLn $ "public: " ++ T.unpack (toB16Text $ exportEd25519PubKey pub)
      putStrLn $ "secret: " ++ T.unpack (toB16Text $ exportEd25519SecretKey sec)
    HD -> do
      let toPhrase = T.unwords . M.elems . mkPhraseMapFromMnemonic
      let prettyErr err = "ERROR generating menmonic: " <> tshow err
      res <- either prettyErr toPhrase <$> genMnemonic12
      T.putStrLn res
