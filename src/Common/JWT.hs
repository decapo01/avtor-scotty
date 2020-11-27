{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.JWT where

import Data.Text (Text)
import GHC.Generics (Generic)


import Data.Aeson (ToJSON, FromJSON, encode)
import Config (secretFile, Config)
import Jose.Jwt (Jwt, JwtError)
import Jose.Jws (hmacEncode)
import Jose.Jwa (JwsAlg(HS384, HS256))

import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BSU

import qualified Jose.Jwt             as Jwt
import qualified Jose.Jwk             as Jwk
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as LByteString



data TokenUser
  = TokenUser
  { tokenUserId :: Text
  , roles :: [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data AuthToken
  = AuthToken
  { authTokenId :: Text
  , authTokenUser :: Maybe TokenUser
  }
  deriving (Show, Generic, ToJSON, FromJSON)


createJwt :: Config -> AuthToken -> IO (Either JwtError Jwt)
createJwt config authToken = do
  key <- readFile $ secretFile config
  return $ hmacEncode HS384  (BSU.fromString key) (LByteString.toStrict $ encode authToken)



-- data Algorithm
--   = AHS256

-- type Key = Text

-- encodeJwt :: ToJSON j => j -> Key -> Algorithm -> Text
-- encodeJwt item key algorithm = do
--   key <- Jwk.encode key
--   i <- Jwt.encode 