{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Monad.Trans.Either
import           System.IO

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Control.Monad.Trans.AWS as AWS
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Sign.V4
import           Data.Aeson.Encode.Pretty


data APIGW = APIGW
newtype APIGWResponse = APIGWResponse Value deriving (Show, ToJSON)

pprint :: ToJSON a => a -> IO ()
pprint = LBS.putStrLn . encodePretty


apigw :: Service
apigw =
    Service
    { _svcAbbrev = "APIGateway"
    , _svcSigner = v4
    , _svcPrefix = "apigateway"
    , _svcVersion = ""
    , _svcEndpoint = defaultEndpoint apigw
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

us x = do
  logger <- AWS.newLogger AWS.Trace stdout
  env <- AWS.newEnv NorthVirginia Discover <&> AWS.envLogger .~ logger
  runResourceT $ runAWST env x


instance AWSRequest APIGW where
  type Rs APIGW = APIGWResponse
  request = get apigw
  response
    = receiveJSON
        (\ s h x ->
           APIGWResponse <$> eitherParseJSON x)

instance ToHeaders APIGW where
  toHeaders
    = const
        (mconcat
           ["Content-Type" =#
              ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON APIGW where
  toJSON APIGW = object []

instance ToPath APIGW where
  toPath = const "/"

instance ToQuery APIGW where
  toQuery = const mempty
