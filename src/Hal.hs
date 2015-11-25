{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Hal where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Function
import Data.List
import Data.Maybe
import Data.Traversable
import Safe
import System.FilePath (takeFileName)
import Text.Show.Pretty (ppShow)

import Language.Java.Lexer
import Language.Java.Parser
import Language.Java.Pretty
import Language.Java.Syntax

import HalTypes

p :: Show a => a -> IO ()
p = putStrLn . ppShow

data E = Parse | Analyze deriving Show

parse = parser compilationUnit
compile path = (assert Parse . parse >=> assert Analyze . analyze) <$>
               readFile path
  where
    assert e = either (Left . (e, takeFileName path, ) . show) Right

pattern SingleInterfaceCU id' members <- CompilationUnit _ _ [InterfaceTypeDecl (ResourceInfoSubInterface id' members)]

pattern ResourceInfoSubInterface id' members <- InterfaceDecl _ (Ident id') [] [ClassRefType (ClassType [(Ident "ResourceInfo",[])])] (InterfaceBody members)

pattern SingleClassCU id' members <- CompilationUnit _ _ [ClassTypeDecl (ClassDecl [Public] (Ident id') [] Nothing [] (ClassBody members))]

pattern SingleEnumCU ident consts <- CompilationUnit _ _ [ClassTypeDecl (EnumDecl [Public] (Ident ident) [] (EnumBody consts _))]

pattern Relation k = (Ident "relation", EVVal (InitExp (Lit (String k))))

pattern HTTPMethod name = (Ident "method", EVVal (InitExp (ExpName (Name [Ident "HttpMethodName",Ident name]))))

pattern Link kvs = Annotation (NormalAnnotation {annName = Name [Ident "Link"], annKV = kvs})

pattern JMethod anns ty ident args <- MethodDecl anns [] ty (Ident ident) args [] _

pattern JSomeMethod ident <- MethodDecl _ _ _ (Ident ident) _ _ _

pattern JProperty ty ident <-
  MethodDecl [] [] ty (Ident ('g':'e':'t':ident)) [] [] (MethodBody _)

pattern JPrivateField ty ident = FieldDecl [Private] ty [VarDecl (VarId (Ident ident)) Nothing]

pattern URIParam name ty <- FormalParam
                    [ Annotation
                        NormalAnnotation
                          { annName = Name [ Ident "UriVariable" ]
                          , annKV =
                              [ ( Ident "name" , EVVal (InitExp (Lit (String name))) ) ]
                          }
                    ]
                    ty
                    False
                    _

pattern Param ty <- FormalParam [] ty False _

pattern ClassRef cs = ClassRefType (ClassType cs)
pattern TypeComponent s = (Ident s, [])
pattern ListOf t = [ ( Ident "java" , [] )
                   , ( Ident "util" , [] )
                   , ( Ident "List" , [ ActualType t ] )
                   ]
pattern MapOf k v = [ ( Ident "java" , [] )
                    , ( Ident "util" , [] )
                    , ( Ident "Map" , [ ActualType k, ActualType v ] )
                    ]

apiType :: Type -> APIType
apiType = \case
  RefType r -> ref r
  unk -> error $ "unknown type: " ++ show unk
  where
    ref = \case
      ClassRef (ListOf r) -> List (ref r)
      ClassRef (MapOf k v) -> Map (ref k) (ref v)
      ClassRef xs -> TypeRef (map components xs)
      unk -> error $ "unknown ref type: " ++ show unk

    components = \case
      TypeComponent x -> x
      unk -> error $ "unknown type component: " ++ show unk

analyze (SingleInterfaceCU rName members) = Right $ DeclResource Resource{..}
  where
    rProperties = lefts clsf
    rMethods = rights clsf
    clsf = flip map members $ \case
      (JProperty (Just ty) ident) -> Left (apiType ty, ident)
      (JMethod [Link links] (fmap apiType -> mRetType) mName args) ->
        let
          mRelName = headNote "expected relation in links" [s | Relation s <- links]
          body = case [ty | Param ty <- args] of
            [ty] -> Just (apiType ty)
            [] -> Nothing
            tys -> error $ "can't handle many potential body parameters: " ++ show tys
          needBody = maybe (error "need a body for this request type") id body
          mHTTPMethod = case headDef "GET" [s | HTTPMethod s <- links] of
            "GET" -> GET
            "POST" -> POST needBody
            "PATCH" -> PATCH needBody
            "PUT" -> PUT needBody
            "DELETE" -> DELETE
            unk -> error $ "unknown HTTP method: " ++ unk
          mURIArgs = [(apiType ty, name) | URIParam name ty <- args]
        in Right Method{..}
      member -> error $ "unknown interface member: " ++ show member

analyze (SingleClassCU name members) = Right $ DeclProductType $ APIProductType name props
  where
    -- due to java overloading, this may contain two properties
    -- with different types but equal names
    props = catMaybes $ flip map [m | MemberDecl m <- members] $ \case
      JMethod _ _ ('s':'e':'t':ident) [Param ty] -> Just (apiType ty, ident)
      JSomeMethod s | "with" `isPrefixOf` s ||
                      "get" `isPrefixOf` s ||
                      s `elem` ["hashCode", "toString", "equals"] -> Nothing
      JPrivateField _ _ -> Nothing -- relying from setters
      member -> error $ "unknown class member: " ++ show member

analyze (SingleEnumCU name consts) = Right $ DeclEnum $ APIEnum name members
  where
    members = [(k,v) | EnumConstant (Ident k) [Lit (String v)] _ <- consts]

analyze _ = Left "unknown class"


files = [
    "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/Account.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/ApiGateway.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/ApiKey.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/ApiKeys.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/BadRequestException.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/BasePathMapping.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/BasePathMappings.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/CacheClusterSize.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/CacheClusterStatus.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/ClientCertificate.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/ClientCertificates.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/ConflictException.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/CreateApiKeyInput.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/CreateBasePathMappingInput.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/CreateDeploymentInput.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/CreateDomainNameInput.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/CreateModelInput.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/CreateResourceInput.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/CreateRestApiInput.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/CreateStageInput.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/Deployment.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/Deployments.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/DomainName.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/DomainNames.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/GenerateClientCertificateInput.java"
  --, "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/GetSdkRequest.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/Integration.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/IntegrationResponse.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/IntegrationType.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/LimitExceededException.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/Method.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/MethodResponse.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/MethodSetting.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/MethodSnapshot.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/Model.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/Models.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/NotFoundException.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/Op.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/PatchDocument.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/PatchOperation.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/PutIntegrationInput.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/PutIntegrationResponseInput.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/PutMethodInput.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/PutMethodResponseInput.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/Resource.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/Resources.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/RestApi.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/RestApis.java"
  --, "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/SdkResponse.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/ServiceUnavailableException.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/Stage.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/StageKey.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/Stages.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/Template.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/TestInvokeMethodRequest.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/TestInvokeMethodResponse.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/ThrottleSettings.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/TooManyRequestsException.java"
  , "aws-apigateway-sdk-java/src/main/java/com/amazonaws/services/apigateway/model/UnauthorizedException.java"
  ]
