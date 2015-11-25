{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HalHaskell where

import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid

import           Language.Haskell.Exts.Extension (Language(..), Extension(..), KnownExtension(..))
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax

import           HalTypes
import qualified HalTypes as Hal

mode' = ParseMode
        { parseFilename = "test"
        , baseLanguage = Haskell2010
        , extensions = [ EnableExtension DataKinds
                       , EnableExtension TypeOperators
                       , EnableExtension ConstraintKinds
                       ]
        , ignoreLanguagePragmas = False
        , ignoreLinePragmas = False
        , fixities = Nothing
        }

test = parseDeclWithMode mode' "type Deployments = \"deployments.xml\" :> Header \"x-api-key\" ApiKey :> ReqBody '[FormUrlEncoded] DeploymentParams :> Post '[XMLStub] Text"

test' = prettyPrint (tyDecl "A" (TS "hello" %:> TS "world"))

tyDecl name = TypeDecl noLoc (Ident name) []
a %:> b = TyInfix a ifx b where ifx = UnQual (Symbol ":>")
infixl 4 %:>

a %:<|> b = TyInfix a ifx b where ifx = UnQual (Symbol ":<|>")
infixl 4 %:<|>

ident = Ident . cap where cap (x:xs) = toUpper x : xs
loident = Ident . uncap where uncap (x:xs) = toLower x : xs

pattern NoLoc = SrcLoc "" (-1) (-1)
pattern Data name ctrs = DataDecl NoLoc DataType [] name [] ctrs []

pattern Ctor decl = QualConDecl NoLoc [] [] decl
pattern Unqualified s = TyCon (UnQual (Ident s))
pattern TS s = TyPromoted (PromotedString s)

declType :: APIType -> Type
declType = \case
  Hal.List ty -> TyList (declType ty)
  Map kty vty -> Unqualified "Map" `TyApp` declType kty `TyApp` declType vty
  TypeRef [x] -> Unqualified x
  TypeRef [] -> error "can't convert empty unqualified type"
  TypeRef ["java", "util", "Date"] -> Unqualified "Date"
  TypeRef qual -> error $ "can't convert qualified type: " ++ show qual

declHaskell = \case
  DeclEnum (APIEnum (ident -> name) members) ->
    [ Data name [ Ctor (ConDecl (ident e) []) | (e, _description) <- members ] ]
  DeclProductType (APIProductType (ident -> name) props) ->
    [ Data name [ Ctor (RecDecl name [ ([loident pname], declType ty) | (ty, pname) <- props ]) ] ]
  DeclResource r@Resource{..} -> let name = ident rName in
     (Data name [ Ctor (RecDecl name [ ([loident pname], declType ty) | (ty, pname) <- rProperties ]) ])
    : (declResourceMethods r <> resourceMethods r)


declResourceMethods Resource{..} =
  case rMethods of
    [] -> []
    xs -> [ TypeDecl NoLoc (ident $ rName ++ "Resources") []
              (foldl1' (%:<|>) [TyCon (UnQual (ident mName)) | Method{..} <- xs ]) ]


declMethod method@Method{..} = TypeDecl NoLoc (ident mName) [] (toServant method)

resourceMethods Resource{..} =
  case rMethods of
    [] -> []
    xs -> map declMethod xs

promotedJson = TyPromoted (PromotedList True [PromotedCon False (UnQual (Ident "JSON"))])

class HasServant a where
  toServant :: a -> Type

instance HasServant (HTTPMethod, Maybe Type) where
  toServant = \case
    (GET, ret) -> Unqualified "Get" `TyApp` promotedJson `tyApp'` ret
    (POST body, ret) -> hasBody "Post" body ret
    (PUT body, ret) -> hasBody "Put" body ret
    (PATCH body, ret) -> hasBody "Patch" body ret
    (DELETE, ret) -> Unqualified "Delete" `TyApp` promotedJson `tyApp'` ret
    where
      a `tyApp'` b = maybe a (a `TyApp`) b
      hasBody meth (declType -> body) ret =
        Unqualified "ReqBody" `TyApp` promotedJson `TyApp` body %:>
        Unqualified meth `TyApp` promotedJson `tyApp'` ret

instance HasServant Property where
  toServant (ty, name) = Unqualified "Capture" `TyApp`
                         TS name `TyApp`
                         declType ty

instance HasServant Method where
  toServant Method{..} =
    fromJust (captures %<> (Just $ toServant (mHTTPMethod, fmap declType mRetType)))
    where
    (%<>) :: Maybe Type -> Maybe Type -> Maybe Type
    Just ty %<> Nothing = Just ty
    Nothing %<> Just ty = Just ty
    Nothing %<> Nothing = Nothing
    Just ty %<> Just ty'= Just (ty %:> ty')

    captures :: Maybe Type
    captures = case mURIArgs of
      (x:xs) -> Just $ foldl' (\acc x -> acc %:> toServant x) (toServant x) xs
      [] -> Nothing

apigw = (DeclResource
     Resource
       { rName = "RestApis"
       , rProperties = []
       , rMethods =
           [ Method
               { mName = "getRestApiById"
               , mRelName = "restapi:by-id"
               , mRetType = Just (TypeRef [ "RestApi" ])
               , mHTTPMethod = GET
               , mURIArgs = [ ( TypeRef [ "String" ] , "restapi_id" ) ]
               }
           , Method
               { mName = "createRestApi"
               , mRelName = "restapi:create"
               , mRetType = Just (TypeRef [ "RestApi" ])
               , mHTTPMethod = POST (TypeRef [ "CreateRestApiInput" ])
               , mURIArgs = []
               }
           , Method
               { mName = "getNext"
               , mRelName = "next"
               , mRetType = Just (TypeRef [ "RestApis" ])
               , mHTTPMethod = GET
               , mURIArgs = []
               }
           , Method
               { mName = "getFirst"
               , mRelName = "first"
               , mRetType = Just (TypeRef [ "RestApis" ])
               , mHTTPMethod = GET
               , mURIArgs = []
               }
           , Method
               { mName = "getItem"
               , mRelName = "item"
               , mRetType = Just (Hal.List (TypeRef [ "RestApi" ]))
               , mHTTPMethod = GET
               , mURIArgs = []
               }
           ]
       })

-- {
--     "_links": {
--         "apigateway:rest-apis": {
--             "href": "/restapis"
--         },
--         "account:update": {
--             "href": "/account"
--         },
--         "apigateway:client-certificates": {
--             "href": "/clientcertificates"
--         },
--         "curies": [
--             {
--                 "templated": true,
--                 "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/account-apigateway-{rel}.html",
--                 "name": "account"
--             },
--             {
--                 "templated": true,
--                 "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-apigateway-{rel}.html",
--                 "name": "apigateway"
--             },
--             {
--                 "templated": true,
--                 "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-apikey-{rel}.html",
--                 "name": "apikey"
--             },
--             {
--                 "templated": true,
--                 "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-domainname-{rel}.html",
--                 "name": "domainname"
--             },
--             {
--                 "templated": true,
--                 "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-restapi-{rel}.html",
--                 "name": "restapi"
--             }
--         ],
--         "restapi:create": {
--             "href": "/restapis"
--         },
--         "restapi:by-id": {
--             "templated": true,
--             "href": "/restapis/{restapi_id}"
--         },
--         "apikey:create": {
--             "href": "/apikeys"
--         },
--         "apikey:by-key": {
--             "templated": true,
--             "href": "/apikeys/{api_Key}"
--         },
--         "domainname:by-name": {
--             "templated": true,
--             "href": "/domainnames/{domain_name}"
--         },
--         "self": {
--             "href": "/"
--         },
--         "apigateway:domain-names": {
--             "href": "/domainnames"
--         },
--         "apigateway:api-keys": {
--             "href": "/apikeys"
--         },
--         "domainname:create": {
--             "href": "/domainnames"
--         }
--     }
-- }
