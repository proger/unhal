module HalTypes where

data APIType = List APIType
             | Map APIType APIType
             | TypeRef [String]
             deriving (Show, Read, Ord, Eq)

type APIName = String
type Property = (APIType, APIName)

data HTTPMethod = GET
                | POST APIType
                | PATCH APIType
                | PUT APIType
                | DELETE deriving (Show, Read)

data Method =
  Method
  { mName :: String
  , mRelName :: String
  , mRetType :: Maybe APIType
  , mHTTPMethod :: HTTPMethod
  , mURIArgs :: [Property]
  } deriving Show

data Resource =
  Resource
  { rName :: String
  , rProperties :: [Property]
  , rMethods :: [Method]
  } deriving Show

-- ok to assume resources have a global namespace:

instance Eq Resource where
  a == b = rName a == rName b

instance Ord Resource where
  a `compare` b = rName a `compare` rName b

data APIProductType = APIProductType String [Property] deriving Show
data APIEnum = APIEnum String [(String, String)] deriving Show

data APIDecl = DeclEnum APIEnum
             | DeclProductType APIProductType
             | DeclResource Resource
             deriving Show
