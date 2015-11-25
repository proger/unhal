{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module HalRelations where

import Data.Char
import Data.Graph
import Data.List
import Data.Maybe
import Data.Tree

import Debug.Trace

import HalTypes
import Hal (compile, files)

compileAll = mapM compile files

cap (x:xs) = toUpper x : xs

typeRef :: APIType -> Maybe String
typeRef = \case
  List a -> typeRef a
  Map _ a -> typeRef a
  TypeRef [a] -> Just a
  TypeRef xs -> Just $ intercalate "." xs

maybeDistinct x = \case
  y | y == x -> Nothing
  y -> Just y

resources decls = [r | Right (DeclResource r) <- decls]
infer xs =
  [(r, cap rName, catMaybes [mRetType >>= typeRef >>= maybeDistinct (cap rName) | Method{..} <- rMethods]) | r@Resource{..} <- xs]


infer' :: [Resource] -> [Node String]
infer' xs =
  [(rName, rName, nub $ catMaybes [mRetType >>= typeRef >>= maybeDistinct (cap rName) | Method{..} <- rMethods])
   | r@Resource{..} <- xs]

dfs' :: Ord a => a -> [Node a] -> Forest a
dfs' e edges = unvertex (dfs g [root'])
  where
    unvertex = fmap (fmap ((\(k, _, _) -> k) . back))
    Just root' = lookup e
    (g, back, lookup) = graphFromEdges edges


dff' :: Ord key => [(node, key, [key])] -> Forest node
dff' edges = unvertex (dff g)
  where
    unvertex = fmap (fmap ((\(k, _, _) -> k) . back))
    (g, back, _) = graphFromEdges edges

type Node a = (a, a, [a])

components' :: Ord key => [(node, key, [key])] -> Forest node
components' edges = unvertex (components g)
  where
    unvertex = fmap (fmap ((\(k, _, _) -> k) . back))
    (g, back, _) = graphFromEdges edges


scc' :: Ord key => [(node, key, [key])] -> Forest node
scc' edges = unvertex (scc g)
  where
    unvertex = fmap (fmap ((\(k, _, _) -> k) . back))
    (g, back, _) = graphFromEdges edges


mapAcc :: b -> (b -> a -> b) -> Tree a -> Tree b
mapAcc acc f (Node x ts) = Node new (map (mapAcc new f) ts)
  where
    new = f acc x


-- there is always a default resource

-- R, path                        | RestApi, /restapis/1
-- actionR  (R:action) -> path    | updateRestapi (restapi:update ) -> /restapis/1
-- actionRT (R:T) -> path/T       | getDeployments (restapi:deployments) -> /restapis/1/deployments/{args}
-- actionTR (T:action) -> path/T  | getDeploymentsById (deployments:by-id) -> /restapis/1/deployments/id/{args}
-- actionT  (T:action)            | createDeplomynets (deployments:create) -> /restapis/1/deployments/{args}
--
-- eg:
-- RestApi, /restapis/1
-- createR

go = do
  rs <- fmap resources compileAll
  return $ infer' rs
  --putStrLn $ drawForest $ fmap (fmap rName) $ dff' $ infer rs
  --return $ map flatten $ fmap (fmap rName) $ dff' $ infer rs
  --putStrLn $ drawForest $ fmap (fmap (show) . mapAcc [] (\ts n -> rName n:ts)) $ dff' $ infer rs

type N k v = (k, v, [v])

dfs1 :: Ord v => v -> [N a v] -> Forest a
dfs1 e edges = unvertex (dfs g [root'])
  where
    unvertex = fmap (fmap ((\(k, _, _) -> k) . back))
    Just root' = lookup e
    (g, back, lookup) = graphFromEdges edges

infer1 :: [Resource] -> [N Resource String]
infer1 xs =
  [(r, rName, nub $ catMaybes [mRetType >>= typeRef >>= maybeDistinct (cap rName) | Method{..} <- rMethods])
   | r@Resource{..} <- xs]

infer2 :: [Either t APIDecl] -> Forest Resource
infer2 = dfs1 "ApiGateway" . infer1 . resources

inferAll = do
  rs <- fmap infer2 compileAll
  putStrLn $ drawForest $ fmap (fmap rName) rs
  print $ length rs


go' = putStrLn $ drawForest $ dfs' root' cycled

root' = f where (f, _, _) = root
root = ( "ApiGateway"
           , "ApiGateway"
           , [ "RestApis"
             , "DomainNames"
             , "ApiKeys"
             , "ClientCertificates"
             , "RestApi"
             , "DomainName"
             , "ApiKey"
             , "Account"
             ]
           )

cycled :: [Node String]
cycled = [ ( "Account" , "Account" , [] )
         , root
         , ( "ApiKey" , "ApiKey" , [] )
         , ( "ApiKeys" , "ApiKeys" , [ "ApiKey" ] )
         , ( "BasePathMapping" , "BasePathMapping" , [] )
         , ( "BasePathMappings"
           , "BasePathMappings"
           , [ "BasePathMapping" ]
           )
         , ( "ClientCertificate" , "ClientCertificate" , [] )
         , ( "ClientCertificates"
           , "ClientCertificates"
           , [ "ClientCertificate" ]
           )
         , ( "Deployment" , "Deployment" , [ "Stages" ] )
         , ( "Deployments" , "Deployments" , [ "Deployment" ] )
         , ( "DomainName"
           , "DomainName"
           , [ "BasePathMappings" , "BasePathMapping" ]
           )
         , ( "DomainNames" , "DomainNames" , [ "DomainName" ] )
         , ( "Integration" , "Integration" , [ "IntegrationResponse" ] )
         , ( "IntegrationResponse" , "IntegrationResponse" , [] )
         , ( "Method" , "Method" , [ "MethodResponse" , "Integration" ] )
         , ( "MethodResponse" , "MethodResponse" , [] )
         , ( "Model" , "Model" , [ "Template" ] )
         , ( "Models" , "Models" , [ "Model" ] )
         , ( "Resource" , "Resource" , [ "Method" ] )
         , ( "Resources" , "Resources" , [ "Resource" ] )
         , ( "RestApi"
           , "RestApi"
           , [ "Resources"
             , "Deployments"
             , "Stages"
             , "Models"
             , "Resource"
             , "Deployment"
             , "Stage"
             , "Model"
             ]
           )
         , ( "RestApis" , "RestApis" , [ "RestApi" ] )
         , ( "Stage" , "Stage" , ["ClientCertificate"] )
         , ( "Stages" , "Stages" , [ "Stage" ] )
         , ( "Template" , "Template" , [] )
         , ( "TestInvokeMethodResponse" , "TestInvokeMethodResponse" , [] )
         ]
