{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{- |
Module      :  Snap.Routes
Copyright   :  (c) Anupam Jain 2016
License     :  MIT (see the file LICENSE)

Maintainer  :  ajnsit@gmail.com
Stability   :  experimental
Portability :  non-portable (uses ghc extensions)

This package provides typesafe URLs for Snap applications.
-}
module Snap.Routes
  ( module Snap.Routes
  , module Routes.Class
  , module Routes.Parse
  , module Snap
  )
where

-- Snap
import Snap

-- Routes
import Routes.Class (Route, RenderRoute(..), ParseRoute(..), RouteAttrs(..))
import Routes.Parse (parseRoutes, parseRoutesNoCheck, parseRoutesFile, parseRoutesFileNoCheck, parseType)
import Routes.TH (mkRenderRouteInstance, mkParseRouteInstance, mkRouteAttrsInstance, mkDispatchClause, ResourceTree(..), MkDispatchSettings(..), defaultGetHandler)

-- Text and Bytestring
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Blaze.ByteString.Builder (toByteString)

import Network.HTTP.Types (encodePath, queryTextToQuery, decodePath, queryToQueryText, Query)

-- TH
import Language.Haskell.TH.Syntax

-- Convenience
import Control.Arrow (second)
import Data.Maybe (fromMaybe)

-- Abstract request data needed for routing
data RequestData master = RequestData
  { currentRoute :: Maybe (Route master)
  , requestPathInfo :: [Text]
  , requestMethod :: ByteString
  }

-- The type of our response handler
type ResponseHandler m = MonadSnap m => m ()

-- The type of our application
type App m sub = RequestData sub -> ResponseHandler m

-- Environment data
data Env sub master = Env
  { envMaster   :: master
  , envSub      :: sub
  , envToMaster :: Route sub -> Route master
  }

-- | A `Handler` generates an App from the master datatype
type RouteHandler sub = forall master m. RenderRoute master => HandlerS m sub master
type HandlerS m sub master = MonadSnap m => Env sub master -> App m sub

-- | Generates everything except actual dispatch
mkRouteData :: String -> [ResourceTree String] -> Q [Dec]
mkRouteData typName routes = do
  let typ = parseType typName
  let rname = mkName $ "_resources" ++ typName
  let resourceTrees = map (fmap parseType) routes
  eres <- lift routes
  let resourcesDec =
          [ SigD rname $ ListT `AppT` (ConT ''ResourceTree `AppT` ConT ''String)
          , FunD rname [Clause [] (NormalB eres) []]
          ]
  rinst <- mkRenderRouteInstance typ resourceTrees
  pinst <- mkParseRouteInstance typ resourceTrees
  ainst <- mkRouteAttrsInstance typ resourceTrees
  return $ concat [ [ainst]
                  , [pinst]
                  , resourcesDec
                  , rinst
                  ]

-- | Generates a 'Routable' instance and dispatch function
mkRouteDispatch :: String -> [ResourceTree String] -> Q [Dec]
mkRouteDispatch typName routes = do
  let typ = parseType typName
  m <- newName "m"
  disp <- mkRouteDispatchClause routes
  return [InstanceD []
          (ConT ''Routable `AppT` VarT m `AppT` typ `AppT` typ)
          [FunD (mkName "dispatcher") [disp]]]

-- | Same as mkRouteDispatch but for subsites
mkRouteSubDispatch :: String -> String -> [ResourceTree a] -> Q [Dec]
mkRouteSubDispatch typName constraint routes = do
  let typ = parseType typName
  disp <- mkRouteDispatchClause routes
  master <- newName "master"
  m <- newName "m"
  -- We don't simply use parseType for GHC 7.8 (TH-2.9) compatibility
  -- ParseType only works on Type (not Pred)
  -- In GHC 7.10 (TH-2.10) onwards, Pred is aliased to Type
  className <- lookupTypeName constraint
  -- Check if this is a classname or a type
  let contract = maybe (error $ "Unknown typeclass " ++ show constraint) (getContract master) className
  return [InstanceD [contract]
          (ConT ''Routable `AppT` VarT m `AppT` typ `AppT` VarT master)
          [FunD (mkName "dispatcher") [disp]]]
  where
    getContract master className =
#if MIN_VERSION_template_haskell(2,10,0)
      ConT className `AppT` VarT master
#else
      ClassP className [VarT master]
#endif

-- Helper that creates the dispatch clause
mkRouteDispatchClause :: [ResourceTree a] -> Q Clause
mkRouteDispatchClause =
  mkDispatchClause MkDispatchSettings
    { mdsRunHandler    = [| runHandler      |]
    , mdsSubDispatcher = [| subDispatcher   |]
    , mdsGetPathInfo   = [| requestPathInfo |]
    , mdsMethod        = [| requestMethod   |]
    , mdsSetPathInfo   = [| setPathInfo     |]
    , mds404           = [| app404          |]
    , mds405           = [| app405          |]
    , mdsGetHandler    = defaultGetHandler
    , mdsUnwrapper     = return
    }


-- | Generates all the things needed for efficient routing.
-- Including your application's `Route` datatype,
-- `RenderRoute`, `ParseRoute`, `RouteAttrs`, and `Routable` instances.
-- Use this for everything except subsites
mkRoute :: String -> [ResourceTree String] -> Q [Dec]
mkRoute typName routes = do
  dat <- mkRouteData typName routes
  disp <- mkRouteDispatch typName routes
  return (disp++dat)

-- TODO: Also allow using the master datatype name directly, instead of a constraint class
-- | Same as mkRoute, but for subsites
mkRouteSub :: String -> String -> [ResourceTree String] -> Q [Dec]
mkRouteSub typName constraint routes = do
  dat <- mkRouteData typName routes
  disp <- mkRouteSubDispatch typName constraint routes
  return (disp++dat)

-- | A `Routable` instance can be used in dispatching.
--   An appropriate instance for your site datatype is
--   automatically generated by `mkRoute`.
class Routable m sub master where
  dispatcher :: HandlerS m sub master

-- | Generates the application middleware from a `Routable` master datatype
routeDispatch :: Routable m master master => Request -> master -> ResponseHandler m
routeDispatch req = customRouteDispatch dispatcher req

-- | Like routeDispatch but generates the application middleware from a custom dispatcher
customRouteDispatch :: HandlerS m master master -> Request -> master -> ResponseHandler m
-- TODO: Should this have master master instead of sub master?
-- TODO: Verify that this plays well with subsites
-- Env master master is converted to Env sub master by subDispatcher
-- Route information is filled in by runHandler
customRouteDispatch customDispatcher req master =
  customDispatcher
      (_masterToEnv master)
      RequestData
        { currentRoute=Nothing
        , requestPathInfo = parsePathInfo $ decodeUtf8 $ rqPathInfo req
        , requestMethod = showMethod $ rqMethod req
        }
  where
    -- Don't want to depend on the Show instance
    showMethod :: Method -> ByteString
    showMethod GET           = "GET"
    showMethod HEAD          = "HEAD"
    showMethod POST          = "POST"
    showMethod PUT           = "PUT"
    showMethod DELETE        = "DELETE"
    showMethod TRACE         = "TRACE"
    showMethod OPTIONS       = "OPTIONS"
    showMethod CONNECT       = "CONNECT"
    showMethod PATCH         = "PATCH"
    showMethod (Method meth) = meth

-- | Render a `Route` and Query parameters to Text
showRouteQuery :: RenderRoute master => Route master -> [(Text,Text)] -> Text
showRouteQuery r q = uncurry _encodePathInfo $ second (map (second Just) . (++ q)) $ renderRoute r

-- | Renders a `Route` as Text
showRoute :: RenderRoute master => Route master -> Text
showRoute = uncurry _encodePathInfo . second (map $ second Just) . renderRoute

_encodePathInfo :: [Text] -> [(Text, Maybe Text)] -> Text
-- Slightly hackish: Convert "" into "/"
_encodePathInfo [] = _encodePathInfo [""]
_encodePathInfo segments = decodeUtf8 . toByteString . encodePath segments . queryTextToQuery

-- | Read a route from Text
-- Returns Nothing if Route reading failed. Just route otherwise
readRoute :: ParseRoute master => Text -> Maybe (Route master)
readRoute = parseRoute . second readQueryString . decodePath . encodeUtf8

-- | Convert a Query to the format expected by parseRoute
readQueryString :: Query -> [(Text, Text)]
readQueryString = map (second (fromMaybe "")) . queryToQueryText

-- PRIVATE

-- Slightly hacky: manually split path into components
parsePathInfo :: Text -> [Text]
-- Hacky, map "" to []
parsePathInfo "" = []
parsePathInfo other = T.splitOn "/" other

-- Set the path info in a RequestData
setPathInfo :: [Text] -> RequestData master -> RequestData master
setPathInfo p reqData = reqData { requestPathInfo = p }

-- Baked in applications that handle 404 and 405 errors
-- On no matching route, skip to next application
app404 :: HandlerS m sub master
app404 _env _rd = pass

-- On matching route, but no matching http method, skip to next application
-- This allows a later route to handle methods not implemented by the previous routes
app405 :: HandlerS m sub master
app405 _env _rd = pass

-- Run a route handler function
-- Currently all this does is populate the route into RequestData
-- But it may do more in the future
runHandler
    :: MonadSnap m
    => HandlerS m sub master
    -> Env sub master
    -> Maybe (Route sub)
    -> App m sub
runHandler h env rout reqdata = h env reqdata{currentRoute=rout}

-- Run a route subsite handler function
subDispatcher
    :: MonadSnap m
    => Routable m sub master
    => (HandlerS m sub master -> Env sub master -> Maybe (Route sub) -> App m sub)
    -> (master -> sub)
    -> (Route sub -> Route master)
    -> Env master master
    -> App m master
subDispatcher _runhandler getSub toMasterRoute env reqData = dispatcher env' reqData'
  where
    env' = _envToSub getSub toMasterRoute env
    reqData' = reqData{currentRoute=Nothing}
    -- qq (k,mv) = (decodeUtf8 k, maybe "" decodeUtf8 mv)
    -- req = snapReq reqData

_masterToEnv :: master -> Env master master
_masterToEnv master = Env master master id

_envToSub :: (master -> sub) -> (Route sub -> Route master) -> Env master master -> Env sub master
_envToSub getSub toMasterRoute env = Env master sub toMasterRoute
  where
    master = envMaster env
    sub = getSub master

-- Convenience function to hook in a route
serveRoute :: (Routable m master master, MonadSnap m) => master -> m ()
serveRoute master = getRequest >>= flip routeDispatch master
