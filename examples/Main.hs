-- TODO: Reduce the number of extensions needed
{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, RankNTypes #-}
module Main where

import Snap.Routes

import Data.Text (Text)
import qualified Data.Text as T

-- MAIN
main :: IO ()
main = quickHttpServe $ serveRoute MyRoute

-- The Master Site argument
data MyRoute = MyRoute

-- Generate routing code
mkRoute "MyRoute" [parseRoutes|
/            HomeR  GET
/hello/#Text HelloR   GET
|]

-- HANDLERS

-- Home Handler
getHomeR :: RouteHandler MyRoute
getHomeR _ _ = writeBS "HOME"

-- Hello Handler
getHelloR :: Text -> RouteHandler MyRoute
getHelloR name _ _ = writeText $ T.append "Hello " name
