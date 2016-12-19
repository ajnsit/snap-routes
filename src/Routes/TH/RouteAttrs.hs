{-# LANGUAGE TemplateHaskell, CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Routes.TH.RouteAttrs
    ( mkRouteAttrsInstance
    ) where

import Routes.TH.Types
import Routes.Class
import Language.Haskell.TH.Syntax
import Data.Set (fromList)
import Data.Text (pack)

mkRouteAttrsInstance :: Type -> [ResourceTree a] -> Q Dec
mkRouteAttrsInstance typ ress = do
    clauses <- mapM (goTree id) ress
    return $ instanceD [] (ConT ''RouteAttrs `AppT` typ)
        [ FunD 'routeAttrs $ concat clauses
        ]

goTree :: (Pat -> Pat) -> ResourceTree a -> Q [Clause]
goTree front (ResourceLeaf res) = fmap return $ goRes front res
goTree front (ResourceParent name _check pieces trees) =
    fmap concat $ mapM (goTree front') trees
  where
    ignored = ((replicate toIgnore WildP ++) . return)
    toIgnore = length $ filter isDynamic pieces
    isDynamic Dynamic{} = True
    isDynamic Static{} = False
    front' = front . ConP (mkName name) . ignored

goRes :: (Pat -> Pat) -> Resource a -> Q Clause
goRes front Resource {..} =
    return $ Clause
        [front $ RecP (mkName resourceName) []]
        (NormalB $ VarE 'fromList `AppE` ListE (map toText resourceAttrs))
        []
  where
    toText s = VarE 'pack `AppE` LitE (StringL s)

instanceD :: Cxt -> Type -> [Dec] -> Dec
#if MIN_VERSION_template_haskell(2,11,0)
instanceD = InstanceD Nothing
#else
instanceD = InstanceD
#endif
