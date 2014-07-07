module EvalQuery where

import HQuery
import Data.List
import Control.Monad
import qualified Data.Traversable as T
import qualified Data.Map.Strict as M
import ConnectHackage
import Distribution.Version
import Distribution.Text
import Distribution.Package
import Language.Haskell.Exts
import Distribution.PackageDescription


-----------------------------------------------------------------------------
-- 1. Evaluation of the MostUsed request.
-----------------------------------------------------------------------------

mostUsed :: Source -> IO [(PkgName,Int)]
mostUsed src = case src of 
    All                    -> do pds <- getAllLatestPDs
                                 let res = useCount M.empty pds
                                 return $ sort' res
    LatestPackageList pkgs -> do pds <- getAllLatestPDs
                                 let res = useCount' pkgs M.empty pds
                                 return $ sort' res    
    otherwise              -> do putStrLn "MostUsed only defined on sources = ALL | LatestPackagelist pkgs"
                                 return []
  where sort' res = sortBy (\(a,n) (b,n') -> compare n' n) (M.toList res)

-----------------------------------------------------------------------------
-- 1.1 Help functions for the All case of mostUsed
-----------------------------------------------------------------------------

useCount :: M.Map PkgName Int -> [(Version,PackageDescription)] -> M.Map PkgName Int
useCount countMap []       = countMap
useCount countMap (pd:pds) = useCount countMap' pds
 where countMap' = addDepends (buildDepends (snd pd)) countMap 

addDepends :: [Dependency] ->  M.Map PkgName Int -> M.Map PkgName Int
addDepends [] countMap     = countMap
addDepends ((Dependency p _):ds) countMap = 
  addDepends ds (M.insertWith (+) (display p) 1 countMap)


-----------------------------------------------------------------------------
-- 1.2 Help functions for the PackageList case of mostUsed
-----------------------------------------------------------------------------

useCount' :: [PkgName] -> M.Map PkgName Int -> [(Version,PackageDescription)] -> M.Map PkgName Int
useCount' _ countMap []            = countMap
useCount' pkgs countMap (pd:pds)   = useCount' pkgs countMap' pds
 where countMap' = addDepends' (buildDepends $ snd pd) pkgs countMap 

addDepends' :: [Dependency] -> [PkgName] -> M.Map PkgName Int -> M.Map PkgName Int
addDepends' [] _ countMap = countMap
addDepends' ((Dependency p _):ds) pkgs countMap =  
  if elem p' pkgs 
    then addDepends' ds pkgs countMap'
    else addDepends' ds pkgs countMap
  where countMap' = M.insertWith (+) p' 1 countMap
        p'= display p


-----------------------------------------------------------------------------
-- 2. Evaluation of request GetLatestDepencies
-----------------------------------------------------------------------------

getLatestDependencies :: Source -> IO (M.Map PkgName (M.Map Version (M.Map PkgName (Maybe Version))))
getLatestDependencies src = case src of
 PackageList pkgs -> do
  pds <- getPDs pkgs
  res <- T.mapM (\mvmap -> case mvmap of
                 Just vmap -> T.mapM (\pd -> getLatestVersionDependencies pd) vmap
                 Nothing -> return $ M.empty)
          pds  
  return res
 LatestPackageList pkgs -> do
  pds <- getLatestPDs pkgs
  res <- mapM (\mpd -> case mpd of 
           Just (v,pd) -> do
             vdeps <- getLatestVersionDependencies pd
             -- Only a singleton map here as we only take the latest versions of the packages.
             return $ M.singleton v vdeps
           Nothing -> return M.empty )
          pds
  return $ M.fromList $ zip pkgs res  
 otherwise -> undefined


-- Takes a package description and returns a map of the latest release
-- satisfying the dependency rules of each package the given package depends on.

getLatestVersionDependencies :: PackageDescription -> IO (M.Map PkgName (Maybe Version))
getLatestVersionDependencies pd = do
 let bdeps = buildDepends pd 
     (pkgs,vranges) = foldr (\(Dependency p vr) (ps,vrs) -> (display p : ps ,vr : vrs)) ([],[]) bdeps
 vmaps <- getVMaps pkgs
 latestvdeps <- mapM 
  (\(vrange,vmap) ->
        case vmap of 
          Nothing    -> return Nothing 
          Just vmap' -> return $ getLatestVersionInRange vmap' vrange)
  (zip vranges vmaps) 
 return $ buildMap pkgs latestvdeps M.empty


-- Takes a VMap and a version range and returns the latest version in the VMap
-- satisfying the version range.

getLatestVersionInRange :: VMap -> VersionRange -> Maybe Version
getLatestVersionInRange vmap vr = 
  case getVersionsInRange' vr (M.keys vmap) of
    [] -> Nothing
    vs -> Just $ maximum vs
