module EvalQuery where

import HQuery
import ConnectHackage
import Distribution.Version
import Language.Haskell.Exts

mostUsed :: Source -> [(PkgName,Integer)]
mostUsed vmap = undefined

getIdents :: Source -> [(PkgName,QName)]
getIdents vmap = undefined


-----------------------------------------------------------------------------
-- Functions taking a source and downloads it and return a VMap of it.
-----------------------------------------------------------------------------

retrieveSource :: Source -> IO [(PkgName,[Version],Maybe VMap)]
retrieveSource src = case src of
 All -> do
   pkgs <- getPkgs
   res <- mapM retrieveAllVersions pkgs
   return res

 PackageList plist -> do
   res <- mapM retrieveAllVersions plist
   return res
 
 LatestPackageList plist -> do 
  res <- mapM retrieveLatestVersion plist
  return res

 PackageVersionList plist -> do
  res <- mapM (\(pkg,lb,ub) -> retrieveVersionsInRange pkg lb ub) plist
  return res

{-
....
-}
