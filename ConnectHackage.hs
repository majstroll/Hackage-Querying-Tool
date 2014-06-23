module ConnectHackage where

-----------------------------------------------------------------------------
-- 0. Imports and type definitions
-----------------------------------------------------------------------------

import Prelude hiding (writeFile)
import HQuery

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import Control.Exception (IOException(..),catch)

import qualified Distribution.Hackage.DB as DB
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Version
import Distribution.Text
import Distribution.ModuleName hiding (main)
import Distribution.PackageDescription.Parse

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.ByteString (hPut)
import qualified Data.ByteString.Lazy as BS

import Data.List (intersperse,map)

import qualified Language.Haskell.Exts as Exts

import Network.Curl.Download (openURI)

import System.Environment
import System.IO
import System.Directory
import System.FilePath

import Text.ParserCombinators.ReadP

type VMap = M.Map Version GenericPackageDescription

-----------------------------------------------------------------------------
-- 1. Top function definitions
-----------------------------------------------------------------------------


main :: IO ()
main = do
 args <- getArgs
 let pkg = head args
 --(_,_,vmap) <- retrieveLatestVersion pkg
 vmap <- getVMap pkg
 case vmap of
  Nothing    -> return ()
  Just vmap' -> case latestPD vmap' of
   Nothing -> return ()
   Just (_,pd) -> putStrLn $ show $ map display (buildDepends pd)


-- Download and extract all versions of a package.

retrieveAllVersions :: String -> IO (String,[Version],Maybe VMap)
retrieveAllVersions pkg = do
 vmap <- getVMap pkg
 case vmap of
  Nothing -> do putStrLn $ "Package " ++ pkg ++ " could not be found!" 
                return (pkg,[],Nothing)
  Just vmap'  -> do
    let vs = pkgVersions vmap'
    cd <- getCurrentDirectory
    downloadVersionTars cd pkg vs
    extractAllTars cd pkg vs
    return (pkg,vs,vmap)

-- Download and extract versions in range of bounds of a package.

retrieveVersionsInRange :: String -> String -> IO (String,[Version],Maybe VMap)
retrieveVersionsInRange pkg vr = do
 vmap <- getVMap pkg
 case vmap of 
  Nothing    -> do putStrLn $ "Package " ++ pkg ++ " could not be found!" 
                   return (pkg,[],Nothing)
  Just vmap' -> do
    let vs = getVersionsInRange vr (pkgVersions vmap')
    cd <- getCurrentDirectory
    downloadVersionTars cd pkg vs
    extractAllTars cd pkg vs
    return (pkg,vs,vmap)

-- Download and extract a specific version of a package.

retrieveSpecificVersion :: String -> String -> IO (String,[Version],Maybe VMap)
retrieveSpecificVersion pkg vstring =
 case readVersion vstring of
  Nothing -> do putStrLn $ "Cannot parse version " ++ vstring ++ "." 
                return (pkg,[],Nothing)
  Just v -> do vmap <- getVMap pkg
               case vmap of 
                Nothing    -> do putStrLn $ "Package " ++ pkg ++ " could not be found!" 
                                 return (pkg,[],Nothing)
                Just vmap' -> do   cd <- getCurrentDirectory
                                   downloadTar cd pkg v
                                   extractVersionTar cd pkg v
                                   return $ (pkg,[v],vmap)

-- Download and extract the latest version of a package.

retrieveLatestVersion :: String -> IO (String,[Version],Maybe VMap)
retrieveLatestVersion pkg =
  do vmap <- getVMap pkg
     case vmap of 
      Nothing    -> do putStrLn $ "Package " ++ pkg ++ " could not be found!" 
                       return (pkg,[],Nothing)
      Just vmap' -> case latestVersion vmap' of
                     Nothing -> do putStrLn $ "Can not find a version of package " ++ pkg ++ "."
                                   return (pkg,[],Nothing)
                     Just v  -> do cd <- getCurrentDirectory
                                   downloadTar cd pkg v
                                   extractVersionTar cd pkg v
                                   return $ (pkg,[v],vmap)

-----------------------------------------------------------------------------
-- 2. Functions for downloading and unzipping tar files.
-----------------------------------------------------------------------------


-- Extract tarballs of all versions of a package.

extractAllTars :: FilePath -> String -> [Version] -> IO ()
extractAllTars cd pkg vs = mapM_ (extractVersionTar cd pkg) vs

-- Extract the tarball of a version of a package.

extractVersionTar :: FilePath -> String -> Version -> IO ()
extractVersionTar cd pkg v = do
 b <- doesDirectoryExist (getExtrDest cd pkg v)
 if not b
   then gzExtract (combine cd pkg) (getTarLoc cd pkg v) >> return ()
   else putStrLn $ (getTarLoc cd pkg v) ++ " already extracted."

-- Extract a compressed tarball (.tar.gz)

gzExtract :: FilePath -> FilePath -> IO Bool
gzExtract destDir tarLoc = 
 catch (do Tar.unpack destDir . Tar.read . GZip.decompress =<< BS.readFile tarLoc
           putStrLn $ "Extracted " ++ tarLoc ++ " successfully."
           return True)
       (\e -> do let err = show (e :: IOException)
                 putStrLn $ err ++ "\nCould not extract " ++ tarLoc ++ "."
                 return False)
-- Destination folder for extraction of tarball.

getExtrDest :: FilePath -> String -> Version -> String 
getExtrDest cd pkg v = cd  </> pkg </> pkg ++ "-" ++ display v


-- Function for generating filepaths to the modules included in a package version.

genModuleLocations :: FilePath -> String -> Version -> [ModuleName] -> [String]
genModuleLocations cd pkg v ms = undefined

-----------------------------------------------------------------------------
-- 3. Functions for downloading tarballs from Hackage given a modulename.
-----------------------------------------------------------------------------


-- Download all versions of a package as tarballs.

downloadVersionTars :: FilePath -> String -> [Version] -> IO ()
downloadVersionTars cd pkg vs =  mapM_ (downloadTar cd pkg) vs

-- Download a single version of a package as a tarball. Returns true if 
-- successful or tarball already downloaded.

downloadTar :: FilePath -> String -> Version -> IO Bool
downloadTar cd pkg v = do 
 b <- doesFileExist $ getTarLoc cd pkg v
 if b
   then do putStrLn $ pkg ++ "-" ++ display v ++ ".tar.gz already downloaded."
           return True
   else catch
    (do let tarUrl =  genTarUrl pkg v
        tar <- openURI tarUrl 
        case tar of
         Left _   -> return False
         Right bs -> do createDirectoryIfMissing True $ cd </> pkg
                        h <-  openFile (getTarLoc cd pkg v) WriteMode
                        hPut h bs
                        hClose h
                        putStrLn $ "Downloaded " ++ pkg ++ "-" ++ display v ++ 
                                   ".tar.gz successfully."
                        return True)
    (\e -> do let err = show (e :: IOException)
              putStrLn $ err ++ "\n Unable to download " ++ pkg ++ "-" ++ 
                         display v ++ ".tar.gz."
              return False)
-- Generate tarball filepath location.

getTarLoc :: FilePath -> String -> Version -> String 
getTarLoc cd pkg v = cd </> pkg </> pkg ++ 
                     "-" ++ display v ++ ".tar.gz"

-- Generate the URL to a package version tarball.

genTarUrl :: String -> Version -> String
genTarUrl pkg v = "http://hackage.haskell.org/package/" ++ pkg ++
 "-" ++ v' ++ "/" ++ pkg ++  "-" ++ v' ++ 
 ".tar.gz"
 where v' = display v
-----------------------------------------------------------------------------
-- 4. Functions for reading the local Hackage package list.
-----------------------------------------------------------------------------

-- Get a list of all packages on Hackage.

getPkgs :: IO [String]
getPkgs = do
 db <- DB.readHackage 
 return $ DB.keys db 

-- Get the version map for a package (if it exists), else Nothing.

getVMap :: String -> IO (Maybe VMap)
getVMap pkg = do
 vmaps <- getVMaps [pkg] 
 return $ case vmaps of
  []       -> Nothing
  (vm:vms) -> vm

getVMaps :: [String] -> IO [Maybe VMap]
getVMaps pkgs = do 
 db <- DB.readHackage
 return $ map ((flip DB.lookup) db) pkgs

-- Return the VMap for each package on Hackage.

getAllVMaps :: IO [VMap]
getAllVMaps = do
 db <- DB.readHackage
 return $ DB.elems db

-- Returns the package description of the latest version of a package.

latestPD :: VMap -> Maybe (Version,PackageDescription)
latestPD vmap = case latestVersion vmap of
 Nothing -> Nothing
 Just v  -> case M.lookup v vmap of
  Nothing -> error "This really should not happen..."
  Just gpd -> Just $ (v,flattenPackageDescription gpd)  

getPDs :: [PkgName] -> IO (M.Map PkgName [PackageDescription] )
getPDs pkgs = do
 vmaps <- getVMaps pkgs 
 let pds = map (\vmap -> case vmap of 
                 Nothing -> Nothing
                 Just vmap' -> Just $ map flattenPackageDescription (M.elems vmap')) 
           vmaps
 return $ M.fromList $ zip pkgs (catMaybes pds)
 
getLatestPDs :: [PkgName] -> IO [(Version,PackageDescription)]
getLatestPDs pkgs = do
 vmaps <- getVMaps pkgs
 return $ catMaybes $ map latestPD (catMaybes vmaps)

-- Takes a VMap and a version range string and returns all package descriptions
-- from the VMap where the version is in range.

getPDsInRange :: VMap -> String -> [PackageDescription]
getPDsInRange vmap vr = map flattenPackageDescription (M.elems vmap')
  where vmap' = M.filterWithKey (\v _ -> withinRange v vr') vmap
        vr' = read vr :: VersionRange 

getAllLatestPDs :: IO [(Version,PackageDescription)]
getAllLatestPDs = do
 vmaps <- getAllVMaps
 return $ catMaybes $ map latestPD vmaps


-- Retrieve the module names from a possible library.

getModules :: Maybe Library -> [ModuleName]
getModules Nothing    = []
getModules (Just lib) = libModules lib

-- Returns all versions of a package.

pkgVersions :: VMap -> [Version]
pkgVersions vmap = DB.keys vmap 


-- Returns the latest version of a package.

latestVersion :: VMap -> Maybe Version

latestVersion vmap = case pkgVersions vmap of
 []       -> Nothing
 versions -> Just $ maximum versions
 

-----------------------------------------------------------------------------
-- 5. Functions for reading and comparing different versions.
-----------------------------------------------------------------------------


-- Given string representations of the upper and lower version bounds, return
-- all versions in that range.

getVersionsInRange :: String -> [Version] -> [Version]
getVersionsInRange vr vs = getVersionsInRange' vr' vs
 where vr' = read vr :: VersionRange
              
getVersionsInRange' :: VersionRange -> [Version] -> [Version]
getVersionsInRange' vr vs = filter ((flip withinRange) vr) vs

-- Function to read a string representation of a Version. Returns nothing if
-- parsing fails.

readVersion :: String -> Maybe Version
readVersion s = case readP_to_S DB.parseVersion s of
 [] -> Nothing
 vs -> Just $ fst $ last vs


-----------------------------------------------------------------------------
-- 6. Functions for getting relative module filepaths.
-----------------------------------------------------------------------------

getModuleLocations :: String -> [Version] -> Maybe VMap -> M.Map Version [FilePath]
getModuleLocations pkg vs vmap = case vmap of
 Nothing    -> M.empty
 Just vmap' -> generateModuleLocations $ findModules vs vmap'

findModules :: [Version] -> VMap -> [(Version,[ModuleName])]
findModules vlist vmap = map (\v -> (v,findVersionModules v vmap)) vlist

findVersionModules :: Version -> VMap -> [ModuleName]
findVersionModules v vmap = case M.lookup v vmap of
  Nothing  -> []
  Just gpd -> getModules $ library $ flattenPackageDescription gpd

generateModuleLocations :: [(Version,[ModuleName])] -> M.Map Version [FilePath]
generateModuleLocations vmods = 
  buildMap vs (map generateVersionModuleLocations mods) M.empty
    where (vs,mods) = unzip vmods

generateVersionModuleLocations :: [ModuleName] -> [FilePath]
generateVersionModuleLocations mods = map (\mod -> toFilePath mod ++ ".hs") mods

-----------------------------------------------------------------------------
-- X. General help functions
-----------------------------------------------------------------------------


getPkgIdents :: [(Version,[FilePath])] -> IO (M.Map Version [Exts.QName])
getPkgIdents vLocs = do
 vIdents <- mapM (\(v,locs) -> do
   vIds <- getVersionIdents v locs
   return (v,vIds)) vLocs
 return $ M.fromList vIdents

getVersionIdents :: Version -> [FilePath] -> IO [Exts.QName]
getVersionIdents v locs = undefined


-- Takes a a list of keys, a list of values and a starting Map and builds a 
-- Map of it. Not really needed as fromList basically does the same thing.

buildMap :: Ord a => [a] -> [b] -> M.Map a b -> M.Map a b
buildMap [] _  map = map
buildMap _  [] map = map
buildMap (k:ks) (v:vs) map = buildMap ks vs (M.insert k v map)

-----------------------------------------------------------------------------
-- X+1. Functions taking a source/sources and downloads it.
-----------------------------------------------------------------------------

retrieveAndGetLocs :: Source -> IO (M.Map PkgName (M.Map Version [FilePath]))
retrieveAndGetLocs src = do
 res <- retrieveSource src
 let (pkgs,_,_) = unzip3 res
     fpVMaps = map (\(pkg,vs,vmap) -> (getModuleLocations pkg vs vmap)) res
 return $ M.fromList $ zip pkgs fpVMaps


retrieveSources :: [Source] -> IO [(PkgName,[Version],Maybe VMap)]
retrieveSources srcs = do
 res <- mapM retrieveSource srcs
 return $ concat res

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
  res <- mapM (\(pkg,vr) -> retrieveVersionsInRange pkg vr) plist
  return res

