module ConnectHackage where

-----------------------------------------------------------------------------
-- 0. Imports and type definitions
-----------------------------------------------------------------------------

import Prelude hiding (lookup,writeFile)

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

import Data.Map (Map (..),lookup)
import Data.Maybe
import Data.ByteString (hPut)
import qualified Data.ByteString.Lazy as BS

import Data.List (intersperse,map)

import Network.Curl.Download (openURI)

import System.Environment
import System.IO
import System.Directory
import System.FilePath

import Text.ParserCombinators.ReadP

type VMap = Map Version GenericPackageDescription

-----------------------------------------------------------------------------
-- 1. Top function definitions
-----------------------------------------------------------------------------


main :: IO ()
main = do
 args <- getArgs
 let pkg = head args
 _ <- retrieveAllVersions pkg
 return ()


-- Download and extract all versions of a package.

retrieveAllVersions :: String -> IO ([Version],Maybe VMap)
retrieveAllVersions pkg = do
 vmap <- getVmap pkg
 case vmap of
  Nothing -> do putStrLn $ "Package " ++ pkg ++ " could not be found!" 
                return ([],vmap)
  Just vmap'  -> do
    let vs = pkgVersions vmap'
    cd <- getCurrentDirectory
    downloadVersionTars cd pkg vs
    extractAllTars cd pkg vs
    return (vs,vmap)

-- Download and extract versions in range of bounds of a package.

retrieveVersionsInRange :: String -> String -> String -> IO ([Version],Maybe VMap)
retrieveVersionsInRange pkg lb ub = do
 vmap <- getVmap pkg
 case vmap of 
  Nothing    -> do putStrLn $ "Package " ++ pkg ++ " could not be found!" 
                   return ([],vmap)
  Just vmap' -> do
    let vs = getVersionsInRange lb ub (pkgVersions vmap')
    cd <- getCurrentDirectory
    downloadVersionTars cd pkg vs
    extractAllTars cd pkg vs
    return (vs,vmap)

-- Download and extract a specific version of a package.

retrieveSpecificVersion :: String -> String -> IO (Maybe PackageDescription)
retrieveSpecificVersion pkg vstring =
 case readVersion vstring of
  Nothing -> do putStrLn $ "Cannot parse version " ++ vstring ++ "." 
                return Nothing
  Just v -> do vmap <- getVmap pkg
               case vmap of 
                Nothing    -> do putStrLn $ "Package " ++ pkg ++ " could not be found!" 
                                 return Nothing
                Just vmap' -> do case lookup v vmap' of
                                  Nothing  -> do putStrLn ("Version " ++ vstring ++ " of package " ++ pkg ++ 
                                                           " could not be found.") 
                                                 return Nothing
                                  Just gpd -> do cd <- getCurrentDirectory
                                                 downloadTar cd pkg v
                                                 extractVersionTar cd pkg v
                                                 return $ Just $ flattenPackageDescription gpd

-- Download and extract the latest version of a package.

retrieveLatestVersion :: String -> IO (Maybe PackageDescription)
retrieveLatestVersion pkg =
  do vmap <- getVmap pkg
     case vmap of 
      Nothing    -> do putStrLn $ "Package " ++ pkg ++ " could not be found!" 
                       return Nothing
      Just vmap' -> case latestVersion vmap' of
                     Nothing -> do putStrLn $ "Can not find a version of package " ++ pkg ++ "."
                                   return Nothing
                     Just v  -> case lookup v vmap' of
                        Nothing  -> do putStrLn ("Weird. Version " ++ display v ++ " of package " ++ pkg ++ 
                                                 " could not be found.") 
                                       return Nothing
                        Just gpd -> do cd <- getCurrentDirectory
                                       downloadTar cd pkg v
                                       extractVersionTar cd pkg v
                                       return $ Just $ flattenPackageDescription gpd

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
 if b
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
getExtrDest cd pkg v = cd  </> pkg </> pkg </> display v


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

-- Get the version map for a package (if it exists), else Nothing.

getVmap :: String -> IO (Maybe VMap)
getVmap pkg = do
 db <- DB.readHackage
 return $ DB.lookup pkg db

-- Get a list of all packages on Hackage.

getPkgs :: IO [String]
getPkgs = do
 db <- DB.readHackage
 return $ DB.keys db 

-- Retrieve the module names from a possible library.

getModules :: Maybe Library -> [ModuleName]
getModules Nothing    = []
getModules (Just lib) = exposedModules lib


-- Returns all versions of a package.

pkgVersions :: Map Version GenericPackageDescription -> [Version]
pkgVersions vmap = DB.keys vmap 


-- Returns the latest version of a package.

latestVersion :: Map Version GenericPackageDescription 
                 -> Maybe Version

latestVersion vmap = case pkgVersions vmap of
 []       -> Nothing
 versions -> Just $ maximum versions
 

-- Returns the package description of the latest version of a package.

latestPD :: (Map Version GenericPackageDescription) 
                 -> Maybe PackageDescription
latestPD vmap = case latestVersion vmap of
 Nothing -> Nothing
 Just v  -> case lookup v vmap of
  Nothing -> error "This really should not happen..."
  Just gpd -> Just $ flattenPackageDescription gpd  

-----------------------------------------------------------------------------
-- 5. Functions for reading and comparing different versions.
-----------------------------------------------------------------------------


-- Given string representations of the upper and lower version bounds, return
-- all versions in that range.

getVersionsInRange :: String -> String -> [Version] -> [Version]
getVersionsInRange lb ub vs = getVersionsInRange' lb' ub' vs
 where lb' = fromJust $ readVersion lb
       ub' = fromJust $ readVersion ub
              
getVersionsInRange' :: Version -> Version -> [Version] -> [Version]
getVersionsInRange' lb ub vs = filter (\v -> lb <= v && v <= ub) vs

-- Function to read a string representation of a Version. Returns nothing if
-- parsing fails.

readVersion :: String -> Maybe Version
readVersion s = case readP_to_S DB.parseVersion s of
 [] -> Nothing
 vs -> Just $ fst $ last vs

