{-# LANGUAGE GADTs #-}

module HQuery where

import Language.Haskell.Exts
import Distribution.Version

type PkgName = String

-----------------------------------------------------------------------------
-- GADT for a query.
-----------------------------------------------------------------------------

data HQuery a where  
 -- A normal query takes a request and an aribtrary number of sources.
 HQuery            :: Request a -> [Source] -> HQuery [a] 

 -- Combinator of queries.
 CombHQueries      :: HQuery a -> HQuery b -> HQuery (a,b) 

{- 

Use cases:

User wants to make a single request R on a single source S.                 :: HQuery R [S]
User wants to make several requests R1 and R2 on a single source S.         :: HQuery (CombRequest R1 R2) [S]
User wants to make a singe request R on several sources S1 + S2 + S3.       :: HQuery R [S1,S2,S3]
User wants to make several requests where each request have its own source. :: CombHQueries (HQuery R1 S1) (HQuery R2 S2) 
-}

-----------------------------------------------------------------------------
-- GADT for different types of requests.
-----------------------------------------------------------------------------

data Request a where
 -- List the most used packages and their use count.
 MostUsed :: Request [(PkgName,Integer)]

 -- Find all idents in the code. 
 GetIdents :: Source -> Request [(PkgName,QName)]

 -- Try to find language extensions. May not be 100% accurate. 
 GetExtensions :: Source -> Request [(PkgName,Extension)]

 -- Find the latest release for every dependency of a package.
 GetLatestDependencies :: Source -> Request [(PkgName,Version)]

 -- What typeclass definitions can be found in source.
 GetTypeClassDefs :: Source -> Request [(PkgName,[ClassDecl])]

 -- What typeclass instances can be found in source.
 TypeClassUsage :: Source -> Request [(PkgName,[InstDecl])] 

-- Find which packages uses a certain typeclass and for which types.
 FindTypeClass :: String -> Source -> Request [(PkgName,[Type])] 

 -- Try to find a type class instance for a specific type.  
 FindInstance :: String -> String -> Source -> Request [(PkgName,QName)] 

 -- Find the reverse dependencies.
 ReverseDependencies :: Source -> Request [(PkgName,(Version,[(PkgName,VersionRange)]))]
 
 -- Combinator of requests.
 CombRequest :: Request a -> Request b -> Request (a,b) 

-----------------------------------------------------------------------------
-- Data type for describing subsets of Hackage.
-----------------------------------------------------------------------------

data Source = 
 All | -- Hackage in whole.
 PackageList [PkgName] | -- Get all versions of the packages.
 LatestPackageList [PkgName] | -- Get the latest version of the packages.
 PackageVersionList [(PkgName,String)] --Package names + versionranges.
                                                
