module Package where

data PackageIdentifier
    = PackageIdentifier {pkgName::String, pkgVersion::Version}
      deriving (Read, Show, Eq, Ord)

data PackageDescription
    = PackageDescription { package      :: PackageIdentifier,
                           licenese     :: License,
                           copyright    :: String,
                           maintainer   :: String,
                           stability    :: String}

data PackageConfig
    =  PackageConfig {packageDescription :: Packagedescription,
                      buildDepends :: [ Dependency ],
                      sources      :: [ FilePath ],
                      extensions   :: [ Extension ],
                      library      :: String,      -- library name
                      extraLibs    :: [ String ],
                      includeDirs  :: [ FilePath ],
                      includes     :: [ FilePath ],
                      options      :: [ (Compiler, [String]) ]
                     }
