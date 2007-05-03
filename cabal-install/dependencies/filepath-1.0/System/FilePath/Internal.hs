--
-- Some short examples:
--
-- You are given a C file, you want to figure out the corresponding object (.o) file:
--
-- @'replaceExtension' file \"o\"@
--
-- Haskell module Main imports Test, you have the file named main:
--
-- @['replaceFileName' path_to_main \"Test\" '<.>' ext | ext <- [\"hs\",\"lhs\"] ]@
--
-- You want to download a file from the web and save it to disk:
--
-- @do let file = 'makeValid' url
--    System.IO.createDirectoryIfMissing True ('takeDirectory' file)@
--
-- You want to compile a Haskell file, but put the hi file under \"interface\"
--
-- @'takeDirectory' file '</>' \"interface\" '</>' ('takeFileName' file \`replaceExtension\` \"hi\"@)
--
-- You want to display a filename to the user, as neatly as possible
--
-- @'makeRelativeToCurrentDirectory' file >>= putStrLn@
--
-- The examples in code format descibed by each function are used to generate
-- tests, and should give clear semantics for the functions.
-----------------------------------------------------------------------------

-- expects CPP definitions for:
--     MODULE_NAME = Posix | Windows
--     IS_WINDOWS  = False | True

module System.FilePath.MODULE_NAME
    (
    -- * Separator predicates
    FilePath,
    pathSeparator, pathSeparators, isPathSeparator,
    searchPathSeparator, isSearchPathSeparator,
    extSeparator, isExtSeparator,

    -- * Path methods (environment $PATH)
    splitSearchPath, getSearchPath,

    -- * Extension methods
    splitExtension,
    takeExtension, replaceExtension, dropExtension, addExtension, hasExtension, (<.>),
    splitExtensions, dropExtensions, takeExtensions,

    -- Note: leave this section to enable some of the tests to work
#ifdef TESTING
    -- * Drive methods
    splitDrive, joinDrive,
    takeDrive, replaceDrive, hasDrive, dropDrive, isDrive,
#endif

    -- * Operations on a FilePath, as a list of directories
    splitFileName,
    takeFileName, replaceFileName, dropFileName,
    takeBaseName, replaceBaseName,
    takeDirectory, replaceDirectory,
    combine, (</>),
    splitPath, joinPath, splitDirectories,

    -- * Low level FilePath operators
    hasTrailingPathSeparator,
    addTrailingPathSeparator,
    dropTrailingPathSeparator,

    -- * File name manipulators
    normalise, equalFilePath,
    makeRelativeToCurrentDirectory, makeRelative,
    isRelative, isAbsolute,
    isValid, makeValid
    )
    where

import Data.Maybe(isJust, fromJust)
import Data.Char(toLower, toUpper)

import System.Environment(getEnv)
import System.Directory(getCurrentDirectory)


infixr 7  <.>
infixr 5  </>





---------------------------------------------------------------------
-- Platform Abstraction Methods (private)

-- | Is the operating system Unix or Linux like
isPosix :: Bool
isPosix = not isWindows

-- | Is the operating system Windows like
isWindows :: Bool
isWindows = IS_WINDOWS


---------------------------------------------------------------------
-- The basic functions

-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > Windows: pathSeparator == '\\'
-- > Posix:   pathSeparator ==  '/'
-- > isPathSeparator pathSeparator
pathSeparator :: Char
pathSeparator = if isWindows then '\\' else '/'

-- | The list of all possible separators.
--
-- > Windows: pathSeparators == ['\\', '/']
-- > Posix:   pathSeparators == ['/']
-- > pathSeparator `elem` pathSeparators
pathSeparators :: [Char]
pathSeparators = if isWindows then "\\/" else "/"

-- | Rather than using @(== 'pathSeparator')@, use this. Test if something
--   is a path separator.
--
-- > isPathSeparator a == (a `elem` pathSeparators)
isPathSeparator :: Char -> Bool
isPathSeparator = (`elem` pathSeparators)


-- | The character that is used to separate the entries in the $PATH environment variable.
--
-- > Windows: searchPathSeparator == ';'
-- > Posix:   searchPathSeparator == ':'
searchPathSeparator :: Char
searchPathSeparator = if isWindows then ';' else ':'

-- | Is the character a file separator?
--
-- > isSearchPathSeparator a == (a == searchPathSeparator)
isSearchPathSeparator :: Char -> Bool
isSearchPathSeparator = (== searchPathSeparator)


-- | File extension character
--
-- > extSeparator == '.'
extSeparator :: Char
extSeparator = '.'

-- | Is the character an extension character?
--
-- > isExtSeparator a == (a == extSeparator)
isExtSeparator :: Char -> Bool
isExtSeparator = (== extSeparator)




---------------------------------------------------------------------
-- Path methods (environment $PATH)

-- | Take a string, split it on the 'searchPathSeparator' character.
--
-- > Windows: splitSearchPath "File1;File2;File3" == ["File1","File2","File3"]
-- > Posix:   splitSearchPath "File1:File2:File3" == ["File1","File2","File3"]
splitSearchPath :: String -> [FilePath]
splitSearchPath = f
    where
    f xs = case break isSearchPathSeparator xs of
           ([],  [])   -> []
           ([],  post) -> f (tail post)
           (pre, [])   -> [pre]
           (pre, post) -> pre : f (tail post)

-- | Get a list of filepaths in the $PATH.
getSearchPath :: IO [FilePath]
getSearchPath = fmap splitSearchPath (getEnv "PATH")


---------------------------------------------------------------------
-- Extension methods

-- | Split on the extension. 'addExtension' is the inverse.
--
-- > uncurry (++) (splitExtension x) == x
-- > uncurry addExtension (splitExtension x) == x
-- > splitExtension "file.txt" == ("file",".txt")
-- > splitExtension "file" == ("file","")
-- > splitExtension "file/file.txt" == ("file/file",".txt")
-- > splitExtension "file.txt/boris" == ("file.txt/boris","")
-- > splitExtension "file.txt/boris.ext" == ("file.txt/boris",".ext")
-- > splitExtension "file/path.txt.bob.fred" == ("file/path.txt.bob",".fred")
-- > splitExtension "file/path.txt/" == ("file/path.txt/","")
splitExtension :: FilePath -> (String, String)
splitExtension x = case d of
                       "" -> (x,"")
                       (y:ys) -> (a ++ reverse ys, y : reverse c)
    where
        (a,b) = splitFileName x
        (c,d) = break isExtSeparator $ reverse b

-- | Get the extension of a file, returns @\"\"@ for no extension, @.ext@ otherwise.
--
-- > takeExtension x == snd (splitExtension x)
-- > takeExtension (addExtension x "ext") == ".ext"
-- > takeExtension (replaceExtension x "ext") == ".ext"
takeExtension :: FilePath -> String
takeExtension = snd . splitExtension

-- | Set the extension of a file, overwriting one if already present.
--
-- > replaceExtension "file.txt" ".bob" == "file.bob"
-- > replaceExtension "file.txt" "bob" == "file.bob"
-- > replaceExtension "file" ".bob" == "file.bob"
-- > replaceExtension "file.txt" "" == "file"
-- > replaceExtension "file.fred.bob" "txt" == "file.fred.txt"
replaceExtension :: FilePath -> String -> FilePath
replaceExtension x y = dropExtension x <.> y

-- | Alias to 'addExtension', for people who like that sort of thing.
(<.>) :: FilePath -> String -> FilePath
(<.>) = addExtension

-- | Remove last extension, and the \".\" preceding it.
--
-- > dropExtension x == fst (splitExtension x)
dropExtension :: FilePath -> FilePath
dropExtension = fst . splitExtension

-- | Add an extension, even if there is already one there.
--   E.g. @addExtension \"foo.txt\" \"bat\" -> \"foo.txt.bat\"@.
--
-- > addExtension "file.txt" "bib" == "file.txt.bib"
-- > addExtension "file." ".bib" == "file..bib"
-- > addExtension "file" ".bib" == "file.bib"
-- > addExtension "/" "x" == "/.x"
-- > takeFileName (addExtension (addTrailingPathSeparator x) "ext") == ".ext"
-- > Windows: addExtension "\\\\share" ".txt" == "\\\\share\\.txt"
addExtension :: FilePath -> String -> FilePath
addExtension file "" = file
addExtension file xs@(x:_) = joinDrive a res
    where
        res = if isExtSeparator x then b ++ xs
              else b ++ [extSeparator] ++ xs

        (a,b) = splitDrive file

-- | Does the given filename have an extension?
--
-- > null (takeExtension x) == not (hasExtension x)
hasExtension :: FilePath -> Bool
hasExtension = any isExtSeparator . takeFileName


-- | Split on all extensions
--
-- > splitExtensions "file.tar.gz" == ("file",".tar.gz")
splitExtensions :: FilePath -> (FilePath, String)
splitExtensions x = (a ++ c, d)
    where
        (a,b) = splitFileName x
        (c,d) = break isExtSeparator b

-- | Drop all extensions
--
-- > not $ hasExtension (dropExtensions x)
dropExtensions :: FilePath -> FilePath
dropExtensions = fst . splitExtensions

-- | Get all extensions
takeExtensions :: FilePath -> String
takeExtensions = snd . splitExtensions



---------------------------------------------------------------------
-- Drive methods

-- | Is the given character a valid drive letter?
-- only a-z and A-Z are letters, not isAlpha which is more unicodey
isLetter :: Char -> Bool
isLetter x = (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')


-- | Split a path into a drive and a path.
--   On Unix, \/ is a Drive.
--
-- > uncurry (++) (splitDrive x) == x
-- > Windows: splitDrive "file" == ("","file")
-- > Windows: splitDrive "c:/file" == ("c:/","file")
-- > Windows: splitDrive "c:\\file" == ("c:\\","file")
-- > Windows: splitDrive "\\\\shared\\test" == ("\\\\shared\\","test")
-- > Windows: splitDrive "\\\\shared" == ("\\\\shared","")
-- > Windows: splitDrive "\\\\?\\UNC\\shared\\file" == ("\\\\?\\UNC\\shared\\","file")
-- > Windows: splitDrive "\\\\?\\d:\\file" == ("\\\\?\\d:\\","file")
-- > Windows: splitDrive "/d" == ("/","d")
-- > Posix:   splitDrive "/test" == ("/","test")
-- > Posix:   splitDrive "//test" == ("//","test")
-- > Posix:   splitDrive "test/file" == ("","test/file")
-- > Posix:   splitDrive "file" == ("","file")
splitDrive :: FilePath -> (FilePath, FilePath)
splitDrive x | isPosix = span (== '/') x

splitDrive x | isJust y = fromJust y
    where y = readDriveLetter x

splitDrive x | isJust y = fromJust y
    where y = readDriveUNC x

splitDrive x | isJust y = fromJust y
    where y = readDriveShare x

splitDrive (x:xs) | isPathSeparator x = addSlash [x] xs

splitDrive x = ("",x)

addSlash a xs = (a++c,d)
    where (c,d) = span isPathSeparator xs

-- http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/naming_a_file.asp
-- "\\?\D:\<path>" or "\\?\UNC\<server>\<share>"
-- a is "\\?\"
readDriveUNC :: FilePath -> Maybe (FilePath, FilePath)
readDriveUNC (s1:s2:'?':s3:xs) | all isPathSeparator [s1,s2,s3] =
    case map toUpper xs of
        ('U':'N':'C':s4:_) | isPathSeparator s4 ->
            let (a,b) = readDriveShareName (drop 4 xs)
            in Just (s1:s2:'?':s3:take 4 xs ++ a, b)
        _ -> case readDriveLetter xs of
                 Just (a,b) -> Just (s1:s2:'?':s3:a,b)
                 Nothing -> Nothing
readDriveUNC x = Nothing

-- c:\
readDriveLetter :: String -> Maybe (FilePath, FilePath)
readDriveLetter (x:':':y:xs) | isLetter x && isPathSeparator y = Just $ addSlash [x,':'] (y:xs)
readDriveLetter (x:':':xs) | isLetter x = Just ([x,':'], xs)
readDriveLetter x = Nothing

-- \\sharename\
readDriveShare :: String -> Maybe (FilePath, FilePath)
readDriveShare (s1:s2:xs) | isPathSeparator s1 && isPathSeparator s2 =
        Just (s1:s2:a,b)
    where (a,b) = readDriveShareName xs
readDriveShare x = Nothing

-- assume you have already seen \\
-- share\bob -> "share","\","bob"
readDriveShareName :: String -> (FilePath, FilePath)
readDriveShareName name = addSlash a b
    where (a,b) = break isPathSeparator name



-- | Join a drive and the rest of the path.
--
-- > uncurry joinDrive (splitDrive x) == x
joinDrive :: FilePath -> FilePath -> FilePath
joinDrive a b | isPosix = a ++ b
              | null a = b
              | null b = a
              | isPathSeparator (last a) = a ++ b
              | otherwise = case a of
                                [a1,':'] | isLetter a1 -> a ++ b
                                _ -> a ++ [pathSeparator] ++ b

-- | Set the drive, from the filepath.
--
-- > replaceDrive x (takeDrive x) == x
replaceDrive :: FilePath -> String -> FilePath
replaceDrive x drv = joinDrive drv (dropDrive x)

-- | Get the drive from a filepath.
--
-- > takeDrive x == fst (splitDrive x)
takeDrive :: FilePath -> FilePath
takeDrive = fst . splitDrive

-- | Delete the drive, if it exists.
--
-- > dropDrive x == snd (splitDrive x)
dropDrive :: FilePath -> FilePath
dropDrive = snd . splitDrive

-- | Does a path have a drive.
--
-- > not (hasDrive x) == null (takeDrive x)
hasDrive :: FilePath -> Bool
hasDrive = not . null . takeDrive


-- | Is an element a drive
isDrive :: FilePath -> Bool
isDrive = null . dropDrive


---------------------------------------------------------------------
-- Operations on a filepath, as a list of directories

-- | Split a filename into directory and file. 'combine' is the inverse.
--
-- > uncurry (++) (splitFileName x) == x
-- > uncurry combine (splitFileName (makeValid x)) == (makeValid x)
-- > splitFileName "file/bob.txt" == ("file/", "bob.txt")
-- > splitFileName "file/" == ("file/", "")
-- > splitFileName "bob" == ("", "bob")
-- > Posix:   splitFileName "/" == ("/","")
-- > Windows: splitFileName "c:" == ("c:","")
splitFileName :: FilePath -> (String, String)
splitFileName x = (c ++ reverse b, reverse a)
    where
        (a,b) = break isPathSeparator $ reverse d
        (c,d) = splitDrive x


-- | Set the filename.
--
-- > replaceFileName (makeValid x) (takeFileName (makeValid x)) == makeValid x
replaceFileName :: FilePath -> String -> FilePath
replaceFileName x y = dropFileName x `combine` y

-- | Drop the filename.
--
-- > dropFileName x == fst (splitFileName x)
dropFileName :: FilePath -> FilePath
dropFileName = fst . splitFileName


-- | Get the file name.
--
-- > takeFileName "test/" == ""
-- > takeFileName x == snd (splitFileName x)
-- > takeFileName (replaceFileName x "fred") == "fred"
-- > takeFileName (combine x "fred") == "fred"
-- > isRelative (takeFileName (makeValid x))
takeFileName :: FilePath -> FilePath
takeFileName = snd . splitFileName

-- | Get the base name, without an extension or path.
--
-- > takeBaseName "file/test.txt" == "test"
-- > takeBaseName "dave.ext" == "dave"
-- > takeBaseName "" == ""
-- > takeBaseName "test" == "test"
-- > takeBaseName (addTrailingPathSeparator x) == ""
-- > takeBaseName "file/file.tar.gz" == "file.tar"
takeBaseName :: FilePath -> String
takeBaseName = dropExtension . takeFileName

-- | Set the base name.
--
-- > replaceBaseName "file/test.txt" "bob" == "file/bob.txt"
-- > replaceBaseName "fred" "bill" == "bill"
-- > replaceBaseName "/dave/fred/bob.gz.tar" "new" == "/dave/fred/new.tar"
-- > replaceBaseName x (takeBaseName x) == x
replaceBaseName :: FilePath -> String -> FilePath
replaceBaseName pth nam = combineAlways a (addExtension nam ext)
    where
        (a,b) = splitFileName pth
        ext = takeExtension b

-- | Is an item either a directory or the last character a path separator?
--
-- > hasTrailingPathSeparator "test" == False
-- > hasTrailingPathSeparator "test/" == True
hasTrailingPathSeparator :: FilePath -> Bool
hasTrailingPathSeparator "" = False
hasTrailingPathSeparator x = isPathSeparator (last x)


-- | Add a trailing file path separator if one is not already present.
--
-- > hasTrailingPathSeparator (addTrailingPathSeparator x)
-- > if hasTrailingPathSeparator x then addTrailingPathSeparator x == x else True
-- > Posix:    addTrailingPathSeparator "test/rest" == "test/rest/"
addTrailingPathSeparator :: FilePath -> FilePath
addTrailingPathSeparator x = if hasTrailingPathSeparator x then x else x ++ [pathSeparator]


-- | Remove any trailing path separators
--
-- > dropTrailingPathSeparator "file/test/" == "file/test"
-- > not (hasTrailingPathSeparator (dropTrailingPathSeparator x)) || isDrive x
-- > Posix:    dropTrailingPathSeparator "/" == "/"
dropTrailingPathSeparator :: FilePath -> FilePath
dropTrailingPathSeparator x =
    if hasTrailingPathSeparator x && not (isDrive x)
    then reverse $ dropWhile isPathSeparator $ reverse x
    else x


-- | Get the directory name, move up one level.
--
-- > Posix:    takeDirectory "/foo/bar/baz" == "/foo/bar"
-- > Posix:    takeDirectory "/foo/bar/baz/" == "/foo/bar/baz"
-- > Windows:  takeDirectory "foo\\bar" == "foo"
-- > Windows:  takeDirectory "foo\\bar\\\\" == "foo\\bar"
-- > Windows:  takeDirectory "C:\\" == "C:\\"
takeDirectory :: FilePath -> FilePath
takeDirectory x = if isDrive file then file
                  else if null res && not (null file) then file
                  else res
    where
        res = reverse $ dropWhile isPathSeparator $ reverse file
        file = dropFileName x

-- | Set the directory, keeping the filename the same.
--
-- > replaceDirectory x (takeDirectory x) `equalFilePath` x
replaceDirectory :: FilePath -> String -> FilePath
replaceDirectory x dir = combineAlways dir (takeFileName x)


-- | Combine two paths, if the second path 'isAbsolute', then it returns the second.
--
-- > combine (takeDirectory (makeValid x)) (takeFileName (makeValid x)) `equalFilePath` makeValid x
-- > Posix:   combine "/" "test" == "/test"
-- > Posix:   combine "home" "bob" == "home/bob"
-- > Windows: combine "home" "bob" == "home\\bob"
combine :: FilePath -> FilePath -> FilePath
combine a b | isAbsolute b = b
            | otherwise = combineAlways a b

-- | Combine two paths, assuming rhs is NOT absolute.
combineAlways :: FilePath -> FilePath -> FilePath
combineAlways a b | null a = b
                  | null b = a
                  | isPathSeparator (last a) = a ++ b
                  | isDrive a = joinDrive a b
                  | otherwise = a ++ [pathSeparator] ++ b


-- | A nice alias for 'combine'.
(</>) :: FilePath -> FilePath -> FilePath
(</>) = combine


-- | Split a path by the directory separator.
--
-- > concat (splitPath x) == x
-- > splitPath "test//item/" == ["test//","item/"]
-- > splitPath "test/item/file" == ["test/","item/","file"]
-- > splitPath "" == []
-- > Windows: splitPath "c:\\test\\path" == ["c:\\","test\\","path"]
-- > Posix:   splitPath "/file/test" == ["/","file/","test"]
splitPath :: FilePath -> [FilePath]
splitPath x = [a | a /= ""] ++ f b
    where
        (a,b) = splitDrive x

        f "" = []
        f x = (a++c) : f d
            where
                (a,b) = break isPathSeparator x
                (c,d) = break (not . isPathSeparator) b

-- | Just as 'splitPath', but don't add the trailing slashes to each element.
--
-- > splitDirectories "test/file" == ["test","file"]
-- > splitDirectories "/test/file" == ["/","test","file"]
-- > joinPath (splitDirectories (makeValid x)) `equalFilePath` makeValid x
-- > splitDirectories "" == []
splitDirectories :: FilePath -> [FilePath]
splitDirectories x =
        if hasDrive x then head xs : f (tail xs)
        else f xs
    where
        xs = splitPath x

        f xs = map g xs
        g x = if null res then x else res
            where res = takeWhile (not . isPathSeparator) x


-- | Join path elements back together.
--
-- > joinPath (splitPath (makeValid x)) == makeValid x

-- Note that this definition on c:\\c:\\, join then split will give c:\\.
joinPath :: [FilePath] -> FilePath
joinPath x = foldr combine "" x






---------------------------------------------------------------------
-- File name manipulators

-- | Equality of two 'FilePath's.
--   If you call @System.Directory.canonicalizePath@
--   first this has a much better chance of working.
--   Note that this doesn't follow symlinks or DOSNAM~1s.
equalFilePath :: FilePath -> FilePath -> Bool
equalFilePath a b = f a == f b
    where
        f x | isWindows = dropTrailSlash $ map toLower $ normalise x
            | otherwise = dropTrailSlash $ normalise x

        dropTrailSlash "" = ""
        dropTrailSlash x | isPathSeparator (last x) = init x
                         | otherwise = x


-- | Contract a filename, based on a relative path.
--
-- > Windows: makeRelative x (x `combine` y) == y || takeDrive x == x
-- > Posix:   makeRelative x (x `combine` y) == y
-- > (isRelative x && makeRelative y x == x) || y `combine` makeRelative y x == x
-- > Windows: makeRelative "C:\\Home" "c:\\home\\bob" == "bob"
-- > Posix:   makeRelative "/Home" "/home/bob" == "/home/bob"
-- > Posix:   makeRelative "/home/" "/home/bob/foo/bar" == "bob/foo/bar"
-- > Posix:   makeRelative "/fred" "bob" == "bob"
-- > Posix:   makeRelative "/file/test" "/file/test/fred" == "fred"
-- > Posix:   makeRelative "/file/test" "/file/test/fred/" == "fred/"
-- > Posix:   makeRelative "some/path" "some/path/a/b/c" == "a/b/c"
makeRelative :: FilePath -> FilePath -> FilePath
makeRelative x y | not (takeDrive x `equalFilePath` takeDrive y) = y
makeRelative x orig = f (dropDrive x) (dropDrive orig)
    where
        f "" y = dropWhile isPathSeparator y
        f x y = let (x1,x2) = g x
                    (y1,y2) = g y
                in if equalFilePath x1 y1 then f x2 y2 else orig

        g x = (dropWhile isPathSeparator a, dropWhile isPathSeparator b)
            where (a,b) = break isPathSeparator $ dropWhile isPathSeparator x


-- | 'makeRelative' the current directory.
makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory x = do
    cur <- getCurrentDirectory
    return $ makeRelative cur x


-- | Normalise a file
--
-- * \/\/ outside of the drive can be made blank
--
-- * \/ -> 'pathSeparator'
--
-- * .\/ -> \"\"
--
-- > Posix:   normalise "/file/\\test////" == "/file/\\test/"
-- > Posix:   normalise "/file/./test" == "/file/test"
-- > Posix:   normalise "/test/file/../bob/fred/" == "/test/file/../bob/fred/"
-- > Posix:   normalise "../bob/fred/" == "../bob/fred/"
-- > Posix:   normalise "./bob/fred/" == "bob/fred/"
-- > Windows: normalise "c:\\file/bob\\" == "C:\\file\\bob\\"
-- > Windows: normalise "\\\\server\\test" == "\\\\server\\test"
-- > Windows: normalise "c:/file" == "C:\\file"
normalise :: FilePath -> FilePath
normalise "" = ""
normalise x = joinDrive (normaliseDrive drv) (f pth) ++ [pathSeparator | isPathSeparator $ last x]
    where
        (drv,pth) = splitDrive x

        f = joinPath . dropDots [] . splitDirectories . propSep

        g x = if isPathSeparator x then pathSeparator else x

        propSep (a:b:xs) | isPathSeparator a && isPathSeparator b = propSep (a:xs)
        propSep (a:xs) | isPathSeparator a = pathSeparator : propSep xs
        propSep (x:xs) = x : propSep xs
        propSep [] = []

        dropDots acc (".":xs) = dropDots acc xs
        dropDots acc (x:xs) = dropDots (x:acc) xs
        dropDots acc [] = reverse acc

normaliseDrive :: FilePath -> FilePath
normaliseDrive x | isPosix = x
normaliseDrive x = if isJust $ readDriveLetter x2 then
                       map toUpper x2
                   else
                       x
    where
        x2 = map repSlash x

        repSlash x = if isPathSeparator x then pathSeparator else x

-- information for validity functions on Windows
-- see http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/naming_a_file.asp
badCharacters = ":*?><|"
badElements = ["CON", "PRN", "AUX", "NUL", "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9", "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9", "CLOCK$"]


-- | Is a FilePath valid, i.e. could you create a file like it?
--
-- > Posix:   isValid "/random_ path:*" == True
-- > Posix:   isValid x == True
-- > Windows: isValid "c:\\test" == True
-- > Windows: isValid "c:\\test:of_test" == False
-- > Windows: isValid "test*" == False
-- > Windows: isValid "c:\\test\\nul" == False
-- > Windows: isValid "c:\\test\\prn.txt" == False
-- > Windows: isValid "c:\\nul\\file" == False
isValid :: FilePath -> Bool
isValid x | isPosix = True
isValid x = not (any (`elem` badCharacters) x2) && not (any f $ splitDirectories x2)
    where
        x2 = dropDrive x
        f x = map toUpper (dropExtensions x) `elem` badElements


-- | Take a FilePath and make it valid; does not change already valid FilePaths.
--
-- > isValid (makeValid x)
-- > if isValid x then makeValid x == x else True
-- > Windows: makeValid "c:\\test:of_test" == "c:\\test_of_test"
-- > Windows: makeValid "test*" == "test_"
-- > Windows: makeValid "c:\\test\\nul" == "c:\\test\\nul_"
-- > Windows: makeValid "c:\\test\\prn.txt" == "c:\\test\\prn_.txt"
-- > Windows: makeValid "c:\\test/prn.txt" == "c:\\test/prn_.txt"
-- > Windows: makeValid "c:\\nul\\file" == "c:\\nul_\\file"
makeValid :: FilePath -> FilePath
makeValid x | isPosix = x
makeValid x = joinDrive drv $ validElements $ validChars pth
    where
        (drv,pth) = splitDrive x

        validChars x = map f x
        f x | x `elem` badCharacters = '_'
            | otherwise = x

        validElements x = joinPath $ map g $ splitPath x
        g x = h (reverse b) ++ reverse a
            where (a,b) = span isPathSeparator $ reverse x
        h x = if map toUpper a `elem` badElements then addExtension (a ++ "_") b else x
            where (a,b) = splitExtensions x


-- | Is a path relative, or is it fixed to the root?
--
-- > Windows: isRelative "path\\test" == True
-- > Windows: isRelative "c:\\test" == False
-- > Posix:   isRelative "test/path" == True
-- > Posix:   isRelative "/test" == False
isRelative :: FilePath -> Bool
isRelative = null . takeDrive


-- | @not . 'isRelative'@
--
-- > isAbsolute x == not (isRelative x)
isAbsolute :: FilePath -> Bool
isAbsolute = not . isRelative
