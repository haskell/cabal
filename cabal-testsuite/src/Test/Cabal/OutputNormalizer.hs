module Test.Cabal.OutputNormalizer (
    NormalizerEnv (..),
    normalizeOutput,
    ) where

import Data.Monoid (Endo (..))

import Distribution.Version
import Distribution.Text
import Distribution.Pretty
import Distribution.Package
import Distribution.System

import Text.Regex.Base
import Text.Regex.TDFA
import Data.Array ((!))

import qualified Data.Foldable as F

normalizeOutput :: NormalizerEnv -> String -> String
normalizeOutput nenv =
    -- Normalize backslashes to forward slashes to normalize
    -- file paths
    backslashToSlash
    -- Install path frequently has architecture specific elements, so
    -- nub it out
  . resub "Installing (.+) in .+" "Installing \\1 in <PATH>"
    -- Things that look like libraries
  . resub "libHS[A-Za-z0-9.-]+\\.(so|dll|a|dynlib)" "<LIBRARY>"
    -- look for PackageHash directories
  . (if buildOS == Windows
     then resub "\\\\(([A-Za-z0-9_]+)(-[A-Za-z0-9\\._]+)*)-[0-9a-f]{4,64}\\\\"
                "\\\\<PACKAGE>-<HASH>\\\\"
     else id)
  . resub "/(([A-Za-z0-9_]+)(-[A-Za-z0-9\\._]+)*)-[0-9a-f]{4,64}/"
          "/<PACKAGE>-<HASH>/"
    -- This is dumb but I don't feel like pulling in another dep for
    -- string search-replace.  Make sure we do this before backslash
    -- normalization!
  . resub (posixRegexEscape (normalizerGblTmpDir nenv) ++ "[a-z0-9\\.-]+") "<GBLTMPDIR>"
  . resub (posixRegexEscape (normalizerCanonicalGblTmpDir nenv) ++ "[a-z0-9\\.-]+") "<GBLTMPDIR>"
    -- Munge away .exe suffix on filenames (Windows)
  . (if buildOS == Windows then resub "([A-Za-z0-9.-]+)\\.exe" "\\1" else id)
    -- tmp/src-[0-9]+ is tmp\src-[0-9]+ in Windows
  . (if buildOS == Windows then resub (posixRegexEscape "tmp\\src-" ++ "[0-9]+") "<TMPDIR>" else id)
  . resub (posixRegexEscape "tmp/src-" ++ "[0-9]+") "<TMPDIR>"
  . resub (posixRegexEscape (normalizerTmpDir nenv) ++ sameDir) "<ROOT>/"
  . resub (posixRegexEscape (normalizerCanonicalTmpDir nenv) ++ sameDir) "<ROOT>/"
    -- Munge away C:\ prefix on filenames (Windows). We convert C:\ to \.
  . (if buildOS == Windows then resub "([A-Z]):\\\\" "\\\\" else id)
  . appEndo (F.fold (map (Endo . packageIdRegex) (normalizerKnownPackages nenv)))
    -- Look for 0.1/installed-0d6uzW7Ubh1Fb4TB5oeQ3G
    -- These installed packages will vary depending on GHC version
    -- Apply this before packageIdRegex, otherwise this regex doesn't match.
  . resub "[0-9]+(\\.[0-9]+)*/installed-[A-Za-z0-9.+]+"
          "<VERSION>/installed-<HASH>"
    -- incoming directories in the store
  . (if buildOS == Windows then resub "\\\\incoming\\\\new-[0-9]+"
                                      "\\\\incoming\\\\new-<RAND>"
                           else id)
    -- incoming directories in the store
  . resub "/incoming/new-[0-9]+"
          "/incoming/new-<RAND>"
    -- Normalize architecture
  . resub (posixRegexEscape (display (normalizerPlatform nenv))) "<ARCH>"
    -- Some GHC versions are chattier than others
  . resub "^ignoring \\(possibly broken\\) abi-depends field for packages" ""
    -- Normalize the current GHC version.  Apply this BEFORE packageIdRegex,
    -- which will pick up the install ghc library (which doesn't have the
    -- date glob).
  . (if normalizerGhcVersion nenv /= nullVersion
        then resub (posixRegexEscape (display (normalizerGhcVersion nenv))
                        -- Also glob the date, for nightly GHC builds
                        ++ "(\\.[0-9]+)?"
                        -- Also glob the ABI hash, for GHCs which support it
                        ++ "(-[a-z0-9]+)?")
                   "<GHCVER>"
        else id)
  . normalizeBuildInfoJson
  . maybe id normalizePathCmdOutput (normalizerCabalInstallVersion nenv)
  -- hackage-security locks occur non-deterministically
  . resub "(Released|Acquired|Waiting) .*hackage-security-lock\n" ""
  . resub "installed: [0-9]+(\\.[0-9]+)*" "installed: <VERSION>"
  where
    sameDir = "(\\.((\\\\)+|\\/))*"
    packageIdRegex pid =
        resub (posixRegexEscape (display pid) ++ "(-[A-Za-z0-9.-]+)?")
              (prettyShow (packageName pid) ++ "-<VERSION>")

    normalizePathCmdOutput cabalInstallVersion =
      -- clear the ghc path out of all supported output formats
      resub ("compiler-path: " <> posixRegexEscape (normalizerGhcPath nenv))
          "compiler-path: <GHCPATH>"
      -- ghc compiler path is already covered by 'normalizeBuildInfoJson'
      . resub ("{\"cabal-version\":\"" ++ posixRegexEscape (display cabalInstallVersion) ++ "\"")
          "{\"cabal-version\":\"<CABAL_INSTALL_VER>\""
      -- Replace windows filepaths that contain `\\` in the json output.
      -- since we need to escape each '\' ourselves, these 8 backslashes match on exactly 2 backslashes
      -- in the test output.
      -- As the json output is escaped, we need to re-escape the path.
      . resub "\\\\\\\\" "\\"

    -- 'build-info.json' contains a plethora of host system specific information.
    --
    -- This must happen before the root-dir normalisation.
    normalizeBuildInfoJson =
        -- Remove ghc path from show-build-info output
        resub ("\"path\":\"" <> posixRegexEscape (normalizerGhcPath nenv) <> "\"")
          "\"path\":\"<GHCPATH>\""
        -- Remove cabal version output from show-build-info output
      . resub ("{\"cabal-lib-version\":\"" ++ posixRegexEscape (display (normalizerCabalVersion nenv)) ++ "\"")
              "{\"cabal-lib-version\":\"<CABALVER>\""
        -- Remove the package id for stuff such as:
        -- > "-package-id","base-4.14.0.0-<some-hash>"
        -- and replace it with:
        -- > "-package-id","<PACKAGEDEP>"
        --
        -- Otherwise, output can not be properly normalized as on MacOs we remove
        -- vowels from packages to make the names shorter.
        -- E.g. "another-framework-0.8.1.1" -> "nthr-frmwrk-0.8.1.1"
        --
        -- This makes it impossible to have a stable package id, thus remove it completely.
        -- Check manually in your test-cases if the package-id needs to be verified.
      . resub ("\"-package-id\",\"([^\"]*)\"")
              "\"-package-id\",\"<PACKAGEDEP>\""

data NormalizerEnv = NormalizerEnv
    { normalizerTmpDir        :: FilePath
    , normalizerCanonicalTmpDir :: FilePath
    -- ^ May differ from 'normalizerTmpDir', especially e.g. on macos, where
    -- `/var` is a symlink for `/private/var`.
    , normalizerGblTmpDir     :: FilePath
    -- ^ The global temp directory: @/tmp@ on Linux, @/var/folders/...@ on macos
    -- and @\\msys64\\tmp@ on Windows.
    --
    -- Note that on windows the actual path would be @C:\\msys64\\tmp@ but we
    -- drop the @C:@ prefix because this path appears sometimes
    -- twice in the same path in some tests, and the second time it doesn't have a @C:@, so
    -- the logic fails to catch it.
    , normalizerCanonicalGblTmpDir :: FilePath
    -- ^ The canonical version of 'normalizerGblTmpDir', might differ in the same
    -- way as above on macos
    , normalizerGhcVersion    :: Version
    , normalizerGhcPath    :: FilePath
    , normalizerKnownPackages :: [PackageId]
    , normalizerPlatform      :: Platform
    , normalizerCabalVersion  :: Version
    , normalizerCabalInstallVersion :: Maybe Version
    }

posixSpecialChars :: [Char]
posixSpecialChars = ".^$*+?()[{\\|"

posixRegexEscape :: String -> String
posixRegexEscape = concatMap (\c -> if c `elem` posixSpecialChars then ['\\', c] else [c])

backslashToSlash :: String -> String
backslashToSlash = map (\c -> if c == '\\' then '/' else c)

-- From regex-compat-tdfa by Christopher Kuklewicz and shelarcy, BSD-3-Clause
-------------------------

resub :: String {- search -} -> String {- replace -} -> String {- input -} -> String
resub _ _ "" = ""
resub regexp repl inp =
  let compile _i str [] = \ _m ->  (str ++)
      compile i str (("\\", (off, len)) : rest) =
        let i' = off + len
            pre = take (off - i) str
            str' = drop (i' - i) str
        in if null str' then \ _m -> (pre ++) . ('\\' :)
             else \ m -> (pre ++) . ('\\' :) . compile i' str' rest m
      compile i str ((xstr, (off, len)) : rest) =
        let i' = off + len
            pre = take (off - i) str
            str' = drop (i' - i) str
            x = read xstr
        in if null str' then \ m -> (pre++) . (fst (m ! x) ++)
             else \ m -> (pre ++) . (fst (m ! x) ++) . compile i' str' rest m
      compiled :: MatchText String -> String -> String
      compiled = compile 0 repl findrefs where
        -- bre matches a backslash then capture either a backslash or some digits
        bre = mkRegex "\\\\(\\\\|[0-9]+)"
        findrefs = map (\m -> (fst (m ! 1), snd (m ! 0))) (matchAllText bre repl)
      go _i str [] = str
      go i str (m : ms) =
        let (_, (off, len)) = m ! 0
            i' = off + len
            pre = take (off - i) str
            str' = drop (i' - i) str
        in if null str' then pre ++ compiled m ""
             else pre ++ compiled m (go i' str' ms)
  in go 0 inp (matchAllText (mkRegex regexp) inp)

mkRegex :: String -> Regex
mkRegex s = makeRegexOpts opt defaultExecOpt s
  where opt = defaultCompOpt { newSyntax = True, multiline = True }
