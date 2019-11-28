{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- |
--
-- @since 2.2.0.0
module Distribution.PackageDescription.Quirks (patchQuirks) where

import Distribution.Compat.Prelude
import Foreign.Ptr                 (castPtr)
import GHC.Fingerprint             (Fingerprint (..), fingerprintData)
import Prelude ()
import System.IO.Unsafe            (unsafeDupablePerformIO)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Map               as Map

-- | Patch legacy @.cabal@ file contents to allow parsec parser to accept
-- all of Hackage.
--
-- Bool part of the result tells whether the output is modified.
--
-- @since 2.2.0.0
patchQuirks :: BS.ByteString -> (Bool, BS.ByteString)
patchQuirks bs = case Map.lookup (BS.take 256 bs, md5 bs) patches of
    Nothing -> (False, bs)
    Just (post, f)
        | post /= md5 output -> (False, bs)
        | otherwise          -> (True, output)
      where
        output = f bs

md5 :: BS.ByteString -> Fingerprint
md5 bs = unsafeDupablePerformIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    fingerprintData (castPtr ptr) len

-- | 'patches' contains first 256 bytes, pre- and post-fingerprints and a patch function.
patches :: Map.Map (BS.ByteString, Fingerprint) (Fingerprint, BS.ByteString -> BS.ByteString)
patches = Map.fromList
    -- http://hackage.haskell.org/package/unicode-transforms-0.3.3
    -- other-modules: .
    -- ReadP assumed dot is empty line
    [ mk "-- This file has been generated from package.yaml by hpack version 0.17.0.\n--\n-- see: https://github.com/sol/hpack\n\nname:                unicode-transforms\nversion:             0.3.3\nsynopsis:            Unicode normalization\ndescription:         Fast Unic"
         (Fingerprint 15958160436627155571 10318709190730872881)
         (Fingerprint 11008465475756725834 13815629925116264363)
         (bsRemove "  other-modules:\n      .\n") -- TODO: remove traling \n to test structural-diff
    -- http://hackage.haskell.org/package/DSTM-0.1.2
    -- http://hackage.haskell.org/package/DSTM-0.1.1
    -- http://hackage.haskell.org/package/DSTM-0.1
    -- Other Modules: no dash
    -- ReadP parsed as section
    , mk "Name: DSTM\nVersion: 0.1.2\nCopyright: (c) 2010, Frank Kupke\nLicense: LGPL\nLicense-File: LICENSE\nAuthor: Frank Kupke\nMaintainer: frk@informatik.uni-kiel.de\nCabal-Version: >= 1.2.3\nStability: provisional\nSynopsis: A framework for using STM within distributed "
         (Fingerprint 6919263071548559054 9050746360708965827)
         (Fingerprint 17015177514298962556 11943164891661867280)
         (bsReplace "Other modules:" "-- ")
    , mk "Name: DSTM\nVersion: 0.1.1\nCopyright: (c) 2010, Frank Kupke\nLicense: LGPL\nLicense-File: LICENSE\nAuthor: Frank Kupke\nMaintainer: frk@informatik.uni-kiel.de\nCabal-Version: >= 1.2.3\nStability: provisional\nSynopsis: A framework for using STM within distributed "
         (Fingerprint 17313105789069667153 9610429408495338584)
         (Fingerprint 17250946493484671738 17629939328766863497)
         (bsReplace "Other modules:" "-- ")
    , mk "Name: DSTM\nVersion: 0.1\nCopyright: (c) 2010, Frank Kupke\nLicense: LGPL\nLicense-File: LICENSE\nAuthor: Frank Kupke\nMaintainer: frk@informatik.uni-kiel.de\nCabal-Version: >= 1.2.3\nStability: provisional\nSynopsis: A framework for using STM within distributed sy"
         (Fingerprint 10502599650530614586 16424112934471063115)
         (Fingerprint 13562014713536696107 17899511905611879358)
         (bsReplace "Other modules:" "-- ")
    -- http://hackage.haskell.org/package/control-monad-exception-mtl-0.10.3
    , mk "name: control-monad-exception-mtl\nversion: 0.10.3\nCabal-Version:  >= 1.10\nbuild-type: Simple\nlicense: PublicDomain\nauthor: Pepe Iborra\nmaintainer: pepeiborra@gmail.com\nhomepage: http://pepeiborra.github.com/control-monad-exception\nsynopsis: MTL instances f"
         (Fingerprint 18274748422558568404 4043538769550834851)
         (Fingerprint 11395257416101232635 4303318131190196308)
         (bsReplace " default- extensions:" "unknown-section")
    -- http://hackage.haskell.org/package/vacuum-opengl-0.0
    -- \DEL character
    , mk "Name:                vacuum-opengl\nVersion:             0.0\nSynopsis:            Visualize live Haskell data structures using vacuum, graphviz and OpenGL.\nDescription:         \DELVisualize live Haskell data structures using vacuum, graphviz and OpenGL.\n     "
         (Fingerprint 5946760521961682577 16933361639326309422)
         (Fingerprint 14034745101467101555 14024175957788447824)
         (bsRemove "\DEL")
    , mk "Name:                vacuum-opengl\nVersion:             0.0.1\nSynopsis:            Visualize live Haskell data structures using vacuum, graphviz and OpenGL.\nDescription:         \DELVisualize live Haskell data structures using vacuum, graphviz and OpenGL.\n   "
         (Fingerprint 10790950110330119503 1309560249972452700)
         (Fingerprint 1565743557025952928 13645502325715033593)
         (bsRemove "\DEL")
    -- http://hackage.haskell.org/package/ixset-1.0.4
    -- {- comments -}
    , mk "Name:                ixset\nVersion:             1.0.4\nSynopsis:            Efficient relational queries on Haskell sets.\nDescription:\n    Create and query sets that are indexed by multiple indices.\nLicense:             BSD3\nLicense-file:        COPYING\nAut"
         (Fingerprint 11886092342440414185 4150518943472101551)
         (Fingerprint 5731367240051983879 17473925006273577821)
         (bsRemoveStarting "{-")
    -- : after section
    -- http://hackage.haskell.org/package/ds-kanren
    , mk "name:                ds-kanren\nversion:             0.2.0.0\nsynopsis:            A subset of the miniKanren language\ndescription:\n  ds-kanren is an implementation of the <http://minikanren.org miniKanren> language.\n  .\n  == What's in ds-kanren?\n  .\n  ['dis"
         (Fingerprint 2804006762382336875 9677726932108735838)
         (Fingerprint 9830506174094917897 12812107316777006473)
         (bsReplace "Test-Suite test-unify:" "Test-Suite \"test-unify:\"" . bsReplace "Test-Suite test-list-ops:" "Test-Suite \"test-list-ops:\"")
    , mk "name:                ds-kanren\nversion:             0.2.0.1\nsynopsis:            A subset of the miniKanren language\ndescription:\n  ds-kanren is an implementation of the <http://minikanren.org miniKanren> language.\n\nlicense:             MIT\nlicense-file:  "
         (Fingerprint 9130259649220396193 2155671144384738932)
         (Fingerprint 1847988234352024240 4597789823227580457)
         (bsReplace "Test-Suite test-unify:" "Test-Suite \"test-unify:\"" . bsReplace "Test-Suite test-list-ops:" "Test-Suite \"test-list-ops:\"")
    , mk "name:                metric\nversion:             0.1.4\nsynopsis:            Metric spaces.\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Vikram Verma\nmaintainer:          me@vikramverma.com\ncategory:            Data\nbuild-type:"
         (Fingerprint 6150019278861565482 3066802658031228162)
         (Fingerprint 9124826020564520548 15629704249829132420)
         (bsReplace "test-suite metric-tests:" "test-suite \"metric-tests:\"")
    , mk "name:                metric\nversion:             0.2.0\nsynopsis:            Metric spaces.\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Vikram Verma\nmaintainer:          me@vikramverma.com\ncategory:            Data\nbuild-type:"
         (Fingerprint 4639805967994715694 7859317050376284551)
         (Fingerprint 5566222290622325231 873197212916959151)
         (bsReplace "test-suite metric-tests:" "test-suite \"metric-tests:\"")
    , mk "name:          phasechange\ncategory:      Data\nversion:       0.1\nauthor:        G\195\161bor Lehel\nmaintainer:    G\195\161bor Lehel <illissius@gmail.com>\nhomepage:      http://github.com/glehel/phasechange\ncopyright:     Copyright (C) 2012 G\195\161bor Lehel\nlicense:     "
         (Fingerprint 10546509771395401582 245508422312751943)
         (Fingerprint 5169853482576003304 7247091607933993833)
         (bsReplace "impl(ghc >= 7.4):" "erroneous-section" . bsReplace "impl(ghc >= 7.6):" "erroneous-section")
    , mk "Name:                smartword\nSynopsis:            Web based flash card for Word Smart I and II vocabularies\nVersion:             0.0.0.5\nHomepage:            http://kyagrd.dyndns.org/~kyagrd/project/smartword/\nCategory:            Web,Education\nLicense: "
         (Fingerprint 7803544783533485151 10807347873998191750)
         (Fingerprint 1665635316718752601 16212378357991151549)
         (bsReplace "build depends:" "--")
    , mk "name:           shelltestrunner\n-- sync with README.md, ANNOUNCE:\nversion:        1.3\ncategory:       Testing\nsynopsis:       A tool for testing command-line programs.\ndescription:\n shelltestrunner is a cross-platform tool for testing command-line\n program"
         (Fingerprint 4403237110790078829 15392625961066653722)
         (Fingerprint 10218887328390239431 4644205837817510221)
         (bsReplace "other modules:" "--")
    -- &&!
    -- http://hackage.haskell.org/package/hblas-0.3.0.0
    , mk "-- Initial hblas.cabal generated by cabal init.  For further \n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP) \n-- "
         (Fingerprint 8570120150072467041 18315524331351505945)
         (Fingerprint 10838007242302656005 16026440017674974175)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further \n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP) \n-- "
         (Fingerprint 5262875856214215155 10846626274067555320)
         (Fingerprint 3022954285783401045 13395975869915955260)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further \n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP) \n-- "
         (Fingerprint 54222628930951453 5526514916844166577)
         (Fingerprint 1749630806887010665 8607076506606977549)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further\n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP)\n-- fo"
         (Fingerprint 6817250511240350300 15278852712000783849)
         (Fingerprint 15757717081429529536 15542551865099640223)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further\n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP)\n-- fo"
         (Fingerprint 8310050400349211976 201317952074418615)
         (Fingerprint 10283381191257209624 4231947623042413334)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further\n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP)\n-- fo"
         (Fingerprint 7010988292906098371 11591884496857936132)
         (Fingerprint 6158672440010710301 6419743768695725095)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further\r\n-- documentation, see http://haskell.org/cabal/users-guide/\r\n\r\n-- The name of the package.\r\nname:                hblas\r\n\r\n-- The package version.  See the Haskell package versioning policy (PVP)"
         (Fingerprint 2076850805659055833 16615160726215879467)
         (Fingerprint 10634706281258477722 5285812379517916984)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further\r\n-- documentation, see http://haskell.org/cabal/users-guide/\r\n\r\n-- The name of the package.\r\nname:                hblas\r\n\r\n-- The package version.  See the Haskell package versioning policy (PVP)"
         (Fingerprint 11850020631622781099 11956481969231030830)
         (Fingerprint 13702868780337762025 13383526367149067158)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further\n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP)\n-- fo"
         (Fingerprint 13690322768477779172 19704059263540994)
         (Fingerprint 11189374824645442376 8363528115442591078)
         (bsReplace "&&!" "&& !")
    -- flag used, but not defined
    , mk "name:                brainheck\nversion:             0.1.0.2\nsynopsis:            Brainh*ck interpreter in haskell\ndescription:         Brainh*ck interpreter written in haskell and taking advantage of many prominent libraries\nhomepage:            https://gi"
         (Fingerprint 6910727116443152200 15401634478524888973)
         (Fingerprint 16551412117098094368 16260377389127603629)
         (bsReplace "flag(llvm-fast)" "False")
    , mk "name:                brainheck\r\nversion:             0.1.0.2\r\nx-revision: 1\r\nsynopsis:            Brainh*ck interpreter in haskell\r\ndescription:         Brainh*ck interpreter written in haskell and taking advantage of many prominent libraries\r\nhomepage:   "
         (Fingerprint 14320987921316832277 10031098243571536929)
         (Fingerprint 7959395602414037224 13279941216182213050)
         (bsReplace "flag(llvm-fast)" "False")
    , mk "name:                brainheck\r\nversion:             0.1.0.2\r\nx-revision: 2\r\nsynopsis:            Brainh*ck interpreter in haskell\r\ndescription:         Brainh*ck interpreter written in haskell and taking advantage of many prominent libraries\r\nhomepage:   "
         (Fingerprint 3809078390223299128 10796026010775813741)
         (Fingerprint 1127231189459220796 12088367524333209349)
         (bsReplace "flag(llvm-fast)" "False")
    , mk "name:                brainheck\r\nversion:             0.1.0.2\r\nx-revision: 3\r\nsynopsis:            Brainh*ck interpreter in haskell\r\ndescription:         Brainh*ck interpreter written in haskell and taking advantage of many prominent libraries\r\nhomepage:   "
         (Fingerprint 13860013038089410950 12479824176801390651)
         (Fingerprint 4687484721703340391 8013395164515771785)
         (bsReplace "flag(llvm-fast)" "False")
    , mk "name:                wordchoice\nversion:             0.1.0.1\nsynopsis:            Get word counts and distributions\ndescription:         A command line tool to compute the word distribution from various types of document, converting to text with pandoc.\nho"
         (Fingerprint 16215911397419608203 15594928482155652475)
         (Fingerprint 15120681510314491047 2666192399775157359)
         (bsReplace "flag(llvm-fast)" "False")
    , mk "name:                wordchoice\r\nversion:             0.1.0.1\r\nx-revision: 1\r\nsynopsis:            Get word counts and distributions\r\ndescription:         A command line tool to compute the word distribution from various types of document, converting to te"
         (Fingerprint 16593139224723441188 4052919014346212001)
         (Fingerprint 3577381082410411593 11481899387780544641)
         (bsReplace "flag(llvm-fast)" "False")
    , mk "name:                wordchoice\nversion:             0.1.0.2\nsynopsis:            Get word counts and distributions\ndescription:         A command line tool to compute the word distribution from various types of document, converting to text with pandoc.\nho"
         (Fingerprint 9321301260802539374 1316392715016096607)
         (Fingerprint 3784628652257760949 12662640594755291035)
         (bsReplace "flag(llvm-fast)" "False")
    , mk "name:                wordchoice\r\nversion:             0.1.0.2\r\nx-revision: 1\r\nsynopsis:            Get word counts and distributions\r\ndescription:         A command line tool to compute the word distribution from various types of document, converting to te"
         (Fingerprint 2546901804824433337 2059732715322561176)
         (Fingerprint 8082068680348326500 615008613291421947)
         (bsReplace "flag(llvm-fast)" "False")
    , mk "name:                wordchoice\nversion:             0.1.0.3\nsynopsis:            Get word counts and distributions\ndescription:         A command line tool to compute the word distribution from various types of document, converting to text with pandoc.\nho"
         (Fingerprint 2282380737467965407 12457554753171662424)
         (Fingerprint 17324757216926991616 17172911843227482125)
         (bsReplace "flag(llvm-fast)" "False")
    , mk "name:                wordchoice\r\nversion:             0.1.0.3\r\nx-revision: 1\r\nsynopsis:            Get word counts and distributions\r\ndescription:         A command line tool to compute the word distribution from various types of document, converting to te"
         (Fingerprint 12907988890480595481 11078473638628359710)
         (Fingerprint 13246185333368731848 4663060731847518614)
         (bsReplace "flag(llvm-fast)" "False")
    , mk "name:                hw-prim-bits\nversion:             0.1.0.0\nsynopsis:            Primitive support for bit manipulation\ndescription:         Please see README.md\nhomepage:            https://github.com/githubuser/hw-prim-bits#readme\nlicense:            "
         (Fingerprint 12386777729082870356 17414156731912743711)
         (Fingerprint 3452290353395041602 14102887112483033720)
         (bsReplace "flag(sse42)" "False")
    , mk "name:                   hw-prim-bits\nversion:                0.1.0.1\nsynopsis:               Primitive support for bit manipulation\ndescription:            Please see README.md\nhomepage:               https://github.com/githubuser/hw-prim-bits#readme\nlicen"
         (Fingerprint 6870520675313101180 14553457351296240636)
         (Fingerprint 12481021059537696455 14711088786769892762)
         (bsReplace "flag(sse42)" "False")
    -- leading zeros in version digits
    -- https://github.com/haskell-infra/hackage-trustees/issues/128
    -- https://github.com/haskell/cabal/issues/5092
    -- https://github.com/haskell/cabal/issues/5138
    , mk "name:            Sit\nversion:         0.2017.02.26\nbuild-type:      Simple\ncabal-version:   >= 1.8\nlicense:         OtherLicense\nlicense-file:    LICENSE\nauthor:          Anonymous\nmaintainer:      Anonymous\nhomepage:        NONE\ncategory:        Dependent"
         (Fingerprint 8458530898096910998 3228538743646501413)
         (Fingerprint 14470502514907936793 17514354054641875371)
         (bsReplace "0.2017.02.26" "0.2017.2.26")
    , mk "name:            Sit\nversion:         0.2017.05.01\nbuild-type:      Simple\ncabal-version:   >= 1.8\nlicense:         OtherLicense\nlicense-file:    LICENSE\nauthor:          Andreas Abel <andreas.abel@gu.se>\nmaintainer:      Andreas Abel <andreas.abel@gu.se>\n"
         (Fingerprint 1450130849535097473 11742099607098860444)
         (Fingerprint 16679762943850814021 4253724355613883542)
         (bsReplace "0.2017.05.01" "0.2017.5.1")
    , mk "name:            Sit\nversion:         0.2017.05.02\nbuild-type:      Simple\ncabal-version:   >= 1.8\nlicense:         OtherLicense\nlicense-file:    LICENSE\nauthor:          Andreas Abel <andreas.abel@gu.se>\nmaintainer:      Andreas Abel <andreas.abel@gu.se>\n"
         (Fingerprint 297248532398492441 17322625167861324800)
         (Fingerprint 634812045126693280 1755581866539318862)
         (bsReplace "0.2017.05.02" "0.2017.5.2")
    , mk "name:            Sit\nversion:         0.2017.5.02\nx-revision: 1\n-- x-revision:1 see https://github.com/haskell-infra/hackage-trustees/issues/128\nbuild-type:      Simple\ncabal-version:   >= 1.8\nlicense:         OtherLicense\nlicense-file:    LICENSE\nauthor: "
         (Fingerprint 3697869560530373941 3942982281026987312)
         (Fingerprint 14344526114710295386 16386400353475114712)
         (bsReplace "0.2017.5.02" "0.2017.5.2")
    , mk "name:            MiniAgda\nversion:         0.2017.02.18\nbuild-type:      Simple\ncabal-version:   >= 1.22\nlicense:         OtherLicense\nlicense-file:    LICENSE\nauthor:          Andreas Abel and Karl Mehltretter\nmaintainer:      Andreas Abel <andreas.abel@i"
         (Fingerprint 17167128953451088679 4300350537748753465)
         (Fingerprint 12402236925293025673 7715084875284020606)
         (bsReplace "0.2017.02.18" "0.2017.2.18")
    , mk "cabal-version:\n  2.0\nname:\n  fast-downward\nversion:\n  0.1.0.0\nbuild-type:\n  Simple\nsynopsis:\n  Solve classical planning problems (STRIPS/SAS+) using Haskell & Fast Downward.\ndescription:\n  @fast-downward@ is a library for modelling classical planning probl"
         (Fingerprint 11256076039027887363 6867903407496243216)
         (Fingerprint 12159816716813155434 5278015399212299853)
         (bsReplace "1.2.03.0" "1.2.3.0")
    , mk "cabal-version:\r\n  2.0\r\nname:\r\n  fast-downward\r\nversion:\r\n  0.1.0.0\r\nx-revision: \r\n  1\r\nbuild-type:\r\n  Simple\r\nsynopsis:\r\n  Solve classical planning problems (STRIPS/SAS+) using Haskell & Fast Downward.\r\ndescription:\r\n  @fast-downward@ is a library for mode"
         (Fingerprint 9216193973149680231 893446343655828508)
         (Fingerprint 10020169545407746427 1828336750379510675)
         (bsReplace "1.2.03.0" "1.2.3.0")
    , mk "cabal-version:\n  2.0\nname:\n  fast-downward\nversion:\n  0.1.0.1\nbuild-type:\n  Simple\nsynopsis:\n  Solve classical planning problems (STRIPS/SAS+) using Haskell & Fast Downward.\ndescription:\n  @fast-downward@ is a library for modelling classical planning probl"
         (Fingerprint 9899886602574848632 5980433644983783334)
         (Fingerprint 12007469255857289958 8321466548645225439)
         (bsReplace "1.2.03.0" "1.2.3.0")
    , mk "cabal-version:\n  2.0\nname:\n  fast-downward\nversion:\n  0.1.1.0\nbuild-type:\n  Simple\nsynopsis:\n  Solve classical planning problems (STRIPS/SAS+) using Haskell & Fast Downward.\ndescription:\n  @fast-downward@ is a library for modelling classical planning probl"
         (Fingerprint 12694656661460787751 1902242956706735615)
         (Fingerprint 15433152131513403849 2284712791516353264)
         (bsReplace "1.2.03.0" "1.2.3.0")
    -- 9 digits limit
    , mk "Name:                SGplus\nVersion:             1.1\nSynopsis:            (updated) Small geometry library for dealing with vectors and collision detection\nLicense:             BSD3\nLicense-file:        LICENSE\nAuthor:              Neil Brown\nMaintainer:  "
         (Fingerprint 17735649550442248029 11493772714725351354)
         (Fingerprint 9565458801063261772 15955773698774721052)
         (bsReplace "1000000000" "100000000")
    , mk "-- Initial control-dotdotdot.cabal generated by cabal init.  For further \n-- documentation, see http://haskell.org/cabal/users-guide/\n\nname:                control-dotdotdot\nversion:             0.1.0.1\nsynopsis:            Haskell operator\n               "
         (Fingerprint 1514257173776509942 7756050823377346485)
         (Fingerprint 14082092642045505999 18415918653404121035)
         (bsReplace "9223372036854775807" "5")
    , mk "name:                data-foldapp\r\nversion:             0.1.1.0\r\nsynopsis:            Fold function applications. Framework for variadic functions.\r\ndescription:         Fold function applications. Framework for variadic functions.\r\nhomepage:            ht"
         (Fingerprint 4511234156311243251 11701153011544112556)
         (Fingerprint 11820542702491924189 4902231447612406724)
         (bsReplace "9223372036854775807" "999" . bsReplace "9223372036854775807" "999")
    , mk "-- Initial data-list-zigzag.cabal generated by cabal init.  For further \r\n-- documentation, see http://haskell.org/cabal/users-guide/\r\n\r\nname:                data-list-zigzag\r\nversion:             0.1.1.1\r\nsynopsis:            A list but with a balanced en"
         (Fingerprint 12475837388692175691 18053834261188158945)
         (Fingerprint 16279938253437334942 15753349540193002309)
         (bsReplace "9223372036854775807" "999")

    ]
  where
    mk a b c d = ((a, b), (c, d))

-- | Helper to create entries in patches
_makePatchKey :: FilePath -> (BS.ByteString -> BS.ByteString) -> NoCallStackIO ()
_makePatchKey fp transform = do
    contents <- BS.readFile fp
    let output = transform contents
    let Fingerprint hi lo = md5 contents
    let Fingerprint hi' lo' = md5 output
    putStrLn
        $ showString "    , mk "
        . shows (BS.take 256 contents)
        . showString "\n         (Fingerprint "
        . shows hi
        . showString " "
        . shows lo
        . showString ")\n         (Fingerprint "
        . shows hi'
        . showString " "
        . shows lo'
        . showString ")"
        $ ""

-------------------------------------------------------------------------------
-- Patch helpers
-------------------------------------------------------------------------------

bsRemove
    :: BS.ByteString  -- ^ needle
    -> BS.ByteString -> BS.ByteString
bsRemove needle haystack = case BS.breakSubstring needle haystack of
    (h, t) -> BS.append h (BS.drop (BS.length needle) t)

bsReplace
    :: BS.ByteString -- ^ needle
    -> BS.ByteString -- ^ replacement
    -> BS.ByteString -> BS.ByteString
bsReplace needle repl haystack = case BS.breakSubstring needle haystack of
    (h, t)
        | not (BS.null t) -> BS.append h (BS.append repl (BS.drop (BS.length needle) t))
        | otherwise -> haystack

bsRemoveStarting
    :: BS.ByteString  -- ^ needle
    -> BS.ByteString -> BS.ByteString
bsRemoveStarting needle haystack = case BS.breakSubstring needle haystack of
    (h, _) -> h
