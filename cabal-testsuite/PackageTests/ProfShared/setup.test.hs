import Test.Cabal.Prelude
import Data.List
import Data.Bifunctor

data BuildWay = StaticWay | DynWay | ProfWay | ProfDynWay
        deriving (Eq, Ord, Show, Read, Enum)

-- Test building with profiling shared support
main = do
    setupTest $ recordMode DoNotRecord $ do
        -- Tests are not robust against missing dynamic libraries yet. Would
        -- be better to fix this.
        skipIfNoSharedLibraries
        -- Skip on GHC 9.0.2 / Ubuntu configuration due to test failure
        ghc902 <- isGhcVersion "== 9.0.2"
        skipIf "GHC 9.0.2 on Linux, https://github.com/haskell/cabal/issues/11387" (ghc902 && isLinux)

    let analyse_result expected r = do

          let ls = lines (resultOutput r)

              library_prefix = "Wanted module build ways(library): "
              executable_prefix = "Wanted module build ways(executable 'Prof'): "

              get_ways prefix = map (drop (length prefix)) (filter (prefix `isPrefixOf`) ls)
              library_ways = read_ways (get_ways library_prefix)
              executable_ways = read_ways (get_ways executable_prefix)

              read_ways raw_ways =
                case raw_ways of
                  -- There should only be one
                  [x] -> (read :: String -> [BuildWay]) x
                  -- Unless there are none, when we don't built the executable for example
                  [] -> []
                  xs -> error "Unexpected number of log lines"

              way = (library_ways, executable_ways)

          unless (bimap (nub . sort) (nub . sort) expected == bimap (nub . sort) (nub . sort) way) $
            assertFailure $ "Expected:" ++ show expected ++ "\n" ++ "Got:" ++ show way

          requireSuccess r
    setupTest $ recordMode DoNotRecord $ do

        has_prof_shared <- hasProfiledSharedLibraries
        has_shared <- hasSharedLibraries

        let v = [ StaticWay ]
            dyn = [ DynWay | has_shared ]
            p_dyn = if has_prof_shared then [ProfDynWay] else p
            p = [ ProfWay ]
            none = []

        let run_test args expected =  do
              setup "configure" args
              res <- setup' "build" []
              analyse_result expected res
              setup "clean" []


        run_test []
          (v <> dyn, v)


        run_test ["--disable-library-vanilla", "--enable-executable-dynamic"]
          (dyn, dyn)

        run_test ["--enable-profiling-shared"]
          (v <> dyn <> p_dyn, v)

        run_test ["--enable-profiling-shared", "--enable-executable-dynamic"]
          (v <> dyn <> p_dyn, dyn)

        run_test ["--enable-executable-dynamic", "--disable-library-vanilla"]
          (dyn, dyn)

        run_test ["--enable-profiling"]
          (v <> dyn <> p, p)

        run_test ["--enable-executable-profiling"]
          (v <> dyn <> p, p)

        run_test ["--enable-executable-profiling", "--enable-executable-dynamic"]
          (v <> dyn <> p_dyn, p_dyn)

        run_test ["--enable-profiling", "--enable-executable-dynamic"]
          (v <> dyn <> p_dyn, p_dyn)

        --v dyn p (p exe)
        run_test ["--enable-library-profiling", "--enable-executable-profiling"]
          (v <> dyn <> p, p)

        run_test ["prof-shared", "--enable-profiling-shared", "--disable-library-vanilla", "--disable-shared"]
          (p_dyn, none)

        -- p p_dyn
        run_test ["prof-shared", "--enable-profiling-shared", "--enable-library-profiling", "--disable-library-vanilla", "--disable-shared"]
          (p <> p_dyn, [])

        -- v p p_dyn
        run_test ["prof-shared","--enable-profiling-shared", "--enable-library-profiling", "--enable-library-vanilla", "--disable-shared"]
          (v <> p <> p_dyn, none)

        -- v dyn p p_dyn
        run_test ["prof-shared", "--enable-profiling-shared", "--enable-library-profiling", "--enable-library-vanilla", "--enable-shared"]
          (v <> dyn <> p <> p_dyn, none)

    let run_cabal_test args expected =  cabalTest $ recordMode DoNotRecord $ do
          has_prof_shared <- hasProfiledSharedLibraries
          has_shared <- hasSharedLibraries
          -- See GHC commit e400b9babdcf11669f963aeec20078fe7ccfca0d
          -- Only installing profiled library is broken on very old ghc-pkg versions
          broken_ghc_pkg <- isGhcVersion "<= 8.6.5"

          let cvt_l StaticWay = [ StaticWay ]
              cvt_l DynWay = [ DynWay | has_shared ]
              cvt_l ProfDynWay = [ProfDynWay | has_prof_shared ]
              cvt_l ProfWay = [ ProfWay ]

          let cvt_e StaticWay =  StaticWay
              cvt_e DynWay = if has_shared then DynWay else error "DynWay"
              cvt_e ProfDynWay = if has_prof_shared then ProfDynWay else ProfWay
              cvt_e ProfWay = ProfWay

          unless (broken_ghc_pkg && (fst expected == [ProfWay])) $ do
            res <- cabal' "v2-build" args
            void $ analyse_result (bimap (concatMap cvt_l) (map cvt_e) expected) res

    run_cabal_test ["--disable-shared"] ([StaticWay], [StaticWay])
    run_cabal_test ["--disable-shared", "--disable-executable-dynamic"] ([StaticWay], [StaticWay])
    run_cabal_test ["--enable-shared"] ([DynWay, StaticWay], [StaticWay])
    run_cabal_test ["--enable-executable-dynamic"] ([DynWay, StaticWay], [DynWay])
    run_cabal_test ["--enable-shared", "--disable-library-vanilla", "--enable-executable-dynamic"] ([DynWay], [DynWay])

    run_cabal_test ["--disable-shared", "--disable-library-vanilla", "--enable-profiling"] ([ProfWay], [ProfWay])

    run_cabal_test ["--disable-shared", "--enable-profiling"] ([ProfWay, StaticWay], [ProfWay])

    run_cabal_test ["--disable-shared", "--enable-profiling-shared", "--enable-profiling"] ([ProfDynWay, ProfWay, StaticWay], [ProfWay])

    run_cabal_test ["--disable-shared", "--enable-profiling", "--enable-profiling-shared", "--enable-executable-dynamic"] ([ProfWay, ProfDynWay, StaticWay], [ProfDynWay])

    run_cabal_test ["--enable-profiling", "--enable-executable-dynamic"] ([ProfDynWay, ProfWay, DynWay, StaticWay], [ProfDynWay])

    run_cabal_test ["prof-shared", "--disable-library-profiling", "--enable-profiling", "--enable-executable-dynamic"] ([ProfDynWay, DynWay, StaticWay], [])
