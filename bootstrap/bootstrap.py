#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
bootstrap.py - bootstrapping utility for cabal-install.

See bootstrap/README.md for usage instructions.
"""

USAGE = """
This utility is only intended for use in building cabal-install
on a new platform. If you already have a functional (if dated) cabal-install
please rather run `cabal install .`.
"""

import argparse
from enum import Enum
import hashlib
import json
from pathlib import Path
import platform
import shutil
import subprocess
import sys
import tempfile
import urllib.request
from textwrap import dedent
from typing import Optional, Dict, List, Tuple, \
                   NewType, BinaryIO, NamedTuple

#logging.basicConfig(level=logging.INFO)

BUILDDIR    = Path('_build')

BINDIR      = BUILDDIR / 'bin'            # binaries go there (--bindir)
DISTDIR     = BUILDDIR / 'dists'          # --builddir
UNPACKED    = BUILDDIR / 'unpacked'       # where we unpack tarballs
TARBALLS    = BUILDDIR / 'tarballs'       # where we download tarballks
PSEUDOSTORE = BUILDDIR / 'pseudostore'    # where we install packages
ARTIFACTS   = BUILDDIR / 'artifacts'      # Where we put the archive
TMPDIR      = BUILDDIR / 'tmp'            #
PKG_DB      = BUILDDIR / 'packages.conf'  # package db

PackageName = NewType('PackageName', str)
Version = NewType('Version', str)
SHA256Hash = NewType('SHA256Hash', str)

class PackageSource(Enum):
    HACKAGE = 'hackage'
    LOCAL = 'local'

BuiltinDep = NamedTuple('BuiltinDep', [
    ('package', PackageName),
    ('version', Version),
])

BootstrapDep = NamedTuple('BootstrapDep', [
    ('package', PackageName),
    ('version', Version),
    ('source', PackageSource),
    # source tarball SHA256
    ('src_sha256', Optional[SHA256Hash]),
    # `revision` is only valid when source == HACKAGE.
    ('revision', Optional[int]),
    ('cabal_sha256', Optional[SHA256Hash]),
    ('flags', List[str]),
    ('component', Optional[str])
])

BootstrapInfo = NamedTuple('BootstrapInfo', [
    ('builtin', List[BuiltinDep]),
    ('dependencies', List[BootstrapDep]),
])

FetchInfo = NamedTuple('FetchInfo', [
    ('url', str),
    ('sha256', SHA256Hash)
])

FetchPlan = Dict[Path, FetchInfo]

local_packages: List[PackageName] = [ "Cabal-syntax"
                                    , "Cabal"
                                    , "Cabal-hooks"
                                    , "Cabal-QuickCheck"
                                    , "Cabal-described"
                                    , "Cabal-tests"
                                    , "Cabal-tree-diff"
                                    , "cabal-install-solver"
                                    , "cabal-install"
                                    , "hooks-exe"
                                    ]

class Compiler:
    def __init__(self, ghc_path: Path):
        if not ghc_path.is_file():
            raise TypeError(f'GHC {ghc_path} is not a file')

        self.ghc_path = ghc_path.resolve()

        exe = ''
        if platform.system() == 'Windows': exe = '.exe'

        info = self._get_ghc_info()
        self.version = info['Project version']
        #self.lib_dir = Path(info['LibDir'])
        #self.ghc_pkg_path = (self.lib_dir / 'bin' / 'ghc-pkg').resolve()
        self.ghc_pkg_path = (self.ghc_path.parent / ('ghc-pkg' + exe)).resolve()
        if not self.ghc_pkg_path.is_file():
            raise TypeError(f'ghc-pkg {self.ghc_pkg_path} is not a file')
        self.hsc2hs_path = (self.ghc_path.parent / ('hsc2hs' + exe)).resolve()
        if not self.hsc2hs_path.is_file():
            raise TypeError(f'hsc2hs {self.hsc2hs_path} is not a file')

    def _get_ghc_info(self) -> Dict[str,str]:
        from ast import literal_eval
        p = subprocess_run([self.ghc_path, '--info'], stdout=subprocess.PIPE, check=True, encoding='UTF-8')
        out = p.stdout.replace('\n', '').strip()
        return dict(literal_eval(out))

PackageSpec = Tuple[PackageName, Version]

class BadTarball(Exception):
    def __init__(self, path: Path, expected_sha256: SHA256Hash, found_sha256: SHA256Hash):
        self.path = path
        self.expected_sha256 = expected_sha256
        self.found_sha256 = found_sha256

    def __str__(self):
        return '\n'.join([
            f'Bad tarball hash: {str(self.path)}',
            f'  expected: {self.expected_sha256}',
            f'  found:    {self.found_sha256}',
        ])

def package_url(package: PackageName, version: Version) -> str:
    return f'http://hackage.haskell.org/package/{package}-{version}/{package}-{version}.tar.gz'

def package_cabal_url(package: PackageName, version: Version, revision: int) -> str:
    return f'http://hackage.haskell.org/package/{package}-{version}/revision/{revision}.cabal'

def verify_sha256(expected_hash: SHA256Hash, f: Path):
    h = hash_file(hashlib.sha256(), f.open('rb'))
    if h != expected_hash:
        raise BadTarball(f, expected_hash, h)

def read_bootstrap_info(path: Path) -> BootstrapInfo:
    obj = json.load(path.open())

    def bi_from_json(o: dict) -> BuiltinDep:
        return BuiltinDep(**o)

    def dep_from_json(o: dict) -> BootstrapDep:
        o['source'] = PackageSource(o['source'])
        return BootstrapDep(**o)

    builtin = [bi_from_json(dep) for dep in obj['builtin'] ]
    deps = [dep_from_json(dep) for dep in obj['dependencies'] ]

    return BootstrapInfo(dependencies=deps, builtin=builtin)

def check_builtin(dep: BuiltinDep, ghc: Compiler) -> None:
    subprocess_run([str(ghc.ghc_pkg_path), 'describe', f'{dep.package}-{dep.version}'],
                   check=True, stdout=subprocess.DEVNULL)
    print(f'Using {dep.package}-{dep.version} from GHC...')
    return

def resolve_dep(dep : BootstrapDep) -> Path:
    if dep.source == PackageSource.HACKAGE:

        tarball = TARBALLS / f'{dep.package}-{dep.version}.tar.gz'
        verify_sha256(dep.src_sha256, tarball)

        cabal_file = TARBALLS / f'{dep.package}.cabal'
        verify_sha256(dep.cabal_sha256, cabal_file)

        UNPACKED.mkdir(parents=True, exist_ok=True)
        shutil.unpack_archive(tarball.resolve(), UNPACKED, 'gztar')
        sdist_dir = UNPACKED / f'{dep.package}-{dep.version}'

        # Update cabal file with revision
        if dep.revision is not None:
            shutil.copyfile(cabal_file, sdist_dir / f'{dep.package}.cabal')

        # We rely on the presence of Setup.hs
        if len(list(sdist_dir.glob('Setup.*hs'))) == 0:
            with open(sdist_dir / 'Setup.hs', 'w') as f:
                f.write('import Distribution.Simple\n')
                f.write('main = defaultMain\n')

    elif dep.source == PackageSource.LOCAL:
        if dep.package in local_packages:
            sdist_dir = Path(dep.package).resolve()
        else:
            raise ValueError(f'Unknown local package {dep.package}')
    return sdist_dir

def install_dep(dep: BootstrapDep, ghc: Compiler) -> None:
    dist_dir = (DISTDIR / f'{dep.package}-{dep.version}').resolve()

    sdist_dir = resolve_dep(dep)

    install_sdist(dist_dir, sdist_dir, ghc, dep.flags, dep.component)

def install_sdist(dist_dir: Path, sdist_dir: Path, ghc: Compiler, flags: List[str], component):
    prefix = PSEUDOSTORE.resolve()
    flags_option = ' '.join(flags)
    setup_dist_dir = dist_dir / 'setup'
    setup = setup_dist_dir / 'Setup'

    build_args = [
        f'--builddir={dist_dir}',
    ]

    configure_args = build_args + [
        f'--package-db={PKG_DB.resolve()}',
        f'--prefix={prefix}',
        f'--bindir={BINDIR.resolve()}',
        f'--extra-prog-path={BINDIR.resolve()}',
        f'--with-compiler={ghc.ghc_path}',
        f'--with-hc-pkg={ghc.ghc_pkg_path}',
        f'--with-hsc2hs={ghc.hsc2hs_path}',
        f'--flags={flags_option}',
        f'{component or ""}'
    ]

    def check_call(args: List[str]) -> None:
        subprocess_run(args, cwd=sdist_dir, check=True)

    setup_dist_dir.mkdir(parents=True, exist_ok=True)

    # Note: we pass -i so GHC doesn't look for anything else
    # This should be fine for cabal-install dependencies.
    check_call([str(ghc.ghc_path), '--make', '-package-env=-', '-i', f'-odir={setup_dist_dir}', f'-hidir={setup_dist_dir}', '-o', setup, 'Setup'])
    check_call([setup, 'configure'] + configure_args)
    check_call([setup, 'build'] + build_args)
    check_call([setup, 'install'] + build_args)

def hash_file(h, f: BinaryIO) -> SHA256Hash:
    while True:
        d = f.read(1024)
        if len(d) == 0:
            return SHA256Hash(h.hexdigest())

        h.update(d)


# Cabal plan.json representation
UnitId = NewType('UnitId', str)
PlanUnit = NewType('PlanUnit', dict)

def bootstrap(info: BootstrapInfo, ghc: Compiler) -> None:
    if not PKG_DB.exists():
        print(f'Creating package database {PKG_DB}')
        PKG_DB.parent.mkdir(parents=True, exist_ok=True)
        subprocess_run([ghc.ghc_pkg_path, 'init', PKG_DB])

    for dep in info.builtin:
        check_builtin(dep, ghc)

    for dep in info.dependencies:
        install_dep(dep, ghc)

# Steps
#######################################################################

def linuxname(i, r):
  i = i.strip() # id
  r = r.strip() # release
  if i == '': return 'linux'
  else: return f"{i}-{r}".lower()

def macname(macver):
  # https://en.wikipedia.org/wiki/MacOS_version_history#Releases
  if macver.startswith('10.12.'): return 'sierra'
  if macver.startswith('10.13.'): return 'high-sierra'
  if macver.startswith('10.14.'): return 'mojave'
  if macver.startswith('10.15.'): return 'catalina'
  if macver.startswith('11.0.'): return 'big-sur'
  else: return macver

def archive_name(cabalversion):
    # Ask platform information
    machine = platform.machine()
    if machine == '': machine = "unknown"

    system = platform.system().lower()
    if system == '': system = "unknown"

    version = system
    if system == 'linux':
        try:
            i = subprocess_run(['lsb_release', '-si'], stdout=subprocess.PIPE, encoding='UTF-8')
            r = subprocess_run(['lsb_release', '-sr'], stdout=subprocess.PIPE, encoding='UTF-8')
            version = linuxname(i.stdout, r.stdout)
        except:
            try:
                with open('/etc/alpine-release') as f:
                    alpinever = f.read().strip()
                    return f'alpine-{alpinever}'
            except:
                pass
    elif system == 'darwin':
        version = 'darwin-' + macname(platform.mac_ver()[0])
    elif system == 'freebsd':
        version = 'freebsd-' + platform.release().lower()

    return f'cabal-install-{cabalversion}-{machine}-{version}'

def make_distribution_archive(cabal_path):
    import tempfile

    print(f'Creating distribution tarball')

    # Get bootstrapped cabal version
    # This also acts as smoke test
    p = subprocess_run([cabal_path, '--numeric-version'], stdout=subprocess.PIPE, check=True, encoding='UTF-8')
    cabalversion = p.stdout.replace('\n', '').strip()

    # Archive name
    basename = ARTIFACTS.resolve() / (archive_name(cabalversion) + '-bootstrapped')

    # In temporary directory, create a directory which we will archive
    tmpdir = TMPDIR.resolve()
    tmpdir.mkdir(parents=True, exist_ok=True)

    rootdir = Path(tempfile.mkdtemp(dir=tmpdir))
    shutil.copy(cabal_path, rootdir / 'cabal')

    # Make archive...
    fmt = 'xztar'
    if platform.system() == 'Windows': fmt = 'zip'
    archivename = shutil.make_archive(basename, fmt, rootdir)

    return archivename

def fetch_from_plan(plan : FetchPlan, output_dir : Path):
  output_dir.resolve()
  output_dir.mkdir(parents=True, exist_ok=True)

  for path in plan:
    output_path = output_dir / path
    url = plan[path].url
    sha = plan[path].sha256
    if not output_path.exists():
      print(f'Fetching {url}...')
      with urllib.request.urlopen(url, timeout = 10) as resp:
        shutil.copyfileobj(resp, output_path.open('wb'))
    verify_sha256(sha, output_path)

def gen_fetch_plan(info : BootstrapInfo) -> FetchPlan :
    sources_dict = {}
    for dep in info.dependencies:
      if not(dep.package in local_packages):
        sources_dict[f"{dep.package}-{dep.version}.tar.gz"] = FetchInfo(package_url(dep.package, dep.version), dep.src_sha256)
        if dep.revision is not None:
          sources_dict[f"{dep.package}.cabal"] = FetchInfo(package_cabal_url(dep.package, dep.version, dep.revision), dep.cabal_sha256)
    return sources_dict

def find_ghc(compiler) -> Compiler:
  if compiler is None:
      path = shutil.which('ghc')
      if path is None:
          raise ValueError("Couldn't find ghc in PATH")
      ghc = Compiler(Path(path))
  else:
      ghc = Compiler(compiler)
  return ghc

def main() -> None:
    parser = argparse.ArgumentParser(
        description="bootstrapping utility for cabal-install.",
        epilog = USAGE,
        formatter_class = argparse.RawDescriptionHelpFormatter)
    parser.add_argument('-d', '--deps', type=Path,
                        help='bootstrap dependency file')
    parser.add_argument('-w', '--with-compiler', type=Path,
                        help='path to GHC')
    parser.add_argument('-s', '--bootstrap-sources', type=Path,
                        help='path to prefetched bootstrap sources archive')
    parser.add_argument('--archive', dest='want_archive', action='store_true')
    parser.add_argument('--no-archive', dest='want_archive', action='store_false')
    parser.set_defaults(want_archive=True)

    subparsers = parser.add_subparsers(dest="command")

    parser_fetch = subparsers.add_parser('build', help='build cabal-install (default)')

    parser_fetch = subparsers.add_parser('fetch', help='fetch all required sources from Hackage (for offline builds)')
    parser_fetch.add_argument('-o','--output', type=Path, default='bootstrap-sources')

    args = parser.parse_args()

    print(dedent("""
        DO NOT use this script if you have another recent cabal-install available.
        This script is intended only for bootstrapping cabal-install on new
        architectures.
    """))

    ghc = find_ghc(args.with_compiler)

    sources_fmt = 'gztar'
    if platform.system() == 'Windows': sources_fmt = 'zip'

    if args.deps is None:
      # We have a tarball with all the required information, unpack it
      if args.bootstrap_sources is not None:
        print(f'Unpacking {args.bootstrap_sources} to {TARBALLS}')
        shutil.unpack_archive(args.bootstrap_sources.resolve(), TARBALLS, sources_fmt)
        args.deps = TARBALLS / 'plan-bootstrap.json'
        print(f"using plan-bootstrap.json ({args.deps}) from {args.bootstrap_sources}")
      else:
        print("The bootstrap script requires a bootstrap plan JSON file.")
        print("See bootstrap/README.md for more information.")
        sys.exit(1)

    info = read_bootstrap_info(args.deps)

    if args.command == 'fetch':
        plan = gen_fetch_plan(info)

        print(f'Fetching sources to bootstrap cabal-install with GHC {ghc.version} at {ghc.ghc_path}...')

        # In temporary directory, create a directory which we will archive
        tmpdir = TMPDIR.resolve()
        tmpdir.mkdir(parents=True, exist_ok=True)

        rootdir = Path(tempfile.mkdtemp(dir=tmpdir))

        fetch_from_plan(plan, rootdir)

        shutil.copyfile(args.deps, rootdir / 'plan-bootstrap.json')

        archivename = shutil.make_archive(args.output, sources_fmt, root_dir=rootdir)

        print(dedent(f"""
            Bootstrap sources saved to {archivename}

            Use these with the command:

            bootstrap.py -w {ghc.ghc_path} -s {archivename}
            """))

    else: # 'build' command (default behaviour)

        print(f'Bootstrapping cabal-install with GHC {ghc.version} at {ghc.ghc_path}...')

        if args.bootstrap_sources is None:
          plan = gen_fetch_plan(info)
          fetch_from_plan(plan, TARBALLS)

        bootstrap(info, ghc)
        cabal_path = (BINDIR / 'cabal').resolve()

        print(dedent(f'''
            Bootstrapping finished!

            The resulting cabal-install executable can be found at

              {cabal_path}
            '''))

        if args.want_archive:
            dist_archive = make_distribution_archive(cabal_path)

            print(dedent(f'''
                The cabal-install executable has been archived for distribution in

                  {dist_archive}
                '''))

        print(dedent(f'''
            You now should use this to build a full cabal-install distribution
            using 'cabal build'.
            '''))

def subprocess_run(args, **kwargs):
    "Like subprocess.run, but also print what we run"

    args_str = ' '.join(map(str, args))
    extras = ''
    if 'cwd' in kwargs:
        extras += f' cwd={kwargs["cwd"]}'
    print(f'bootstrap: running{extras} {args_str}')

    return subprocess.run(args, **kwargs)

if __name__ == '__main__':
    main()
