#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
bootstrap.py - bootstrapping utility for cabal-install.

See bootstrap.py --help for usage instructions.
"""

USAGE = """
This utility is only intended for use in building cabal-install
on a new platform. If you already have a functional (if dated) cabal-install
please rather run `cabal v2-install .`.

Typical usage:

  1. On a system with functional cabal-install, install the same GHC version
     as you will use to bootstrap on the host system.

  2. Build a dependency description file (bootstrap-deps.json) by running:

       bootstrap.py --extract-plan -d bootstrap-deps.json -w /path/to/ghc

  3. Copy `bootstrap-deps.json` to the bootstrapping environment.

  4. On the system you are bootstrapping, run

       bootstrap.py -d bootstrap-deps.json -w /path/to-ghc

"""

from enum import Enum
import hashlib
import logging
import json
from pathlib import Path
import shutil
import subprocess
from textwrap import dedent
from typing import Set, Optional, Dict, List, Tuple, \
                   NewType, BinaryIO, NamedTuple, TypeVar

#logging.basicConfig(level=logging.INFO)

PACKAGES = Path('packages')
PKG_DB = PACKAGES / 'packages.conf'
HACKAGE_TARBALL = Path.home() / '.cabal' / 'packages' / 'hackage.haskell.org' / '01-index.tar'

PackageName = NewType('PackageName', str)
Version = NewType('Version', str)
SHA256Hash = NewType('SHA256Hash', str)

class PackageSource(Enum):
    HACKAGE = 'hackage'
    PREEXISTING = 'pre-existing'
    LOCAL = 'local'

BootstrapDep = NamedTuple('BootstrapDep', [
    ('package', PackageName),
    ('version', Version),
    ('source', PackageSource),
    # source tarball SHA256
    ('src_sha256', Optional[SHA256Hash]),
    # `revision` is only valid when source == HACKAGE.
    ('revision', Optional[int]),
    ('cabal_sha256', Optional[SHA256Hash]),
])

class Compiler:
    def __init__(self, ghc_path: Path):
        if not ghc_path.is_file():
            raise TypeError(f'GHC {ghc_path} is not a file')

        self.ghc_path = ghc_path.resolve()

        info = self._get_ghc_info()
        self.version = info['Project version']
        #self.lib_dir = Path(info['LibDir'])
        #self.ghc_pkg_path = (self.lib_dir / 'bin' / 'ghc-pkg').resolve()
        self.ghc_pkg_path = (self.ghc_path.parent / 'ghc-pkg').resolve()
        if not self.ghc_pkg_path.is_file():
            raise TypeError(f'ghc-pkg {self.ghc_pkg_path} is not a file')

    def _get_ghc_info(self) -> Dict[str,str]:
        from ast import literal_eval
        p = subprocess.run([self.ghc_path, '--info'], stdout=subprocess.PIPE, check=True, encoding='UTF-8')
        out = p.stdout.replace('\n', '').strip()
        return dict(literal_eval(out))

def get_revision(cabal_contents: str) -> int:
    """
    "Parse" the revision field from a .cabal file.
    """
    import re
    m = re.search('x-revision: *([0-9]+)', cabal_contents)
    if m is None:
        return 0
    else:
        return int(m.group(1))

PackageSpec = Tuple[PackageName, Version]

def find_pkg_revisions(index_tarball: Path,
                       pkgs: Set[PackageSpec],
                       ) -> Dict[PackageSpec, Tuple[SHA256Hash, int]]:
    """
    Find the current revision number and hash of the most recent revision of
    the given package name/versions present in an index tarball. Ideally we
    would request this information from Hackage, but sadly this isn't currently
    possible as hackage-server provides no way to do so.

    This interface is a bit awkward but it allows us to find all of the
    information we need from the index in a single pass. This saves a
    significant amount of time.
    """
    from tarfile import TarFile, TarInfo
    tar = TarFile.open(index_tarball)
    interesting_paths = {
        f'{pkg_name}/{version}/{pkg_name}.cabal': (pkg_name, version)
        for pkg_name, version in pkgs
    } # type: Dict[str, PackageSpec]

    logging.info(f'Searching for revisions of {pkgs}')
    result = {} # type: Dict[PackageSpec, Tuple[SHA256Hash, int]]
    while True:
        tar_info = tar.next()
        if tar_info is None:
            break

        spec = interesting_paths.get(tar_info.name)
        if spec is not None:
            f = tar.extractfile(tar_info)
            contents = f.read()
            revision = get_revision(contents.decode('UTF-8'))
            h = hashlib.sha256()
            h.update(contents)
            result[spec] = (SHA256Hash(h.hexdigest()), revision)

    return result

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
    return f'https://hackage.haskell.org/package/{package}-{version}/{package}-{version}.tar.gz'

def package_cabal_url(package: PackageName, version: Version, revision: int) -> str:
    return f'https://hackage.haskell.org/package/{package}-{version}/revision/{revision}.cabal'

def verify_sha256(expected_hash: SHA256Hash, f: Path):
    h = hash_file(hashlib.sha256(), f.open('rb'))
    if h != expected_hash:
        raise BadTarball(f, expected_hash, h)

def fetch_package(package: PackageName,
                  version: Version,
                  src_sha256: SHA256Hash,
                  revision: Optional[int],
                  cabal_sha256: Optional[SHA256Hash],
                  ) -> Path:
    import urllib.request

    # Download source distribution
    out = PACKAGES / (f'{package}-{version}.tar.gz')
    if not out.exists():
        print(f'Fetching {package}-{version}...')
        out.parent.mkdir(parents=True, exist_ok=True)
        url = package_url(package, version)
        with urllib.request.urlopen(url) as resp:
            shutil.copyfileobj(resp, out.open('wb'))

    # Download revised cabal file
    cabal_file = PACKAGES / f'{package}.cabal'
    if revision is not None and not cabal_file.exists():
        assert cabal_sha256 is not None
        url = package_cabal_url(package, version, revision)
        with urllib.request.urlopen(url) as resp:
            shutil.copyfileobj(resp, cabal_file.open('wb'))
            verify_sha256(cabal_sha256, cabal_file)

    verify_sha256(src_sha256, out)
    return out

def read_bootstrap_deps(path: Path) -> List[BootstrapDep]:
    obj = json.load(path.open())

    def from_json(o: dict) -> BootstrapDep:
        o['source'] = PackageSource(o['source'])
        return BootstrapDep(**o)

    return [from_json(dep) for dep in obj['dependencies']]

def write_bootstrap_deps(path: Path, deps: List[BootstrapDep]):
    def to_json(dep: BootstrapDep) -> object:
        return {
            'package': dep.package,
            'version': dep.version,
            'source': dep.source.value,
            'src_sha256': dep.src_sha256,
            'revision': dep.revision,
            'cabal_sha256': dep.cabal_sha256,
        }

    obj = {
        'dependencies': [to_json(dep) for dep in deps],
    }
    json.dump(obj, path.open('w'), indent=2)

def install_dep(dep: BootstrapDep, ghc: Compiler) -> None:
    if dep.source == PackageSource.PREEXISTING:
        # We expect it to be in the compiler's bootstrap package set
        subprocess.run([str(ghc.ghc_pkg_path), 'describe', f'{dep.package}-{dep.version}'],
                       check=True, stdout=subprocess.DEVNULL)
        print(f'Using {dep.package}-{dep.version} from GHC...')
        return

    elif dep.source == PackageSource.HACKAGE:
        assert dep.src_sha256 is not None
        tarball = fetch_package(dep.package, dep.version, dep.src_sha256,
                                dep.revision, dep.cabal_sha256)
        subprocess.run(['tar', 'zxf', tarball.resolve()],
                       cwd=PACKAGES, check=True)
        sdist_dir = PACKAGES / f'{dep.package}-{dep.version}'

        # Update cabal file with revision
        if dep.revision is not None:
            shutil.copyfile(PACKAGES / f'{dep.package}.cabal',
                            sdist_dir / f'{dep.package}.cabal')

    elif dep.source == PackageSource.LOCAL:
        if dep.package == 'Cabal':
            sdist_dir = Path('Cabal').resolve()
        elif dep.package == 'cabal-install':
            sdist_dir = Path('cabal-install').resolve()
        else:
            raise ValueError(f'Unknown local package {dep.package}')

    install_sdist(sdist_dir, ghc)

def install_sdist(sdist_dir: Path, ghc: Compiler):
    prefix = (PACKAGES / 'tmp').resolve()
    configure_args = [
        f'--package-db={PKG_DB.resolve()}',
        f'--prefix={prefix}',
        f'--with-compiler={ghc.ghc_path}',
        f'--with-hc-pkg={ghc.ghc_pkg_path}',
    ]

    def check_call(args: List[str]) -> None:
        subprocess.run(args, cwd=sdist_dir, check=True)

    check_call([str(ghc.ghc_path), '--make', '-package-env', '-', 'Setup'])
    check_call(['./Setup', 'configure'] + configure_args)
    check_call(['./Setup', 'build'])
    check_call(['./Setup', 'install'])

def hash_file(h, f: BinaryIO) -> SHA256Hash:
    while True:
        d = f.read(1024)
        if len(d) == 0:
            return SHA256Hash(h.hexdigest())

        h.update(d)


# Cabal plan.json representation
UnitId = NewType('UnitId', str)
PlanUnit = NewType('PlanUnit', dict)

def read_plan(project_dir: Path) -> Dict[UnitId, PlanUnit]:
    path = project_dir / 'dist-newstyle' / 'cache' / 'plan.json'
    plan = json.load(path.open('rb'))
    return {
        UnitId(c['id']): PlanUnit(c)
        for c in plan['install-plan']
    }

def extract_plan() -> List[BootstrapDep]:
    units = read_plan(Path('.'))
    target_unit = [
        unit
        for unit in units.values()
        if unit['pkg-name'] == 'cabal-install'
        if unit['component-name'] == 'exe:cabal'
    ][0]

    def unit_to_bootstrap_dep(unit: PlanUnit) -> BootstrapDep:
        package = unit['pkg-name']
        version = unit['pkg-version']
        cabal_sha256 = None
        revision = None

        if 'pkg-src' in unit \
                and unit['pkg-src']['type'] == 'local':
            source = PackageSource.LOCAL
        elif unit['type'] == 'configured':
            source = PackageSource.HACKAGE
        elif unit['type'] == 'pre-existing':
            source = PackageSource.PREEXISTING

        return BootstrapDep(package = package,
                            version = version,
                            source = source,
                            src_sha256 = unit.get('pkg-src-sha256'),
                            # these will be handled in a second pass below...
                            revision = None,
                            cabal_sha256 = None,
                            )

    def unit_ids_deps(unit_ids: List[UnitId]) -> List[BootstrapDep]:
        deps = [] # type: List[BootstrapDep]
        for unit_id in unit_ids:
            unit = units[unit_id]
            deps += unit_deps(unit)
            deps.append(unit_to_bootstrap_dep(unit))

        return deps

    def unit_deps(unit: PlanUnit) -> List[BootstrapDep]:
        if unit['type'] == 'pre-existing':
            return []

        deps = [] # type: List[BootstrapDep]
        if 'components' in unit:
            for comp_name, comp in unit['components'].items():
                deps += unit_ids_deps(comp['depends'])
        if 'depends' in unit:
            deps += unit_ids_deps(unit['depends'])

        return deps

    deps = remove_duplicates(unit_deps(target_unit) + [unit_to_bootstrap_dep(target_unit)])

    # Find all of the package revisions and cabal file SHAs.
    pkgs_needing_rev_info = set((dep.package, dep.version)
                                for dep in deps
                                if dep.source == PackageSource.HACKAGE) # type: Set[PackageSpec]
    rev_info = find_pkg_revisions(HACKAGE_TARBALL, pkgs_needing_rev_info)
    def update_rev_info(dep: BootstrapDep) -> BootstrapDep:
        if dep.source == PackageSource.HACKAGE:
            info = rev_info.get((dep.package, dep.version))
            assert info is not None
            cabal_sha256, revision = info
            dep = dep._replace(cabal_sha256=cabal_sha256, revision=revision)
            assert dep.revision is not None
            assert dep.cabal_sha256 is not None

        return dep

    return [update_rev_info(dep) for dep in deps]

a = TypeVar('a')

def remove_duplicates(xs: List[a]) -> List[a]:
    # it's easier to build lists and remove duplicates later than
    # to implement an order-preserving set.
    out = [] # type: List[a]
    for x in xs:
        if x not in out:
            out.append(x)

    return out

def bootstrap(deps: List[BootstrapDep], ghc: Compiler) -> None:
    if not PKG_DB.exists():
        print(f'Creating package database {PKG_DB}')
        PKG_DB.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run([ghc.ghc_pkg_path, 'init', PKG_DB])

    for dep in deps:
        install_dep(dep, ghc)

def main() -> None:
    import argparse
    parser = argparse.ArgumentParser(
        description="bootstrapping utility for cabal-install.",
        epilog = USAGE,
        formatter_class = argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--extract-plan', action='store_true',
                        help='generate bootstrap-deps.json from plan.json')
    parser.add_argument('-d', '--deps', type=Path, default='bootstrap-deps.json',
                        help='bootstrap dependency file')
    parser.add_argument('-w', '--with-compiler', type=Path,
                        help='path to GHC')
    args = parser.parse_args()

    # Find compiler
    if args.with_compiler is None:
        path = shutil.which('ghc')
        if path is None:
            raise ValueError("Couldn't find ghc in PATH")
        ghc = Compiler(Path(path))
    else:
        ghc = Compiler(args.with_compiler)

    print(f'Bootstrapping cabal-install with GHC {ghc.version} at {ghc.ghc_path}...')

    if args.extract_plan:
        subprocess.run(['cabal', 'v2-build', '--dry',
                        '--project-file', 'cabal.project.release',
                        '-w', ghc.ghc_path])
        deps = extract_plan()
        write_bootstrap_deps(args.deps, deps)
        print(f'dependencies written to {args.deps}')
    else:
        print(dedent("""
            DO NOT use this script if you have another recent cabal-install available.
            This script is intended only for bootstrapping cabal-install on new
            architectures.
        """))

        deps = read_bootstrap_deps(args.deps)
        bootstrap(deps, ghc)
        cabal_path = (PACKAGES / 'tmp' / 'bin' / 'cabal').resolve()

        print(dedent(f'''
            Bootstrapping finished!

            The resulting cabal-install executable can be found at

                {cabal_path}

            You now should use this to build a full cabal-install distribution
            using v2-build.
        '''))

if __name__ == '__main__':
    main()
