#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
release.py - build the release of cabal-install"
"""

USAGE = """
This utility is only intended for use in building cabal-install
binary distributions on platforms with existing cabal-install.
"""

# TODO, by using v2-install we build from sdists, which is good
# But we cannot get plan.json, to get dependency-receipt
# https://github.com/haskell/cabal/issues/6988

# TODO provide DWARF enabled builds?

# We don't build documentation, its well built by readthedocs.
# We cannot make tarball, as the private key for signing should be on the builder machine.
# We also don't use caching, this way we have one moving part less.

import os
import platform
import shutil
import subprocess

from pathlib import Path
from textwrap import dedent
from typing import NamedTuple

DEFAULT_INDEXSTATE='2020-07-23T11:14:13Z'

Args = NamedTuple('Args', [
    ('compiler', Path),
    ('cabal', Path),
    ('indexstate', str),
    ('rootdir', Path),
    ('builddir', Path),
    ('static', bool),
    ('ofdlocking', bool),
])

# utils
#######################################################################

def subprocess_run(args, **kwargs):
    "Like subprocess.run, but also print what we run"

    args = list(map(str, args)) # For Windows, https://www.scivision.dev/windows-python-pathlib-subprocess-bug/
    args_str = ' '.join(map(str, args))
    extras = ''
    if 'cwd' in kwargs:
        extras += f' cwd={kwargs["cwd"]}'
    print(f'%{extras} {args_str}')

    return subprocess.run(args, **kwargs)

# archive name
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
    machine = platform.machine().lower()
    if machine == '': machine = "unknown"
    if machine == 'amd64': machine = "x86_64"

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
                    version = f'alpine-{alpinever}'
            except:
                pass
    elif system == 'darwin':
        version = 'darwin-' + macname(platform.mac_ver()[0])
    elif system == 'freebsd':
        version = 'freebsd-' + platform.release().lower()

    return f'cabal-install-{cabalversion}-{machine}-{version}'

# Steps
#######################################################################

def step_makedirs(args: Args):
    (args.builddir / 'bin').mkdir(parents=True, exist_ok=True)
    (args.builddir / 'cabal').mkdir(parents=True, exist_ok=True)

# 57936384
def step_config(args: Args):
    splitsections = ''
    if platform.system() == 'Linux':
        splitsections = 'split-sections: True'

    # https://github.com/Mistuke/CabalChoco/blob/d0e1d2fd8ce13ab4271c4b906ca0bde3b710a310/3.2.0.0/cabal/tools/chocolateyInstall.ps1#L289
    extraprogpath = str(args.builddir / 'bin')
    if platform.system() == 'Windows':
        msysbin = Path('C:\\tools\\msys64\\usr\\bin')
        if msysbin.is_dir():
            extraprogpath = extraprogpath + "," + str(msysbin)

    config = dedent(f"""
        repository hackage.haskell.org
          url: http://hackage.haskell.org/

        remote-build-reporting: anonymous
        remote-repo-cache:      {args.builddir}/cabal/packages

        write-ghc-environment-files: never
        install-method: copy
        overwrite-policy: always

        documentation: False
        {splitsections}

        build-summary:     {args.builddir}/cabal/logs/build.log
        installdir:        {args.builddir}/bin
        logs-dir:          {args.builddir}/cabal/logs
        store-dir:         {args.builddir}/cabal/store
        symlink-bindir:    {args.builddir}/bin
        world-file:        {args.builddir}/cabal/world
        extra-prog-path:   {extraprogpath}

        jobs: 1

        install-dirs user
          prefix: {args.builddir}
    """)

    with open(args.builddir / 'cabal' / 'config', 'w') as f:
        f.write(config)

    cabal_project_local =''
    if args.static:
        # --enable-executable-static doesn't affect "non local" executables, as in v2-install project
        cabal_project_local += dedent("""
            package cabal-install
                executable-static: True
        """)
    cabal_project_local += dedent(f"""
        package lukko
            flags: {'+' if args.ofdlocking else '-'}ofd_locking
    """)

    with open(args.rootdir / 'cabal.project.release.local', 'w') as f:
        f.write(cabal_project_local)

def make_env(args: Args):
    env = {
        'PATH': os.environ['PATH'],
        'CABAL_DIR': str(args.builddir),
        'CABAL_CONFIG': str(args.builddir / 'cabal' / 'config'),
    }
    # https://superuser.com/questions/1079017/is-there-an-environment-variable-for-c-users-username-appdata-local-temp-in-w
    # In particular, we surely need 'TEMP'
    # And also SYSTEMROOT to make 'curl' work!
    envvars = [
        'LANG',
        'HOME', 'HOMEDRIVE', 'HOMEPATH',
        'TMP', 'TEMP',
        'PATHEXT', 'APPDATA', 'LOCALAPPDATA', 'SYSTEMROOT',
    ]
    for key in envvars:
        if key in os.environ:
            env[key] = os.environ[key]

    return env

def step_cabal_update(args: Args):
    env = make_env(args)
    subprocess_run([
        args.cabal,
        'v2-update',
        '-v',
        f'--index-state={args.indexstate}',
    ], check=True, env=env)

def step_cabal_install(args: Args):
    env = make_env(args)
    subprocess_run([
        args.cabal,
        'v2-install',
        '-v',
        'cabal-install:exe:cabal',
        '--project-file=cabal.project.release',
        f'--with-compiler={args.compiler}',
    ], check=True, env=env)

def step_make_archive(args: Args):
    import tempfile

    print(f'Creating distribution tarball')

    # Get bootstrapped cabal version
    # This also acts as smoke test
    cabal_path = args.builddir / 'bin' / 'cabal'
    if platform.system() == 'Windows':
        cabal_path = cabal_path.with_suffix('.exe')
    p = subprocess_run([cabal_path, '--numeric-version'], stdout=subprocess.PIPE, check=True, encoding='UTF-8')
    cabalversion = p.stdout.replace('\n', '').strip()

    # Archive name
    name = archive_name(cabalversion)
    if args.static:
        name = name + "-static"
    if not args.ofdlocking:
        name = name + "-noofd"

    basename = args.builddir / 'artifacts' / name

    # In temporary directory, create a directory which we will archive
    tmpdir = args.builddir / 'tmp'
    tmpdir.mkdir(parents=True, exist_ok=True)

    rootdir = Path(tempfile.mkdtemp(dir=tmpdir))
    shutil.copy(cabal_path, rootdir / 'cabal')

    # Make archive...
    fmt = 'xztar'
    if platform.system() == 'Windows': fmt = 'zip'
    archivename = shutil.make_archive(basename, fmt, rootdir)

    return archivename

# Main procedure
#######################################################################

def main():
    import argparse

    parser = argparse.ArgumentParser(
        description="release packaging utility for cabal-install.",
        epilog = USAGE,
        formatter_class = argparse.RawDescriptionHelpFormatter)

    class EnableDisable(argparse.Action):
        def __call__(self, parser, namespace, values, option_string=None):
            value = option_string.startswith('--enable')
            setattr(namespace, self.dest, value)

    parser.add_argument('-w', '--with-compiler', type=str, default='ghc', help='path to GHC')
    parser.add_argument('-C', '--with-cabal', type=str, default='cabal', help='path to cabal-install')
    parser.add_argument('-i', '--index-state', type=str, default=DEFAULT_INDEXSTATE, help='index state of Hackage to use')
    parser.add_argument('--enable-static-executable', '--disable-static-executable', dest='static', nargs=0, default=False, action=EnableDisable, help='Statically link cabal executable')
    parser.add_argument('--enable-ofd-locking', '--disable-ofd-locking', dest='ofd_locking', nargs=0, default=True, action=EnableDisable, help='OFD locking (lukko)')

    args = parser.parse_args()

    rootdir = Path('.').resolve()
    args = Args(
        compiler   = Path(shutil.which(args.with_compiler)),
        cabal      = Path(shutil.which(args.with_cabal)),
        indexstate = args.index_state,
        rootdir    = rootdir,
        builddir   = rootdir.resolve() / '_build',
        static     = args.static,
        ofdlocking = args.ofd_locking
    )

    print(dedent(f"""
        compiler:    {args.compiler}
        cabal:       {args.cabal}
        index-state: {args.indexstate}
        builddir:    {args.builddir}
        static:      {args.static}
        ofd-locking: {args.ofdlocking}
    """))

    # Check tools
    subprocess_run([args.compiler, '--version'], check=True)
    subprocess_run([args.compiler, '--print-project-git-commit-id'], check=True)
    subprocess_run([args.cabal, '--version'], check=True)

    step_makedirs(args)
    step_config(args)
    step_cabal_update(args)
    step_cabal_install(args)
    archivename = step_make_archive(args)

    print(dedent(f'''
        Packaging finished!

        Distribution have been archived in

            {archivename}

    '''))

if __name__ == '__main__':
    main()
