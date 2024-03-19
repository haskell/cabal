#!/usr/bin/env python3

from pathlib import Path
import argparse
import json
import logging
import subprocess
import gitlab
import os

logging.basicConfig(level=logging.INFO)

def strip_prefix(s, prefix):
    if s.startswith(prefix):
        return s[len(prefix):]
    else:
        return None

def job_triple(job_name):
    bindists = {
            "build-linux-alpine: [i386, alpine3_12, 9.0.2]": "i386-linux-alpine3_12",
            "build-linux-alpine: [x86_64, alpine3_12]": "x86_64-linux-alpine3_12",
            "build-linux: [aarch64, aarch64-linux, deb10]": "aarch64-linux-deb10",
            "build-linux: [aarch64, aarch64-linux, deb11]": "aarch64-linux-deb11",
            "build-linux: [i386, x86_64-linux, deb9]": "i386-linux-deb9",
            "build-linux: [x86_64, x86_64-linux, centos7]": "x86_64-linux-centos7",
            "build-linux: [x86_64, x86_64-linux, deb10]": "x86_64-linux-deb10",
            "build-linux: [x86_64, x86_64-linux, deb11]": "x86_64-linux-deb11",
            "build-linux: [x86_64, x86_64-linux, deb9]": "x86_64-linux-deb9",
            "build-linux: [x86_64, x86_64-linux, fedora33]": "x86_64-linux-fedora33",
            "build-linux: [x86_64, x86_64-linux, rocky8]": "x86_64-linux-rocky8",
            "build-linux: [x86_64, x86_64-linux, ubuntu18_04]": "x86_64-linux-ubuntu18_04",
            "build-linux: [x86_64, x86_64-linux, ubuntu20_04]": "x86_64-linux-ubuntu20_04",
            "build-x86_64-windows": "x86_64-windows",
            "build-aarch64-darwin": "aarch64-darwin",
            "build-x86_64-darwin": "x86_64-darwin",
    }

    if job_name in bindists:
        return bindists[job_name]
    else:
        return None

class UnhandledJobException(Exception):
    # Raised when there is a release job in the pipeline but we don"t explicitly handle it.
    def __init__(self, name):
        self.message = f"{name} is a release job but not downloaded"
        super().__init__(self.message)

def fetch_artifacts(release: str, pipeline_id: int,
                    dest_dir: Path, gl: gitlab.Gitlab):
    dest_dir.mkdir(exist_ok=True)
    # Write the pipeline id into output directory
    with open(f"{dest_dir}/metadata.json", "w") as out: json.dump({ "pipeline_id": pipeline_id }, out)

    proj = gl.projects.get("haskell/cabal")
    pipeline = proj.pipelines.get(pipeline_id)
    tmpdir = Path("fetch-gitlab")
    tmpdir.mkdir(exist_ok=True)
    for pipeline_job in pipeline.jobs.list(all=True):
        if len(pipeline_job.artifacts) == 0:
            logging.info(f"job {pipeline_job.name} ({pipeline_job.id}) has no artifacts")
            continue

        job = proj.jobs.get(pipeline_job.id)
        triple = job_triple(job.name)
        if triple is None:
            if job.name.startswith("build"):
                raise(UnhandledJobException(job.name))
            logging.info(f"ignoring {job.name}")
            continue

        try:
            destdir = tmpdir / job.name
            zip_name = Path(f"{tmpdir}/{job.name}.zip")
            if not zip_name.exists() or zip_name.stat().st_size == 0:
                logging.info(f"downloading archive {zip_name} for job {job.name} (job {job.id})...")
                with open(zip_name, "wb") as f:
                    job.artifacts(streamed=True, action=f.write)

            if zip_name.stat().st_size == 0:
                logging.info(f"artifact archive for job {job.name} (job {job.id}) is empty")
                continue

            subprocess.run(["unzip", "-bo", zip_name, "-d", destdir])

            if job.name == "build-x86_64-windows":
                bindist_files = list(destdir.glob("out/cabal-install*.zip"))
                bindist = bindist_files[0]
                dest = dest_dir / bindist.name
                final_destination = Path("../../release") / Path(bindist.name)
                bindist.rename(final_destination)
                logging.info(f"Extracted {job.name} to {final_destination}")
            else:
                bindist_files = list(destdir.glob("out/cabal-install*.tar.xz"))
                logging.info(f"bindist_files: {bindist_files}")
                bindist = bindist_files[0]
                logging.info(f"Bindist: {bindist}")
                dest = dest_dir / bindist.name
                final_destination = Path("../../release") / Path(bindist.name)
                bindist.rename(final_destination)

        except Exception as e:
            logging.error(f"Error fetching job {job.name}: \"{e}\"")
            pass

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--pipeline", "-p", required=True, type=int, help="pipeline id")
    parser.add_argument("--release", "-r", required=True, type=str, help="release name")
    parser.add_argument("--output", "-o", type=Path, default=Path.cwd(), help="output directory")
    parser.add_argument("--profile", "-P", default="haskell",
                        help="python-gitlab.cfg profile name")
    args = parser.parse_args()
    gl = gitlab.Gitlab.from_config(args.profile)
    fetch_artifacts(args.release, args.pipeline,
                    dest_dir=args.output, gl=gl)

if __name__ == "__main__":
    main()

