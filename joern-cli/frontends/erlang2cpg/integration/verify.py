#!/usr/bin/env python3
"""Run release checks from a fresh external directory. Preserve commands and output on disk."""

import argparse
import hashlib
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
FIXTURE_METHODS = "demo:dispatch/3,demo:notify/2,demo:route/1,demo:through/2,demo:wait/0,helper:keep/1,helper:keep/2,helper:observe/1"
OTP_SHA = "05737d130706c7189a8e6750d9c2252d2cc7987e"
OTP_DIGEST = "174723f975a5bea8343289ddabe52eb27ce61bd5e854c09067d9061af4069bdf"


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("stage", type=Path, help="Joern distribution directory with joern and joern-parse launchers")
    parser.add_argument("--proplists", type=Path, help=f"lib/stdlib/src/proplists.erl at erlang/otp commit {OTP_SHA}")
    parser.add_argument("--only", action="append", choices=["smoke", "languages", "explicit", "detected", "console",
                                                          "reverse", "headers", "partial", "failure", "runtime", "proplists"])
    args = parser.parse_args()
    stage = args.stage.resolve()
    work = Path(tempfile.mkdtemp(prefix="erlang2cpg-release-")).resolve()
    print(f"WORKDIR={work}", flush=True)
    for name in ["fixture", "smoke"]:
        shutil.copytree(HERE / name, work / name)
    shutil.copyfile(HERE / "query.sc", work / "query.sc")

    failures = []
    command_count = 0

    def run(tool, *argv, env=None, success=True):
        nonlocal command_count
        command_count += 1
        launcher = stage / tool
        require(launcher.is_file(), f"missing launcher {launcher}")
        cmd = list(map(str, [launcher, *argv]))
        print(f"$ {shlex.join(cmd)}", flush=True)
        result = subprocess.run(cmd, cwd=work, env=env, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, text=True, errors="replace", timeout=600)
        log = work / f"{command_count:02}-{tool}.log"
        log.write_text(f"$ {shlex.join(cmd)}\n{result.stdout}\nexit={result.returncode}\n")
        print(f"exit={result.returncode} log={log}", flush=True)
        require((result.returncode == 0) == success, result.stdout[-5000:])
        return result.stdout

    def parse(source, graph, language=None, **kw):
        command = ["--language", language] if language else []
        return run("joern-parse", *command, source, "--output", graph, **kw)

    def query(source, mode="open", check="identities", expected="", env=None):
        cmd = ["--script", "query.sc", "--param", f"input={source}", "--param", f"mode={mode}", "--param", f"check={check}"]
        if expected:
            cmd.extend(["--param", f"expected={expected}"])
        out = run("joern", *cmd, env=env)
        for line in out.splitlines():
            if line.startswith(("INTERNAL=", "FLOW=", "PROPLISTS ", "PASS ")):
                print(line)
        require(f"PASS {check} {mode}" in out, out[-5000:])

    def smoke():
        parse("smoke/id_demo.erl", "smoke.cpg", "ERLANG")
        query("smoke.cpg", expected="id_demo:id/1")
        renamed = (work / "smoke/id_demo.erl").read_text().replace("id_demo", "renamed").replace("id(", "copy(").replace("id/", "copy/")
        (work / "renamed.erl").write_text(renamed)
        parse("renamed.erl", "renamed.cpg")
        query("renamed.cpg", expected="renamed:copy/1")

    def fixture(name, language=None):
        parse("./fixture", name + ".cpg", language)
        query(name + ".cpg", check="fixture", expected=FIXTURE_METHODS)

    def reverse():
        # Keep valid module basenames and change relative sort order independently of filesystem enumeration.
        for prefix, filename in [("a", "helper.erl"), ("b", "demo.erl")]:
            path = work / "reverse" / prefix
            path.mkdir(parents=True)
            shutil.copyfile(work / "fixture" / filename, path / filename)
        parse("reverse", "reverse.cpg")
        query("reverse.cpg", check="fixture", expected=FIXTURE_METHODS)

    def broken(path):
        path.mkdir()
        (path / "broken.erl").write_text('-module(broken).\n-include("absent.hrl").\nf(X) -> X.\n')

    def partial():
        path = work / "space in source"
        broken(path)
        shutil.copyfile(work / "smoke/id_demo.erl", path / "id_demo.erl")
        out = parse("space in source", "partial.cpg")
        require("absent.hrl" in out and "broken.erl" in out, out)
        query("partial.cpg", expected="id_demo:id/1")

    def headers():
        root = work / "headers"
        includes = root / "include dir"
        includes.mkdir(parents=True)
        (includes / "shared.hrl").write_text("-ifdef(FEATURE).\nheader_value() -> ?VALUE.\n-endif.\n")
        for name, value in [("left", 101), ("right", 202)]:
            (root / (name + ".erl")).write_text(
                f'-module({name}).\n-define(VALUE,{value}).\n-include("shared.hrl").\n'
            )
        run("joern-parse", "headers", "--output", "headers.cpg", "--frontend-args",
            "--include", includes, "--define", "FEATURE")
        query("headers.cpg", check="headers", expected="left:header_value/0,right:header_value/0")

    def failure():
        broken(work / "bad")
        out = parse("bad", "empty.cpg", success=False)
        require("absent.hrl" in out and not (work / "empty.cpg").exists(), out)
        parse("smoke", "existing.cpg", "ERLANG")
        before = (work / "existing.cpg").read_bytes()
        out = parse("bad", "existing.cpg", success=False)
        require((work / "existing.cpg").read_bytes() == before, "failed import damaged previous graph")
        require("Successfully wrote graph to" not in out, out)

    def runtime():
        bindir = work / "path-without-otp"
        bindir.mkdir()
        for name in ("sh", "bash", "env", "uname", "greadlink", "readlink", "dirname", "basename", "java",
                     "sed", "grep", "awk", "cut", "tr", "head", "tail", "cat", "which", "expr", "wc", "ls", "sort",
                     "find", "xargs", "realpath", "pwd", "id", "tput", "mktemp", "mkdir", "rm", "cygpath"):
            executable = shutil.which(name)
            if executable:
                (bindir / name).symlink_to(executable)
        env = os.environ.copy()
        env["PATH"] = str(bindir)
        print(f"PATH={bindir}")
        out = parse("smoke", "missing.cpg", "ERLANG", env=env, success=False)
        require(re.search(r"(escript|Erlang/OTP|OTP 27)", out, re.I) and not (work / "missing.cpg").exists(), out)
        (work / "other.c").write_text("int unchanged_frontend(int x) { return x; }\n")
        parse("other.c", "other.cpg", "NEWC", env=env)
        query("other.cpg", check="other", env=env)

    def proplists():
        require(args.proplists and args.proplists.is_file(), "supply --proplists with the pinned OTP source")
        source = args.proplists.read_bytes()
        require(hashlib.sha256(source).hexdigest() == OTP_DIGEST, f"proplists.erl digest mismatch at {OTP_SHA}")
        (work / "proplists.erl").write_bytes(source)
        parse("proplists.erl", "proplists.cpg")
        query("proplists.cpg", check="otp")

    def languages():
        out = run("joern-parse", "--list-languages")
        require(re.search(r"^\s*-\s+erlang\s*$", out, re.M | re.I), out)

    cases = {"smoke": smoke, "languages": languages, "explicit": lambda: fixture("explicit", "ERLANG"),
             "detected": lambda: fixture("detected"), "console": lambda: query("./fixture", "code", "fixture", FIXTURE_METHODS),
             "reverse": reverse, "headers": headers, "partial": partial, "failure": failure, "runtime": runtime, "proplists": proplists}
    for name in args.only or cases:
        try:
            cases[name]()
            print(f"PASS {name}", flush=True)
        except (OSError, RuntimeError, subprocess.TimeoutExpired) as exc:
            failures.append(name)
            print(f"FAIL {name}: {exc}", flush=True)
    return int(bool(failures))


if __name__ == "__main__":
    raise SystemExit(main())
