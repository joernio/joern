#!/usr/bin/env python3

import subprocess
import sys
import os
import shutil

def abs_path(path):
    return os.path.abspath(os.path.realpath(path))

def stage(script_abs_dir):
    print(f"Staging joern in {script_abs_dir}")

    try:
         os.chdir(script_abs_dir)
    except Exception as e:
        print(f"Failed to change directory to {script_abs_dir}: {e}")
        sys.exit(1)

    try:
        # Run sbt tasks
        subprocess.run([
            "sbt",
            "-Dsbt.log.noformat=true",
            "clean",
            "joerncli/stage",
            "querydb/createDistribution"
        ], check=True, shell=True)
    except subprocess.CalledProcessError:
        print("sbt stage failed")
        sys.exit(1)

def frontends_tests(script_abs_dir):
    frontends = ["c", "jssrc", "javasrc", "java", "ghidra", "pythonsrc", "php"]
    min_method_count = {
        "c": 2,
        "jssrc": 3,
        "javasrc": 7,
        "java": 7,
        "ghidra": 100,
        "pythonsrc": 2,
        "php": 3,
    }
    expected_method = {
        "c": "print_number",
        "jssrc": "lookForProperty",
        "javasrc": "callsExternalMethod",
        "java": "callsExternalMethod",
        "ghidra": "reallocarray",
        "pythonsrc": "my_fun",
        "php": "foo",
    }

    for frontend in frontends:
        # Remove workspace directory if it exists
        workspace_dir = os.path.join(script_abs_dir, "workspace")
        if os.path.exists(workspace_dir):
            try:
                shutil.rmtree(workspace_dir)
            except Exception as e:
                print(f"Failed to remove workspace directory: {e}")
                sys.exit(1)

        input_path = os.path.join("tests", "code", frontend)
        test_script = os.path.join("tests", "frontends-testscript.sc")
        args = [
            "joern",
            "--script", test_script,
            "--param", f"\"inputPath={input_path}\"",
            "--param", f"\"minMethodCount={min_method_count[frontend]}\"",
            "--param", f"\"expectedMethod={expected_method[frontend]}\"",
            "--param", f"\"frontend={frontend}\"",
        ]
        try:
            subprocess.run(args, check=True, shell=True)
        except subprocess.CalledProcessError:
            print(f"Script [{test_script}] failed to run for frontend={frontend}")
            sys.exit(1)

        print(f"Frontend [{frontend}] tested successfully")

def scripts_test(script_abs_dir):
    joern_scripts_dir = os.path.join(script_abs_dir, "joern-cli/src/main/resources/scripts")
    testcode_root = os.path.join(script_abs_dir, "joern-cli/src/test/resources/testcode")

    # List of scripts and mapping to input code directories
    scripts = [
        "c/pointer-to-int.sc",
        "c/syscalls.sc",
        "c/userspace-memory-access.sc",
        "c/malloc-overflow.sc",
        "c/malloc-leak.sc",
        "c/const-ish.sc",
    ]
    code = {
        "c/pointer-to-int.sc": "unsafe-ptr",
        "c/syscalls.sc": "syscalls",
        "c/userspace-memory-access.sc": "syscalls",
        "c/malloc-overflow.sc": "malloc-overflow",
        "c/malloc-leak.sc": "leak",
        "c/const-ish.sc": "const-ish",
    }

    for script in scripts:
        script_path = os.path.join(joern_scripts_dir, script)
        input_path = os.path.join(testcode_root, code[script])
        args = [
            "joern",
            "--script", script_path,
            "--param", f"\"inputPath={input_path}\""
        ]
        try:
            subprocess.run(args, check=True, shell=True)
        except subprocess.CalledProcessError:
            print(f"Script [{script}] failed to run successfully.")
            sys.exit(1)
        print(f"Script [{script}] passed...\n")

    print("All scripts tested successfully.")

def querydb_test(script_abs_dir):
    querydb_zip = os.path.join(script_abs_dir, "querydb/target/querydb.zip")

    print("Installing querydb")

    # Remove plugin if present
    try:
        subprocess.run(["joern", "--remove-plugin", "querydb"], check=True, shell=True)
    except subprocess.CalledProcessError:
        print("Warning: Failed to remove 'querydb' plugin (it may not be installed yet), continuing...")

    # Add plugin
    try:
        subprocess.run(["joern", "--add-plugin", querydb_zip], check=True, shell=True)
    except subprocess.CalledProcessError:
        print(f"Failed to add plugin {querydb_zip}")
        sys.exit(1)

    # Check for 'sql-injection' query
    try:
        proc = subprocess.run(
            ["joern-scan", "--list-query-names"],
            check=True, 
            shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            encoding="utf-8"
        )
        query_names = proc.stdout
    except subprocess.CalledProcessError as e:
        print("Failed to list query names from joern-scan.")
        print(e.stderr)
        sys.exit(1)

    sqli_count = sum(1 for line in query_names.splitlines() if "sql-injection" in line)

    if sqli_count == 0:
        print("error: query 'sql-injection' from querydb not found - something wrong with the querydb installation?")
        sys.exit(1)

    print("querydb integration tested successfully.")

def main():
    script_abs_path = abs_path(sys.argv[0])
    script_abs_dir = os.path.dirname(script_abs_path)

    stage(script_abs_dir)
    
    # frontends_smoketest() TODO: implement proper smoke test with small test projects
    frontends_tests(script_abs_dir)
    scripts_test(script_abs_dir)
    querydb_test(script_abs_dir)

    print("Success. Go analyse some code.")

if __name__ == "__main__":
    main()
