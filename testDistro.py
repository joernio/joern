#!/usr/bin/env python3

import subprocess
import sys
import os
import shutil
import tempfile
import requests

# Prerequisite:
# > sbt joerncli/stage querydb/createDistribution

def executable_name(executable):
    # Determine script name for Windows vs. Unix
    if os.name == 'nt':
        return f"{executable}.bat"
    else:
        return f"./{executable}"

JOERN = executable_name("joern")
JOERN_SCAN = executable_name("joern-scan")
JOERN_SLICE = executable_name("joern-slice")

def param_for_windows(param):
    if os.name == 'nt':
        return f"\"{param}\""
    else:
        return param

def stage(stage_function):
    def wrapper_function(*args, **kwargs):
        print('### Executing stage', stage_function.__name__, "###")
        stage_function(*args, **kwargs)
        print('### Successfully executed stage', stage_function.__name__, '###\n')
    return wrapper_function

def abs_path(path):
    return os.path.abspath(os.path.realpath(path))

def remove_workspace(script_abs_dir):
    workspace_dir = os.path.join(script_abs_dir, "workspace")
    if os.path.exists(workspace_dir):
        try:
            shutil.rmtree(workspace_dir)
        except Exception as e:
            print(f"Failed to remove workspace directory: {e}")
            sys.exit(1)

@stage
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
        remove_workspace(script_abs_dir)
        input_path = os.path.join("tests", "code", frontend)
        test_script = os.path.join("tests", "frontends-testscript.sc")
        args = [
            JOERN,
            "--script", test_script,
            "--param", param_for_windows(f"inputPath={input_path}"),
            "--param", param_for_windows(f"minMethodCount={min_method_count[frontend]}"),
            "--param", param_for_windows(f"expectedMethod={expected_method[frontend]}"),
            "--param", param_for_windows(f"frontend={frontend}")
        ]
        try:
            print(f"Running script [{test_script}] for frontend={frontend}")
            proc = subprocess.run(args, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        except subprocess.CalledProcessError:
            print(f"Script [{test_script}] failed to run for frontend={frontend}")
            print(f"stdout was:\n{proc.stdout}")
            print(f"stderr was:\n{proc.stderr}")
            sys.exit(1)

        print(f"Frontend [{frontend}] tested successfully")

@stage
def scripts_test(script_abs_dir):
    remove_workspace(script_abs_dir)

    joern_scripts_dir = os.path.join(script_abs_dir, "joern-cli", "src", "main", "resources", "scripts")
    testcode_root = os.path.join(script_abs_dir, "joern-cli", "src", "test", "resources", "testcode")

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
            JOERN,
            "--script", script_path,
            "--param", param_for_windows(f"inputPath={input_path}")
        ]
        try:
            print(f"Running script [{script}]")
            proc = subprocess.run(args, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        except subprocess.CalledProcessError:
            print(f"Script [{script}] failed to run successfully.")
            print(f"stdout was:\n{proc.stdout}")
            print(f"stderr was:\n{proc.stderr}")
            sys.exit(1)

        print(f"Script [{script}] passed...\n")

    print("All scripts tested successfully.")

@stage
def querydb_test(script_abs_dir):
    remove_workspace(script_abs_dir)

    querydb_zip = os.path.join(script_abs_dir, "querydb", "target", "querydb.zip")

    try:
        print("Running remove-plugin")
        subprocess.run([JOERN, "--remove-plugin", "querydb"], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    except subprocess.CalledProcessError:
        print("Failed to remove 'querydb' plugin (it may not be installed yet), continuing...")

    try:
        print("Running add-plugin")
        proc = subprocess.run([JOERN, "--add-plugin", querydb_zip], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    except subprocess.CalledProcessError:
        print(f"Failed to add plugin {querydb_zip}")
        print(f"stdout was:\n{proc.stdout}")
        print(f"stderr was:\n{proc.stderr}")
        sys.exit(1)

    try:
        print("Listing querydb query names")
        proc = subprocess.run(
            [JOERN_SCAN, "--list-query-names"],
            check=True, 
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            encoding="utf-8"
        )
        query_names = proc.stdout
    except subprocess.CalledProcessError:
        print("Failed to list query names from joern-scan.")
        sys.exit(1)

    sqli_count = sum(1 for line in query_names.splitlines() if "sql-injection" in line)

    if sqli_count == 0:
        print("Error: query 'sql-injection' from querydb not found - something wrong with the querydb installation?")
        print(f"stdout was:\n{proc.stdout}")
        print(f"stderr was:\n{proc.stderr}")   
        sys.exit(1)

@stage
def scan_test(script_abs_dir):
    remove_workspace(script_abs_dir)

    with tempfile.TemporaryDirectory() as tmpdirname:
        foo_path = os.path.join(tmpdirname, "foo")
        os.makedirs(foo_path, exist_ok=True)

        with open(f"{foo_path}/foo.c", "w") as f:
            f.write("int foo(int a, int b, int c, int d, int e, int f) {}")

        try:
            print(f"Running joern on {foo_path} with --run scan")
            proc = subprocess.run([JOERN, "--src", foo_path, "--run", "scan"], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        except subprocess.CalledProcessError:
            print("Failed to run scan")
            print(f"stdout was:\n{proc.stdout}")
            print(f"stderr was:\n{proc.stderr}")
            sys.exit(1)
        
        try:
            print(f"Running joern-scan on {foo_path}")
            proc = subprocess.run([JOERN_SCAN, foo_path], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        except subprocess.CalledProcessError:
            print("Failed to execute joern-scan")
            print(f"stdout was:\n{proc.stdout}")
            print(f"stderr was:\n{proc.stderr}")            
            sys.exit(1)

        try:
            print(f"Running joern-scan on {foo_path} with --dump")
            subprocess.run([JOERN_SCAN, "--dump"], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        except subprocess.CalledProcessError:
            print("Failed to scan and dump from joern-scan")
            print(f"stdout was:\n{proc.stdout}")
            print(f"stderr was:\n{proc.stderr}")    
            sys.exit(1)

@stage
def slice_test(script_abs_dir):
    remove_workspace(script_abs_dir)

    with tempfile.TemporaryDirectory() as tmpdirname:
        slice_path = os.path.join(tmpdirname, "slice")
        os.makedirs(slice_path, exist_ok=True)

        test_file = os.path.join(script_abs_dir, "tests", "code", "javasrc", "SliceTest.java")
        out_file = os.path.join(slice_path, "dataflow-slice-javasrc.json")
        slice_script = os.path.join(script_abs_dir, "tests", "test-dataflow-slice.sc")

        try:
            print(f"Running joern-slice on {slice_path}")
            proc = subprocess.run([
                JOERN_SLICE, 
                "data-flow", 
                test_file,
                "-o", 
                out_file
            ], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        except subprocess.CalledProcessError:
            print("Failed to execute joern-slice")
            print(f"stdout was:\n{proc.stdout}")
            print(f"stderr was:\n{proc.stderr}")   
            sys.exit(1)

        result = subprocess.run([
            JOERN, 
            "--script", 
            slice_script, 
            "--param", 
            param_for_windows(f"sliceFile={out_file}")
        ], capture_output=True, text=True)

        expected_string = 'List(boolean b, b, this, s, "MALICIOUS", s, new Foo("MALICIOUS"), s, s, "SAFE", s, b, this, this, b, s, System.out)'
        if expected_string not in result.stdout:
            print(f"Slice did not yield expected output. Got {result.stdout} instead")
            sys.exit(1)

@stage
def sarif_test(script_abs_dir):
    remove_workspace(script_abs_dir)

    with tempfile.TemporaryDirectory() as tmpdirname:
        sarif_path = os.path.join(tmpdirname, "sarif")
        os.makedirs(sarif_path, exist_ok=True)

        test_code = os.path.join(script_abs_dir, "tests", "code", "sarif-test")

        try:
            print(f"Running joern-scan on {sarif_path} with --store")
            proc = subprocess.run(
                [JOERN_SCAN, test_code, "--store"],
                check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE
            )
        except subprocess.CalledProcessError:
            print("Failed to execute joern-scan")
            print(f"stdout was:\n{proc.stdout}")
            print(f"stderr was:\n{proc.stderr}")   
            sys.exit(1)

        cpg_file = os.path.join(script_abs_dir, "workspace", "sarif-test", "cpg.bin")
        out_file = os.path.join(sarif_path, "test.sarif")
        script_path = os.path.join(script_abs_dir, "tests", "test-sarif.sc")

        try:
            print(f"Running joern with script {script_path}")
            proc = subprocess.run(
                [
                    JOERN,
                    "--script", script_path,
                    "--param", param_for_windows(f"cpgFile={cpg_file}"),
                    "--param", param_for_windows(f"outFile={out_file}")
                ],
                check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE
            )
        except subprocess.CalledProcessError:
            print("Failed to execute joern sarif script")
            print(f"stdout was:\n{proc.stdout}")
            print(f"stderr was:\n{proc.stderr}")   
            sys.exit(1)

        with open(out_file, "rb") as f:
            files = {
                "postedFiles": ("test.sarif", f, "application/octet-stream")
            }
            response = requests.post(
                "https://sarifweb.azurewebsites.net/Validation/ValidateFiles",
                files=files
            )
        response.raise_for_status()
        result = response.json()
        exit_code = int(result.get("exitCode", 1))

        print(f"SARIF Validation Exit Code: {exit_code}")
        if exit_code != 0:
            print(f"Resulting sarif file is invalid!")
            sys.exit(1)

def main():
    script_abs_path = abs_path(sys.argv[0])
    script_abs_dir = os.path.dirname(script_abs_path)
    os.chdir(script_abs_dir)
    
    # frontends_smoketest() TODO: implement proper smoke test with small test projects
    
    frontends_tests(script_abs_dir)
    scripts_test(script_abs_dir)
    querydb_test(script_abs_dir)
    scan_test(script_abs_dir)
    slice_test(script_abs_dir)
    sarif_test(script_abs_dir)

    print("Success. Go analyse some code.")

if __name__ == "__main__":
    main()
