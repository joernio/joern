#!/usr/bin/env python3

import subprocess
import sys
import os
import shutil
import tempfile
import requests

# Prerequisite:
# > sbt joerncli/stage querydb/createDistribution

def executable_name(executable, from_path = False):
    # Determine script name for Windows vs. Unix
    if os.name == 'nt':
        return f"{executable}.bat"
    else:
        if not from_path:
            return f"./{executable}"
        else:
            return executable

JOERN = executable_name("joern")
JOERN_SCAN = executable_name("joern-scan")
JOERN_SLICE = executable_name("joern-slice")
SBT = executable_name("sbt", from_path = True)

def param_for_windows(param):
    # --param arg for joern needs enclosing quotes for Windows
    if os.name == 'nt':
        return f"\"{param}\""
    else:
        return param

def run_externally(args, check, description, failure_msg, cwd = None, env = None):
    print(description)
    proc = subprocess.run(args, capture_output = True, text = True, cwd = cwd, env = env)
    if check and proc.returncode != 0:
        print(failure_msg)
        if proc.stdout:
            print(f"stdout was:\n{proc.stdout}")
        if proc.stderr:
            print(f"stderr was:\n{proc.stderr}")
        sys.exit(1)
    return proc

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
def frontends_smoketest(script_abs_dir):
    remove_workspace(script_abs_dir)
    test_script = os.path.join("tests", "frontends-smoketest.sc")
    args = [JOERN, "--script", test_script]
    run_externally(args, True, f"Running script [{test_script}]", f"Script [{test_script}] failed to run successfully")

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
        run_externally(args, True, f"Running script [{test_script}] for frontend={frontend}", f"Script [{test_script}] failed to run for frontend={frontend}")

        print(f"Frontend [{frontend}] tested successfully.")

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
        run_externally(args, True, f"Running script [{script}]", f"Script [{script}] failed to run successfully")

        print(f"Script [{script}] passed")

    print("All scripts tested successfully")

@stage
def querydb_test(script_abs_dir):
    remove_workspace(script_abs_dir)

    querydb_zip = os.path.join(script_abs_dir, "querydb", "target", "querydb.zip")

    args = [JOERN, "--remove-plugin", "querydb"]
    run_externally(args, False, "Running remove-plugin", "Running remove-plugin failed")

    args = [JOERN, "--add-plugin", querydb_zip]
    run_externally(args, True, "Running add-plugin", f"Failed to add plugin {querydb_zip}")

    args = [JOERN_SCAN, "--list-query-names"]
    proc = run_externally(args, True, "Listing querydb query names", "Failed to list query names from joern-scan")

    query_names = proc.stdout
    sqli_count = sum(1 for line in query_names.splitlines() if "sql-injection" in line)

    if sqli_count == 0:
        print("Error: query 'sql-injection' from querydb not found - something wrong with the querydb installation?")
        sys.exit(1)

@stage
def scan_test(script_abs_dir):
    remove_workspace(script_abs_dir)

    with tempfile.TemporaryDirectory() as tmpdirname:
        foo_path = os.path.join(tmpdirname, "foo")
        os.makedirs(foo_path, exist_ok=True)

        with open(f"{foo_path}/foo.c", "w") as f:
            f.write("int foo(int a, int b, int c, int d, int e, int f) {}")

        args = [JOERN, "--src", foo_path, "--run", "scan"]
        run_externally(args, True, f"Running joern on {foo_path} with --run scan", "Failed to run scan")    
        
        args = [JOERN_SCAN, foo_path]
        run_externally(args, True, f"Running joern-scan on {foo_path}", "Failed to execute joern-scan")

        args = [JOERN_SCAN, "--dump"]
        run_externally(args, True, f"Running joern-scan on {foo_path} with --dump", "Failed to scan and dump from joern-scan")

@stage
def slice_test(script_abs_dir):
    remove_workspace(script_abs_dir)

    with tempfile.TemporaryDirectory() as tmpdirname:
        slice_path = os.path.join(tmpdirname, "slice")
        os.makedirs(slice_path, exist_ok=True)

        test_file = os.path.join(script_abs_dir, "tests", "code", "javasrc", "SliceTest.java")
        out_file = os.path.join(slice_path, "dataflow-slice-javasrc.json")
        slice_script = os.path.join(script_abs_dir, "tests", "test-dataflow-slice.sc")

        args = [JOERN_SLICE, "data-flow", test_file, "-o", out_file]
        run_externally(args, True, f"Running joern-slice on {slice_path}", "Failed to execute joern-slice")

        args = [JOERN, "--script", slice_script, "--param", param_for_windows(f"sliceFile={out_file}")]
        proc = run_externally(args, True, f"Running joern with script {slice_script}", f"Failed to execute joern script {slice_script}")

        expected_string = 'List(boolean b, b, this, s, "MALICIOUS", s, new Foo("MALICIOUS"), s, s, "SAFE", s, b, this, this, b, s, System.out)'
        if expected_string not in proc.stdout:
            print(f"Slice did not yield expected output. Got {proc.stdout} instead")
            sys.exit(1)

@stage
def sarif_test(script_abs_dir):
    remove_workspace(script_abs_dir)

    with tempfile.TemporaryDirectory() as tmpdirname:
        sarif_path = os.path.join(tmpdirname, "sarif")
        os.makedirs(sarif_path, exist_ok=True)

        test_code = os.path.join(script_abs_dir, "tests", "code", "sarif-test")

        args = [JOERN_SCAN, test_code, "--store"]
        run_externally(args, True, f"Running joern-scan on {sarif_path} with --store", "Failed to execute joern-scan")

        cpg_file = os.path.join(script_abs_dir, "workspace", "sarif-test", "cpg.bin")
        out_file = os.path.join(sarif_path, "test.sarif")
        script_path = os.path.join(script_abs_dir, "tests", "test-sarif.sc")

        args = [JOERN, "--script", script_path, "--param", param_for_windows(f"cpgFile={cpg_file}"), "--param", param_for_windows(f"outFile={out_file}")]
        run_externally(args, True, f"Running joern with script {script_path}", "Failed to execute joern sarif script")

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

@stage
def schema_extender_test(script_abs_dir):
    stage_dir = os.path.join(script_abs_dir, "joern-cli", "target", "universal", "stage")
    scripts_dir = os.path.join(stage_dir, "scripts")
    os.makedirs(scripts_dir, exist_ok=True)
    script_path = os.path.join(scripts_dir, "SchemaExtenderTest.sc")

    schema_extender_dir = os.path.join(stage_dir, "schema-extender")
    cpg_version_file = os.path.join(schema_extender_dir, "cpg-version")

    with open(cpg_version_file, "r") as f:
        cpg_version = f.read().rstrip()

    args = [ SBT, "clean", "replaceDomainClassesInJoern"]
    env = os.environ | {"CPG_VERSION": cpg_version}
    run_externally(args, True, "Running sbt clean replaceDomainClassesInJoern", "Failed to execute sbt replaceDomainClassesInJoern", cwd = schema_extender_dir, env = env)
            
    with open(script_path, "w") as f:
        f.write("assert(nodes.ExampleNode.Label == \"EXAMPLE_NODE\")\nassert(nodes.ExampleNode.PropertyNames.ExampleProperty == \"EXAMPLE_PROPERTY\")")

    args = [JOERN, "--script", script_path]
    run_externally(args, True, f"Running joern with script {script_path}", "Failed to execute joern script", cwd = stage_dir)

def main():
    script_abs_path = abs_path(sys.argv[0])
    script_abs_dir = os.path.dirname(script_abs_path)
    os.chdir(script_abs_dir)
    
    frontends_smoketest(script_abs_dir)
    frontends_tests(script_abs_dir)
    scripts_test(script_abs_dir)
    querydb_test(script_abs_dir)
    scan_test(script_abs_dir)
    slice_test(script_abs_dir)
    sarif_test(script_abs_dir)
    schema_extender_test(script_abs_dir)

    print("Success. Go analyse some code.")

if __name__ == "__main__":
    main()
