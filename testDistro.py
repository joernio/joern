#!/usr/bin/env python3

# Prerequisite:
# > sbt joerncli/stage querydb/createDistribution

import subprocess
import sys
import os
import shutil
import tempfile
import requests
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass
import logging

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

def stage(func):
    """Decorator for test stages with logging"""
    def wrapper(*args, **kwargs):
        stage_name = func.__name__.replace('_', ' ').title()
        logger.info(f"=== Starting Stage: {stage_name} ===")
        try:
            result = func(*args, **kwargs)
            logger.info(f"=== Stage Completed: {stage_name} ===\n")
            return result
        except Exception as e:
            logger.error(f"=== Stage Failed: {stage_name} - {e} ===")
            raise
    return wrapper

@dataclass
class FrontendConfig:
    """Configuration for frontend tests"""
    min_method_count: int
    expected_method: str

class TestRunner:
    """Main test runner class with improved organization and error handling"""
    
    def __init__(self, script_dir: Path):
        self.joern_exe = self.executable_name("joern")
        self.joern_scan_exe = self.executable_name("joern-scan")
        self.joern_slice_exe = self.executable_name("joern-slice")
        self.sbt_exe = self.executable_name("sbt", from_path=True)

        self.script_dir = script_dir
        self.workspace_dir = script_dir / "workspace"
        
        # Frontend configurations
        self.frontend_configs = {
            "c": FrontendConfig(2, "print_number"),
            "jssrc": FrontendConfig(3, "lookForProperty"), 
            "javasrc": FrontendConfig(7, "callsExternalMethod"),
            "java": FrontendConfig(7, "callsExternalMethod"),
            "ghidra": FrontendConfig(100, "reallocarray"),
            "pythonsrc": FrontendConfig(2, "my_fun"),
            "php": FrontendConfig(3, "foo"),
        }
        
        # Script configurations for testing
        self.script_configs = {
            "c/pointer-to-int.sc": "unsafe-ptr",
            "c/syscalls.sc": "syscalls", 
            "c/userspace-memory-access.sc": "syscalls",
            "c/malloc-overflow.sc": "malloc-overflow",
            "c/malloc-leak.sc": "leak",
            "c/const-ish.sc": "const-ish",
        }

    @staticmethod
    def executable_name(executable: str, from_path: bool = False) -> str:
        """Get platform-specific executable name"""
        if os.name == 'nt':
            return f"{executable}.bat"
        return executable if from_path else f"./{executable}"

    @staticmethod
    def param_for_windows(param: str) -> str:
        """Add quotes for Windows parameter handling"""
        return f'"{param}"' if os.name == 'nt' else param

    def run_command(self, args: List[str], description: str, 
                   check: bool = True, cwd: Optional[Path] = None, 
                   env: Optional[Dict] = None) -> subprocess.CompletedProcess:
        """Run external command with proper error handling and logging"""
        logger.info(description)
        logger.debug(f"Command: {' '.join(args)}")
        
        try:
            proc = subprocess.run(
                args, 
                capture_output=True, 
                text=True, 
                cwd=cwd, 
                env=env,
                timeout=300  # 5 minute timeout
            )
            
            if check and proc.returncode != 0:
                error_msg = f"Failed: {description}"
                logger.error(error_msg)
                if proc.stdout:
                    logger.error(f"stdout: {proc.stdout}")
                if proc.stderr:
                    logger.error(f"stderr: {proc.stderr}")
                raise RuntimeError(error_msg)
                
            return proc
            
        except subprocess.TimeoutExpired:
            logger.error(f"Timed out: {description}")
            raise
        except Exception as e:
            logger.error(f"Execution failed: {description} - {e}")
            raise

    def clean_workspace(self):
        """Remove workspace directory if it exists"""
        if self.workspace_dir.exists():
            try:
                shutil.rmtree(self.workspace_dir)
                logger.debug(f"Removed workspace: {self.workspace_dir}")
            except Exception as e:
                logger.error(f"Failed to remove workspace: {e}")
                raise

    @stage
    def frontends_smoketest(self):
        """Run basic frontend smoke test"""
        self.clean_workspace()
        test_script = self.script_dir / "tests" / "frontends-smoketest.sc"
        args = [self.joern_exe, "--script", str(test_script)]
        self.run_command(args, f"Frontend smoketest: {test_script.name}")

    @stage  
    def frontends_tests(self):
        """Test all frontend configurations"""
        test_script = self.script_dir / "tests" / "frontends-testscript.sc"
        
        for frontend, config in self.frontend_configs.items():
            self.clean_workspace()
            input_path = self.script_dir / "tests" / "code" / frontend
            
            args = [
                self.joern_exe,
                "--script", str(test_script),
                "--param", self.param_for_windows(f"inputPath={input_path}"),
                "--param", self.param_for_windows(f"minMethodCount={config.min_method_count}"),
                "--param", self.param_for_windows(f"expectedMethod={config.expected_method}"),
                "--param", self.param_for_windows(f"frontend={frontend}")
            ]
            
            self.run_command(args, f"Frontend test [{frontend}]")
            logger.info(f"Frontend [{frontend}] tested successfully")

    @stage
    def scripts_test(self):
        """Test predefined script configurations"""
        self.clean_workspace()
        
        joern_scripts_dir = self.script_dir / "joern-cli" / "src" / "main" / "resources" / "scripts"
        testcode_root = self.script_dir / "joern-cli" / "src" / "test" / "resources" / "testcode"

        for script_name, code_name in self.script_configs.items():
            script_path = joern_scripts_dir / script_name
            input_path = testcode_root / code_name
            
            args = [
                self.joern_exe,
                "--script", str(script_path),
                "--param", self.param_for_windows(f"inputPath={input_path}")
            ]
            
            self.run_command(args, f"Script test [{script_name}]")
            logger.info(f"Script [{script_name}] passed")

    @stage
    def querydb_test(self):
        """Test querydb plugin functionality"""
        self.clean_workspace()
        
        querydb_zip = self.script_dir / "querydb" / "target" / "querydb.zip"

        # Remove and re-add plugin
        self.run_command([self.joern_exe, "--remove-plugin", "querydb"], 
                        "Remove querydb plugin", check=False)
        
        self.run_command([self.joern_exe, "--add-plugin", str(querydb_zip)], 
                        "Add querydb plugin")

        # Verify plugin installation
        proc = self.run_command([self.joern_scan_exe, "--list-query-names"], 
                               "List query names")
        
        sqli_count = sum(1 for line in proc.stdout.splitlines() if "sql-injection" in line)
        if sqli_count == 0:
            raise RuntimeError("Query 'sql-injection' from querydb not found")

    @stage
    def scan_test(self):
        """Test scanning functionality"""
        self.clean_workspace()
        
        with tempfile.TemporaryDirectory() as tmp_dir:
            foo_path = Path(tmp_dir) / "foo"
            foo_path.mkdir()
            
            # Create test file
            (foo_path / "foo.c").write_text("int foo(int a, int b, int c, int d, int e, int f) {}")
            
            # Run tests
            self.run_command([self.joern_exe, "--src", str(foo_path), "--run", "scan"],
                           f"Joern with --src {foo_path} and --run scan")
            
            self.run_command([self.joern_scan_exe, str(foo_path)],
                           f"Joern-scan on {foo_path}")
            
            self.run_command([self.joern_scan_exe, "--dump"],
                           "Joern-scan dump")

    @stage
    def slice_test(self):
        """Test slicing functionality"""
        self.clean_workspace()

        with tempfile.TemporaryDirectory() as tmp_dir:
            slice_path = Path(tmp_dir) / "slice"
            slice_path.mkdir()
            
            test_file = self.script_dir / "tests" / "code" / "javasrc" / "SliceTest.java"
            out_file = slice_path / "dataflow-slice-javasrc.json"
            slice_script = self.script_dir / "tests" / "test-dataflow-slice.sc"
            
            # Run slice
            self.run_command([self.joern_slice_exe, "data-flow", str(test_file), "-o", str(out_file)],
                           f"Joern slice on {test_file}")
            
            # Verify slice output
            proc = self.run_command([self.joern_exe, "--script", str(slice_script), 
                                   "--param", self.param_for_windows(f"sliceFile={out_file}")],
                                  f"Verify slice with {slice_script}")
            
            expected_string = 'List(boolean b, b, this, s, "MALICIOUS", s, new Foo("MALICIOUS"), s, s, "SAFE", s, b, this, this, b, s, System.out)'
            if expected_string not in proc.stdout:
                raise RuntimeError(f"Slice output validation failed. Got: {proc.stdout}")

    @stage
    def sarif_test(self):
        """Test SARIF output validation"""
        self.clean_workspace()

        with tempfile.TemporaryDirectory() as tmp_dir:
            sarif_path = Path(tmp_dir) / "sarif"
            sarif_path.mkdir()
            
            test_code = self.script_dir / "tests" / "code" / "sarif-test"
            
            # Generate SARIF
            self.run_command([self.joern_scan_exe, str(test_code), "--store"],
                           "Generate SARIF with joern-scan")
            
            cpg_file = self.workspace_dir / "sarif-test" / "cpg.bin"
            out_file = sarif_path / "test.sarif"
            script_path = self.script_dir / "tests" / "test-sarif.sc"
            
            self.run_command([self.joern_exe, "--script", str(script_path),
                            "--param", self.param_for_windows(f"cpgFile={cpg_file}"),
                            "--param", self.param_for_windows(f"outFile={out_file}")],
                            f"Test SARIF file with {script_path}")
            
            # Validate SARIF
            self._validate_sarif_file(out_file)

    def _validate_sarif_file(self, sarif_file: Path):
        """Validate SARIF file using external service"""
        try:
            with open(sarif_file, "rb") as f:
                files = {"postedFiles": ("test.sarif", f, "application/octet-stream")}
                response = requests.post(
                    "https://sarifweb.azurewebsites.net/Validation/ValidateFiles",
                    files=files,
                    timeout=30
                )
            
            response.raise_for_status()
            result = response.json()
            exit_code = int(result.get("exitCode", 1))
            
            logger.info(f"SARIF Validation Exit Code: {exit_code}")
            if exit_code != 0:
                raise RuntimeError("SARIF file validation failed")
                
        except requests.RequestException as e:
            logger.error(f"SARIF validation request failed: {e}")
            raise

    @stage
    def schema_extender_test(self):
        """Test schema extender functionality"""
        stage_dir = self.script_dir / "joern-cli" / "target" / "universal" / "stage"
        scripts_dir = stage_dir / "scripts"
        scripts_dir.mkdir(parents=True, exist_ok=True)
        
        script_path = scripts_dir / "SchemaExtenderTest.sc"
        schema_extender_dir = stage_dir / "schema-extender"
        cpg_version_file = schema_extender_dir / "cpg-version"
        
        # Read CPG version
        cpg_version = cpg_version_file.read_text().strip()
        
        # Run SBT command
        env = os.environ.copy()
        env["CPG_VERSION"] = cpg_version
        
        self.run_command([self.sbt_exe, "clean", "replaceDomainClassesInJoern"],
                        "Run SBT clean replaceDomainClassesInJoern", 
                        cwd=schema_extender_dir, env=env)
        
        # Create and run test script
        test_script_content = (
            'assert(nodes.ExampleNode.Label == "EXAMPLE_NODE")\n'
            'assert(nodes.ExampleNode.PropertyNames.ExampleProperty == "EXAMPLE_PROPERTY")'
        )
        script_path.write_text(test_script_content)
        
        self.run_command([self.joern_exe, "--script", str(script_path)],
                        f"Run Joern with schema extender test script {script_path}", cwd=stage_dir)

    def run_all_tests(self):
        """Run all test stages"""
        test_stages = [
            self.frontends_smoketest,
            self.frontends_tests, 
            self.scripts_test,
            self.querydb_test,
            self.scan_test,
            self.slice_test,
            self.sarif_test,
            self.schema_extender_test
        ]
        
        for test_stage in test_stages:
            test_stage()
        
        logger.info("🎉 All tests completed successfully! Go analyse some code.")


def main():
    """Main entry point"""
    try:
        script_path = Path(sys.argv[0]).resolve()
        script_dir = script_path.parent
        os.chdir(script_dir)
        
        runner = TestRunner(script_dir)
        runner.run_all_tests()

    except KeyboardInterrupt:
        logger.error("Tests interrupted by user")
        sys.exit(1)
    except Exception as e:
        logger.error(f"Test execution failed: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
