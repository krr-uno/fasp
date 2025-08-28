import re
import unittest
import subprocess


from fasp.app import main


class TestControl(unittest.TestCase):

    def execute_clingo(self, program: str) -> str:
        """
        Executes the Clingo solver on the given program and returns its output.
        """
        result = subprocess.run(
            ["clingo", "--mode=parse"], input=program, text=True, capture_output=True
        )
        return result.stdout, result.stderr

    def assert_errors(self, program: str, expected_errors: list[str]):
        """
        Asserts that the Clingo solver produces the expected error messages for the given program.
        """
        output, error = self.execute_clingo(program)
        error_lines = error.splitlines()
        # error_lines = [line for line in output_lines if "error" in line.lower()]
        self.assertCountEqual(error_lines, expected_errors)

    def test_fact(self):
        program = "a."
        expected_errors = []
        self.assert_errors(program, expected_errors)
        program = "a"
        expected_errors = """
            -:1:2-3: error: expected one of ':-' '.' but got <eof>
            *** ERROR: (clingo): parsing failed
        """
        expected_errors = [
            l for line in expected_errors.strip().splitlines() if (l := line.strip())
        ]
        self.assert_errors(program, expected_errors)

    def test_two_facts(self):
        program = "a. b."
        expected_errors = []
        self.assert_errors(program, expected_errors)
        program = "a b."
        expected_errors = """
            -:1:3-4: error: expected one of ':-' '.' but got <identifier>
            *** ERROR: (clingo): parsing failed
        """
        expected_errors = [
            l for line in expected_errors.strip().splitlines() if (l := line.strip())
        ]
        self.assert_errors(program, expected_errors)

    def test_missing_dot_or_comma_with_fact(self):
        program = "a :- b. c."
        expected_errors = []
        self.assert_errors(program, expected_errors)
        program = "a :- b, c."
        expected_errors = []
        self.assert_errors(program, expected_errors)
        program = "a :- b c."
        expected_errors = """
            -:1:8-9: error: expected one of '.' but got <identifier>
            *** ERROR: (clingo): parsing failed
        """
        expected_errors = [
            l for line in expected_errors.strip().splitlines() if (l := line.strip())
        ]
        self.assert_errors(program, expected_errors)

    def test_missing_dot(self):
        program = "a :- b. c :- d."
        expected_errors = []
        self.assert_errors(program, expected_errors)
        program = "a :- b, c : -d."
        expected_errors = []
        self.assert_errors(program, expected_errors)
        program = "a :- b c :- d."
        expected_errors = """
            -:1:8-9: error: expected one of '.' but got <identifier>
            *** ERROR: (clingo): parsing failed
        """
        expected_errors = [
            l for line in expected_errors.strip().splitlines() if (l := line.strip())
        ]
        self.assert_errors(program, expected_errors)

    def test_unclosed_string(self):
        program = 'p("Some string").'
        expected_errors = []
        self.assert_errors(program, expected_errors)
        program = 'p("Some string).'
        expected_errors = """
            -:1:3-4: error: expected one of <term> but got <error>
            *** ERROR: (clingo): parsing failed
        """
        expected_errors = [
            l for line in expected_errors.strip().splitlines() if (l := line.strip())
        ]
        self.assert_errors(program, expected_errors)

    def test_unclosed_strings(self):
        program = 'p("Some string","Other string").'
        expected_errors = []
        self.assert_errors(program, expected_errors)
        program = program = 'p("Some string,"Other string").'
        expected_errors = """
            -:1:17-22: error: expected one of ')' ',' ';' but got <variable>
            *** ERROR: (clingo): parsing failed
        """
        expected_errors = [
            l for line in expected_errors.strip().splitlines() if (l := line.strip())
        ]
        self.assert_errors(program, expected_errors)

    def test_unclosed_string_with_dot(self):
        program = 'p("Some string.", "Other string").'
        expected_errors = []
        self.assert_errors(program, expected_errors)
        program = program = 'p("Some string., "other string").'
        expected_errors = """
            -:1:19-24: error: expected one of ')' ',' ';' but got <identifier>
            *** ERROR: (clingo): parsing failed
        """
        expected_errors = [
            l for line in expected_errors.strip().splitlines() if (l := line.strip())
        ]
        self.assert_errors(program, expected_errors)
