import textwrap
import unittest

from fasp.parser import lexer


class TestNonCodeBlocksRE(unittest.TestCase):
    """
    Test class for the non-code blocks regular expression.
    """

    def find_first(self, source: str, start: int):
        """
        Find the first non-code block delimiter in the source code.
        """
        match = lexer._PATTERN_NON_CODE_BLOCKS_DEFAULT.search(source, start)
        return match

    def assertFindFirst(self, code, start, pos, value):
        match = self.find_first(code, start)
        self.assertIsNotNone(match)
        self.assertEqual(match.group(), value)
        self.assertEqual(match.start(), pos)

    def test_find_first_non_code_block_delimiter(self):
        self.assertFindFirst(
            '"string"',
            0,
            0,
            '"string"',
        )
        self.assertFindFirst(
            'p(1,2,"string").',
            0,
            6,
            '"string"',
        )
        self.assertFindFirst(
            'p(1,2,"unclosed string).',
            0,
            6,
            '"unclosed string).',
        )
        self.assertFindFirst(
            r' ", %* block comment *% ',
            0,
            1,
            r'", %* block comment *% ',
        )
        self.assertFindFirst(
            r'", %* block comment *% body.',
            0,
            0,
            r'", %* block comment *% body.',
        )
        string = r"""p("this string has a %* %  *%", %* block comment *% :- body. % line comment"""
        self.assertFindFirst(
            string,
            0,
            2,
            r'"this string has a %* %  *%"',
        )
        self.assertFindFirst(
            string,
            29,
            29,
            r'", %* block comment *% :- body. % line comment',
        )
        self.assertFindFirst(
            string,
            30,
            32,
            r"%*",
        )
        self.assertFindFirst(
            string,
            48,
            49,
            r"*%",
        )

    def test_find_dot(self):
        match = lexer._PATTERN_DOT.search("abc.def")
        self.assertIsNotNone(match)
        self.assertEqual(match.start(), 3)
        self.assertEqual(match.end(), 4)

    def test_find_dot2(self):
        match = lexer._PATTERN_DOT.search("abc..def.")
        self.assertIsNotNone(match)
        self.assertEqual(match.start(), 8)
        self.assertEqual(match.end(), 9)

    def test_find_dot3(self):
        match = lexer._PATTERN_DOT.search("abc..def. other .")
        self.assertIsNotNone(match)
        self.assertEqual(match.start(), 8)
        self.assertEqual(match.end(), 9)

    def test_find_dot_reverse(self):
        match = lexer._PATTERN_DOT_REVERSE.search("abc.def")
        self.assertIsNotNone(match)
        self.assertEqual(match.start(), 3)
        self.assertEqual(match.end(), 4)

    def test_find_dot2_reverse(self):
        match = lexer._PATTERN_DOT_REVERSE.search("abc..def.")
        self.assertIsNotNone(match)
        self.assertEqual(match.start(), 8)
        self.assertEqual(match.end(), 9)

    def test_find_dot3_reverse(self):
        match = lexer._PATTERN_DOT_REVERSE.search("abc..def. other .")
        self.assertIsNotNone(match)
        self.assertEqual(match.start(), 16)
        self.assertEqual(match.end(), 17)


class TestFindAssignments(unittest.TestCase):

    def test_two_assignment_rules(self):
        source = textwrap.dedent(
            """\
        f(1) := 2.
        g(3) := 4.
        """
        )
        assignments = lexer._find_assignments(source)
        self.assertEqual(len(assignments), 2)
        self.assertEqual(source[assignments[0].start : assignments[0].end], ":=")
        self.assertEqual(assignments[0].start, 5)
        self.assertEqual(assignments[0].end, 7)
        self.assertEqual(source[assignments[1].start : assignments[1].end], ":=")
        self.assertEqual(assignments[1].start, 16)
        self.assertEqual(assignments[1].end, 18)

    def test_clingo_code(self):
        source = textwrap.dedent(
            """\
        %  l comment 1.      
        %* b comment 0. *%
              %  l comment 2.  
        f(1) %* b comment 1. *% :- %* b comment 2. *% p("string. .. .\\" end of string"). %* b comment 3. *% %  l comment 3.  
                %  l comment 4.  
        %* b comment 4. *%
        g(3) %* b comment 5. *% :- %* b comment 6. *% 4. %* b comment 7. *% %  l comment 5.  
               %  l comment 6.                    
        %* b comment 8. *%
        """
        )
        assignments = lexer._find_assignments(source)
        self.assertEqual(len(assignments), 0)

    def test_strings(self):
        source = textwrap.dedent(
            """\
        f("string . %* %.") := "string 2 %* not a comment." :- b("string.").
        p("this is not an assignment :="). %* b comment 9. *%
        """
        )
        assignments = lexer._find_assignments(source)
        self.assertEqual(len(assignments), 2)
        self.assertEqual(source[assignments[0].start : assignments[0].end], ":=")
        self.assertEqual(assignments[0].start, 20)
        self.assertEqual(assignments[0].end, 22)
        self.assertEqual(len(assignments[0].previous_non_code_tokens), 1)
        non_code_token = assignments[0].previous_non_code_tokens[0]
        self.assertEqual(
            source[non_code_token.start : non_code_token.end], '"string . %* %."'
        )
        self.assertEqual(assignments[1].type, None)
        non_code_tokens = assignments[1].previous_non_code_tokens
        self.assertEqual(len(non_code_tokens), 4)
        self.assertEqual(
            source[non_code_tokens[0].start : non_code_tokens[0].end],
            '"string 2 %* not a comment."',
        )
        self.assertEqual(
            source[non_code_tokens[1].start : non_code_tokens[1].end], '"string."'
        )
        self.assertEqual(
            source[non_code_tokens[2].start : non_code_tokens[2].end],
            '"this is not an assignment :="',
        )
        self.assertEqual(
            source[non_code_tokens[3].start : non_code_tokens[3].end],
            "%* b comment 9. *%",
        )

    def test_line_comment_before_assignment(self):
        source = textwrap.dedent(
            """\
        %  l comment.      
        f(1) := 2."""
        )
        assignments = lexer._find_assignments(source)
        self.assertEqual(len(assignments), 1)
        self.assertEqual(source[assignments[0].start : assignments[0].end], ":=")
        self.assertEqual(assignments[0].start, 25)
        non_code_tokens = assignments[0].previous_non_code_tokens
        self.assertEqual(len(non_code_tokens), 1)
        self.assertEqual(
            source[non_code_tokens[0].start : non_code_tokens[0].end],
            "%  l comment.      \n",
        )

    def test_line_comment_after_assignment(self):
        source = textwrap.dedent(
            """\
        f(1) := 2.
        f(2) := 3.
                                 %  l comment.      
        """
        )
        assignments = lexer._find_assignments(source)
        self.assertEqual(len(assignments), 3)
        self.assertEqual(source[assignments[0].start : assignments[0].end], ":=")
        self.assertEqual(assignments[0].previous_non_code_tokens, [])
        self.assertEqual(source[assignments[1].start : assignments[1].end], ":=")
        self.assertEqual(assignments[1].previous_non_code_tokens, [])
        self.assertIsNone(assignments[2].type)
        non_code_tokens = assignments[2].previous_non_code_tokens
        self.assertEqual(len(non_code_tokens), 1)
        self.assertEqual(
            source[non_code_tokens[0].start : non_code_tokens[0].end],
            "%  l comment.      \n",
        )

    def test_line_comment_end(self):
        source = textwrap.dedent(
            """\
        f(1) := 2.
        f(2) := 3.
                                 %  l comment.      """
        )
        assignments = lexer._find_assignments(source)
        self.assertEqual(len(assignments), 3)
        self.assertEqual(source[assignments[0].start : assignments[0].end], ":=")
        self.assertEqual(assignments[0].previous_non_code_tokens, [])
        self.assertEqual(source[assignments[1].start : assignments[1].end], ":=")
        self.assertEqual(assignments[1].previous_non_code_tokens, [])
        self.assertIsNone(assignments[2].type)
        non_code_tokens = assignments[2].previous_non_code_tokens
        self.assertEqual(len(non_code_tokens), 1)
        self.assertEqual(
            source[non_code_tokens[0].start : non_code_tokens[0].end],
            "%  l comment.      ",
        )

    def test_comments(self):
        source = textwrap.dedent(
            """\
        %  l comment 1.      
        %* b comment 0. *%
              %  l comment 2.  
        f(1) %* b comment 1. *% := %* b comment 2. *% 2. %* b comment 3. *% %  l comment 3.  
                %  l comment 4.  
        %* b comment 4. *%
        g(3) %* b comment 5. *% := %* b comment 6. *% 4. %* b comment 7. *% %  l comment 5.  
               %  l comment 6. 
        %* b comment 8. *%"""
        )
        assignments = lexer._find_assignments(source)
        self.assertEqual(len(assignments), 3)
        self.assertEqual(source[assignments[0].start : assignments[0].end], ":=")
        self.assertEqual(assignments[0].start, 89)
        self.assertEqual(assignments[0].end, 91)
        non_code_tokens = assignments[0].previous_non_code_tokens
        self.assertEqual(len(non_code_tokens), 4)
        self.assertEqual(
            source[non_code_tokens[0].start : non_code_tokens[0].end],
            "%  l comment 1.      \n",
        )
        self.assertEqual(
            source[non_code_tokens[1].start : non_code_tokens[1].end],
            "%* b comment 0. *%",
        )
        self.assertEqual(
            source[non_code_tokens[2].start : non_code_tokens[2].end],
            "%  l comment 2.  \n",
        )
        self.assertEqual(
            source[non_code_tokens[3].start : non_code_tokens[3].end],
            "%* b comment 1. *%",
        )
        self.assertEqual(source[assignments[1].start : assignments[1].end], ":=")
        self.assertEqual(assignments[1].start, 220)
        non_code_tokens = assignments[1].previous_non_code_tokens
        self.assertEqual(len(non_code_tokens), 6)
        self.assertEqual(
            source[non_code_tokens[0].start : non_code_tokens[0].end],
            "%* b comment 2. *%",
        )
        self.assertEqual(
            source[non_code_tokens[1].start : non_code_tokens[1].end],
            "%* b comment 3. *%",
        )
        self.assertEqual(
            source[non_code_tokens[2].start : non_code_tokens[2].end],
            "%  l comment 3.  \n",
        )
        self.assertEqual(
            source[non_code_tokens[3].start : non_code_tokens[3].end],
            "%  l comment 4.  \n",
        )
        self.assertEqual(
            source[non_code_tokens[4].start : non_code_tokens[4].end],
            "%* b comment 4. *%",
        )
        self.assertEqual(
            source[non_code_tokens[5].start : non_code_tokens[5].end],
            "%* b comment 5. *%",
        )
        self.assertIsNone(assignments[2].type)
        non_code_tokens = assignments[2].previous_non_code_tokens
        self.assertEqual(len(non_code_tokens), 5)
        self.assertEqual(
            source[non_code_tokens[0].start : non_code_tokens[0].end],
            "%* b comment 6. *%",
        )
        self.assertEqual(
            source[non_code_tokens[1].start : non_code_tokens[1].end],
            "%* b comment 7. *%",
        )
        self.assertEqual(
            source[non_code_tokens[2].start : non_code_tokens[2].end],
            "%  l comment 5.  \n",
        )
        self.assertEqual(
            source[non_code_tokens[3].start : non_code_tokens[3].end],
            "%  l comment 6. \n",
        )
        self.assertEqual(
            source[non_code_tokens[4].start : non_code_tokens[4].end],
            "%* b comment 8. *%",
        )

    def test_nested_comment(self):
        source = textwrap.dedent(
            """\
        %* block comment with a % % %* nested comment % % *% that continues. *% % and a line comment
        f(1) := 2."""
        )
        assignments = lexer._find_assignments(source)
        self.assertEqual(len(assignments), 1)
        self.assertEqual(source[assignments[0].start : assignments[0].end], ":=")
        non_code_tokens = assignments[0].previous_non_code_tokens
        self.assertEqual(len(non_code_tokens), 2)
        self.assertEqual(
            source[non_code_tokens[0].start : non_code_tokens[0].end],
            "%* block comment with a % % %* nested comment % % *% that continues. *%",
        )
        self.assertEqual(
            source[non_code_tokens[1].start : non_code_tokens[1].end],
            "% and a line comment\n",
        )

    def test_unclosed_comment(self):
        source = textwrap.dedent(
            """\
        f(1) := 2.
        %* unclosed comment
        f(3) := 4.
        """
        )
        assignments = lexer._find_assignments(source)
        self.assertEqual(len(assignments), 2)
        self.assertEqual(source[assignments[0].start : assignments[0].end], ":=")
        self.assertIsInstance(assignments[1], lexer.ErrorToken)
        self.assertEqual(assignments[1].start, 11)
        self.assertEqual(assignments[1].end, len(source))
        self.assertEqual(
            source[assignments[1].start : assignments[1].end],
            textwrap.dedent(
                """\
                %* unclosed comment
                f(3) := 4.
                """
            ),
        )

    def test_unclosed_string(self):
        source = textwrap.dedent(
            """\
        f(0) := 1.
        f("unclosed string) := 2.
        """
        )
        assignments = lexer._find_assignments(source)
        self.assertEqual(len(assignments), 2)
        self.assertEqual(source[assignments[0].start : assignments[0].end], ":=")
        self.assertIsInstance(assignments[1], lexer.ErrorToken)
        self.assertEqual(assignments[1].start, 13)
        self.assertEqual(assignments[1].end, len(source))
        self.assertEqual(
            source[assignments[1].start : assignments[1].end],
            textwrap.dedent(
                """\
                "unclosed string) := 2.
                """
            ),
        )

    def test_unclosed_string_in_clingo_code(self):
        source = textwrap.dedent(
            """\
        p("some string").
        f("unclosed string) := 2.
        """
        )
        assignments = lexer._find_assignments(source)
        self.assertEqual(len(assignments), 2)
        self.assertIsNone(assignments[0].type)
        no_code_tokens = assignments[0].previous_non_code_tokens
        self.assertEqual(
            source[no_code_tokens[0].start : no_code_tokens[0].end], '"some string"'
        )
        self.assertIsInstance(assignments[1], lexer.ErrorToken)
        self.assertEqual(assignments[1].start, 20)
        self.assertEqual(assignments[1].end, len(source))
        self.assertEqual(
            source[assignments[1].start : assignments[1].end],
            textwrap.dedent(
                """\
                "unclosed string) := 2.
                """
            ),
        )

    def test_unclosed_comment_in_clingo_code(self):
        source = textwrap.dedent(
            """\
            p("some string").
            %* unclosed comment
            """
        )
        assignments = lexer._find_assignments(source)
        self.assertEqual(len(assignments), 2)
        self.assertIsNone(assignments[0].type)
        no_code_tokens = assignments[0].previous_non_code_tokens
        self.assertEqual(
            source[no_code_tokens[0].start : no_code_tokens[0].end], '"some string"'
        )
        self.assertIsInstance(assignments[1], lexer.ErrorToken)
        self.assertEqual(assignments[1].start, 18)
        self.assertEqual(assignments[1].end, len(source))
        self.assertEqual(
            source[assignments[1].start : assignments[1].end],
            textwrap.dedent(
                """\
                %* unclosed comment
                """
            ),
        )


class TestFindDot(unittest.TestCase):
    
    def test_find_dot(self):
        source = textwrap.dedent(
            """\
            f(a) := 1.
            """
        )
        assignments = lexer._find_assignments(source)
        previous_dot = lexer._find_previous_dot(source, 0, assignments[0].start, assignments[0].previous_non_code_tokens)
        self.assertIsNone(previous_dot)
        next_dot = lexer._find_next_dot(source, assignments[0].end, len(source), assignments[0].previous_non_code_tokens)
        self.assertIsNotNone(next_dot)
        self.assertEqual(source[:next_dot.end()], "f(a) := 1.")

    def test_find_dot2(self):
        source = textwrap.dedent(
            """\
            f(a) := 1.
            f(b) := 1.
            """
        )
        assignments = lexer._find_assignments(source)
        previous_dot = lexer._find_previous_dot(source, 0, assignments[0].start, assignments[0].previous_non_code_tokens)
        self.assertIsNone(previous_dot)
        next_dot = lexer._find_next_dot(source, assignments[0].end, assignments[1].start, assignments[0].previous_non_code_tokens)
        self.assertIsNotNone(next_dot)
        self.assertEqual(source[:next_dot.end()], "f(a) := 1.")
        previous_dot = lexer._find_previous_dot(source, 0, assignments[1].start, assignments[1].previous_non_code_tokens)
        self.assertIsNotNone(previous_dot)
        next_dot = lexer._find_next_dot(source, assignments[1].end, len(source), [])
        self.assertIsNotNone(next_dot)
        self.assertEqual(source[previous_dot.end()+1:next_dot.end()], "f(b) := 1.")
        

    def test_string(self):
        source = textwrap.dedent(
            """\
            f("one string. something else.") := "another string. something else.".
            fact("fact string").
            g("yet another string. something else.") := "final string string. something else.".
            """
        )
        assignments = lexer._find_assignments(source)
        previous_dot = lexer._find_previous_dot(source, 0, assignments[0].start, assignments[0].previous_non_code_tokens)
        self.assertIsNone(previous_dot)
        next_dot = lexer._find_next_dot(source, assignments[0].end, assignments[1].start, assignments[1].previous_non_code_tokens)
        self.assertIsNotNone(next_dot)
        self.assertEqual(source[0:next_dot.end()], 'f("one string. something else.") := "another string. something else.".')
        previous_dot = lexer._find_previous_dot(source, 0, assignments[1].start, assignments[1].previous_non_code_tokens)
        self.assertIsNotNone(previous_dot)
        next_dot = lexer._find_next_dot(source, assignments[1].end, len(source), assignments[2].previous_non_code_tokens)
        self.assertIsNotNone(next_dot)
        self.assertEqual(source[previous_dot.end()+1:next_dot.end()], 'g("yet another string. something else.") := "final string string. something else.".')
        

class TestFindAssignmentsRules(unittest.TestCase):
    def test_find_assignment_rules(self):
        source = textwrap.dedent(
            """\
        f(1) := 2. f(a) := b.
           # comment
        f(3) := 4.  f(5) := 6.

        """
        )
        assignments = lexer._find_assignments(source)
        rules = lexer._find_assignment_rules(source, assignments)
        self.assertEqual(len(rules), 4)
        self.assertIsInstance(rules[0], lexer._UnParsedAssignmentRule)
        self.assertIsInstance(rules[1], lexer._UnParsedAssignmentRule)
        self.assertIsInstance(rules[2], lexer._UnParsedAssignmentRule)
        self.assertIsInstance(rules[3], lexer._UnParsedAssignmentRule)
        self.assertEqual(rules[0].source, 'f(1) := 2.')
        self.assertEqual(rules[1].source, ' f(a) := b.')
        self.assertEqual(rules[2].source, '\n   # comment\nf(3) := 4.')
        self.assertEqual(rules[3].source, '  f(5) := 6.')
        self.assertEqual(rules[0].start_line, 0)
        self.assertEqual(rules[1].start_line, 0)
        self.assertEqual(rules[2].start_line, 0)
        self.assertEqual(rules[3].start_line, 2)
        self.assertEqual(rules[0].start_col, 0)
        self.assertEqual(rules[1].start_col, 10)
        # rules[2].start_col does not matter because it starts with a new line
        self.assertEqual(rules[3].start_col, 10)
        self.assertEqual(rules[0].assignment_pos, 5)
        self.assertEqual(rules[1].assignment_pos, 6)
        self.assertEqual(rules[2].assignment_pos, 5 + len("\n   # comment\n"))
        self.assertEqual(rules[3].assignment_pos, 7)

    def test_starting_clingo(self):
        source = textwrap.dedent(
            """\
        p(a).
           # comment
        p(b) :- p(a).
        f(3) := 4.
        """
        )
        assignments = lexer._find_assignments(source)
        rules = lexer._find_assignment_rules(source, assignments)
        self.assertEqual(len(rules), 2)
        self.assertIsInstance(rules[0], lexer._UnParsedClingoCode)
        self.assertIsInstance(rules[1], lexer._UnParsedAssignmentRule)
        self.assertEqual(rules[0].source, 'p(a).\n   # comment\np(b) :- p(a).')
        self.assertEqual(rules[0].start_line, 0)
        self.assertEqual(rules[0].start_col, 0)
        self.assertEqual(rules[1].source, '\nf(3) := 4.')
        self.assertEqual(rules[1].start_line, 2)
        self.assertEqual(rules[1].start_col, 13)

    def test_ending_clingo(self):
        source = textwrap.dedent(
            """\
        f(3) := 4.
        p(a).
           # comment
        p(b) :- p(a).
        """
        )
        assignments = lexer._find_assignments(source)
        rules = lexer._find_assignment_rules(source, assignments)
        self.assertEqual(len(rules), 2)
        self.assertIsInstance(rules[0], lexer._UnParsedAssignmentRule)
        self.assertIsInstance(rules[1], lexer._UnParsedClingoCode)
        self.assertEqual(rules[0].source, 'f(3) := 4.')
        self.assertEqual(rules[1].source, '\np(a).\n   # comment\np(b) :- p(a).\n')
        self.assertEqual(rules[0].start_line, 0)
        self.assertEqual(rules[0].start_col, 0)
        self.assertEqual(rules[1].start_line, 0)
        # rules[1].start_col does not matter

    def test_middle_clingo(self):
        source = textwrap.dedent(
            """\
        f(3) := 4.
        p(a).
           # comment
        p(b) :- p(a).
        f(5) := 6.
        """
        )
        assignments = lexer._find_assignments(source)
        rules = lexer._find_assignment_rules(source, assignments)
        self.assertEqual(len(rules), 3)
        self.assertIsInstance(rules[0], lexer._UnParsedAssignmentRule)
        self.assertIsInstance(rules[1], lexer._UnParsedClingoCode)
        self.assertIsInstance(rules[2], lexer._UnParsedAssignmentRule)
        self.assertEqual(rules[0].source, 'f(3) := 4.')
        self.assertEqual(rules[1].source, '\np(a).\n   # comment\np(b) :- p(a).')
        self.assertEqual(rules[2].source, '\nf(5) := 6.')
