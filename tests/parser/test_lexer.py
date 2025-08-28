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
        # print(match)
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
            '"',
        )
        self.assertFindFirst(
            'p(1,2,"string").',
            0,
            6,
            '"',
        )
        self.assertFindFirst(
            'p(1,2,"unclosed string).',
            0,
            6,
            '"',
        )
        self.assertFindFirst(
            r"""", %* block comment *%""",
            0,
            0,
            r'"',
        )
        self.assertFindFirst(
            r"""", %* block comment *% body.""",
            0,
            0,
            r'"',
        )
        string = r"""p("this string has a %* %  *%", %* block comment *% :- body. % line comment"""
        self.assertFindFirst(
            string,
            0,
            2,
            r'"',
        )
        self.assertFindFirst(
            string,
            29,
            29,
            r'"',
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
            50,
            r"%",
        )

    def find_string_end(self, source: str, start: int):
        """
        Find the first non-code block delimiter in the source code.
        """
        match = lexer._PATTERN_STRING_END.match(source, start)
        return match

    def assertFindStringEnd(self, code, start, value):
        match = self.find_string_end(code, start)
        if value is None:
            self.assertIsNone(match)
            return
        self.assertIsNotNone(match)
        self.assertEqual(match.group(), value)

    def test_find_string_end(self):
        self.assertFindStringEnd(
            '"string"',
            1,
            'string"',
        )
        self.assertFindStringEnd(
            'p(1,2,"string").',
            7,
            'string"',
        )
        self.assertFindStringEnd(
            'p(1,2,"unclosed string).',
            7,
            None,
        )
        self.assertFindStringEnd(
            r'p(1,2,"string % %* aa *% % %* % *% string").',
            7,
            r'string % %* aa *% % %* % *% string"',
        )
        self.assertFindStringEnd(
            r'p(1,2,"string \" \\ \" \\").',
            7,
            r'string \" \\ \" \\"',
        )
        self.assertFindStringEnd(
            r'p(1,2,"unclosed % string).',
            7,
            None,
        )
        self.assertFindStringEnd(
            r'p(1,2,"unclosed %* string).',
            7,
            None,
        )
        self.assertFindStringEnd(
            r'p(1,2,"unclosed *% string).',
            7,
            None,
        )
        self.assertFindStringEnd(
            r'p(1,2,"string %* *% string).',
            7,
            None,
        )
        self.assertFindStringEnd(
            r'p(1,2,"string %* % *% string).',
            7,
            None,
        )
        self.assertFindStringEnd(
            r'p(1,2,"string %* % *% %* string).',
            7,
            None,
        )
        self.assertFindStringEnd(
            r'p(1,2,"string %* % *% %* *% string).',
            7,
            None,
        )
        self.assertFindStringEnd(
            r'p(1,2,"very long string very long string).',
            7,
            None,
        )
        # string = r'''p("this string has a %* %  *%", %* block comment *% :- body. % line comment'''
        # self.assertFindStringEnd(
        #     string,
        #     0,
        #     2,
        #     r'"',
        # )
        # self.assertFindStringEnd(
        #     string,
        #     29,
        #     29,
        #     r'"',
        # )
        # self.assertFindStringEnd(
        #     string,
        #     30,
        #     32,
        #     r'%*',
        # )
        # self.assertFindStringEnd(
        #     string,
        #     48,
        #     50,
        #     r'%',
        # )
