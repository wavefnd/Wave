import unittest
from pathlib import Path
import sys

sys.path.append(str(Path(__file__).resolve().parent))
import importlib.util

spec = importlib.util.spec_from_file_location("format_rust", Path(__file__).resolve().parent / "format-rust.py")
format_rust = importlib.util.module_from_spec(spec)
sys.modules["format_rust"] = format_rust
spec.loader.exec_module(format_rust)

class TestFormatRustRegressions(unittest.TestCase):

    def _strip_lines(self, code_snippet):
        """Helper to run strip_lexical_content across multiple lines with state tracking."""
        state = {
            "block_comment": 0,
            "string": None,
            "char": False,
            "escape": False,
        }
        lines = code_snippet.splitlines(keepends=True)
        return "".join(format_rust.strip_lexical_content(line, state) for line in lines)

    def test_ordinary_strings_and_escapes(self):
        code = 'let s = "hello \\"world\\" { return 1; }";\n'
        stripped = self._strip_lines(code)
        # Ordinary string contents should be blanked out, but braces inside shouldn't break things
        self.assertIn("let s =", stripped)
        self.assertNotIn("hello", stripped)

    def test_raw_strings_with_different_hash_counts(self):
        # Test standard raw string, 1-hash raw string, and multi-hash raw strings
        code = (
            'let r1 = r"ordinary raw";\n'
            'let r2 = r#"embedded { braces } and "quotes"#;\n'
            'let r3 = r###"multiple hashes ### here { }"###;\n'
        )
        stripped = self._strip_lines(code)
        # Ensure the content inside raw strings is properly masked out without throwing type errors
        self.assertNotIn("embedded", stripped)
        self.assertNotIn("multiple", stripped)

    def test_byte_raw_strings(self):
        code = 'let br = br##"byte raw string data ##"##;\n'
        stripped = self._strip_lines(code)
        self.assertNotIn("byte raw string data", stripped)

    def test_multiline_raw_strings(self):
        code = (
            'let multi = r#"\n'
            '    fun fake_inner_function() -> i32 { return 0; }\n'
            '"#;\n'
        )
        stripped = self._strip_lines(code)
        # A fake function inside a raw string should be completely masked out
        # so function_ranges() doesn't falsely detect it as a real function start.
        self.assertNotIn("fun fake_inner_function", stripped)

    def test_invalid_token_boundary_recognition(self):
        # A prefix attached to an identifier should NOT trigger raw-string mode
        code = 'let not_raw = identifier_r = "regular string"#;\n'
        stripped = self._strip_lines(code)
        # Should parse normally without treating identifier_r as a raw string keyword
        self.assertIn("identifier_r", stripped)

    def test_code_after_ordinary_and_byte_strings(self):
        code = (
            r'let text = "brace } and \"quote\""; let number = 7;' + "\n"
            + 'let bytes = b"payload { }"; fn recovered() {}\n'
        )
        stripped = self._strip_lines(code)
        self.assertIn("let number = 7;", stripped)
        self.assertIn("fn recovered() {}", stripped)
        self.assertNotIn("brace", stripped)
        self.assertNotIn("payload", stripped)

    def test_function_ranges_after_plain_string(self):
        source = (
            'fn first() {\n'
            '    let text = "hello";\n'
            '}\n'
            'fn second() {}\n'
        )
        self.assertEqual(
            format_rust.function_ranges(source.splitlines(keepends=True)),
            [(0, 2, 0), (3, 3, 0)],
        )

    def test_spacing_after_mixed_string_literals(self):
        source = (
            'fn first() {\n'
            + r'    let plain = "escaped \" brace { }";' + "\n"
            + '    let raw = r#"raw } { text"#;\n'
            + '    let bytes = b"byte } {";\n'
            + '    let byte_raw = br##"raw bytes } {"##;\n'
            + '}\n'
            + 'fn second() {}\n'
        )
        expected = source.replace('}\nfn second()', '}\n\nfn second()')
        self.assertEqual(
            format_rust.function_ranges(source.splitlines(keepends=True)),
            [(0, 5, 0), (6, 6, 0)],
        )
        self.assertEqual(format_rust.add_function_spacing(source), expected)
        self.assertEqual(format_rust.add_function_spacing(expected), expected)

    def test_formatting_idempotency(self):
        sample = (
            "pub fn first() -> i32 {\n"
            "    return 1;\n"
            "}\n"
            "\n"
            "pub fn second() -> i32 {\n"
            "    return 2;\n"
            "}\n"
        )
        pass1 = format_rust.add_function_spacing(sample)
        pass2 = format_rust.add_function_spacing(pass1)
        self.assertEqual(pass1, pass2, "Formatting must be strictly idempotent")

    def test_spacing_after_character_literals(self):
        for literal in ["'a'", "'_'", "'é'", "'🦀'", "'{'", "'}'",
                        r"'\''", r"'\\'", r"'\n'", r"'\x61'", r"'\u{1F980}'",
                        "b'a'", "b'_'", "b'{'", r"b'\''", r"b'\\'", r"b'\x61'"]:
            with self.subTest(literal=literal):
                source = f"fn first() {{\n    let c = {literal};\n}}\nfn second() {{}}\n"
                expected = source.replace("}\nfn second", "}\n\nfn second")
                self.assertEqual(format_rust.add_function_spacing(source), expected)
                self.assertEqual(format_rust.add_function_spacing(expected), expected)
                self.assertEqual(
                    format_rust.function_ranges(source.splitlines(keepends=True)),
                    [(0, 2, 0), (3, 3, 0)],
                )

    def test_lifetimes_and_loop_labels_survive_character_literals(self):
        source = (
            "fn first<'a>(text: &'a str, other: &'_ str) -> &'a str {\n"
            "    let _: &'static str = other;\n"
            "    'outer: loop { let c = 'a'; break 'outer; }\n"
            "    text\n"
            "}\n"
            "fn second<'℘>(text: &'℘ str) {}\n"
            "fn third() {}\n"
        )
        expected = source.replace("}\nfn ", "}\n\nfn ")
        self.assertEqual(format_rust.add_function_spacing(source), expected)
        self.assertEqual(format_rust.add_function_spacing(expected), expected)

    def test_spacing_before_attached_function_metadata(self):
        for attachment in [
            "#[test]\n",
            '#[cfg_attr(\n    test,\n    doc = "brackets [ ] and braces { }",\n)]\n#[test]\n',
            "/// Documentation\n/// More documentation\n",
            "/** Documentation\n * with a nested /* comment */\n */\n",
            "// An ordinary attached comment\n",
            "/* An ordinary\n * block comment */\n",
            "/// Documentation\n#[test]\n// A comment\n",
            "#[test]\n\n",
            "/// Documentation\n\n",
        ]:
            with self.subTest(attachment=attachment):
                source = "fn first() {}\n" + attachment + "fn second() {}\n"
                expected = "fn first() {}\n\n" + attachment + "fn second() {}\n"
                self.assertEqual(format_rust.add_function_spacing(source), expected)
                self.assertEqual(format_rust.add_function_spacing(expected), expected)

    def test_multiline_ordinary_comments_attach_to_functions(self):
        # Keep the block-comment state across lines, even blank lines and
        # nested comments, instead of mistaking comment text for Rust code.
        for attachment in [
            "/* ordinary first line\n * ordinary second line\n */\n",
            "/* text\n\n * after a blank line */\n",
            "/* outer /* nested */ outer again\n * continued */\n",
            "/* ordinary\n * continued */ #[inline]\n",
            "#[inline]\n/* ordinary\n * continued */\n",
        ]:
            with self.subTest(attachment=attachment):
                source = "fn first() {}\n" + attachment + "fn second() {}\n"
                expected = "fn first() {}\n\n" + attachment + "fn second() {}\n"
                lines = source.splitlines(keepends=True)
                self.assertEqual(format_rust.attached_item_start(lines, 0, len(lines) - 1), 1)
                self.assertEqual(format_rust.add_function_spacing(source), expected)
                self.assertEqual(format_rust.add_function_spacing(expected), expected)

    def test_unicode_lifetime_identifier_continuations(self):
        # Multiscalar identifiers and combining characters must not leave
        # the lexer in character-literal mode.
        for name in ["αβ", "école", "℘x", "a\u0301", "Ω\u0308_β2", "a·β", "字\u0301_語"]:
            with self.subTest(lifetime=name):
                source = (
                    f"fn first<'{name}>(value: &'{name} str) {{\n"
                    "    let ch = '℘';\n"
                    "}\nfn second() {}\n"
                )
                expected = source.replace("}\nfn second()", "}\n\nfn second()")
                self.assertEqual(
                    format_rust.function_ranges(source.splitlines(keepends=True)),
                    [(0, 2, 0), (3, 3, 0)],
                )
                self.assertEqual(format_rust.add_function_spacing(source), expected)
                self.assertEqual(format_rust.add_function_spacing(expected), expected)

    def test_metadata_spacing_in_nested_scopes(self):
        source = (
            "impl Example {\n"
            "    fn first() {}\n"
            "    /// The second method\n"
            "    #[inline]\n"
            "    fn second() {}\n"
            "}\n"
        )
        expected = source.replace("    ///", "\n    ///")
        self.assertEqual(format_rust.add_function_spacing(source), expected)
        self.assertEqual(format_rust.add_function_spacing(expected), expected)

    def test_spacing_preserves_unrelated_items_and_comment_association(self):
        for between in [
            "const VALUE: i32 = 1;\n#[test]\n",
            "struct Other {}\n/// Documentation\n",
            "}\nimpl Other {\n#[test]\n",
            "//! Inner documentation\n",
            "/*! Inner documentation */\n",
            "// A comment about the previous function\n\n",
            "\n// Already separated\n",
            "\n#[test]\n",
        ]:
            with self.subTest(between=between):
                source = "fn first() {}\n" + between + "fn second() {}\n"
                self.assertEqual(format_rust.add_function_spacing(source), source)

    def test_trailing_comment_stays_with_previous_function(self):
        source = "fn first() {} // First function\n// Second function\nfn second() {}\n"
        expected = source.replace("\n// Second", "\n\n// Second")
        self.assertEqual(format_rust.add_function_spacing(source), expected)

if __name__ == "__main__":
    unittest.main()
