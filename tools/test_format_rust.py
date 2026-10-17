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

if __name__ == "__main__":
    unittest.main()
