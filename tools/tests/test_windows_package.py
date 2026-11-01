# SPDX-License-Identifier: MPL-2.0
"""PE release inspection rejects incomplete, foreign and unresolved payloads."""
from pathlib import Path
import struct
import subprocess
import tempfile
import unittest
from unittest.mock import patch

from tools import windows_package as pe
from tools.check_msvc_package import controlled_environment


def write_pe(path, machine=0x8664):
    data = bytearray(512)
    data[:2] = b'MZ'
    struct.pack_into('<I', data, 60, 64)
    data[64:68] = b'PE\0\0'
    struct.pack_into('<HH', data, 68, machine, 1)
    struct.pack_into('<H', data, 84, 112)
    struct.pack_into('<H', data, 88, 0x20b)
    struct.pack_into('<II', data, 216, 256, 256)
    path.write_bytes(data)
    return path


class PackageTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        # Windows temp paths may use an 8.3 alias (e.g. RUNNER~1).
        # dependency_closure returns canonical paths on every host.
        self.root = Path(self.temporary.name).resolve()
        self.exe = write_pe(self.root / 'wavec.exe')
        self.inspector = self.root / 'llvm-readobj.exe'
        self.inspector.touch()

    def closure(self, mapping, **kwargs):
        def inspect(path, tool):
            self.assertEqual(tool, self.inspector)
            return mapping.get(path.name.lower(), [])
        with patch.object(pe, 'imports', side_effect=inspect):
            return pe.dependency_closure([self.exe], 'x86_64-pc-windows-msvc',
                                         self.inspector, [self.root], **kwargs)

    def test_inspector_is_mandatory_and_failures_are_not_empty_import_lists(self):
        with self.assertRaises(FileNotFoundError):
            pe.imports(self.exe, self.root / 'missing.exe')
        for code, out, err in [(1, '', 'failed'), (0, '', ''),
                               (0, 'Format: COFF-x86-64\n', 'warning')]:
            with self.subTest(code=code, out=out, err=err), patch.object(
                    pe.subprocess, 'run', return_value=subprocess.CompletedProcess([], code, out, err)):
                with self.assertRaises(ValueError):
                    pe.imports(self.exe, self.inspector)
        with patch.object(pe.subprocess, 'run', side_effect=subprocess.TimeoutExpired('readobj', 30)):
            with self.assertRaises(subprocess.TimeoutExpired):
                pe.imports(self.exe, self.inspector)

    def test_corrupt_pe_cannot_reach_inspector(self):
        valid = self.exe.read_bytes()
        damaged_offset = bytearray(valid)
        struct.pack_into('<I', damaged_offset, 60, 0xffffffff)
        for data in [b'', b'MZ', valid[:160], valid[:400], damaged_offset]:
            self.exe.write_bytes(data)
            with self.subTest(length=len(data)), patch.object(pe.subprocess, 'run') as run:
                with self.assertRaises(ValueError):
                    pe.imports(self.exe, self.inspector)
                run.assert_not_called()

    def test_transitive_dependencies_and_cycles_are_complete(self):
        first = write_pe(self.root / 'LLVM.dll')
        second = write_pe(self.root / 'zstd.dll')
        payloads, prerequisites = self.closure({
            'wavec.exe': ['LLVM.dll', 'KERNEL32.dll', 'api-ms-win-core-file-l1-1-0.dll'],
            'llvm.dll': ['ZSTD.dll'], 'zstd.dll': ['LLVM.dll'],
        })
        self.assertEqual(payloads, {'llvm.dll': first, 'zstd.dll': second})
        self.assertEqual(prerequisites, {})

    def test_missing_transitive_and_mingw_imports_are_fatal(self):
        write_pe(self.root / 'LLVM.dll')
        with self.assertRaisesRegex(FileNotFoundError, 'missing dependency absent.dll'):
            self.closure({'wavec.exe': ['LLVM.dll'], 'llvm.dll': ['absent.dll']})
        with self.assertRaisesRegex(ValueError, 'retired MinGW'):
            self.closure({'wavec.exe': ['libwinpthread-1.dll']})

    def test_foreign_root_and_foreign_dependency_are_fatal(self):
        write_pe(self.exe, 0xaa64)
        with self.assertRaisesRegex(ValueError, 'wrong PE machine'):
            self.closure({})
        write_pe(self.exe)
        write_pe(self.root / 'LLVM.dll', 0xaa64)
        with self.assertRaisesRegex(ValueError, 'wrong PE machine'):
            self.closure({'wavec.exe': ['LLVM.dll']})

    def test_vc_runtime_must_be_installed_and_is_not_bundled(self):
        mapping = {'wavec.exe': ['VCRUNTIME140.dll']}
        write_pe(self.root / 'vcruntime140.dll')
        self.assertFalse(pe.system_dll('VCRUNTIME140.dll'))
        with self.assertRaisesRegex(FileNotFoundError, 'VC runtime prerequisite'):
            self.closure(mapping)
        installed = self.root / 'System32'
        installed.mkdir()
        runtime = write_pe(installed / 'vcruntime140.dll')
        payloads, prerequisites = self.closure(mapping, runtime_dirs=[installed])
        self.assertEqual(payloads, {})
        self.assertEqual(prerequisites, {'vcruntime140.dll': runtime})

    def test_malformed_import_name_is_rejected(self):
        output = 'Format: COFF-x86-64\nImport {\n Name: ..\\evil.dll\n}\n'
        with patch.object(pe.subprocess, 'run', return_value=subprocess.CompletedProcess([], 0, output, '')):
            with self.assertRaisesRegex(ValueError, 'invalid imported DLL'):
                pe.imports(self.exe, self.inspector)

    def test_empty_imports_and_names_with_spaces_are_distinguished(self):
        for output, names in [
            ('Format: COFF-x86-64\n', []),
            ('Format: COFF-x86-64\nImport {\n Name: vendor library.dll\n}\n'
             'DelayImport {\n Name: another.dll\n}\n', ['vendor library.dll', 'another.dll']),
        ]:
            with patch.object(pe.subprocess, 'run', return_value=subprocess.CompletedProcess([], 0, output, '')):
                self.assertEqual(pe.imports(self.exe, self.inspector), names)
        with patch.object(pe.subprocess, 'run', return_value=subprocess.CompletedProcess(
                [], 0, 'Format: COFF-x86-64\nImport {\n}\n', '')):
            with self.assertRaisesRegex(ValueError, 'incomplete PE import'):
                pe.imports(self.exe, self.inspector)

    def test_delay_import_symbols_are_not_additional_dlls(self):
        # llvm-readobj --coff-imports nests symbol Import records here.
        output = '''Format: COFF-x86-64
Import {
  Name: KERNEL32.dll
  Symbol: ExitProcess (0)
}
DelayImport {
  Name: vendor.dll
  Attributes: 0x1
  Import {
    Symbol: vendor_fn (0)
    Address: 0x140001038
  }
  Import {
    Symbol: another_fn (1)
    Address: 0x140001040
  }
}
DelayImport {
  Name: second.dll
  Import {
    Symbol: other_fn (0)
    Address: 0x140001048
  }
}
'''
        for arch in ('x86-64', 'ARM64'):
            with self.subTest(arch=arch), patch.object(
                    pe.subprocess, 'run', return_value=subprocess.CompletedProcess(
                        [], 0, output.replace('x86-64', arch), '')):
                self.assertEqual(pe.imports(self.exe, self.inspector),
                                 ['KERNEL32.dll', 'vendor.dll', 'second.dll'])

    def test_incomplete_or_ambiguous_import_scopes_are_rejected(self):
        for body in (
            'Import {\n Name: vendor.dll\n',
            'DelayImport {\n Name: vendor.dll\n Import {\n Symbol: fn (0)\n}\n',
            'DelayImport {\n Import {\n Symbol: fn (0)\n}\n}\n',
            'Import {\n Name: first.dll\n Name: second.dll\n}\n',
            'Import {\n Name: first.dll\n Import {\n Name: second.dll\n}\n}\n',
            'Name: orphan.dll\n',
            '}\n',
        ):
            with self.subTest(body=body), patch.object(
                    pe.subprocess, 'run', return_value=subprocess.CompletedProcess(
                        [], 0, 'Format: COFF-x86-64\n' + body, '')):
                with self.assertRaisesRegex(ValueError, 'incomplete PE import'):
                    pe.imports(self.exe, self.inspector)

    def test_arm64x_hybrid_view_does_not_change_native_dependencies(self):
        output = '''Format: COFF-ARM64X
Import {
  Name: native.dll
  Symbol: native_fn (0)
}
DelayImport {
  Name: native-delay.dll
  Import {
    Symbol: delayed_fn (0)
    Address: 0x180001000
  }
}
HybridObject {
  Format: COFF-ARM64EC
  Arch: aarch64
  AddressSize: 64bit
  Import {
    Name: ec-only.dll
    Symbol: ec_fn (0)
  }
  DelayImport {
    Name: ec-delay.dll
    Import {
      Symbol: delayed_ec_fn (0)
      Address: 0x180002000
    }
  }
}
'''
        write_pe(self.exe, 0xaa64)
        with patch.object(pe.subprocess, 'run', return_value=subprocess.CompletedProcess(
                [], 0, output, '')):
            self.assertEqual(pe.imports(self.exe, self.inspector),
                             ['native.dll', 'native-delay.dll'])

    def test_hybrid_view_keeps_structural_and_name_validation(self):
        for body in (
            'HybridObject {\n',
            'HybridObject {\n Import {\n}\n}\n',
            'HybridObject {\n Import {\n Name: a.dll\n Name: b.dll\n}\n}\n',
            'HybridObject {\n HybridObject {\n}\n}\n',
            'Import {\n Name: native.dll\n HybridObject {\n}\n}\n',
            'HybridObject {\n Import {\n Name: ../bad.dll\n}\n}\n',
            'HybridObject {\n DelayImport {\n Name: ec.dll\n Import {\n}\n}\n',
        ):
            with self.subTest(body=body), patch.object(
                    pe.subprocess, 'run', return_value=subprocess.CompletedProcess(
                        [], 0, 'Format: COFF-ARM64X\n' + body, '')):
                with self.assertRaises(ValueError):
                    pe.imports(self.exe, self.inspector)

    def test_package_processes_cannot_inherit_build_toolchain_or_home(self):
        env = controlled_environment(self.root / 'package', self.root / 'home', {
            'SystemRoot': 'C:/Windows', 'ProgramFiles(x86)': 'C:/Program Files (x86)',
            'PATH': 'C:/build-llvm/bin;C:/msys64/bin', 'LIB': 'C:/build-sdk/lib',
            'INCLUDE': 'C:/build-sdk/include', 'WAVE_LLVM_HOME': 'C:/build-llvm',
            'LLVM_SYS_211_PREFIX': 'C:/build-llvm', 'HOME': 'C:/checkout',
            'VCToolsInstallDir': 'C:/VS/build', 'WindowsSdkDir': 'C:/SDK/build',
        })
        for key in ('LIB', 'INCLUDE', 'WAVE_LLVM_HOME', 'LLVM_SYS_211_PREFIX',
                    'VCToolsInstallDir', 'WindowsSdkDir'):
            self.assertNotIn(key, env)
        self.assertNotIn('build-llvm', env['PATH'])
        self.assertNotIn('msys64', env['PATH'])
        self.assertEqual(env['HOME'], str(self.root / 'home'))
        self.assertEqual(env['ProgramFiles(x86)'], 'C:/Program Files (x86)')


if __name__ == '__main__':
    unittest.main()
