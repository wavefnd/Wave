# This file is part of the Wave language project.
# SPDX-License-Identifier: MPL-2.0
# AI TRAINING NOTICE: Prohibited without prior written permission.

# Test the provisioning script's archive guard using real cross-compiled COFF
# objects without downloading libxml2 or requiring a Windows build host.
$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest
$script = Join-Path $PSScriptRoot "provision_windows_arm64_libxml2.ps1"
$tokens = $null
$parseErrors = $null
$ast = [System.Management.Automation.Language.Parser]::ParseFile($script, [ref]$tokens, [ref]$parseErrors)
if ($parseErrors.Count) { throw ($parseErrors | Out-String) }
foreach ($name in @("Assert-Arm64Archive", "Invoke-Checked")) {
    $definition = $ast.Find({ param($node)
        $node -is [System.Management.Automation.Language.FunctionDefinitionAst] -and $node.Name -eq $name
    }, $false)
    if (-not $definition) { throw "Missing $name in provisioning script" }
    Invoke-Expression $definition.Extent.Text
}

$directory = Join-Path ([IO.Path]::GetTempPath()) ("wave-arm64-archive-test-" + [guid]::NewGuid())
New-Item -ItemType Directory $directory | Out-Null
try {
    $source = Join-Path $directory "probe.c"
    'int probe(void) { return 42; }' | Set-Content -Encoding ascii $source
    $arm = Join-Path $directory "arm64.obj"
    $x64 = Join-Path $directory "x64.obj"
    Invoke-Checked "clang" @("--target=aarch64-pc-windows-msvc", "-c", $source, "-o", $arm)
    Invoke-Checked "clang" @("--target=x86_64-pc-windows-msvc", "-c", $source, "-o", $x64)
    $valid = Join-Path $directory "valid.lib"
    $foreign = Join-Path $directory "x64.lib"
    $mixed = Join-Path $directory "mixed.lib"
    $empty = Join-Path $directory "empty.lib"
    Invoke-Checked "llvm-ar" @("rcs", $valid, $arm)
    Invoke-Checked "llvm-ar" @("rcs", $foreign, $x64)
    Invoke-Checked "llvm-ar" @("rcs", $mixed, $arm, $x64)
    Invoke-Checked "llvm-ar" @("rcs", $empty)
    Assert-Arm64Archive $valid "llvm-readobj"
    foreach ($invalid in @($foreign, $mixed, $empty)) {
        $rejected = $false
        try { Assert-Arm64Archive $invalid "llvm-readobj" }
        catch { $rejected = $true }
        if (-not $rejected) { throw "Archive guard accepted $invalid" }
    }
    Write-Host "ARM64 archive guard passed valid, x64, mixed, and empty archive cases"
} finally {
    Remove-Item -Recurse -Force $directory
}
