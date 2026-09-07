# This file is part of the Wave language project.
# SPDX-License-Identifier: MPL-2.0
# AI TRAINING NOTICE: Prohibited without prior written permission.

# llvm-sys 211 cannot dynamically link LLVM on MSVC, even with prefer-dynamic.
# The official LLVM ARM64 SDK requests xml2s.lib through --system-libs, but
# does not ship it. Build an ABI-compatible libxml2 2.13 static library from
# checksum-pinned GNOME sources; never substitute an x64 library.
$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

function Invoke-Checked([string]$Program, [string[]]$Arguments) {
    & $Program @Arguments
    if ($LASTEXITCODE -ne 0) { throw "$Program failed with exit code $LASTEXITCODE" }
}

function Assert-Arm64Archive([string]$Library, [string]$Readobj) {
    # llvm-readobj visits every COFF member, so mixed-architecture archives fail
    # here before Cargo/MSVC reaches a confusing unresolved-symbol diagnostic.
    $headers = & $Readobj --file-headers $Library
    if ($LASTEXITCODE -ne 0) { throw "Could not inspect libxml2 archive" }
    $machines = @($headers | Select-String '^\s*Machine:')
    if ($machines.Count -eq 0) { throw "No COFF objects found in libxml2 archive" }
    foreach ($machine in $machines) {
        if ($machine.Line -notmatch 'Machine: IMAGE_FILE_MACHINE_ARM64 \(0xAA64\)') {
            throw "Non-ARM64 member in libxml2 archive: $machine"
        }
    }
    Write-Host "Verified $($machines.Count) ARM64 libxml2 objects"
}

if ($env:PROCESSOR_ARCHITECTURE -ne "ARM64") { throw "Expected a native ARM64 runner" }
$version = "2.13.9"
$sha256 = "a2c9ae7b770da34860050c309f903221c67830c86e4a7e760692b803df95143a"
$root = Join-Path $env:RUNNER_TEMP "wave-libxml2-arm64-$version"
$archive = Join-Path $root "libxml2-$version.tar.xz"
$source = Join-Path $root "libxml2-$version"
$build = Join-Path $root "build"
$install = Join-Path $root "install"
New-Item -ItemType Directory -Force -Path $root | Out-Null
Invoke-Checked "curl.exe" @("--fail", "--location", "--retry", "3",
    "https://download.gnome.org/sources/libxml2/2.13/libxml2-$version.tar.xz", "--output", $archive)
if ((Get-FileHash $archive -Algorithm SHA256).Hash.ToLowerInvariant() -ne $sha256) {
    throw "libxml2 source checksum mismatch"
}
Invoke-Checked "tar.exe" @("-xJf", $archive, "-C", $root)

# Import the native ARM64 MSVC/Windows SDK environment for clang-cl and Ninja.
$vswhere = Join-Path ${env:ProgramFiles(x86)} "Microsoft Visual Studio\Installer\vswhere.exe"
$visualStudio = & $vswhere -latest -products '*' -property installationPath
if ($LASTEXITCODE -ne 0 -or -not $visualStudio) { throw "Visual Studio was not found" }
$devCmd = Join-Path $visualStudio "Common7\Tools\VsDevCmd.bat"
$environment = & cmd.exe /d /s /c "`"$devCmd`" -no_logo -arch=arm64 -host_arch=arm64 >nul && set"
if ($LASTEXITCODE -ne 0) { throw "Could not initialize native ARM64 MSVC" }
foreach ($line in $environment) {
    if ($line -match '^([^=]+)=(.*)$') {
        [Environment]::SetEnvironmentVariable($Matches[1], $Matches[2], "Process")
    }
}
if ($env:VSCMD_ARG_TGT_ARCH -ne "arm64") { throw "MSVC is not targeting ARM64" }

$llvmBin = Split-Path $env:LLVM_CONFIG_PATH
$clang = Join-Path $llvmBin "clang-cl.exe"
$systemLibraryOutput = & $env:LLVM_CONFIG_PATH --system-libs --link-static
if ($LASTEXITCODE -ne 0) { throw "llvm-config --system-libs failed" }
$systemLibraries = $systemLibraryOutput -join " "
Write-Host "LLVM static system libraries: $systemLibraries"
if ($systemLibraries -notmatch '\bxml2s\.lib\b') {
    throw "The pinned LLVM SDK no longer requests xml2s.lib; review this provisioning contract"
}

# /MD matches Rust's default MSVC CRT. No zlib, lzma, iconv, Python or DLL
# dependencies are introduced. Keep the pre-2.14 XML ABI used by LLVM 21.
Invoke-Checked "cmake" @("-S", $source, "-B", $build, "-G", "Ninja",
    "-DCMAKE_BUILD_TYPE=Release", "-DCMAKE_INSTALL_PREFIX=$install",
    "-DCMAKE_C_COMPILER=$clang", "-DCMAKE_C_COMPILER_TARGET=aarch64-pc-windows-msvc",
    "-DCMAKE_MSVC_RUNTIME_LIBRARY=MultiThreadedDLL", "-DBUILD_SHARED_LIBS=OFF",
    "-DLIBXML2_WITH_ICONV=OFF", "-DLIBXML2_WITH_ICU=OFF", "-DLIBXML2_WITH_LZMA=OFF",
    "-DLIBXML2_WITH_ZLIB=OFF", "-DLIBXML2_WITH_PYTHON=OFF", "-DLIBXML2_WITH_PROGRAMS=OFF",
    "-DLIBXML2_WITH_TESTS=OFF", "-DLIBXML2_WITH_FTP=OFF", "-DLIBXML2_WITH_HTTP=OFF",
    "-DLIBXML2_WITH_MODULES=OFF", "-DLIBXML2_WITH_TLS=OFF")
Invoke-Checked "cmake" @("--build", $build, "--config", "Release", "--parallel", "2")
Invoke-Checked "cmake" @("--install", $build, "--config", "Release")
$libDir = Join-Path $install "lib"
$library = Join-Path $libDir "xml2s.lib"
Copy-Item (Join-Path $libDir "libxml2s.lib") $library -Force

Assert-Arm64Archive $library (Join-Path $llvmBin "llvm-readobj.exe")

# llvm-sys supplies xml2s; libxml2's Windows entropy/socket helpers also use
# these Windows SDK import libraries. Preserve pre-existing Rust flags.
"LIB=$libDir;$env:LIB" | Out-File $env:GITHUB_ENV -Encoding utf8 -Append
"RUSTFLAGS=$env:RUSTFLAGS -l bcrypt -l ws2_32" | Out-File $env:GITHUB_ENV -Encoding utf8 -Append
"WAVE_LIBXML2_LICENSE=$source\Copyright" | Out-File $env:GITHUB_ENV -Encoding utf8 -Append
