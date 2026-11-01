project('Wave', version: run_command(find_program('python', 'python3', version: '>=3.11'), '-c',
    'import sys, tomllib; print(tomllib.load(open(sys.argv[1], "rb"))["package"]["version"])',
    files('Cargo.toml'), check: true).stdout().strip(),
    meson_version: '>=1.8.5', default_options: ['buildtype=release'])

# Cargo remains the Rust dependency/build authority, including both build.rs
# scripts. Meson owns commands, options and dependencies around that build.
python = find_program('python', 'python3', version: '>=3.11')
fs = import('fs')
source_root = meson.project_source_root()
# Register these as reconfiguration inputs as well as reading them below.
cargo_manifest = fs.read('Cargo.toml')
toolchain_pins = fs.read('tools/ci/toolchains.json')
metadata = run_command(python, '-c',
    'import json, pathlib, tomllib; r=pathlib.Path(__import__("sys").argv[1]); '
    + 'print(tomllib.loads((r/"Cargo.toml").read_text())["package"]["version"]); '
    + 'print(json.loads((r/"tools/ci/toolchains.json").read_text())["RUST_VERSION"])',
    source_root, check: true).stdout().strip().split('\n')
version = metadata[0]
rust_version = metadata[1]
cargo_dir = source_root / get_option('cargo_target_dir')
build_env = environment()
build_env.set('PYTHONPATH', source_root)
build_env.set('CARGO_BUILD_JOBS', '2')
build_env.set('CARGO_TARGET_DIR', cargo_dir)
if get_option('llvm_prefix') != ''
    build_env.set('LLVM_SYS_211_PREFIX', get_option('llvm_prefix'))
    build_env.set('WAVE_LLVM_HOME', get_option('llvm_prefix'))
    config_name = host_machine.system() == 'windows' ? 'llvm-config.exe' : 'llvm-config'
    build_env.set('LLVM_CONFIG_PATH', get_option('llvm_prefix') / 'bin' / config_name)
endif
if get_option('compiler')
    rust_toolchain = get_option('toolchain')
    if rust_toolchain == ''
        rust_toolchain = rust_version
    endif
    cargo = find_program('cargo', native: true)
    rustc = find_program('rustc', native: true)
    rust_identity = run_command(rustc, '+' + rust_toolchain, '-vV', check: true).stdout()
    if not rust_identity.contains('release: ' + rust_version + '\n')
        error('The selected Rust toolchain must match pinned version ' + rust_version)
    endif
    native_target = ''
    foreach line : rust_identity.split('\n')
        if line.startswith('host: ')
            native_target = line.substring(6).strip()
        endif
    endforeach
    if native_target == ''
        error('The pinned Rust toolchain did not report its host triple')
    endif
    selected_target = get_option('compiler_target')
    triple = selected_target == 'native' ? native_target : selected_target
    profile = get_option('buildtype')
    if profile not in ['debug', 'release']
        error('Use -Dbuildtype=debug or -Dbuildtype=release; Cargo owns optimization profiles')
    endif
    cargo_profile = profile == 'release' ? 'release' : 'debug'
    binary_name = triple.contains('windows') ? 'wavec.exe' : 'wavec'
    binary_dir = selected_target == 'native' ? cargo_dir : cargo_dir / triple
    cargo_binary = binary_dir / cargo_profile / binary_name

    features = get_option('llvm_features')
    if features == 'auto'
        features = 'default'
        if triple == 'aarch64-pc-windows-msvc'
            features = 'llvm-target-aarch64'
        elif triple == 'x86_64-pc-windows-msvc'
            features = 'llvm-target-core64'
        endif
    endif
    cargo_args = [cargo, '+' + rust_toolchain, 'build', '--locked', '--jobs', '2']
    if selected_target != 'native'
        cargo_args += ['--target', triple]
    endif
    if profile == 'release'
        cargo_args += '--release'
    endif
    if features != 'default'
        cargo_args += ['--no-default-features', '--features', features]
    endif
    adapter_args = ['--target', triple, '--binary', cargo_binary, '--output', '@OUTPUT@']
    if profile == 'release'
        adapter_args += '--release'
    endif
    compiler = custom_target('wavec', output: binary_name,
        command: [python, '-m', 'tools.build.cargo_env', adapter_args, '--', cargo_args],
        env: build_env, build_by_default: true, build_always_stale: true, console: true)

    package_args = [python, '-m', 'tools.build.package', '--target', triple,
        '--binary', cargo_binary, '--version', version]
    if profile == 'release'
        package = run_target('package', command: package_args, depends: compiler, env: build_env)
        # CI has already validated this Cargo output; retain package-only staging.
        run_target('package-existing', command: package_args, env: build_env)
        alias_target('release', compiler, package)
        meson.add_install_script(python, files('tools/build/install.py'),
            '--target', triple, '--binary', cargo_binary, '--bindir', get_option('bindir'),
            '--llvm-prefix', get_option('llvm_prefix'),
            install_tag: 'runtime')
    endif
    run_target('rust-target', command: [find_program('rustup', native: true),
        'target', 'add', '--toolchain', rust_toolchain, triple])
endif

# Meson's clean removes its own outputs; distclean explicitly retains the old
# repository-scoped Cargo/archive cleanup contract. It never removes .tmp/.
all_targets = [
    'x86_64-unknown-linux-gnu', 'aarch64-unknown-linux-gnu',
    'riscv64gc-unknown-linux-gnu', 'loongarch64-unknown-linux-gnu',
    'x86_64-unknown-freebsd', 'x86_64-unknown-openbsd', 'x86_64-unknown-netbsd',
    'x86_64-unknown-dragonfly', 'x86_64-unknown-redox', 'x86_64-unknown-fuchsia',
    'x86_64-unknown-haiku', 'x86_64-pc-windows-msvc', 'aarch64-pc-windows-msvc',
    'aarch64-apple-darwin', 'x86_64-apple-darwin',
]
run_target('distclean', command: [python, '-m', 'tools.build.clean', all_targets], env: build_env)

test('tooling', python, args: ['-m', 'unittest', 'discover', '-s', 'tools/tests', '-t', '.', '-p', 'test_*.py'],
    workdir: source_root, env: build_env, timeout: 1800, suite: 'tooling', is_parallel: false)
if get_option('compiler')
    rust_test_args = ['+' + rust_toolchain, 'test', '--locked', '--workspace',
        '--all-targets', '--jobs', '2']
    if selected_target != 'native'
        rust_test_args += ['--target', triple]
    endif
    if features != 'default'
        rust_test_args += ['--no-default-features', '--features', features]
    endif
    test('rust', cargo, args: rust_test_args, workdir: source_root, env: build_env,
        timeout: 1800, suite: 'rust', is_parallel: false)
endif
test('std-policy', python, args: ['tools/check_std_policy.py'],
    workdir: source_root, env: build_env, timeout: 60, suite: 'policy')

# Keep the existing per-platform evidence, timeout and fail-closed CI policies.
# These procedures also call Meson for compiler builds and distribution staging.
ci_args = [python, '-m', 'tools.ci.' + get_option('ci_family'), '--target', get_option('ci_target')]
foreach lane : get_option('ci_lanes')
    ci_args += ['--lane', lane]
endforeach
if get_option('ci_provision')
    ci_args += '--provision'
endif
if get_option('ci_report') != ''
    ci_args += ['--report-json', source_root / get_option('ci_report')]
endif
run_target('ci', command: ci_args, env: build_env)
summary({'Version': version, 'Rust': rust_version})
if get_option('compiler')
    summary({'Compiler host': triple, 'Cargo profile': profile,
        'LLVM features': features, 'Cargo output': cargo_binary})
endif
