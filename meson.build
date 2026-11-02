project('Wave', version: run_command(find_program('cargo', native: true),
    'pkgid', '--locked', '--offline', '--manifest-path', meson.current_source_dir() / 'Cargo.toml',
    check: true).stdout().strip().split('@')[-1],
    meson_version: '>=1.8.5', default_options: ['buildtype=release'])

# Cargo owns Rust compilation; Meson owns local dependencies and test selection.
fs = import('fs')
source_root = meson.project_source_root()
build_root = meson.project_build_root()
cargo = find_program('cargo', native: true)
rustc = find_program('rustc', native: true)
toolchain_pins = fs.read('tools/ci/toolchains.json')
# Read the bootstrap pin before wave-tools exists. The native tool validates the
# complete JSON and confirms this value immediately after Cargo builds it.
rust_version = toolchain_pins.split('"RUST_VERSION"')[1].split('"')[1]
rust_toolchain = get_option('toolchain')
if rust_toolchain == ''
    rust_toolchain = rust_version
endif
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

# Build the maintenance binary for the real host without compiler/LLVM flags.
# Configure-time Cargo performs its normal freshness checks. Source inputs below
# make changes reconfigure this graph; no Python project dispatcher is involved.
native_env = environment()
foreach name : ['RUSTFLAGS', 'CARGO_ENCODED_RUSTFLAGS', 'CARGO_BUILD_TARGET',
    'RUSTC_WRAPPER', 'RUSTC_WORKSPACE_WRAPPER', 'LLVM_SYS_211_PREFIX', 'LLVM_CONFIG_PATH',
    'CARGO_TARGET_' + native_target.to_upper().replace('-', '_') + '_RUSTFLAGS']
    native_env.unset(name)
endforeach
# Repository target tables describe cross builds. Native tools must use the
# host C driver, including FreeBSD where the table names a cross-only linker.
if build_machine.system() != 'windows'
    native_env.set('CARGO_TARGET_' + native_target.to_upper().replace('-', '_') + '_LINKER', 'cc')
else
    native_env.unset('CARGO_TARGET_' + native_target.to_upper().replace('-', '_') + '_LINKER')
endif
native_env.set('CARGO_BUILD_JOBS', '2')
# Share one host-only Cargo cache across feature/profile Meson build directories.
tools_dir = source_root / 'target' / 'wave-tools-native'
run_command(cargo, '+' + rust_toolchain, 'build', '--locked', '--jobs', '2',
    '-p', 'wave-tools', '--target', native_target, '--target-dir', tools_dir,
    env: native_env, check: true)
tool_name = build_machine.system() == 'windows' ? 'wave-tools.exe' : 'wave-tools'
native_binary = tools_dir / native_target / 'debug' / tool_name
configure_file(input: native_binary, output: tool_name, copy: true)
wave_tools = find_program(build_root / tool_name, native: true)
version = run_command(wave_tools, 'config', 'version', check: true).stdout().strip()
if run_command(wave_tools, 'config', 'rust-version', check: true).stdout().strip() != rust_version
    error('Rust tooling pin differs from the bootstrap pin')
endif
foreach input : run_command(wave_tools, 'config', 'build-inputs', check: true).stdout().strip().split('\n')
    fs.read(input)
endforeach
cargo_dir = source_root / get_option('cargo_target_dir')
build_env = environment()
build_env.set('WAVE_TOOLS', build_root / tool_name)
build_env.set('CARGO_BUILD_JOBS', '2')
build_env.set('CARGO_TARGET_DIR', cargo_dir)
if get_option('llvm_prefix') != ''
    build_env.set('LLVM_SYS_211_PREFIX', get_option('llvm_prefix'))
    build_env.set('WAVE_LLVM_HOME', get_option('llvm_prefix'))
    config_name = host_machine.system() == 'windows' ? 'llvm-config.exe' : 'llvm-config'
    build_env.set('LLVM_CONFIG_PATH', get_option('llvm_prefix') / 'bin' / config_name)
endif
if get_option('compiler')
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
    cargo_args = [cargo, '+' + rust_toolchain, 'build', '--locked', '--jobs', '2', '--message-format=json-render-diagnostics']
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
        command: [wave_tools, 'build-cargo', adapter_args, '--', cargo_args],
        env: build_env, build_by_default: true, build_always_stale: true, console: true)

    package_args = [wave_tools, 'package', '--target', triple,
        '--binary', cargo_binary, '--version', version]
    if profile == 'release'
        package = run_target('package', command: package_args, depends: compiler, env: build_env)
        # CI has already validated this Cargo output; retain package-only staging.
        run_target('package-existing', command: package_args, env: build_env)
        alias_target('release', compiler, package)
        meson.add_install_script(wave_tools, 'install',
            '--target', triple, '--binary', cargo_binary, '--bindir', get_option('bindir'),
            '--llvm-prefix', get_option('llvm_prefix'),
            install_tag: 'runtime')
    endif
    run_target('rust-target', command: [find_program('rustup', native: true),
        'target', 'add', '--toolchain', rust_toolchain, triple])
endif

# distclean retains the owned Cargo/archive cleanup contract and preserves .tmp/.
run_target('distclean', command: [wave_tools, 'clean', '--cargo-target-dir', cargo_dir], env: build_env)

test('tooling', cargo, args: ['+' + rust_toolchain, 'test', '--locked', '--jobs', '2',
    '-p', 'wave-tools', '--target', native_target, '--target-dir', tools_dir],
    workdir: source_root, env: native_env, timeout: 1800, suite: 'tooling', is_parallel: false)
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
test('std-policy', wave_tools, args: ['check', 'std-policy'],
    workdir: source_root, env: build_env, timeout: 60, suite: 'policy')
run_target('format', command: [wave_tools, 'maintain', 'format'], env: build_env)
run_target('format-check', command: [wave_tools, 'maintain', 'format', '--check'], env: build_env)
run_target('std-api', command: [wave_tools, 'maintain', 'std-api'], env: build_env)
run_target('case-matrix', command: [wave_tools, 'matrix', '--json'], env: build_env)
summary({'Version': version, 'Rust': rust_version})
if get_option('compiler')
    summary({'Compiler host': triple, 'Cargo profile': profile,
        'LLVM features': features, 'Cargo output': cargo_binary})
endif
