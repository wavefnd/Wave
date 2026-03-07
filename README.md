<div align="center">
<a href="https://www.wave-lang.dev">
<img src="https://wave-lang.dev/img/favicon.ico" alt="Wave Programming Language Logo" width="120" />
</a>
<br/>
<h1>Wave</h1>
<p><strong>Systems Programming Language</strong></p>

<!-- creator note -->
<p style="font-size: 0.9em; color: #777;">
Created by <a href="https://github.com/LunaStev" style="color: #777; text-decoration: none;"><strong>LunaStev</strong></a>
</p>

<p>
<a href="https://www.wave-lang.dev"><strong>Website</strong></a> ·
<a href="https://www.wave-lang.dev/docs/intro/"><strong>Docs</strong></a> ·
<a href="https://blog.wave-lang.dev/"><strong>Blog</strong></a> ·
<a href="https://discord.gg/3nev5nHqq9"><strong>Community</strong></a>
</p>
<div>
<a href="https://github.com/wavefnd/Wave/releases">
<img src="https://img.shields.io/github/v/release/wavefnd/Wave?style=for-the-badge&include_prereleases&logo=github&color=5865F2" alt="Latest version"/>
</a>
<a href="https://github.com/wavefnd/Wave/actions/workflows/rust.yml">
<img src="https://img.shields.io/github/actions/workflow/status/wavefnd/Wave/rust.yml?logo=rust&style=for-the-badge&branch=master&label=build" alt="Build Status"/>
</a>
<a href="https://discord.gg/3nev5nHqq9">
<img src="https://img.shields.io/badge/Discord-Join%20Us-5865F2?style=for-the-badge&logo=discord&logoColor=white" alt="Discord" />
</a>
<a href="https://github.com/wavefnd/Wave/blob/master/LICENSE">
<img src="https://img.shields.io/badge/License-MPL%202.0-blue?style=for-the-badge" alt="License"/>
</a>
</div>
</div>

---

The information about this project is official and can be found on the [TechPedia Wiki](https://techpedia.wiki/) and the [official website](https://wave-lang.dev/).

---

## 🚀 Quick Start

```bash
curl -fsSL https://wave-lang.dev/install.sh | bash -s -- latest
```

---

## About Wave

Wave is a systems programming language designed for low-level control and high performance.
It has no builtin functions — all functionality is provided through the standard library.

```kotlin
fun main() {
    println("Hello World");
}
```

---

## Build From Source

```bash
git clone https://github.com/wavefnd/Wave.git
cd Wave
cargo build
```

Compiler binary path:

- `target/debug/wavec` (development build)
- `target/release/wavec` (release build)

---

## Target Support

<p>
Wave follows a tiered platform policy to set clear expectations for stability, CI, and standard library coverage.
</p>

<details open>
  <summary><strong>🥇 Tier 1 · Primary</strong> — <code>Linux</code>, <code>Darwin</code>, <code>WaveOS</code></summary>
  <ul>
    <li>Full standard library support</li>
    <li>Required CI coverage</li>
    <li>ABI stability commitment</li>
    <li>Release-blocking platforms</li>
  </ul>
</details>

<details>
  <summary><strong>🥈 Tier 2 · Secondary</strong> — <code>FreeBSD</code>, <code>Redox</code>, <code>Fuchsia</code></summary>
  <ul>
    <li>Build support maintained</li>
    <li>Partial standard library coverage</li>
    <li>Open to community collaboration</li>
  </ul>
</details>

<details>
  <summary><strong>🥉 Tier 3 · Experimental</strong> — <code>OpenBSD</code></summary>
  <ul>
    <li>Compiler build/compile path prioritized</li>
    <li>Minimal standard library coverage</li>
  </ul>
</details>

<details>                                                                                    
  <summary><strong>🪦 Tier 4 · Unofficial</strong> — <code>Windows</code></summary>          
  <ul>                                                                                       
    <li>Build may work in some environments, but is not guaranteed</li>                      
    <li>No official standard library target at this time</li>                                
    <li>Community-maintained status</li>                                                     
  </ul>                                                                                      
</details> 

---

## CLI Usage

```bash
wavec run <file>
wavec build <file>
wavec build <file> -o <file>
wavec build <file> -c
```

Useful global options:

- `-O0..-O3`, `-Os`, `-Oz`, `-Ofast`
- `--debug-wave=tokens,ast,ir,mc,hex,all`
- `--link=<lib>`
- `-L <path>`
- `--dep-root=<path>`
- `--dep=<name>=<path>`

---

## Contributing

Contributions are welcome! Please read the [contributing guidelines](CONTRIBUTING.md) before submitting a pull request.

---

## What can do?

### [Doom Demo](https://github.com/wavefnd/Wave/tree/master/examples/doom.wave)

![doom-demo-gif](.github/doom-demo.gif)

---

<p align="center">
<a href="https://star-history.com/#wavefnd/Wave&Date">
<img src="https://api.star-history.com/svg?repos=wavefnd/Wave&type=Date" alt="Star History Chart" width="80%">
</a>
</p>

---

<p align="center"> <strong>Built with ❤️ by the Wave community</strong><br/> <sub>© 2025 Wave Programming Language • LunaStev • <a href="LICENSE">Mozilla Public License 2.0</a></sub> </p>
