<div align="center">
<a href="https://www.wave-lang.dev">
<img src="https://wave-lang.dev/img/favicon.ico" alt="Wave Programming Language Logo" width="120" />
</a>
<br/>
<h1>Wave</h1>
<p><strong>The Systems Language for a New Era</strong></p>
<p>
<a href="https://www.wave-lang.dev"><strong>Website</strong></a> ·
<a href="https://www.wave-lang.dev/docs/intro/"><strong>Docs</strong></a> ·
<a href="https://discord.gg/Kuk2qXFjc5"><strong>Community</strong></a>
</p>
<div>
<a href="https://github.com/LunaStev/Wave/releases">
<img src="https://img.shields.io/github/v/release/LunaStev/Wave?style=for-the-badge&include_prereleases&logo=github&color=5865F2" alt="Latest version"/>
</a>
<a href="https://github.com/LunaStev/Wave/actions/workflows/rust.yml">
<img src="https://img.shields.io/github/actions/workflow/status/LunaStev/Wave/rust.yml?logo=rust&style=for-the-badge&branch=master&label=build" alt="Build Status"/>
</a>
<a href="https://discord.gg/Kuk2qXFjc5">
<img src="https://img.shields.io/badge/Discord-Join%20Us-5865F2?style=for-the-badge&logo=discord&logoColor=white" alt="Discord" />
</a>
</div>
</div>
<br/>
<table>
<tr>
<td valign="top" width="60%">
<h3>What is Wave?</h3>
<p>
<strong>Wave</strong> is a next-generation systems programming language that harmonizes low-level control with high-level elegance. It is designed to empower developers to build everything from bare-metal operating systems to high-performance applications with a single, consistent toolchain.
</p>
<p>
Our philosophy is simple: you should never have to choose between performance and productivity. Wave offers both.
</p>
</td>
<td valign="top" width="40%">
<pre>
<code class="language-wave">
// Your first Wave program
fun main() {
    println("Hello, powerful new world!");
}
</code>
</pre>
</td>
</tr>
</table>

---

> **Warning:**  
> Wave is in its early stages of development. The first official version, v0.0.1, has not yet been released.

---

## Get Started in Seconds
Install the latest version of the Wave compiler and toolchain with a single command.

```bash
curl -fsSL https://wave-lang.dev/install.sh | bash -s -- latest
```

---

## ✨ Core Principles
<table width="100%">
<tr align="center">
<td width="33%">
<h3>⚡️ Blazing Fast</h3>
<p>Compile to native machine code with direct memory management and full hardware access. No VM, no garbage collector overhead. Just pure speed.</p>
</td>
<td width="33%">
<h3>🛡️ Built for Safety</h3>
<p>A modern type system and ownership model that helps eliminate entire classes of bugs at compile-time, ensuring robust and secure software.</p>
</td>
<td width="33%">
<h3>✍️ Expressive Syntax</h3>
<p>Enjoy a clean, intuitive syntax that makes code easy to read and write, allowing you to focus on logic rather than boilerplate.</p>
</td>
</tr>
</table>

---

## ❤️ Join the Wave
Wave is built by the community, for the community. Your contribution, whether it's code, documentation, or feedback, helps shape the future of the language.
<table width="100%">
<tr align="center">
<td width="50%">
<h3>🤝 Contribute</h3>
<p>Found a bug or have an idea? We'd love your help. Check out our contributing guide to get started.</p>
<a href="https://github.com/LunaStev/Wave/blob/master/CONTRIBUTING.md">
<img src="https://img.shields.io/badge/Contributing%20Guide-333?style=for-the-badge&logo=github" alt="Contributing Guide"/>
</a>
</td>
<td width="50%">
<h3>💖 Sponsor</h3>
<p>If you believe in our vision, consider sponsoring the project. Your support keeps development going.</p>
<a href="https://github.com/sponsors/LunaStev">
<img src="https://img.shields.io/badge/Sponsor%20LunaStev-EA4AAA?style=for-the-badge&logo=github-sponsors" alt="Sponsor LunaStev"/>
</a>
</td>
</tr>
</table>

---

## Examples

<details>
<summary>► Click to see the <strong>Fibonacci Sequence</strong> example</summary>

```wave
fun fibonacci(n: i32) -> i32 {
    if (n == 0) {
        return 0;
    }
    
    if (n == 1) {
        return 1;
    }
    
    var prev :i32 = 0;
    var curr :i32 = 1;
    var next :i32;
    var i :i32 = 2;
    
    while (i <= n) {
        next = prev + curr;
        prev = curr;
        curr = next;
        i = i + 1;
    }
    
    return curr;
}

fun main() {
    var i :i32 = 0;
    var result :i32;
    
    while (i <= 10) {
        result = fibonacci(i);
        println("fibonacci({}) = {}", i, result);
        i = i + 1;
    }

    println("END FIBONACCI");
}
```

</details>
<details>
<summary>► Click to see the <strong>Pointer Swap</strong> example</summary>

### Pointer Swap

```wave
fun main() {
    var a: i32 = 10;
    var b: i32 = 20;
    
    var p1: ptr<i32> = &a;
    var p2: ptr<i32> = &b;
    
    println("Before:");
    println("a = {}, b = {}", a, b);
    println("p1 = {}, p2 = {}", deref p1, deref p2);
    
    var temp: i32 = deref p1;
    deref p1 = deref p2;
    deref p2 = temp;
    
    println("After:");
    println("a = {}, b = {}", a, b);
    println("p1 = {}, p2 = {}", deref p1, deref p2);
}
```

</details>
<br/>

More examples are available inside `test/`.

---

<p align="center">
<a href="https://star-history.com/#LunaStev/Wave&Date">
<img src="https://api.star-history.com/svg?repos=LunaStev/Wave&type=Date" alt="Star History Chart" width="80%">
</a>
</p>
<p align="center">
<sub>Released under the <a href="LICENSE">MPL-2.0 License</a>.</sub>
</p>