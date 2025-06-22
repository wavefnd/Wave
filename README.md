<div align="center">
  <img src="https://wave-lang.dev/img/favicon.ico" alt="Wave Programming Language Logo" width="100" />
  <h1>Wave Programming Language</h1>
  <p>
    <a href="https://www.wave-lang.dev">Website</a> •
    <a href="https://github.com/LunaStev/Wave/blob/master/CONTRIBUTING.md">Contributing</a> •
    <a href="LICENSE">LICENSE</a>
  </p>
</div>

<div align="center">
  <a href="https://discord.gg/Kuk2qXFjc5" target="_blank">
    <img src="https://img.shields.io/badge/Discord-5865F2?style=for-the-badge&logo=discord&logoColor=white" alt="Discord" />
  </a>
  <a href="mailto:lunastev@gurmstudios.com" target="_blank">
    <img src="https://img.shields.io/badge/Email-D14836?style=for-the-badge&logo=gmail&logoColor=white" alt="Email" />
  </a>
</div>

---

> **Warning:**  
> The official version of this project has not yet been released. The first version will be distributed as v0.0.1.

![Latest version](https://img.shields.io/github/v/release/LunaStev/Wave?style=for-the-badge&include_prereleases)

![Code size](https://img.shields.io/github/languages/code-size/LunaStev/Wave?style=for-the-badge&logo=github)
![Downloads](https://img.shields.io/github/downloads/LunaStev/Wave/total?color=%2324cc24&style=for-the-badge&logo=github)
![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/LunaStev/Wave/rust.yml?logo=rust&style=for-the-badge&branch=master)

---

## Overview

**Wave** is a next-generation programming language designed for developing operating systems, system software, and applications—entirely using **Wave**.  
We're building a **Wave** compiler with full hardware access.

---

## Sponsor Us

Wave is an open-source programming language built with love, care, and a long-term vision.
It’s a project that aims to push the boundaries of what low-level languages can be — without sacrificing clarity or safety.

If you believe in that vision, even a small gesture of support can make a big difference.
Wave is developed by an independent creator with no corporate backing, and your sponsorship helps keep it alive and evolving.


[![Sponsor LunaStev](https://img.shields.io/badge/Sponsor%20LunaStev-%23ea4aaa?style=for-the-badge&logo=github-sponsors&logoColor=white)](https://github.com/sponsors/LunaStev)

---

## Examples

### Fibonacci sequence

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

### Pointer Swap Example

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

More examples are available inside `test/`.

---

## Concept

<p align="center">
  <img src=".github/scalability1.svg" alt="Wave Concept Diagram" width="60%">
</p>

---

## Sponsors ❤️

A huge thank you to our sponsors for supporting this project!

<p align="center">
  <a href="https://ko-fi.com/heymanbug">
    <img src="https://ko-fi.com/img/anon7.png?v=10" width="100" alt="heymanbug" />
    <br>
    <sub><b>heymanbug</b></sub>
  </a>
</p>

---

## Contributing

Interested in contributing? Check out our [Contributing Guide](https://github.com/LunaStev/Wave/wiki/Contributing) to get started.

---

## Cool graphs

[![Star History Chart](https://api.star-history.com/svg?repos=LunaStev/Wave&type=Date)](https://star-history.com/#LunaStev/Wave&Date)

---

## LICENSE

Wave is released under the [MPL-2.0 License](LICENSE).
