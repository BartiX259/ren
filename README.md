<div align="center">
  <picture>
    <img alt="The Ren Programming Language"
         src="preview.png"
         width="70%">
  </picture>

[![Docs](https://img.shields.io/badge/Website-Overview_and_Docs-brightgreen)](https://bartix259.github.io/ren-docs/) [![VSCode Extension](https://img.shields.io/badge/VSCode-Extension-blue)](https://github.com/BartiX259/ren-vscode)
</div>


This repository contains the compiler and standard library for my hobby programming language **Ren**.

## Language overview

**Ren** is a low level programming language focused on ergonomics. It has a lot of functionality built into the language and the standard library while compiling to [nasm](https://www.nasm.us/) x64 assembly using Linux system calls.

## Installation

1. Install [cargo](https://github.com/rust-lang/cargo).

2. Clone this repository and compile:

```
git clone https://github.com/BartiX259/ren.git
cd ren
cargo build --release
```

3. Add the binary to PATH, it should be in `target/release/`.

## Getting started

To start using ren, learn more at https://bartix259.github.io/ren-docs/.
