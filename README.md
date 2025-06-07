
# Ren

This repository contains the compiler and standard library for my hobby programming language **Ren**. 

## Language overview

**Ren** is a low level programming language focused on ergonomics. It has a lot of functionality built into the language and the standard library while compiling to [nasm](https://www.nasm.us/) x64 assembly using linux system calls.

## Hello, world!

This is a simple "Hello, world!" program in Ren.
```
import lib/std
fn main() {
    print("Hello, world!\n");
}
```
The standard library is just another Ren file, in this case located at 
```
lib/std.re
```
To build and run:
```
ren hello.re
./out
```

## Basic types

The basic types in Ren are `int`, `char` and `bool`, which all work just like in other languages.

```
fn main() {
	let int = 5;
	let char = 'a';
	let bool = true;
}
```

## String type

In Ren, there is no string type, but rather a char slice and a char list.

```
import lib/std
fn main() {
	let char_slice = "asd";
	let char_list = +"asd";
}
```

The `+` operator is used to copy a slice onto the heap, creating a list.

## Collections

For collections, Ren has slices, arrays and lists.

```
import lib/std
fn main() {
	let array = [1, 2, 3];
	let list = +[1, 2, 3];
}
```
The `+` operator can be used to create a list literal.

## Slices and ranges

A range is created by using the `..` operator between two integers.

A slice can be created by indexing a collection with a range.

```
fn main() {
	let arr1234 = [1, 2, 3, 4];
	let sl23    = arr1234[1..3];
	let sl123   = arr1234[..3];
	let sl234   = arr1234[1..];
	let sl1234  = arr1234[..];
}
```

Slices and ranges are iterable.

```
import lib/std
fn main() {
	for i in 0..5 {
		print(i);
	}
	for i in [0, 1, 2, 3, 4] {
		print(i);
	}
}
```

## Structs and tuples

Ren supports structs and tuples.
```
import lib/std
fn main() {
    let struct = (x: 1, y: "asd");
    let tuple = (1, "asd");
    print("{struct.x}, {struct.y}\n"); 
    print("{tuple[0]}, {tuple[1]}\n");
}
```

## Generic types

You can make generic functions by declaring the generic type in angled brackets.
```
pub fn split<T>(sl: <T>, split: T) -> [<T>] {
	let base = 0;
	decl res: [<T>];
	for i in 0..len(sl) {
		if sl[i] == split {
			let s = sl[base..i];
			push(&res, s);
			base = i + 1;
		}
	}
	push(&res, sl[base..]);
	return res;
}
```

