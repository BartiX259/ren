import lib/std

fn push<T>(list: *[T], el: T) {
	let ptr = *(list as **any + 1);
	if ptr == null {
		ptr = alloc(sizeof(el) * 5);
		*(list as **any + 1) = ptr;
	}
	let cap = *((ptr as *int - 1)) - 8;
	let size = len(list) * sizeof(el);
    if size >= cap {
		let new_ptr = alloc(cap * 2);
		copy(ptr, new_ptr, size);
		ptr = new_ptr;
		*(list as **any + 1) = new_ptr;
    }
	copy(&el, ptr as *T + len(list), sizeof(el));
	*(list as *any as *int) += 1;
}

fn push<T>(list: *[T], sl: <T>) {
	let ptr = *(list as **any + 1);
	if ptr == null {
		ptr = alloc(sizeof(*(sl as *T)) * 5);
		*(list as **any + 1) = ptr;
	}
	let cap = *((ptr as *int - 1)) - 8;
	let size = len(list) * sizeof(*(sl as *T));
	let add_size = len(sl) * sizeof(*(sl as *T));
    if size + add_size > cap {
		let new_ptr = alloc(cap + add_size);
		copy(ptr, new_ptr, size);
		ptr = new_ptr;
		*(list as **any + 1) = new_ptr;
    }
	copy(sl as *T, ptr as *T + len(list), add_size);
	*(list as *any as *int) += len(sl);
}

fn split<T>(sl: <T>, split: T) -> [<T>] {
	let base = 0;
	decl res: [<T>];
	for let i = 0; i < len(sl); i += 1 {
		if sl[i] == split {
			let s = sl[base..i];
			push(&res, s);
			base = i + 1;
		}
	}
	push(&res, sl[base..]);
	return res;
}

fn main() -> int {
	let x = +"bsckzxc";
	push(&x, "kurwa");
	print(x);
	let y = split(x, 'k');
	print(len(y));
	print(y);
	print("\n---\n");
	for let i = 0; i < len(y); i += 1 {
		print("{i}: {y[i]}\n");
	}
}
