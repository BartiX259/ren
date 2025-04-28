import lib/std

fn push<T>(list: *[T], el: T) {
	let ptr = *(((list as **any) + 1));
	let cap = *((ptr - 1) as *int) - 8;
	let size = len(list) * sizeof(el);
    if size == cap {
		let new_ptr = alloc(cap * 2);
		copy(ptr, new_ptr, size);
		*(((list as **any) + 1)) = new_ptr;
    }
	copy(&el, *(((list as **any) + 1) as **T) + len(list), sizeof(el));
	*((list as *any) as *int) += 1;
}

fn t(x: <char>) {
	print(x[0..2]);
}

fn main() -> int {
	let x = [1, 2, 3, 4];
	let y = +(x[..3]);
	push(&y, 5);
	y[1] = 49;
	print(y);
}
