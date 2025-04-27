import lib/std

fn push<T>(list: *[T], el: T) {
    if len(list) == cap(list) {
		let new_cap = cap(list) * 2;
        *(((list as *any) + 8) as *int) = new_cap;
		let old_ptr = *(((list as **any) + 2));
		let new_ptr = alloc(new_cap * sizeof(el));
		copy(*(((list as **any) + 2)), new_ptr, len(list) * sizeof(el));
		*(((list as **any) + 2)) = new_ptr;
    }
	copy(&el, *(((list as **any) + 2) as **T) + len(list), sizeof(el));
	*((list as *any) as *int) += 1;
}

fn main() -> int {
	let x = +[(1, 1, 1), (2, 2, 2), (3, 3, 3)];
	let p = 1..3;
	let y = x[p];
	print(len(y));
	print(y[1][1]);
}
