
import lib/std

fn main() -> int {
	let x = alloc(8) as *int;
	*x = 69;
	print(*x);
	print(x as int);
	let y = 5;
	alloc(8);
	alloc(16);
	print(*x);
	print(x as int);
}
