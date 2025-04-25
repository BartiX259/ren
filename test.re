import pubgen
import lib/std

fn add<A, B>(a: A, b: B) -> A {
	return a + b;
}

fn main() -> int {
	let x = t("asd");
	print(x);
	let z = t(5);
	print(z);
	test();
	print('\n');
	print(add('a', 5));
}
