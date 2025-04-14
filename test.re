import lib/std

fn t() -> (x: int, y: int) {
	return (1, 2);
}

fn test(x: any[]) {

}

fn main() -> int {
	print(t().y);
}
