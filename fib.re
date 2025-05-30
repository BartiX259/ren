import lib/std

fn fib(n: int) -> int {
    if n == 0 {
        return 0;
    }
    if n == 1 {
        return 1;
    }
    return fib(n-1) + fib(n-2);
}

fn main(num: int) {
  print(fib(num));
}

