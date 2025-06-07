import lib/std

fn opt(x: ?<char>) -> ?<char> {
  print(x!);
  return "nah";
}

fn main() {
  let res = opt("halo");
  print(res);
}

