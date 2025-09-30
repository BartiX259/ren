import lib/std

fn main() {
    decl list: [(x: int, y: (int, <char>))];
    for i in 0..100 {
        push(&list, (x: i, y: (100 - i, "q{i}" as <char>)));
    }
    for x, y in list {
        let i, str = y;
        print(x + i);
        print(str);
        print('\n');
    }
}
