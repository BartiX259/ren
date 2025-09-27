import lib/std
import lib/json

type h = (x: int, y: <char>);
type k = int;

fn pretty_print(func: fn(<char>) -> <char>, str: <char>) {
    print(func("pretty\n"));
    print(func(str));
}

fn pr(t: <char>) -> <char> {
    return "pretty: {t}";
}

fn main() {
    pretty_print(pr, "asd");
}
