import lib/std
import lib/json

type h = (x: int, y: <char>);
type k = int;

fn main() {
    let x = list_dir(".");
    let g = to_json(x);

    print(g);
}
