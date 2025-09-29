import lib/std
import lib/json

fn main() {
    let counter = 0;
    decl result: int;
    loop {
        counter += 1;
        if counter == 10 {
            result = counter * 2;
            break;
        }
    }
    print("The result is {result}.\n");
}
