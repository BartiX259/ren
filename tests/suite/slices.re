import lib/std

fn double(x: int) -> int {
    return x * x;
}

fn is_even(x: int) -> bool {
    return x % 2 == 0;
}

fn mult_reducer(before: int, new: int) -> int {
    return before * new;
}

fn main() {
    let arr = [1, 2, 3, 4];
    let doubled = map(arr, double);
    let even = filter(arr, is_even);
    let all_even = all(arr, is_even);
    let some_even = some(arr, is_even);
    let even_even = all(even, is_even);
    let even_arr = map(arr, is_even);
    let mult = reduce(arr, mult_reducer, 1);
    let sum = sum(arr);
    print("{arr}\n{doubled}\n{even}\n{even_arr}\n{all_even}, {some_even}, {even_even}, {mult}, {sum}\n");
    for n, e in zip(arr[..], even_arr) {
        print("{n}: {e}\n");
    }
}
