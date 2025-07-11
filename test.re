import lib/std

fn main() {
    let config = { "port": 8080 };

    let port = get(config, "port") else 9000;
    print("Using port: {port}\n"); // Prints 8080

    let timeout = get(config, "timeout") else 30;
    print("Using timeout: {timeout}\n"); // Prints 30
}
