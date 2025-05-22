#[macro_export]
macro_rules! time {
    ($e: expr) => {
        let now = std::time::Instant::now();
        $e;
        println!(
            "{} - duration: {}us",
            stringify!($e),
            now.elapsed().as_micros()
        );
    };
}
