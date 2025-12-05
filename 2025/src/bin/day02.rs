use std::io;
use std::iter::successors;

fn count_digits(id: u64) -> u32 {
    let mut power_of_10 = 1;
    let mut count = 0;
    while power_of_10 < id {
        power_of_10 *= 10;
        count += 1;
    }
    count
}

/// Checks if `id` (which has `len` digits) can be divided into `parts` repeated parts.
fn is_somehow_invalid(id: u64, len: u32, parts: u32) -> bool {
    if len % parts > 0 {
        return false;
    }

    let part_len = len / parts;
    let part_multiplier = 10_u64.pow(part_len);
    let divisor = successors(Some(1_u64), |n| n.checked_mul(part_multiplier))
        .take(parts as usize)
        .sum::<u64>();
    id % divisor == 0 && id / divisor < part_multiplier
}

fn is_invalid_1(id: u64, len: u32) -> bool {
    is_somehow_invalid(id, len, 2)
}

fn is_invalid_2(id: u64, len: u32) -> bool {
    (2..=len).any(|parts| is_somehow_invalid(id, len, parts))
}

fn main() -> io::Result<()> {
    let mut line = String::new();
    io::stdin().read_line(&mut line)?;
    let results = line
        .split(',')
        .flat_map(|s| {
            let (a, b) = s.split_once('-').unwrap();
            a.parse().unwrap()..=b.parse().unwrap()
        })
        .map(|n| {
            let len = count_digits(n);
            (is_invalid_1(n, len) as u64 * n, is_invalid_2(n, len) as u64 * n)
        })
        .fold((0, 0), |(acc1, acc2), (n1, n2)| (acc1 + n1, acc2 + n2));
    println!("Results: {:?}", results);

    Ok(())
}
