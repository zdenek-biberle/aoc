use std::cmp::Reverse;
use std::io;

fn max_with_index(xs: &[u64]) -> Option<(usize, u64)> {
    let (idx, value) = xs.iter().enumerate().max_by_key(|(i, v)| (*v, Reverse(*i)))?;
    Some((idx, *value))
}

fn maximum_joltage(batteries: &[u64], num_batteries: usize) -> Option<u64> {
    let mut acc = 0;
    let mut start_idx = 0;
    for i in (0..num_batteries).rev() {
        let (idx, value) = max_with_index(&batteries[start_idx..batteries.len() - i])?;
        start_idx += idx + 1;
        acc = acc * 10 + value;
    }
    Some(acc)
}

fn main() -> io::Result<()> {
    let mut total_joltage_1 = 0;
    let mut total_joltage_2 = 0;
    for line in io::stdin().lines() {
        let line = line?;
        let batteries = line
            .chars()
            .map(|c| c.to_digit(10).map(Into::into))
            .collect::<Option<Vec<u64>>>()
            .ok_or(io::ErrorKind::InvalidData)?;
        total_joltage_1 += maximum_joltage(&batteries, 2).ok_or(io::ErrorKind::InvalidInput)?;
        total_joltage_2 += maximum_joltage(&batteries, 12).ok_or(io::ErrorKind::InvalidInput)?;
    }
    println!("Part 1: {}, part 2: {}", total_joltage_1, total_joltage_2);
    Ok(())
}
