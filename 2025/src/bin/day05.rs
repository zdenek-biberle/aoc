use rangemap::RangeSet;
use std::io;

fn main() -> io::Result<()> {
    let set_of_fresh_ids = io::stdin()
        .lines()
        .take_while(|line| match line {
            Ok(l) => l != "",
            Err(_) => true,
        })
        .map(|line| {
            let line = line?;
            let (l_str, r_str) = line.split_once('-').ok_or(io::ErrorKind::InvalidData)?;
            let l = l_str
                .parse::<u64>()
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
            let r = r_str
                .parse::<u64>()
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
            Ok(l..r + 1)
        })
        .collect::<io::Result<RangeSet<_>>>()?;

    let fresh_available_ingredients = io::stdin()
        .lines()
        .map(|line| {
            let line = line?;
            let id = line
                .parse::<u64>()
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
            Ok(set_of_fresh_ids.contains(&id))
        })
        .try_fold(0u64, |acc, is_fresh: io::Result<_>| {
            Ok::<u64, io::Error>(acc + (is_fresh? as u64))
        })?;

    let count_of_fresh_ids = set_of_fresh_ids
        .iter()
        .map(|range| range.clone().count())
        .sum::<usize>();

    println!(
        "Part 1: {}, part 2: {}",
        fresh_available_ingredients, count_of_fresh_ids
    );

    Ok(())
}
