use std::io;
use std::io::Read;

const EIGHT_NEIGHBORS: [(i32, i32); 8] = [
    (-1, -1), (0, -1), (1, -1),
    (-1, 0), /*'sup*/ (1, 0),
    (-1, 1), (0, 1), (1, 1),
];

fn remove_accessible_rolls(field: &mut [&mut [u8]]) -> i32 {
    let w = field.len() as i32;
    let h = field[0].len() as i32;

    for y in 0..h {
        for x in 0..w {
            if field[y as usize][x as usize] != b'@' { continue; }

            let num_neighbors = EIGHT_NEIGHBORS
                .iter()
                .map(|(dx, dy)| (x + *dx, y + *dy))
                .filter(|&(nx, ny)| nx >= 0 && ny >= 0 && nx < w && ny < h)
                .map(|(nx, ny)| (field[ny as usize][nx as usize] != b'.') as i32)
                .sum::<i32>();

            if num_neighbors < 4 {
                field[y as usize][x as usize] = b'#';
            }
        }
    }

    let mut accessible_rolls = 0;
    for row in field {
        for cell in row.iter_mut() {
            if *cell == b'#' {
                accessible_rolls += 1;
                *cell = b'.';
            }
        }
    }
    accessible_rolls
}
fn main() -> io::Result<()> {
    let mut input = io::stdin().bytes().collect::<Result<Vec<u8>, _>>()?;
    let mut field = input.split_mut(|b| *b == b'\n').collect::<Vec<_>>();

    let part_one = remove_accessible_rolls(&mut field);
    let mut total_removed_rolls = part_one;

    loop {
        let removed_rolls = remove_accessible_rolls(&mut field);
        total_removed_rolls += removed_rolls;
        if removed_rolls == 0 {
            break;
        }
    }

    println!("Part 1: {}, part 2: {}", part_one, total_removed_rolls);

    Ok(())
}
