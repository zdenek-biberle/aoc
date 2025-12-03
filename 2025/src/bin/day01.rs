use std::io;

fn main() -> io::Result<()> {
    let mut position: i32 = 50;
    let mut zeroes_at_end = 0;
    let mut zero_clicks = 0;
    for line in io::stdin().lines() {
        let line = line?;
        let mut chars = line.chars();
        let direction = if chars.next().unwrap() == 'L' { -1 } else { 1 };
        let how_much = chars.as_str().parse().unwrap();

        // I'm too lazy to do modulus and stuff. Let's use a dumb loop!
        for _ in 0..how_much {
            position = (position + direction).rem_euclid(100);
            if position == 0 {
                zero_clicks += 1;
            }
        }

        if position == 0 {
            zeroes_at_end += 1;
        }
    }
    println!("zeroes_at_end: {}, zero_clicks: {}", zeroes_at_end, zero_clicks);
    Ok(())
}
