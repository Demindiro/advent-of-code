#![feature(map_first_last)]

mod grid;

use grid::*;
use minifb::*;
use std::fs;
use std::time::Duration;

fn main() {
    let fps = 60;
	let ripple_factor = 20;
	let steps_per_draw = 1 << 10;

    let grid = Grid::parse_input(fs::read("../input.txt").unwrap());
    let grid = grid.enlarge(5);

    // ARGB
    let mut buffer = grid
        .data()
        .iter()
        .copied()
        .map(|n| n)
        .map(|n| (n * 255 / 9) as u32)
        .collect::<Box<_>>();
    let w = grid.width().try_into().unwrap();
    let h = grid.height().try_into().unwrap();
    dbg!(w, h);
    let mut window = Window::new(
        "Day 15",
        w,
        h,
        WindowOptions {
            scale: Scale::X1,
            ..Default::default()
        },
    )
    .unwrap();
    window.update_with_buffer(&buffer, w, h).unwrap();
    window.update_with_buffer(&buffer, w, h).unwrap();

    println!("Press Enter to start");
    std::io::stdin().read_line(&mut Default::default()).unwrap();

	/*
	let mut solve = || {
		let mut steps @ mut max_risk = 0;

		// Do pre-run to figure out max risk
		grid.bfs(|flood, scan| {
			for &(x, y) in scan.iter() {
				max_risk = max_risk.max(flood.get(x, y).unwrap());
			}
		});
		max_risk /= ripple_factor;

		// Flood fill
		let flood = grid.bfs(|flood, scan| {
			for &(x, y) in scan.iter() {
				if steps % steps_per_draw == 0 {
					std::thread::sleep(Duration::from_nanos(1_000_000_000 / fps));
					for c in buffer.iter_mut() {
						*c &= 0x00_ff_ff;
					}
					window.update_with_buffer(&buffer, w, h).unwrap();
				}
				steps += 1;
				let v = flood.get(x, y).unwrap();
				let v = (255 * v / max_risk) as u8;
				buffer[y * w + x] &= 0x00_00_ff;
				buffer[y * w + x] |= (v as u32) << 8;
				buffer[y * w + x] |= 0xff_00_00;
			}
		});
		(flood, steps)
	};
	*/

	let mut solve = || {
		let mut steps @ mut max_risk = 0;

		// Do pre-run to figure out max risk
		grid.bfs2(|flood, (x, y)| {
			max_risk = max_risk.max(flood.get(x, y).unwrap());
		});
		max_risk /= ripple_factor;

		// Flood fill
		let flood = grid.bfs2(|flood, (x, y)| {
			if steps % steps_per_draw == 0 {
				std::thread::sleep(Duration::from_nanos(1_000_000_000 / fps));
				for c in buffer.iter_mut() {
					*c &= 0x00_ff_ff;
				}
				window.update_with_buffer(&buffer, w, h).unwrap();
			}
			steps += 1;
			let v = flood.get(x, y).unwrap();
			let v = (255 * v / max_risk) as u8;
			buffer[y * w + x] &= 0x00_00_ff;
			buffer[y * w + x] |= (v as u32) << 8;
			buffer[y * w + x] |= 0xff_00_00;
		});
		(flood, steps)
	};

	let (flood, steps) = solve();

    println!(
        "{} (steps: {})",
        flood.get(flood.width() - 1, flood.height() - 1).unwrap(),
		steps
    );
    for c in buffer.iter_mut() {
        *c &= 0x00_ff_ff;
    }

    // Walk back
    let (mut x, mut y) = (flood.width() - 1, flood.height() - 1);
    while (x, y) != (0, 0) {
        buffer[y * w + x] = 0xff_00_00;
        window.update_with_buffer(&buffer, w, h).unwrap();

        let f = |(dx, dy)| {
            flood
                .get(x.wrapping_add(dx as usize), y.wrapping_add(dy as usize))
                .unwrap_or(usize::MAX)
        };
        let (dx, dy) = [(-1isize, 0isize), (0, -1), (1, 0), (0, 1)]
            .iter()
            .copied()
            .min_by(|a, b| f(*a).cmp(&f(*b)))
            .unwrap();
        x = x.wrapping_add(dx as usize);
        y = y.wrapping_add(dy as usize);

        std::thread::sleep(Duration::from_nanos(1_000_000_000 / fps));
    }
    println!("Done");

    std::thread::sleep(Duration::MAX);
}
