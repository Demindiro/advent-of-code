mod grid;

use std::fs;
use grid::*;
use minifb::*;

fn main() {
    let grid = Grid::parse_input(fs::read("../input.txt").unwrap());

    //println!("{}", grid.bfs());

	let grid = grid.enlarge(5);

	// ARGB
	let mut buffer = grid.data().iter().copied().map(|n| n)
		.map(|n| (n * 255 / 9) as u32).collect::<Box<_>>();
	let w = grid.width().try_into().unwrap();
	let h = grid.height().try_into().unwrap();
	dbg!(w, h);

	let mut window = Window::new("Day 15", w, h, WindowOptions {
		scale: Scale::X1,
		..Default::default()
	})
		.unwrap();
	window.update_with_buffer(&buffer, w, h).unwrap();
	window.update_with_buffer(&buffer, w, h).unwrap();
	println!("Press Enter to start");
	std::io::stdin().read_line(&mut Default::default()).unwrap();

	// Do pre-run to figure out max risk
	let mut max_risk = 0;
	grid.bfs(|flood, scan| {
		for (x, y) in scan.iter().copied() {
			max_risk = max_risk.max(flood.get(x, y).unwrap());
		}
	});

	use std::time::Duration;
    println!("{}", grid.bfs(|flood, scan| {
		std::thread::sleep(Duration::from_nanos(1_000_000_000 / 60));
		for c in buffer.iter_mut() {
			*c &= 0x00_ff_ff;
		}
		for (x, y) in scan.iter().copied() {
			let v = flood.get(x, y).unwrap();
			let v = (255 * v / max_risk) as u8;
			buffer[y * w + x] &= 0x00_00_ff;
			buffer[y * w + x] |= (v as u32) << 8;
			buffer[y * w + x] |= 0xff_00_00;
		}
		window.update_with_buffer(&buffer, w, h).unwrap();
	}));

	std::thread::sleep(Duration::MAX);
}
