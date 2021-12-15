use std::collections::HashSet;
use std::mem;

pub struct Grid {
    width: usize,
    height: usize,
    data: Box<[usize]>,
}

impl Grid {
    pub fn parse_input(text: Vec<u8>) -> Self {
        let height = text.split(|&c| c == b'\n').count() - 1; // off by one, idk
        let width = text.split(|&c| c == b'\n').next().unwrap().len();
        let data = text
            .into_iter()
            .filter(|&c| c != b'\n')
            .map(|c| usize::from(c - b'0'))
            .collect();
        Self {
            height,
            width,
            data,
        }
    }

	pub fn width(&self) -> usize {
		self.width
	}

	pub fn height(&self) -> usize {
		self.height
	}

	pub fn data(&self) -> &[usize] {
		&self.data
	}

    pub fn bfs<F>(&self, mut feedback: F) -> Self
	where
		F: FnMut(&Self, &HashSet<(usize, usize)>),
	{
        let mut flood = Self {
            width: self.width,
            height: self.height,
            data: self.data.iter().map(|_| usize::MAX).collect(),
        };
		flood.data[0] = 0;
        let mut scan = HashSet::new();
        let mut scan_next = HashSet::new();
        scan.insert((0, 0));
        loop {
			feedback(&flood, &scan);
            if scan.is_empty() {
                return flood;
            }
            for (x, y) in scan.drain() {
                let v = flood.get(x, y).unwrap();
                let mut f = |x, y| {
                    let v = v + self.get(x, y).unwrap();
                    if v < flood.get(x, y).unwrap() {
                        flood.set(x, y, v).unwrap();
						scan_next.insert((x, y));
                    }
                };
                (x < self.width - 1).then(|| f(x + 1, y));
                (y < self.height - 1).then(|| f(x, y + 1));
                (x > 0).then(|| f(x - 1, y));
                (y > 0).then(|| f(x, y - 1));
            }
            mem::swap(&mut scan, &mut scan_next);
        }
    }

    pub fn enlarge(self, n: usize) -> Self {
        let (sx, sy) = (self.width, self.height);
        let f = |x, y, mx, my| -> usize {
			(mx + my + self.get(x, y).unwrap() - 1) % 9 + 1
		};
        Self {
            width: self.width * n,
            height: self.height * n,
            data: (0..n).flat_map(move |my| {
					(0..sy).flat_map(move |y| {
						(0..n).flat_map(move |mx| {
							(0..sx).map(move |x| {
								f(x, y, mx, my)
							})
						})
                    })
                })
                .collect(),
        }
    }

    pub fn get(&self, x: usize, y: usize) -> Option<usize> {
        (x < self.width && y < self.height).then(|| self.data.get(y * self.width + x).copied()).flatten()
    }

    fn set(&mut self, x: usize, y: usize, value: usize) -> Option<()> {
        (x < self.width && y < self.height).then(|| self.data.get_mut(y * self.width + x).map(|v| *v = value)).flatten()
    }
}
