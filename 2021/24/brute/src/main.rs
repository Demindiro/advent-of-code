use std::sync::atomic::{AtomicUsize, Ordering};

#[inline(always)]
fn f(a: i64, b: i64, c: i64, w: i64, z: i64) -> i64 {
    let x = i64::from(z % 26 + b != w);
    return z / a * (25 * x + 1) + (w + c) * x;
}

/*
fn main() {
    let z = 0;
    for a in (1..=9).rev() {
        // 0
        let z = f(1, 13, 3, a, z);
        for b in (1..=9).rev() {
            // 1
            let z = f(1, 11, 12, b, z);
            println!("{}{}", a, b);
            for c in (1..=9).rev() {
                // 2
                let z = f(1, 15, 9, c, z);
                for d in (1..=9).rev() {
                    // 3
                    let z = f(26, -6, 12, d, z);
                    for e in (1..=9).rev() {
                        // 4
                        let z = f(1, 15, 2, e, z);
                        for o in (1..=9).rev() {
                            // 5
                            let z = f(26, -8, 1, o, z);
                            for g in (1..=9).rev() {
                                // 6
                                let z = f(26, -4, 1, g, z);
                                for h in (1..=9).rev() {
                                    // 7
                                    let z = f(1, 15, 13, h, z);
                                    for i in (1..=9).rev() {
                                        // 8
                                        let z = f(1, 10, 1, i, z);
                                        for j in (1..=9).rev() {
                                            // 9
                                            let z = f(1, 11, 6, j, z);
                                            for k in (1..=9).rev() {
                                                // 10
                                                let z = f(26, -11, 2, k, z);
                                                for l in (1..=9).rev() {
                                                    // 11
                                                    let z = f(26, 0, 11, l, z);
                                                    for m in (1..=9).rev() {
                                                        // 12
                                                        let z = f(26, -8, 10, m, z);
                                                        for n in (1..=9).rev() {
                                                            // 13
                                                            let z = f(26, -7, 3, n, z);
                                                            if z == 0 {
                                                                println!(
                                                                    "{}{}{}{}{}{}{}{}{}{}{}{}{}{}",
                                                                    a,
                                                                    b,
                                                                    c,
                                                                    d,
                                                                    e,
                                                                    o,
                                                                    g,
                                                                    h,
                                                                    i,
                                                                    j,
                                                                    k,
                                                                    l,
                                                                    m,
                                                                    n
                                                                );
																return;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
*/

fn main() {
    let z = 0;
    for a in 1..=9 {
        // 0
        let z = f(1, 13, 3, a, z);
        for b in 8..=9 {
            // 1
            let z = f(1, 11, 12, b, z);
            println!("{}{}", a, b);
            for c in 1..=9 {
                // 2
                let z = f(1, 15, 9, c, z);
                for d in 1..=9 {
                    // 3
                    let z = f(26, -6, 12, d, z);
                    for e in 1..=9 {
                        // 4
                        let z = f(1, 15, 2, e, z);
                        for o in 1..=9 {
                            // 5
                            let z = f(26, -8, 1, o, z);
                            for g in 1..=9 {
                                // 6
                                let z = f(26, -4, 1, g, z);
                                for h in 1..=9 {
                                    // 7
                                    let z = f(1, 15, 13, h, z);
                                    for i in 1..=9 {
                                        // 8
                                        let z = f(1, 10, 1, i, z);
                                        for j in 1..=9 {
                                            // 9
                                            let z = f(1, 11, 6, j, z);
                                            for k in 1..=9 {
                                                // 10
                                                let z = f(26, -11, 2, k, z);
                                                for l in 1..=9 {
                                                    // 11
                                                    let z = f(26, 0, 11, l, z);
                                                    for m in 1..=9 {
                                                        // 12
                                                        let z = f(26, -8, 10, m, z);
                                                        for n in 1..=9 {
                                                            // 13
                                                            let z = f(26, -7, 3, n, z);
                                                            if z == 0 {
                                                                println!(
                                                                    "{}{}{}{}{}{}{}{}{}{}{}{}{}{}",
                                                                    a,
                                                                    b,
                                                                    c,
                                                                    d,
                                                                    e,
                                                                    o,
                                                                    g,
                                                                    h,
                                                                    i,
                                                                    j,
                                                                    k,
                                                                    l,
                                                                    m,
                                                                    n
                                                                );
																return;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
