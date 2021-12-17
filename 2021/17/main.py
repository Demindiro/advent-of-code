#((lx, ly), (ux, uy)) = ((20, -10), (30, -5))
((lx, ly), (ux, uy)) = ((128, -142), (160, -88))

(max_height, count, x) = (0, 0, 0)
for vx2 in range(99999999):
    for vy in range(-(vx2 * vx2), vx2 * vx2):
        (vx, x, y, mh) = (vx2, 0, 0, 0)
        while not (lx <= x <= ux and ly <= y <= uy):
            (x, y, vx, vy) = (x + vx, y + vy, vx - int(vx > 0), vy - 1)
            if x > ux or y < ly:
                break
            mh = max(mh, y)
        else:
            max_height = max(max_height, mh)
            count += 1
    if x > 2 * ux:
        break

print(max_height)
print(count)
