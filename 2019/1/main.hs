input :: String -> [Int]
input l = [read l :: Int | l <- lines l]

calc x = max (x `div` 3 - 2) 0

calc_rec x
	| x <= 0    = 0
	| otherwise = calc x + calc_rec (calc x)

main = do
	n <- readFile "input"
	let l = input n
	print (sum [calc x | x <- l])
	print (sum [calc_rec x | x <- l])
