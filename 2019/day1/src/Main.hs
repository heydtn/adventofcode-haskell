calculateFuel1 :: Integer -> Integer
calculateFuel1 mod = floor (fromIntegral mod / 3) - 2

calculateFuel2 :: Integer -> Integer
calculateFuel2 mod
    | fuel <= 0 = 0
    | otherwise = fuel + calculateFuel2 fuel
    where fuel = floor (fromIntegral mod / 3) - 2

main = do
    f <- readFile "src/day1.input"
    let values = map read . lines $ f 
    let fuel calculator = sum . map calculator $ values
    return (fuel calculateFuel1, fuel calculateFuel2)
