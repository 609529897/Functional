chian :: Integer -> [Integer]
chian 1 = [1]
chian n
    | even n = n:chian (n `div` 2)
    | odd n = n:chian (n * 3 + 1)

numLongChians :: Int
numLongChians = length (filter isLong (map chian [1..100])
    where isLong xs = length xs > 15