import Data.List
import Data.Typeable

groupNumbers :: String -> [[String]]
groupNumbers s = groupBy (\x y -> x /= "" && y /= "") (lines s)

removeEmptyLists :: [[String]] -> [[String]]
removeEmptyLists = filter (not . elem "")

caloriesPerElf :: [Int] -> [Int]
caloriesPerElf = map sum . groupBy (\x y -> x /= 0 && y /= 0)

maxCalories :: [Int] -> Int
maxCalories = maximum

mapStringsToInts :: [[String]] -> [[Int]]
mapStringsToInts = map (map read)

reduceToSum :: [[Int]] -> [Int]
reduceToSum = map sum

sumTopThree :: [Int] -> Int
sumTopThree xs = sum (take 3 (reverse (sort xs)))

main :: IO()
main = do
  input <- readFile "inputs/1.txt"
  print (typeOf input)
  let calories = groupNumbers input
  -- print (calories)
  let rem = removeEmptyLists calories
  let intify = mapStringsToInts rem
  let summed = reduceToSum intify
  print (maxCalories summed)

  let top3 = sumTopThree summed

  print (top3)