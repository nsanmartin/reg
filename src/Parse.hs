module Parse
where
import Data.List.Split


-- todo: use readMaybe or any other thing here
splitRegsStr :: String -> (String, [Int])
splitRegsStr regsStr = let (l:r) = splitOn "." regsStr in (l, map read r)


-- todo: use readMaybe or any other thing here
readIndex :: String -> [Int]
readIndex = (map read) . (splitOn ",")
