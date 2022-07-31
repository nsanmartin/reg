module Parse
where
import Data.List.Split (splitOn)

-- todo: use readMaybe or any other thing here?
splitRegsStr :: String -> (String, [Int])
splitRegsStr regsStr = case splitOn "." regsStr of
    (x:[]) -> (x, [])
    (x:y:[]) -> (x, map (read . ("0x"++) . (:[])) y)
    _ -> error $ "bad input: " ++ regsStr

