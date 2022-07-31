module Parse
where
import Data.List.Split
import Store


-- todo: use readMaybe or any other thing here?
splitRegsStr :: String -> (String, [Int])
splitRegsStr regsStr = case splitOn "." regsStr of
    (x:[]) -> (x, [])
    (x:y:[]) -> (x, map (read . ("0x"++) . (:[])) y)
    _ -> error $ "bad input: " ++ regsStr

splitRegStr :: String -> (String, String)
splitRegStr ls = go ls [] []
    where go [] rs ixs = (rs, ixs)
          go xxs@(x:xs) rs ixs = if not (x `elem` regList)
                                  then (rs, xxs)
                                  else go xs (x:rs) ixs
