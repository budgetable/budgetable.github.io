module Utils.List where

dropIndex :: Int -> [a] -> [a]
dropIndex idx xs = take idx xs <> drop (idx + 1) xs

moveUp :: Int -> [a] -> [a]
moveUp idx xs
  | idx == 0 = xs
  | otherwise =
    take (idx - 1) xs -- everything before the next one up
    <> [xs !! idx] -- me
    <> take 1 (drop (idx - 1) xs) -- the one that was the next one up
    <> drop (idx + 1) xs -- everything after me

moveDown :: Int -> [a] -> [a]
moveDown idx xs
  | idx == length xs - 1 = xs
  | otherwise =
    take idx xs -- everything before me
    <> take 1 (drop (idx + 1) xs) -- the next one after me
    <> [xs !! idx] -- me
    <> drop (idx + 2) xs -- everything after the next one down
