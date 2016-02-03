--data MyNum = Nothing | Just Int
  --deriving Show

--lift f Main.Nothing _ = Main.Nothing
--lift f _ Main.Nothing = Main.Nothing
--lift f (Main.Just a) (Main.Just b) = (Main.Just (f a b))

lift f Nothing _ = Nothing
lift f _ Nothing = Nothing
lift f (Just a) (Just b) = (Just (f a b))


data Tree a = Node a (Tree a) (Tree a)
            | Empty
  deriving Show

-- Your code goes here.


add node Empty = Node node Empty Empty
add node (Node a b c) = case (node > a) of
  True -> (Node a b (add node c))
  False -> (Node a (add node b) c)
 