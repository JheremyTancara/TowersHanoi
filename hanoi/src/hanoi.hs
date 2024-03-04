module Hanoi where
    
type Post = String
type Mov = (Post, Post)

hanoi :: Integer -> Post -> Post -> Post -> [Mov]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, c)] ++ hanoi (n - 1) b a c
