let uncurry2 f (x, y) = f x y

let structUncurry2 (f: 'a -> 'b -> 'c) ((x, y): struct ('a * 'b)) : 'c = f x y

let curry2 f x y = f (x, y)
