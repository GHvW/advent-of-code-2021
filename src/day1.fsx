open System
open System.IO

let miniinput =
    [ 199
      200
      208
      210
      200
      207
      240
      269
      260
      263 ]


let increased x y = x < y


let uncurry2 f (x, y) = f x y


let findIncreaseCount: seq<int> -> int =
    Seq.pairwise
    >> Seq.filter (uncurry2 increased)
    >> Seq.length


// printfn "increase count: %i" (increaseCount miniinput)

let result =
    File.ReadLines(@"./input-files/day1.txt")
    |> Seq.map Int32.Parse
    |> findIncreaseCount


printfn "part 1 increase count: %i" result


// -------- Part 2 --------------


let findWindowedIncreaseCount: seq<int> -> int =
    Seq.windowed 3
    >> Seq.map (Array.sum)
    >> findIncreaseCount


let result2 =
    File.ReadLines(@"./input-files/day1.txt")
    |> Seq.map Int32.Parse
    |> findWindowedIncreaseCount


printfn "part 2 increase count: %i" result2
