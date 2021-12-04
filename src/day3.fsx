#load "helpers.fsx"

open System
open System.IO
open Helpers


// I recommend averting your eyes and moving a way from this one
// I had a little too much fun being far to uncleverly clever


let gammaToEpsilon: int -> int = (~~~)


let bitSteps max =
    Seq.unfold
        (fun bit ->
            if bit > max then
                None
            else
                Some(bit, bit * 2))
        1

let zeros = Seq.initInfinite (fun _ -> 0)


let vectorAdd (vec1: seq<int>) (vec2: seq<int>) : seq<int> =
    vec2 |> Seq.zip vec1 |> Seq.map (uncurry2 (+))


let bitsAsVector n = bitSteps n |> Seq.map ((&&&) n)


let ensureEqualVecSize (vec: int []) (n, bitcount) : seq<int> * seq<int> =
    if vec.Length > bitcount then
        (vec, Seq.append (bitsAsVector n) zeros)
    else
        (Seq.append vec zeros, bitsAsVector n)


let reducer =
    (uncurry2 ensureEqualVecSize)
    >> (uncurry2 vectorAdd)
    >> Seq.toArray


let addBitToInt it (index, bit) = it ||| (bit <<< index)


let bitCount = float >> Math.Log2 >> int


let toGamma countsVec totalCount =
    let mid = totalCount / 2 // check this

    countsVec
    |> Seq.mapi (fun i n -> if n > mid then (1, i) else (0, i))
    |> Seq.fold addBitToInt 0

let powerConsumption (countsVec, totalCount) =
    let gamma = toGamma countsVec totalCount
    gamma * (gammaToEpsilon gamma)


let data =
    File.ReadLines @"./input-files/day3.txt"
    |> Seq.map (fun it -> Convert.ToInt32(it, 2)) // base 2 convert


let result =
    data
    |> Seq.map (fun bytes -> (bytes, bitCount bytes))
    |> Seq.fold (fun (arr, n) next -> (reducer (arr, next), n + 1)) ([||], 0)
    |> powerConsumption


printfn "power consumption is %i" result
