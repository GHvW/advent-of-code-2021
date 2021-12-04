#load "helpers.fsx"

open System
open System.IO
open Helpers


// I recommend averting your eyes and moving a way from this one
// I had a little too much fun being far to uncleverly clever


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


let bitsAsVector n = bitSteps n |> Seq.mapi (fun index next -> (n &&& next) >>> index)


let ensureEqualVecSize (vec: int []) (n, bitcount) : seq<int> * seq<int> =
    if vec.Length > bitcount then
        (vec, Seq.append (bitsAsVector n) zeros)
    else
        (Seq.append vec zeros, bitsAsVector n)


let reducer =
    (uncurry2 ensureEqualVecSize)
    >> (uncurry2 vectorAdd)
    >> Seq.toArray


let addBitToInt it index = it ||| (1 <<< index)


let bitCount = float >> Math.Log2 >> int


let powerConsumption (countsVec, totalCount) : int =
    let mid = totalCount / 2

    countsVec
    |> Seq.zip (Seq.initInfinite id)
    |> Seq.fold (fun (gamma, epsilon) (i, next) ->
        if next > mid then
            (addBitToInt gamma i, epsilon)
        else
            (gamma, addBitToInt epsilon i)) (0, 0)
    |> (uncurry2 (*))


let data =
    File.ReadLines @"./input-files/day3.txt"
    |> Seq.map (fun it -> Convert.ToInt32(it, 2)) // base 2 convert


let result =
    data
    |> Seq.map (fun bytes -> (bytes, bitCount bytes))
    |> Seq.fold (fun (arr, n) next -> (reducer (arr, next), n + 1)) ([||], 0)
    |> powerConsumption


printfn "power consumption is %i" result


// -------------------- Part 2 ----------------------------