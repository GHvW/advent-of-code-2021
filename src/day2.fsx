#load "helpers.fsx"

open System
open System.IO
open Helpers

type Direction =
    | Up of int
    | Down of int
    | Forward of int
    | None


let inputToDirection (input: string) : Direction =
    match input.Split(" ") with
    | [| "up"; units |] -> Direction.Up(Int32.Parse units)
    | [| "down"; units |] -> Direction.Down(Int32.Parse units)
    | [| "forward"; units |] -> Direction.Forward(Int32.Parse units)
    | _ -> Direction.None


let movePosition ((position, depth): struct (int * int)) (direction: Direction) : struct (int * int) =
    match direction with
    | Direction.Up units -> struct (position, depth - units)
    | Direction.Down units -> struct (position, depth + units)
    | Direction.Forward units -> struct (position + units, depth)
    | Direction.None -> struct (position, depth)


let result =
    File.ReadLines @"./input-files/day2.txt"
    |> Seq.map inputToDirection
    |> Seq.fold movePosition (0, 0)
    |> (structUncurry2 (*))


printfn "part 1 multiple: %i" result


// -------------- Part 2 -----------------------


let positionAndAim
    ((aim, position, depth): struct (int * int * int))
    (direction: Direction)
    : struct (int * int * int) =
    match direction with
    | Direction.Up units -> struct (aim - units, position, depth)
    | Direction.Down units -> struct (aim + units, position, depth)
    | Direction.Forward units -> struct (aim, position + units, depth + (aim * units))
    | Direction.None -> struct (aim, position, depth)


let result2 =
    File.ReadLines @"./input-files/day2.txt"
    |> Seq.map inputToDirection
    |> Seq.fold positionAndAim (0, 0, 0)
    |> fun struct (_, position, depth) -> position * depth


printfn "part 2 multiple: %i" result
