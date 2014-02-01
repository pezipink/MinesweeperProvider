﻿namespace MinesweeperProvider

module Value =
    let count (chars:char[][]) (x,y) =
        [-1,-1; 0,-1; 1,-1
         -1, 0;       1, 0
         -1, 1; 0, 1; 1, 1]
        |> List.filter (fun (dx,dy) ->
            let x, y = x + dx, y + dy
            y>=0 && y<chars.Length && 
            x>=0 && x<chars.[y].Length &&
            chars.[y].[x] = '*'
        )
        |> List.length
    let from chars (x,y) c =
        let neighbours = count chars (x,y)
        match c with
        | '*' -> c
        | '.' when neighbours > 0 -> '0' + char (neighbours)
        | '.' -> ' '
        | _ -> new System.ArgumentException("Unexpected character") |> raise

[<AutoOpen>]
module Algorithm =
    let flood canFill fill (x,y) =
        let rec next = function
            | [] -> ()
            | ps ->
                let qs = 
                    ps 
                    |> List.filter canFill
                    |> List.collect (fun (x,y) -> 
                        [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
                    )
                ps |> List.iter fill
                qs |> next
        next [(x,y)]

type SquareState = { Value:char; mutable IsShowing:bool }
type SquareInfo = { X:int; Y:int; Value:char }

type Minefield (squares:SquareState[][],hit) =
    let mutable mineHit = hit
    let height, width = squares.Length, squares.[0].Length
    let isInRange (x,y) =
        y >= 0  && y < height &&
        x >= 0  && x < width
    let surroundingSquares (x,y) =
        let values = System.Collections.Generic.List<_>()
        let canFill (x,y) = 
            isInRange (x,y) &&
            squares.[y].[x].Value=' ' &&
            not squares.[y].[x].IsShowing
        let fill (x,y) = 
            if isInRange (x,y) then
                let square = squares.[y].[x]
                if not square.IsShowing then
                    square.IsShowing <- true
                    values.Add(x,y,square.Value)
        flood canFill fill (x,y)
        values |> Seq.toList |> Seq.distinct |> Seq.toArray
    let mines =
        squares |> Array.mapi (fun y row -> 
            row |> Array.mapi (fun x square -> 
                (x,y,'*'), (square.Value = '*')
            )
        )
        |> Array.concat
        |> Array.filter snd
        |> Array.map fst
    let reveal (x,y) =
        let square = squares.[y].[x]
        if square.IsShowing
        then [||]
        else
            match square.Value with
            | '*' -> mineHit <-true
                     square.IsShowing <- true
                     mines
            | ' ' -> surroundingSquares (x,y)
            |  c  ->
                square.IsShowing <- true
                [|x,y,c|]
        |> Array.map (fun (x,y,c) -> { X=x; Y=y; Value= if c = ' ' then '_' else c })

    new (chars:char[][]) =
        Minefield(
            chars |> Array.mapi (fun y row ->
                row |> Array.mapi (fun x c -> 
                    let value = Value.from chars (x,y) c
                    { Value=value; IsShowing=false} )
            ),false)
    member field.MineHit = mineHit
    member field.Mines = mines.Length
    member field.Width = width
    member field.Height = height
    member field.Reveal(x,y) = reveal (x,y)
    member field.Clone() = 
        Minefield(
            squares |> Array.map(fun row -> 
                row |> Array.map(fun s -> { IsShowing = s.IsShowing; Value = s.Value })) ,field.MineHit)        
    member field.Display =
          squares |> Array.mapi (fun y row -> 
            row |> Array.mapi (fun x square -> 
                if square.IsShowing then square.Value else '#'))
