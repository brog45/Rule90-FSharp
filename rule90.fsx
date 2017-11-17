// So we've got a row of cells, one of which (number 40) is alive.
let cells = List.init 80 (fun i -> if i = 40 then 1 else 0)

// Let's print living cells as "X" and dead cells as a space, " ".
let cellChar =
    function
    | 0 -> " "
    | _ -> "X"

// printCells prints a row of cells
let printCells rowOfCells =
    rowOfCells 
    |> Seq.map cellChar       // map each cell to a character
    |> Seq.iter (printf "%s") // iterate over the sequence and print each cell's character
    printfn ""                // then start a new line

// next calculates what the next row of cells should look like, given the 
// current row.
let next rowOfCells = 
    [ 0 ] @ rowOfCells @ [ 0 ] 
    |> Seq.windowed 3
    |> Seq.map (fun a -> (a.[0] + a.[2]) % 2)
    |> Seq.toList

// A sequence of with a loop inside that yields the result of each iteration 
// abstracts the loop from what we may want to do with the result of each 
// iteration.
let culture = seq {
    let mutable generation = cells
    while true do
        yield generation
        generation <- next generation
}

// take 7 iterations and print them
culture 
|> Seq.take 7 
|> Seq.iter printCells

// vim: et ai ts=4 sw=4
