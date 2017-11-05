// So we've got a row of cells, one of which (number 40) is alive.
let cells =
    Array.init 80 (fun i -> if i = 40 then 1 else 0)
    |> Array.toList

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

// loop is a tail recursive function that prints cells then calculates the 
// next row of cells, repeating for the specified number of iterations.
let rec loop iterations cells =
    if iterations > 0 then
        cells |> printCells
        cells |> next |> loop (iterations - 1)
    else
        ()    

// print 7 iterations
cells |> loop 7

// vim: et ai ts=4 sw=4
