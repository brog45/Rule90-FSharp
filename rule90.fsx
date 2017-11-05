let cellChar =
    function
    | 0 -> " "
    | 1 -> "X"
    | _ -> "E"

let printSeq seq1 =
    seq1
    |> Seq.map cellChar
    |> Seq.iter (printf "%s")
    printfn ""

let cells =
    Array.init 80 (fun i -> if i = 40 then 1 else 0)
    |> Array.toList

let next cells = 
    [ 0 ] @ cells @ [ 0 ] 
    |> Seq.windowed 3
    |> Seq.map (fun a -> (a.[0] + a.[2]) % 2)
    |> Seq.toList

let rec loop iterations cells =
    if iterations > 0 then
        cells |> printSeq
        cells |> next |> loop (iterations - 1)
    else
        ()    

cells |> loop 7

// vim: et ai ts=4 sw=4
