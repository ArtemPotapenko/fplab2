module fplab2.Program

open fplab2.HashSet

let set = HashSet<int>(16) |> add 5 |> add 8

set.table |> Array.map Option.get |> ignore
