namespace fplab2

module HashSet =

    type HashSet<'T when 'T : equality>(capacity: int ) =
        
        let mutable table = Array.init capacity (fun _ -> None: option<'T>)
        let mutable size = 0
        let loadFactor = 0.75
        
        let hashIndex x len = (hash x % len + len) % len
        
        
        let resize () =
            let newCapacity = table.Length * 2
            let newTable = Array.init newCapacity (fun _ -> None : 'T option)
            let rec tryInsert index item =
                match table.[index] with
                | Some x when x = item -> () // Already present, do nothing
                | None ->
                    newTable.[index] <- Some item
                    size <- size + 1
                | _ ->
                    tryInsert ((index + 1) % size) item
            for i = 0 to table.Length - 1 do
                match table.[i] with
                | Some value -> tryInsert i value
                | None -> ()
            table <- newTable
        
        static member Default () = HashSet<'T> (16)
        
        member this.Capacity = table.Length
        member this.Add(item: 'T) =
            if float size / float table.Length >= loadFactor  then resize()
            let rec tryInsert index =
                match table.[index] with
                | Some x when x = item -> () // Already present, do nothing
                | None ->
                    table.[index] <- Some item
                    size <- size + 1
                | _ ->
                    tryInsert ((index + 1) % size)
            let index = hashIndex item  table.Length
            tryInsert index
        
        member this.Contains(item: 'T) =
            let rec tryFind index =
                match table.[index] with
                | Some x when x = item -> true
                | None -> false
                | _ -> tryFind ((index + 1) % size)
            let index = hashIndex item  table.Length
            tryFind index

        member this.Remove(item: 'T) =
            let rec tryRemove index =
                match table.[index] with
                | Some x when x = item ->
                    table.[index] <- None
                    size <- size - 1
                | Some _ -> tryRemove ((index + 1) % size)
                | None -> ()
            let index = hashIndex item table.Length
            tryRemove index
        member this.Size = size
        
        
        
        member this.Print() =
            table |> Array.choose id |> Array.iter (fun x -> printf "%A " x)
            printfn ""
        
        member this.Iter(f: 'T -> unit) = table |> Array.iter (fun x ->
            match x with
            | None -> ()
            |_ -> f x.Value
           )
        member this.IterRev(f : 'T -> unit) = table |> Array.rev |> Array.iter (fun x ->
            match x with
            | None -> ()
            |_ -> f x.Value
           )
        static member iter (f: 'T -> unit) (set : HashSet<'T>) = set.Iter(f)
        
        static member map (f : 'T -> 'M) (set : HashSet<'T>) =
            let newSet = HashSet<'M>(set.Capacity)
            set.Iter(fun x -> newSet.Add(f x))
            newSet
        
        static member filter (f : 'T -> bool) (set : HashSet<'T>) =
            let newSet = HashSet<'T>(set.Capacity)
            set.Iter(fun x -> if f x then newSet.Add(x))
            newSet
        static member foldl (f : 'M -> 'T -> 'M) (acc :  'M) (set : HashSet<'T>) =
            let mutable mut_acc = acc
            let nextAcc x = (mut_acc <- f mut_acc x)
            set.Iter(nextAcc)
            mut_acc
        
        static member foldr (f : 'M -> 'T -> 'M) (acc :  'M) (set : HashSet<'T>) =
            let mutable mut_acc = acc
            let nextAcc x = (mut_acc <- f mut_acc x)
            set.IterRev(nextAcc)
            mut_acc
            
    let (@) (set1 : HashSet<'T>) (set2 : HashSet<'T>) =
          let set3 = HashSet<'T>(set1.Capacity + set2.Capacity)
          set1.Iter(fun (x) -> set3.Add(x))
          set2.Iter(fun (x) -> set3.Add(x))
          set3