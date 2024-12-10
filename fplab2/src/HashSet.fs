namespace fplab2

open Microsoft.FSharp.Core

module HashSet =

    type HashSet<'T when 'T: equality>(table: 'T option array, size: int) =
        member val table = table
        member val size = size
        member val loadFactor = 0.75
        member val capacity = table.Length

        new(capacity: int) =
            let table = Array.init capacity (fun _ -> None: 'T option)
            HashSet(table, 0)

    let hashIndex x len = (hash x % len + len) % len
    let default_set<'T when 'T: equality> () = HashSet<'T>(16)

    let resize (set: HashSet<'T>) =
        let newCapacity = set.capacity * 2
        let table = Array.init newCapacity (fun _ -> None: 'T option)

        let rec tryInsert index item =
            match table[index] with
            | Some x when x = item -> () 
            | None -> table[index] <- Some item
            | _ -> tryInsert ((index + 1) % newCapacity) item

        set.table
        |> Array.map Option.get
        |> Array.iter (fun x -> tryInsert (hashIndex x set.capacity) x)

        HashSet(table, set.size)

    let add (x: 'T) (hash_set: HashSet<'T>) =
        let rec tryInsert index item (set: HashSet<'T>) : HashSet<'T> =
            match set.table[index] with
            | Some x when x = item -> set 
            | None ->
                set.table[index] <- Some item
                HashSet(set.table, set.size + 1)
            | _ -> tryInsert ((index + 1) % set.capacity) item set

        match float hash_set.size / float hash_set.capacity with
        | l when l >= hash_set.loadFactor ->
            let hash_set1 = hash_set |> resize
            let index = hashIndex x hash_set1.capacity
            let hash_set2 = tryInsert index x hash_set1
            hash_set2
        | _ ->
            let index = hashIndex x hash_set.capacity
            let hash_set1 = tryInsert index x hash_set
            hash_set1

    let contains (item: 'T) (set: HashSet<'T>) =
        let rec tryFind index =
            match set.table.[index] with
            | Some x when x = item -> true
            | None -> false
            | _ -> tryFind ((index + 1) % set.capacity)

        let index = hashIndex item set.capacity
        tryFind index

    let remove (item: 'T) (set: HashSet<'T>) =
        let rec tryRemove index =
            match set.table.[index] with
            | Some x when x = item ->
                set.table.[index] <- None
                HashSet(set.table, set.size - 1)
            | Some _ -> tryRemove ((index + 1) % set.capacity)
            | None -> set

        let index = hashIndex item set.capacity
        tryRemove index

    let iter (f: 'T -> unit) (set: HashSet<'T>) =
        set.table
        |> Array.iter (fun x ->
            match x with
            | None -> ()
            | _ -> f x.Value)

    let map (f: 'T -> 'M) (set: HashSet<'T>) =
        let table = Array.init set.capacity (fun _ -> None: 'M option)

        let rec tryInsert index item =
            match table[index] with
            | Some x when x = item -> () 
            | None -> table[index] <- Some item
            | _ -> tryInsert ((index + 1) % set.capacity) item

        set.table |> Array.filter Option.isSome
        |> Array.iter (fun x -> tryInsert (hashIndex (f x.Value) set.capacity) (f x.Value))

        let newSize = table |> Array.filter Option.isSome |> Array.length

        HashSet(table, newSize)

    let foldl (f: 'M -> 'T -> 'M) (acc: 'M) (set: HashSet<'T>) =
        set.table |> Array.filter Option.isSome |> Array.map _.Value |> Array.fold f acc

    let foldr (f: 'M -> 'T -> 'M) (acc: 'M) (set: HashSet<'T>) =
        set.table
        |> Array.rev
        |> Array.filter Option.isSome
        |> Array.map _.Value
        |> Array.fold f acc



    let equal (set1: HashSet<'T>) (set2: HashSet<'T>) =
        set1 |> map (set2 |> contains) |> foldl (&&) true
        && set2 |> map (set1 |> contains) |> foldl (&&) true

    let (@) (set1: HashSet<'T>) (set2: HashSet<'T>) =
        let table = Array.init (set1.capacity + set2.capacity) (fun _ -> None: 'T option)
        let rec tryInsert index item =
            match table[index] with
            | Some x when x = item -> () 
            | None -> table[index] <- Some item
            | _ -> tryInsert ((index + 1) % set1.capacity + set2.capacity) item
        set1 |> iter (fun x -> tryInsert (hashIndex x (set1.capacity + set2.capacity)) x) 
        set2 |> iter (fun x -> tryInsert (hashIndex x  (set1.capacity + set2.capacity)) x)
        let newSize = table |> Array.filter Option.isSome |> Array.length
        HashSet(table, newSize)

    let filter (f: 'T -> bool) (set: HashSet<'T>) =
        let table = Array.init set.capacity (fun _ -> None: 'T option)

        let rec tryInsert index item =
            match table[index] with
            | Some x when x = item -> ()
            | None -> table[index] <- Some item
            | _ -> tryInsert ((index + 1) % set.capacity) item

        set.table |> Array.filter Option.isSome |> Array.filter (fun x -> f x.Value)
        |> Array.iter (fun x -> tryInsert (hashIndex  x.Value set.capacity) x.Value)
        let newSize = table |> Array.filter Option.isSome |> Array.length
        HashSet(table, newSize)
