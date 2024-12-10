## Лабораторная работа №2 (F#)

Потапенко Артем P3334

## OA-SET

### Структура данных

```F#
  type HashSet<'T when 'T: equality>(table: 'T option array, size: int) =
        member val table = table
        member val size = size
        member val loadFactor = 0.75
        member val capacity = table.Length

        new(capacity: int) =
            let table = Array.init capacity (fun _ -> None: 'T option)
            HashSet(table, 0)

```

## Реализованные функции

### Hash

```F#
let hashIndex x len = (hash x % len + len) % len
```

### Resize

Изменение размера массива, если он заполнен

```F#
let resize (set: HashSet<'T>) =
        let newCapacity = set.capacity * 2
        let table = Array.init newCapacity (fun _ -> None: 'T option)

        let rec tryInsert index item =
            match table[index] with
            | Some x when x = item -> () // Already present, do nothing
            | None -> table[index] <- Some item
            | _ -> tryInsert ((index + 1) % newCapacity) item

        set.table
        |> Array.map Option.get
        |> Array.iter (fun x -> tryInsert (hashIndex x set.capacity) x)

        HashSet(table, set.size)
```

### Add

Добавляет элемент в set

```F#
let add (x: 'T) (hash_set: HashSet<'T>) =
        let rec tryInsert index item (set: HashSet<'T>) : HashSet<'T> =
            match set.table[index] with
            | Some x when x = item -> set // Already present, do nothing
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
```

### Remove

Удаляет элемент

```F#
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
```

### Contains

Проверка существования элемента

```F#
let contains (item: 'T) (set: HashSet<'T>) =
        let rec tryFind index =
            match set.table.[index] with
            | Some x when x = item -> true
            | None -> false
            | _ -> tryFind ((index + 1) % set.capacity)

        let index = hashIndex item set.capacity
        tryFind index
```

### Iter

Применение функции к каждому элементы последовательно (ForEach)

```F#
let iter (f: 'T -> unit) (set: HashSet<'T>) =
        set.table
        |> Array.iter (fun x ->
            match x with
            | None -> ()
            | _ -> f x.Value)
```

### map 
Изменение каждого элемента

```F#
let map (f: 'T -> 'M) (set: HashSet<'T>) =
        let table = Array.init set.capacity (fun _ -> None: 'M option)

        let rec tryInsert index item =
            match table[index] with
            | Some x when x = item -> () // Already present, do nothing
            | None -> table[index] <- Some item
            | _ -> tryInsert ((index + 1) % set.capacity) item

        set.table |> Array.filter Option.isSome
        |> Array.iter (fun x -> tryInsert (hashIndex (f x.Value) set.capacity) (f x.Value))

        let newSize = table |> Array.filter Option.isSome |> Array.length

        HashSet(table, newSize)

```

### fold

```fsharp
let foldl (f: 'M -> 'T -> 'M) (acc: 'M) (set: HashSet<'T>) =
        set.table |> Array.filter Option.isSome |> Array.map _.Value |> Array.fold f acc

```
### Equals
Эквивалентность
```fsharp
let equal (set1: HashSet<'T>) (set2: HashSet<'T>) =
        set1 |> map (set2 |> contains) |> foldl (&&) true
        && set2 |> map (set1 |> contains) |> foldl (&&) true

```

### Оператор @

"Сложение" двух set'ов

```F#
let (@) (set1: HashSet<'T>) (set2: HashSet<'T>) =
        let mutable set3 = HashSet<'T>(set1.capacity + set2.capacity)
        set1 |> iter (fun (x) -> set3 <- set3 |> add x)
        set2 |> iter (fun (x) -> set3 <- set3 |> add x)
        set3
```

## Тесты

### Проверти - тесты

Проверяют свойства моноида:
```fsharp

let pow (n : int) (set: HashSet<int>)  =
    let rec helper (set: HashSet<int>) (n) (ans: HashSet<int>) =
        match n with
        | 0 -> ans
        | n -> helper set (n - 1) (ans @ set)

    helper set n (default_set<int>())

[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let neutral(set: HashSet<int>) =
    let mergeSet = set @ default_set<int>()
    equal mergeSet set
    
[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let association (set1 : HashSet<int>) (set2 : HashSet<int>) (set3 : HashSet<int>) =
    let merge1 = (set1 @ set2) @ set3
    let merge2 = set1 @ (set2 @ set3)
    equal merge1 merge2

[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let ``sum_degree`` (set : HashSet<int>) (n : int) (m : int) =
    (set |> pow  (n + m)) .Equals ((set |> pow  n) @ (set |> pow m))


[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let ``degree_of_degree`` (set : HashSet<int>) (n : int) (m : int) =
    (set |> pow (n * m)) .Equals (set |> pow n |> pow m)       
```

Unit тесты:

```fsharp

[<SetUp>]
let Setup () = ()

[<Test>]
let TestAdd () =
    let set = default_set<int> () |> add 5 |> add 3
    Assert.AreEqual(set.size, 2)
    Assert.IsTrue(set |> contains 3)

[<Test>]
let TestRemove () =
    let set = default_set<int>()
    Assert.AreEqual(set.size, 0)
    let set = set |> add 3 |> add 3
    Assert.AreEqual(set.size, 1)
    Assert.IsTrue(set |> contains(3))
    let set = set |> remove 3 
    Assert.AreEqual(set.size, 0)
    Assert.IsFalse(set |> contains(3))
    let set2 = set|> remove(4)
    Assert.AreEqual(set2.size, 0)

[<Test>]
let TestConcatenate () =
    let set1 = default_set<int>() |> add 3
    let set2 = default_set<int>() |> add 5
    let empty_set = HashSet<int>(0)

    let set3 = set1 @ set2
    Assert.AreEqual(set3.size, 2)
    Assert.IsTrue(set3 |> contains 3)
    Assert.AreEqual(set3.size, (set3 @ empty_set).size)

[<Test>]
let TestIter () =
    let mutable set = default_set<int>() |> add 3 |> add 4 |> add 5 |> add 6 |> add 7
    let set_map = set |> map (fun x -> x + 1)
    Assert.AreEqual(set_map.size, 5)
    Assert.IsTrue(set_map |> contains 8)

    let set_filter = set |> filter (fun x -> x > 5)
    Assert.AreEqual(set_filter.size, 2)

    let set_sum = set |> foldl (+) 0
    Assert.AreEqual(set_sum, 25)
```
