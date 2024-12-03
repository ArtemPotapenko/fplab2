## Лабораторная работа №2 по F#

Потапенко Артем P3334

## ВАРИАНТ - OA-SET

### Структура данных

```F#
type HashSet<'T when 'T: equality>
let mutable table = Array.init capacity (fun _ -> None: option<'T>)
        let mutable size = 0
        let loadFactor = 0.75
```

## Реализованные функции

### Hash

```F#
let hashIndex x len = (hash x % len + len) % len
```

### Resize

Изменение размера массива, если он заполнен

```F#
            let resize () =
            let newCapacity = table.Length * 2
            let newTable = Array.init newCapacity (fun _ -> None: 'T option)

            let rec tryInsert index item =
                match table.[index] with
                | Some x when x = item -> () // Already present, do nothing
                | None ->
                    newTable.[index] <- Some item
                    size <- size + 1
                | _ -> tryInsert ((index + 1) % size) item

            for i = 0 to table.Length - 1 do
                match table.[i] with
                | Some value -> tryInsert i value
                | None -> ()

            table <- newTable
```

### Add

Добавляет элемент в set

```F#
member this.Add(item: 'T) =
            if float size / float table.Length >= loadFactor then
                resize ()

            let rec tryInsert index =
                match table.[index] with
                | Some x when x = item -> () // Already present, do nothing
                | None ->
                    table.[index] <- Some item
                    size <- size + 1
                | _ -> tryInsert ((index + 1) % size)

            let index = hashIndex item table.Length
            tryInsert index
```

### Remove

Удаляет элемент

```F#
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
```

### Contains

Проверка существования элемента

```F#
member this.Contains(item: 'T) =
            let rec tryFind index =
                match table.[index] with
                | Some x when x = item -> true
                | None -> false
                | _ -> tryFind ((index + 1) % size)

            let index = hashIndex item table.Length
            tryFind index
```

### Iter

Применение функции к каждому элементы последовательно (ForEach)

```F#
member this.Iter(f: 'T -> unit) =
            table
            |> Array.iter (fun x ->
                match x with
                | None -> ()
                | _ -> f x.Value)
```

### map 
Изменение каждого элемента

```F#
static member map (f: 'T -> 'M) (set: HashSet<'T>) =
            let newSet = HashSet<'M>(set.Capacity)
            set.Iter(fun x -> newSet.Add(f x))
            newSet

```

### fold

```fsharp
static member foldl (f: 'M -> 'T -> 'M) (acc: 'M) (set: HashSet<'T>) =
            let mutable mut_acc = acc
            let nextAcc x = (mut_acc <- f mut_acc x)
            set.Iter(nextAcc)
            mut_acc
```
### Equals
Эквивалентность
```fsharp
member this.Equals(obj : HashSet<'T>) =
           let mutable check = true
           obj |> HashSet.iter (fun x -> if not(this.Contains(x)) then check <- false)
           this |> HashSet.iter (fun x -> if not(obj.Contains(x)) then check <- false)
           check
```

### Оператор @

"Сложение" двух set'ов

```F#
    let (@) (set1: HashSet<'T>) (set2: HashSet<'T>) =
        let set3 = HashSet<'T>(set1.Capacity + set2.Capacity)
        set1.Iter(fun (x) -> set3.Add(x))
        set2.Iter(fun (x) -> set3.Add(x))
        set3

```

## Тесты

### Проверти - тесты

Проверяют свойства моноида:
```fsharp
[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let ``нейтральный элемент`` (set: HashSet<int>) =
    let mergeSet = set @ HashSet.Default()
    mergeSet.Equals(set)
    
[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let ``ассоциативность`` (set1 : HashSet<int>) (set2 : HashSet<int>) (set3 : HashSet<int>) =
    let merge1 = (set1 @ set2) @ set3
    let merge2 = set1 @ (set2 @ set3)
    merge1.Equals(merge2)

[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let ``свойства степеней (сумма)`` (set : HashSet<int>) (n : int) (m : int) =
    (set |> pow  (n + m)) .Equals ((set |> pow  n) @ (set |> pow m))


[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let ``свойства степеней (степень степени`` (set : HashSet<int>) (n : int) (m : int) =
    (set |> pow (n * m)) .Equals (set |> pow n |> pow m)    
```

Unit тесты:

```fsharp
[<Test>]
let TestAdd () =
    let set = HashSet<int>.Default()
    set.Add(3)
    set.Add(3)
    set.Add(5)
    Assert.AreEqual(set.Size, 2)
    Assert.IsTrue(set.Contains(3))

[<Test>]
let TestRemove () =
    let set = HashSet<int>.Default()
    Assert.AreEqual(set.Size, 0)
    set.Add(3)
    set.Add(3)
    Assert.AreEqual(set.Size, 1)
    Assert.IsTrue(set.Contains(3))
    set.Remove(3)
    Assert.AreEqual(set.Size, 0)
    Assert.IsFalse(set.Contains(3))
    set.Remove(4)
    Assert.AreEqual(set.Size, 0)

[<Test>]
let TestConcatenate () =
    let set1 = HashSet<int>.Default()
    let set2 = HashSet<int>.Default()
    let empty_set = HashSet<int>(0)
    set1.Add(3)
    set2.Add(5)
    let set3 = set1 @ set2
    Assert.AreEqual(set3.Size, 2)
    Assert.IsTrue(set3.Contains(3))
    Assert.AreEqual(set3.Size, (set3 @ empty_set).Size)

[<Test>]
let TestIter () =
    let mutable set = HashSet<int>.Default()
    set.Add(3)
    set.Add(4)
    set.Add(5)
    set.Add(6)
    set.Add(7)

    let set_map = set |> HashSet.map (fun x -> x + 1)
    Assert.AreEqual(set_map.Size, 5)
    Assert.IsTrue(set_map.Contains(8))

    let set_filter = set |> HashSet.filter (fun x -> x > 5)
    Assert.AreEqual(set_filter.Size, 2)

    let set_sum = set |> HashSet.foldl (+) 0
    Assert.AreEqual(set_sum, 25)

```