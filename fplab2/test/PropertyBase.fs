module PropertyTests

open FsCheck
open FsCheck.Xunit
open fplab2.HashSet


let genSet (genValue: Gen<'Value>) : Gen<HashSet<'Value>> =
    gen {
        let set = HashSet<'Value>.Default()
        let! arr = Gen.listOf(genValue)
        arr |> List.iter (fun x -> set.Add(x))
        return set
    }

let arbHashSet<'Value when 'Value : comparison> () : Arbitrary<HashSet<'Value>> =
    Arb.fromGen (genSet Arb.generate) 

type HashSetGenerators =
    static member HashSetArbitrary< 'Value when 'Value : comparison>() : Arbitrary<HashSet<'Value>> =
        arbHashSet()

Arb.register<HashSetGenerators>() |> ignore


let copy (hashSet: HashSet<'Value>) : HashSet< 'Value> =
    let emptySet = HashSet<'Value>.Default()
    hashSet |> HashSet.iter (fun x -> emptySet.Add(x))
    emptySet

let pow (n : int) (set: HashSet<int>)  =
    let rec helper (set: HashSet<int>) (n) (ans: HashSet<int>) =
        match n with
        | 0 -> ans
        | n -> helper set (n - 1) (ans @ set)

    helper set n (HashSet<int>.Default())

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