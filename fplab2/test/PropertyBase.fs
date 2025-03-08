module fplab2.test.PropertyPase

open FsCheck
open FsCheck.Xunit
open fplab2.HashSet


let genSet (genValue: Gen<'Value>) : Gen<HashSet<'Value>> =
    gen {
        let! arr = Gen.listOf (genValue)
        let set = arr |> List.fold (fun acc x -> acc |> add (x)) (default_set<'Value> ())
        return set
    }

let arbHashSet<'Value when 'Value: equality> () : Arbitrary<HashSet<'Value>> = Arb.fromGen (genSet Arb.generate)

type HashSetGenerators =
    static member HashSetArbitrary<'Value when 'Value: equality>() : Arbitrary<HashSet<'Value>> = arbHashSet ()

Arb.register<HashSetGenerators> () |> ignore

[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let neutralMonoid (set: HashSet<int>) =
    let mergeSet = (default_set<int> ()) @ (set)
    equal mergeSet set


[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let associationMonoid (set1: HashSet<int>) (set2: HashSet<int>) (set3: HashSet<int>) =
    let merge1 = (set1 @ set2) @ set3
    let merge2 = set1 @ (set2 @ set3)
    equal merge1 merge2

[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let foldlEqualFoldr (set: HashSet<int>) =
    (set |> foldl (+) 0) = (set |> foldr (+) 0)

[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let removeContainsTest (set: HashSet<int>) n =
    let set = set |> remove n
    not (contains n set)

[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let addContainsProp (set: HashSet<int>) n =
    let set = set |> add n
    contains n set


[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let mapIncrementProp (set: HashSet<int>) =
    let newSet = set |> map ((+) 1)
    (newSet |> foldr (+) 0) = (set |> foldr (+) 0) + set.size


[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let addAssociation (set: HashSet<int>) n m =
    let set1 = set |> add n |> add m
    let set2 = set |> add m |> add n
    equal set1 set2


[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let delContainsProp (set: HashSet<int>) n =
    let set = set |> remove n
    not (contains n set)