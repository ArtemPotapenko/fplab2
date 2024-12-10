module PropertyTests

open FsCheck
open FsCheck.Xunit
open fplab2.HashSet


let genSet (genValue: Gen<'Value>) : Gen<HashSet<'Value>> =
    gen {
        let mutable set = HashSet<'Value>.Default()
        let! arr = Gen.listOf(genValue)
        arr |> List.iter (fun x -> set <- set |> add(x))
        return set
    }

let arbHashSet<'Value when 'Value : equality> () : Arbitrary<HashSet<'Value>> =
    Arb.fromGen (genSet Arb.generate) 

type HashSetGenerators =
    static member HashSetArbitrary< 'Value when 'Value : comparison>() : Arbitrary<HashSet<'Value>> =
        arbHashSet()

Arb.register<HashSetGenerators>() |> ignore



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