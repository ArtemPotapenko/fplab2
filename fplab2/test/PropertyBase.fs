module PropertyTests

open FsCheck
open FsCheck.Xunit
open fplab2.HashSet


let genSet (genValue: Gen<'Value>) : Gen<HashSet<'Value>> =
    gen {
        let! arr = Gen.listOf(genValue)
        let set = arr |> List.fold (fun acc x -> acc |> add(x)) (default_set<'Value>())
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
    let mergeSet = (default_set<int>()) @ set
    equal mergeSet set
    
[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let association (set1 : HashSet<int>) (set2 : HashSet<int>) (set3 : HashSet<int>) =
    let merge1 = (set1 @ set2) @ set3
    let merge2 = set1 @ (set2 @ set3)
    equal merge1 

[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let ``sum_degree`` (set : HashSet<int>) (n : int) (m : int) =
    (set |> pow  (n + m)) .Equals ((set |> pow  n) @ (set |> pow m))


[<Property(Arbitrary = [| typeof<HashSetGenerators> |])>]
let ``degree_of_degree`` (set : HashSet<int>) (n : int) (m : int) =
    (set |> pow (n * m)) .Equals (set |> pow n |> pow m)    