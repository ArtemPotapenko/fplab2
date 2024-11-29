module fplab2.test.PropertyBase

open FsCheck
open NUnit.Framework
open fplab2.HashSet
let createHashSet (elements : 'a list) = HashSet.create elements



let genElements = Gen.listOf (Gen.choose(0, 10))

[<Property>]
let prop_HashSet_Add (hSet : HashSet<'a>) (element : 'a) =
    hSet.Add(element)
    hSet.Contains(element)

[<Test>]
let prop_HashSet_Remove (hSet : HashSet<'a>) (element : 'a) =
    hSet.Add(element)
    hSet.Remove(element)
    not (hSet.Contains(element))

let prop_HashSet_Contains (hSet : HashSet<'a>) (element : 'a) =
   