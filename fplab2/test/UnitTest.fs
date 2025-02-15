module fplab2.test.UnitTest

open NUnit.Framework
open fplab2.HashSet


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
    let set = default_set<int>() |> add 3 |> add 4 |> add 5 |> add 6 |> add 7
    let set_map = set |> map (fun x -> x + 1)
    Assert.AreEqual(set_map.size, 5)
    Assert.IsTrue(set_map |> contains 8)

    let set_filter = set |> filter (fun x -> x > 5)
    Assert.AreEqual(set_filter.size, 2)

    let set_sum = set |> foldl (+) 0
    Assert.AreEqual(set_sum, 25)

let TestFold () =
    let set = default_set<int>() |> add 3 |> add 4 |> add 5 |> add 6 |> add 7
    let set_sum = set |> foldl (+) 0
    Assert.AreEqual(set_sum, 25)
    let set  = default_set<int>() |> add 5 |> add 4
    Assert.AreEqual(set |> foldr (*) 1, 20)

let TestEqual () =
    let set = default_set<int>() |> add 5 |> add 4
    let set2 = default_set<int>() |> add 5 |> add 5 |> add 3
    let set2 = set2 |> add 4 |> remove 3
    Assert.True(set |> equal set2)
    Assert.AreEqual(set2.size, 2)