module fplab2.test.UnitTest

open NUnit.Framework
open fplab2.HashSet


[<SetUp>]
let Setup () = ()

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
