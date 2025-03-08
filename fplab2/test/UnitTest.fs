module fplab2.test.UnitTest

open Xunit
open fplab2.HashSet




[<Fact>]
let TestAdd () =
    let set = default_set<int> () |> add 5 |> add 3
    Assert.Equal(set.size, 2)
    Assert.True(set |> contains 3)

[<Fact>]
let TestRemove () =
    let set = default_set<int> ()
    let set = set |> add 3 |> add 3
    Assert.Equal(set.size, 1)
    Assert.True(set |> contains (3))
    
    let set = set |> remove 3
    Assert.Equal(set.size, 0)
    Assert.False(set |> contains (3))
    
    let set2 = set |> remove (4)
    Assert.Equal(set2.size, 0)

[<Fact>]
let TestConcatenate () =
    let set1 = default_set<int> () |> add 3
    let set2 = default_set<int> () |> add 5
    let empty_set = HashSet<int>(0)
    let set3 = set1 @ set2
    Assert.Equal(set3.size, 2)
    Assert.True(set3 |> contains 3)
    Assert.Equal(set3.size, (set3 @ empty_set).size)

[<Fact>]
let TestIter () =
    let set = default_set<int> () |> add 3 |> add 4 |> add 5 |> add 6 |> add 7
    let set_map = set |> map (fun x -> x + 1)
    Assert.Equal(set_map.size, 5)
    Assert.True(set_map |> contains 8)

    let set_filter = set |> filter (fun x -> x > 5)
    Assert.Equal(set_filter.size, 2)

    let set_sum = set |> foldl (+) 0
    Assert.Equal(set_sum, 25)

[<Fact>]
let TestFold () =
    let set = default_set<int> () |> add 3 |> add 4 |> add 5 |> add 6 |> add 7
    let set_sum = set |> foldl (+) 0
    Assert.Equal(set_sum, 25)
    
    let set = default_set<int> () |> add 5 |> add 4
    Assert.Equal(set |> foldr (*) 1, 20)

[<Fact>]
let TestEqual () =
    let set = default_set<int> () |> add 5 |> add 4
    let set2 = default_set<int> () |> add 5 |> add 5 |> add 3
    let set2 = set2 |> add 4 |> remove 3
    Assert.True(set |> equal set2)
    Assert.Equal(set2.size, 2)
