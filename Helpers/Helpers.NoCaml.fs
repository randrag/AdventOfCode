namespace Helpers

  /// Override OCaml signatures in .NET to avoid horrors such as <Async<Result<int,string> option> list>
  /// Another horror : Option<seq<obj> list>
[<AutoOpen>]
module NoCaml =
  open FsToolkit.ErrorHandling


  module Array =
    let toList = Array.toList : array<'a> -> List<'a>

  module List =
    let isNotEmpty l = not (List.isEmpty l)

    /// Returns a new collection containing only the elements of the collection for which the given predicate returns "true"
    let filter = List.filter : ('a -> bool) -> List<'a> -> List<'a>

    let isSingleton l = List.length l = 1

    /// Will throw on empty list
    let withoutLastX l = List.take ((List.length l) - 1) l

    /// val groupBy : projection:('T -> 'Key) -> list:'T list -> ('Key * 'T list) list (requires equality and equality)  'T is 'a 'Key is 'b  Applies a key-generating function to each element of a list and yields a list of unique keys. Each unique key contains a list of all elements that match to this key.
    /// Overwritten for signature: FROM : Full name: Microsoft.FSharp.Collections.List.groupBy
    let groupBy = List.groupBy : ('a -> 'b) -> List<'a> -> List<'b * List<'a>>

    /// val sortBy : projection:('T -> 'Key) -> list:'T list -> 'T list (requires comparison and comparison)
    /// Sorts the given list using keys given by the given projection. Keys are compared using Operators.compare.  Full name: Microsoft.FSharp.Collections.List.sortBy
    let sortBy = List.sortBy : ('T -> 'Key) -> List<'T> -> List<'T>

    /// val sortByDescending : projection:('T -> 'Key) -> list:'T list -> 'T list (requires comparison and comparison)
    /// Sorts the given list in descending order using keys given by the given projection. Keys are compared using Microsoft.FSharp.Core.Operators.compare.
    let sortByDescending = List.sortByDescending : ('T -> 'Key) -> List<'T> -> List<'T>

    ///val truncate : count:int -> list:'T list -> 'T list  'T is 'a Returns at most N elements in a new list. Params:count – The maximum number of items to return.list – The input list.Returns:The result list.Full name: Microsoft.FSharp.Collections.List.truncate
    let truncate = List.truncate : int -> List<'T> -> List<'T>

    /// For each element of the list, applies the given function. Concatenates all the results and return the combined list.
    let collect = List.collect : ('a -> List<'b>) -> List<'a> -> List<'b>

    /// Returns a list with head as its first element and tail as its subsequent elements
    let Cons = List.Cons : 'a * List<'a> -> List<'a>

    /// Returns a new list with the elements in reverse order
    let rev = List.rev : List<'a> -> List<'a>

    /// Returns a set of the distinct elements in the list. Duplicates are silently eliminated.
    let toSet = Set.ofList :  List<'a> -> Set<'a>

    /// Applies the given function to each element of the list. Returns the list comprised of the results x for each element where the function returns Some(x)
    let choose = List.choose : ('a -> Option<'b>) -> List<'a> -> List<'b>

    /// Returns a new list that contains the elements of each the lists in order.
    let concat = List.concat : seq<List<'a>> -> List<'a>

    /// Creates a list by calling the given generator on each index.
    let init = List.init : int -> (int -> 'a) -> List<'a>

    let repeatAndCollect n (aL : List<'a>) = [1..n] |> List.collect (fun _ -> aL) : List<'a>

    let requireNotEmpty (error : 'e) (list : List<'a>) : Result<List<'a>,'e> =
      match list with
      | [] -> Error error
      | l -> Ok l

    // Concatenate lists
    let (@) = (@) : List<'a> -> List<'a> -> List<'a>

    let sequenceResultM = List.sequenceResultM : List<Result<'a,'b>> -> Result<List<'a>,'b>

    let sort a = List.sort a : List<_>

    let tryHead (aL : List<'a>) = List.tryHead aL : Option<'a>

    let unzip = List.unzip : ('a * 'b) list -> List<'a> * List<'b>
    let zip   = List.zip : List<'a> -> List<'b> -> List<'a * 'b>
    let zip3  = List.zip3 : List<'a> -> List<'b> -> List<'c> -> List<'a * 'b * 'c>
    let zip4 l1 l2 l3 l4 =
      List.zip (List.zip l1 l2) (List.zip l3 l4)
      |> List.map (fun ((a, b),(c, d)) -> (a, b, c, d)) : List<_>

    /// Builds a new collection whose elements are the results of applying the
    /// given function to each of the elements of the collection.
    let map = List.map : ('T -> 'U) -> List<'T> -> List<'U>
    let mapi = List.mapi : (int -> 'a -> 'b) -> List<'a> -> List<'b>

    let chunkBySize = List.chunkBySize : int -> List<'a> -> List<List<'a>>

  module Map =
    let tryFind = Map.tryFind : 'a -> Map<'a,'b> -> Option<'b>

    let toList = Map.toList : Map<'a,'b> -> List<'a * 'b>


  module Option =
    let map  = Option.map : ('a -> 'b) -> Option<'a> -> Option<'b>
    let map2 = Option.map2 : ('a -> 'b -> 'c) -> Option<'a> -> Option<'b> -> Option<'c>
    let map3 = Option.map3 : ('a -> 'b -> 'c -> 'd) -> Option<'a> -> Option<'b> -> Option<'c> -> Option<'d>
    let flatten = Option.flatten : Option<Option<'T>> -> Option<'T>

  let Some = Option.Some : 'a -> Option<'a>
  let None = Option.None : Option<'a>

  module Seq =
    /// Builds a list from the given collection.
    let toList = Seq.toList : seq<'a> -> List<'a>

    // Returns the first element of the sequence, or None if the sequence is empty.
    let tryHead = Seq.tryHead : seq<'a> -> Option<'a>

  module Set =
    let toList = Set.toList : Set<'a> -> List<'a>
