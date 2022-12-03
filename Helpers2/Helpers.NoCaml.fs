namespace Helpers

/// Override OCaml signatures in .NET to avoid horrors such as <Async<Result<int,string> option> list>,
/// which then ends up reading <List<Async<Option<Result<int, string>>>>
/// Another example : Option<seq<obj> list>  which should be Option<List<seq<obj>>>
/// Todo:
///    Do this properly and completely also importing all the doc strings
///    The fsharp core is very stable, so even if we do this by hand it should not be too much work to keep up.
module NoCaml =
   open FsToolkit.ErrorHandling

   type Array<'a> = 'a[] // another funny notation

   module List =

      /// <summary>Returns a new collection containing only the elements of the collection
      /// for which the given predicate returns "true"</summary>
      ///
      /// <param name="predicate">The function to test the input elements.</param>
      /// <param name="list">The input list.</param>
      ///
      /// <returns>A list containing only the elements that satisfy the predicate.</returns>
      ///
      /// <example id="filter-1">
      /// <code lang="fsharp">
      /// let input = [1, "Luke"; 2, "Kirk"; 3, "Kenobi"; 4, "Spock"]
      ///
      /// let isComingFromStarTrek (x,_) = isEven x
      ///
      /// input |> List.filter isComingFromStarTrek
      /// </code>
      /// Evaluates to <c>[(2, "Kirk"); (4, "Spock")]</c>
      /// </example>
      let inline filter predicate list =
         List.filter (predicate : 'a -> bool) (list : List<'a>) : List<'a>

      let inline ofArray (list : array<'a>) = List.ofArray list : List<'a>


      /// val groupBy : projection:('T -> 'Key) -> list:'T list -> ('Key * 'T list) list (requires equality and equality)  'T is 'a 'Key is 'b  Applies a key-generating function to each element of a list and yields a list of unique keys. Each unique key contains a list of all elements that match to this key.
      /// Overwritten for signature: FROM : Full name: Microsoft.FSharp.Collections.List.groupBy
      let inline groupBy f l = List.groupBy (f : 'a -> 'b) (l : List<'a>) : List<'b * List<'a>>

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

      /// Applies the given function to each element of the list. Returns the list comprised of the results x for each element where the function returns Some(x)
      let choose = List.choose : ('a -> Option<'b>) -> List<'a> -> List<'b>

      /// Returns a new list that contains the elements of each the lists in order.
      let concat = List.concat : seq<List<'a>> -> List<'a>

      /// Returns a list that contains no duplicate entries according to generic hash and equality comparisons on the entries. If an element occurs multiple times in the list then the later occurrences are discarded.
      let distinct = List.distinct : List<'a> -> List<'a>

      /// Creates a list by calling the given generator on each index.
      let init = List.init : int -> (int -> 'a) -> List<'a>

      // Concatenate lists
      let (@) = (@) : List<'a> -> List<'a> -> List<'a>

      /// Builds a new collection whose elements are the results of applying the
      /// given function to each of the elements of the collection.
      let map = List.map : ('a -> 'b) -> List<'a> -> List<'b>
      let mapi = List.mapi : (int -> 'a -> 'b) -> List<'a> -> List<'b>

      let chunkBySize = List.chunkBySize : int -> List<'a> -> List<List<'a>>

      let tryFind = List.tryFind : ('a -> bool) -> List<'a> -> Option<'a>

      /// <summary>Splits a list into two lists, at the given index.</summary>
      ///
      /// <param name="index">The index at which the list is split.</param>
      /// <param name="list">The input list.</param>
      ///
      /// <returns>The two split lists.</returns>
      ///
      /// <exception cref="T:System.InvalidOperationException">Thrown when split index exceeds the number of elements
      /// in the list.</exception>
      ///
      /// <example id="splitat-1">
      /// <code lang="fsharp">
      /// let input = [8; 4; 3; 1; 6; 1]
      ///
      /// let front, back = input |> List.splitAt 3
      /// </code>
      /// Evaluates <c>front</c> to <c>[8; 4; 3]</c> and <c>back</c> to <c>[1; 6; 1]</c>.
      /// </example>
      let splitAt index (list : List<'a>) = List.splitAt index list : List<'a> * List<'a>

      /// If all ok, gives Ok List<'a>, else gives Error List<'e>
      let sequenceResultA = List.sequenceResultA : List<Result<'a,'e>> -> Result<List<'a>, List<'e>>

      let toArray l = List.toArray (l : List<'a>) : Array<'a>

   module Option =
      let map  = Option.map : ('a -> 'b) -> Option<'a> -> Option<'b>
      let map2 = Option.map2 : ('a -> 'b -> 'c) -> Option<'a> -> Option<'b> -> Option<'c>
      let map3 = Option.map3 : ('a -> 'b -> 'c -> 'd) -> Option<'a> -> Option<'b> -> Option<'c> -> Option<'d>
      let flatten = Option.flatten : Option<Option<'T>> -> Option<'T>
      let Some = Option.Some : 'a -> Option<'a>
      let None = Option.None : Option<'a>
      let bind = Option.bind : ('a -> Option<'b>) -> Option<'a> -> Option<'b>
      let sequenceResult = Option.sequenceResult : Option<Result<'a,'b>> -> Result<Option<'a>,'b>

   // repeat these at root level
   let Some = Option.Some : 'a -> Option<'a>
   let None = Option.None : Option<'a>

   module Map =
      let tryFind = Map.tryFind : 'a -> Map<'a,'b> -> Option<'b>

   module Seq =
      /// Builds a list from the given collection.
      let toList = Seq.toList : seq<'a> -> List<'a>

      // Returns the first element of the sequence, or None if the sequence is empty.
      let tryHead = Seq.tryHead : seq<'a> -> Option<'a>


   module AsyncOption =
      let map' = AsyncOption.map : ('a -> 'b) -> Async<Option<'a>> -> Async<Option<'b>>
      let map = AsyncOption.map : ('a -> 'b) -> Async<Option<'a>> -> Async<Option<'b>>
