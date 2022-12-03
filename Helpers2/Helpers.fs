namespace Helpers


[<AutoOpen>]
module Helpers =

   let print a = printf $"%A{a}"
   let printn a = printfn $"%A{a}"
   let p = print
   let pn = printn
   let po a = printfn $"%A{a}"; a
   let ps s a = printfn $"%s{s}%A{a}"
   let pso s a = printfn $"%s{s}%A{a}"; a
   let plo s a = printf $"%s{s}%A{a}"; a
   let flip f a b = f b a

   let r = System.Console.ReadKey () |> ignore
   let ro x = System.Console.ReadKey () |> ignore; x

   type UndefinedType = exn // Place holder when modelling types
   let Unreachable () = failwith "Unexpected execution of code regarded as unreachable"
   let NotImplemented () = failwith "Behaviour not implemented" // Place holder when modelling behaviour


  // functional wrappers around some .net types
  module Int32 =
    /// Will throw exception on invalid input
    let toStringX (i : System.Int32) = i.ToString()

    /// Invalid input yields None
    let fromStringO (s : string) =
      match System.Int32.TryParse s with
      | true, r -> Some r
      | _ -> None

  module Guid =
    let toString (guid : System.Guid) = guid.ToString()
    let toUpperString (guid: System.Guid) = guid.ToString().ToUpper()
    let toStringWithoutDashes (g : System.Guid) = g.ToString().Replace("-", System.String.Empty).ToLower()
    let generateNewA () = FsToolkit.ErrorHandling.Async.singleton <| System.Guid.NewGuid() // https://blog.ploeh.dk/2016/04/11/async-as-surrogate-io/
    let fromStringR (s : string) =
      match System.Guid.TryParse (s.ToLower())  with
      | true, s -> Ok s
      | false, _ -> Error "Cannot parse Guid from provided string"
    let fromStringX (s : string) = System.Guid.Parse (s.ToLower())
    let toFingerPrintString = toString >> (fun x -> x.Substring(0,8))


  module Curried2 =
    let inline fst a _ = a
    let inline snd _ b = b

  module NoneTuples =
    let None2 = (None, None)
    let None3 = (None, None, None)
    let None4 = (None, None, None, None)
    let None5 = (None, None, None, None, None)
    let None6 = (None, None, None, None, None, None)
    let None7 = (None, None, None, None, None, None, None)
    let None8 = (None, None, None, None, None, None, None, None)

open FsToolkit.ErrorHandling
open FsToolkit.ErrorHandling.Operator.Result

[<RequireQualifiedAccess>]
module Option =

  /// Gets the value or throws an exception with the provided message
  let getOrFailWith message (aO : Option<'a>) =
    match aO with
    | Some value -> value
    | None -> failwith message

  let apply (abFO : Option<'a -> 'b>) (aO : Option<'a>) : Option<'b> =
    match abFO, aO with
    | Some abF, Some r -> Some (abF r)
    | _ , _ -> None

  /// Convert Option<'a> to System.Nullable<'a>
  /// This does not work for Option<string>, where you have to use stringO |> Option.defaultValue null
  let toNullable (aO : Option<'a>) =
      match aO with
      | None   -> System.Nullable<_>()
      | Some a -> System.Nullable<_>(a)

  /// Returns a singleton set if Some, empty set if None
  let toSet : Option<'a> -> Set<'a> = function
    | Some a -> Set.singleton a
    | None -> Set.empty

  module Operators =
    let (<!>) = Option.map : ('a -> 'b) -> Option<'a> -> Option<'b>
    let (<*>) = apply

  let map4 f o1 o2 o3 o4 =
    let (<!>) = Option.map : ('a -> 'b) -> Option<'a> -> Option<'b>
    let (<*>) = apply
    f <!> o1 <*> o2 <*> o3 <*> o4

  let sequenceAsync (vOA : Option<Async<'a>>) : Async<Option<'a>> =
    match vOA with
    | Some vA -> Async.map Some vA
    | None    -> Async.singleton None

type AsyncOption<'value> = Async<Option<'value>>

[<RequireQualifiedAccess>]
module AsyncOption =
  let sequenceResult aor = Async.map Option.sequenceResult aor : Async<Result<Option<'a>,'b>>
  let map' = AsyncOption.map : ('a -> 'b) -> Async<Option<'a>> -> Async<Option<'b>>
  let map = AsyncOption.map : ('a -> 'b) -> Async<Option<'a>> -> Async<Option<'b>>

  let getOrFailWith message aor = Async.map (Option.getOrFailWith message) aor

//==============================================
// The `Validation` type is the same as the `Result` type but with a *list* for failures
// rather than a single value. This allows `Validation` types to be combined
// by combining their errors ("applicative-style")
//==============================================

type Validation<'Success,'Failure> =
    Result<'Success,List<'Failure>>

/// Functions for the `Validation` type (mostly applicative)
[<RequireQualifiedAccess>]
module Validation =

    /// Alias for Result.Map
    let map = Result.map

    let mapErrors (fn : 'b -> 'c) (v : Validation<'a, 'b>) : Validation<'a, 'c> =
      match v with
      | Ok a -> Ok a
      | Error list -> list |> List.map fn |> Error

    /// Apply a Validation<fn> to a Validation<x> applicatively
    let apply (fV : Validation<_,_>) (xV : Validation<_,_>) : Validation<_,_> =
        match fV, xV with
        | Ok f, Ok x -> Ok (f x)
        | Error errs1, Ok _ -> Error errs1
        | Ok _, Error errs2 -> Error errs2
        | Error errs1, Error errs2 -> Error (errs1 @ errs2)

    // combine a list of Validation, applicatively
    let sequence (aListOfValidations : List<Validation<_,_>>) : Validation<List<'a>,'b> =
        let (<*>) = apply
        let (<!>) = Result.map
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok [] // empty list inside Result

        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR aListOfValidations initialValue


    //-----------------------------------
    // Converting between Validations and other types

    let ofResult xR : Validation<_,_> =
        xR |> Result.mapError List.singleton

    let toResult (xV : Validation<_,_>) : Result<_,_> =
        xV

    let isOk = Result.isOk


type AsyncResult<'value, 'error> = Async<Result<'value, 'error>>
type AsyncResultOption<'value, 'error> = Async<Result<Option<'value>,'error>>
type AsyncOptionResult<'value, 'error> = Async<Option<Result<'value,'error>>>

module Seq =
  let sequenceAsync (aLA : seq<Async<'a>> ) : Async<seq<'a>> =
    aLA
    |> Async.Sequential
    |> Async.map Seq.ofArray

  let groupByAndMap (keyF : 'a -> 'key) (mapF : 'a -> 'c) (aS : seq<'a>)
    : seq<'key * seq<'c>> =
      Seq.groupBy keyF aS
      |> Seq.map (fun (key, aL) -> key, aL |> Seq.map mapF)

  let toMap = Map.ofSeq
  let toMapOfMap (ss : seq<'a * ('b * 'c)>) =
    ss
    |> groupByAndMap fst snd
    |> Seq.map (fun (a, bcS) -> a, toMap bcS)
    |> toMap

  let toSet s = s |> Set.ofSeq

  /// Converts a List<key * list> to a map by concatenating the lists under the keys
  let toMapWithListConcat (l : seq<'key * seq<'b>>) : Map<'key, seq<'b>> =
      let mapmap' f = Map.map (fun _ -> f) // because circular reference between modules not allowed

      l
      |> groupByAndMap fst snd
      |> toMap
      |> mapmap' Seq.concat

  let sequenceMap (s : seq<Map<'key, 'value>>) : Map<'key, seq<'value>> =

    let seqFolder (state : Map<'key,seq<'value>>) (t : Map<'key, 'value>) : Map<'key,seq<'value>> =
      let mapFolder state key value =
          let currentSequenceForKey = Map.tryFind key state |> Option.defaultValue Seq.empty
          let newSequenceForKey = Seq.append currentSequenceForKey (Seq.singleton value)
          Map.add key newSequenceForKey state // replace or add new sequence under the key

      t |> Map.fold mapFolder state

    s |> Seq.fold seqFolder Map.empty

  let sequenceToAsyncOfUnit (aSAu : seq<Async<unit>> ) : Async<unit> =
    aSAu |> sequenceAsync |> Async.Ignore

module List =
  let innerJoin (l1 : List<'a>) (l2 : List<'b>) (compareFn : 'a -> 'b -> bool) =
    l1
    |> List.collect (fun e1 ->
         l2
         |> List.filter (compareFn e1)
         |> List.map (fun e2 -> e1, e2)
      )

  /// Inserts new element after first item that satisfies predicate
  /// Returns None if the predicate is not satisfied by any element in the list
  /// Not the fastest possible implementation
  let tryInsertAfter (predF : 'a -> bool) (aToInsert : 'a) (aL : List<'a>) : Option<List<'a>> = option {
    let! insertAtIndex = aL |> List.tryFindIndex predF |> Option.map ((+) 1)
    let beforeL, afterL = aL |> List.splitAt insertAtIndex
    return List.concat [beforeL; [aToInsert]; afterL]
  }


  let groupByAndMap (keyF : 'a -> 'key) (mapF : 'a -> 'c) (aL : List<'a>)
    : List<'key * List<'c>> =
      List.groupBy keyF aL
      |> List.map (fun (key, aL) -> key, aL |> List.map mapF)

  let requireNotEmpty (e : 'e) (aL : List<'a>) : Result<List<'a>, 'e> =
    if List.isEmpty aL then Error e else Ok aL

  let sequenceOption (aLO : List<Option<'a>>) =
    if aLO |> List.contains None then None else Some (aLO |> List.map Option.get)

  let sequenceAsync (aLA : List<Async<'a>> ) : Async<List<'a>> =
    aLA
    |> Async.Sequential
    |> Async.map List.ofArray

  let sequenceToAsyncOfUnit (aLAu : List<Async<unit>> ) : Async<unit> =
    aLAu
    |> sequenceAsync
    |> Async.Ignore

  let singleton = List.singleton : 'a -> List<'a>

  /// CAREFUL this function will discard / overwrite duplicate key values
  let toMap = Map.ofList : List<'a * 'b> -> Map<'a,'b>

  /// As standard but throws if duplicate key is encountered
  let toMapX (l : List<'a * 'b>) =
    if (l |> List.map fst |> List.distinct |> List.length) = (l |> List.map fst |> List.length)
    then Map.ofList l
    else failwith "Can't convert list with duplicate keys to map"

  /// Converts a list of key * value without discarding duplicate key values by combining values in a list
  let toMapWithListValues (l : List<'key * 'value>) =

    let folder (acc : Map<'key, List<'value>>) ((key, value) : 'key * 'value) : Map<'key, List<'value>> =
      acc
      |> Map.change key (function
          | Some existingList -> Some (existingList @ [value])
          | None -> Some [value] )

    l |> List.fold folder Map.empty

  /// Converts a List<key * list> to a map by concatenating the lists under the keys
  let toMapWithListConcat (l : List<'key * List<'b>>) : Map<'key,List<'b>> =
      let mapMap' f = Map.map (fun _ -> f) // because circular reference between modules not allowed

      l
      |> groupByAndMap fst snd
      |> toMap
      |> mapMap' List.concat

  let mapSnd (f : 'b -> 'c) (tupleList : List<'a * 'b>) : List<'a * 'c> =
    List.map (fun (a,b) -> a, f b) tupleList

  let mapFst (f : 'a -> 'b) (tupleList : List<'a * 'c>) : List<'b * 'c> =
    List.map (fun (a,c) -> ((f a), c) ) tupleList

  /// FoldBack with argument orders 'corrected' so that they match fold
  /// Applies a function to each element of the collection, starting from the end, threading an accumulator argument through the computation. If the input function is f and the elements are i0...iN then computes f i0 (...(f iN s)).
  let foldBack' (folder : 'State -> 'T -> 'State) (state : 'State) (list : List<'T>) =
    List.foldBack (flip folder) list state

  /// ScanBack with argument orders corrected
  /// Like foldBack, but returns both the intermediary and final results
  ///
  /// Applies a function to each element of the collection, starting from the end, threading an accumulator argument through the computation. If the input function is f and the elements are i0...iN then computes f i0 (...(f iN s)).
  let scanBack' (folder : 'State -> 'T -> 'State) (state : 'State) (list : List<'T>) =
    List.scanBack (flip folder) list state : List<_>

  let concat2 l1 l2 = List.concat [l1; l2]

  /// Returns a list of lists with the element e inserted in every possible position in list l
  /// [1; 2; 3] |> distribute 4 = [[4; 1; 2; 3]; [1; 4; 2; 3]; [1; 2; 4; 3]; [1; 2; 3; 4]]
  let rec distributeElement e l : List<List<'a>> =
    match l with
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distributeElement e xs' -> x::xs]

  /// Returns a list of all possible permutations of the input list
  /// List will contain n! lists, where n is the number of elements in the input list
  let rec allPermutations l : List<List<'a>> =
    match l with
    | [] -> [[]]
    | e::xs -> List.collect (distributeElement e) (allPermutations xs)

  /// Returns a set of the distinct elements in the list. Duplicates are silently eliminated.
  let toSet = Set.ofList :  List<'a> -> Set<'a>


  let unzip4 (l: List<('a * 'b * 'c * 'd)>) : List<'a> * List<'b> * List<'c> * List<'d> =
    let la = l |> List.map (fun (a,_,_,_) -> a)
    let lb = l |> List.map (fun (_,b,_,_) -> b)
    let lc = l |> List.map (fun (_,_,c,_) -> c)
    let ld = l |> List.map (fun (_,_,_,d) -> d)
    (la,lb,lc,ld)


  /// gives two lists back, excluding the splitting element
  let splitOnceOnExcl (f : 'a -> bool) l =
    let rec inner acc remaining =
      match remaining with
      | []                     -> (List.rev acc, [])
      | head::tail when f head -> (List.rev acc, tail)
      | head::tail             -> inner (head::acc) tail

    inner [] l

  /// Splits a list into multiple lists at each position where an element matches the predicate
  /// The element which matches the predicate is discarded
  let splitMultipleOnExcl (f : 'a -> bool) l : List<List<'a>> =
    let rec inner acc remaining =
      match remaining with
      | [] -> acc |> List.filter (List.isEmpty >> not) |> List.rev
      | something ->
          let (element, remaining) = splitOnceOnExcl f something
          inner (element::acc) remaining

    inner [[]] l



  /// Splits a list into two at the element where the predicate is first true.
  /// Element which causes true predicate is placed in second list
  /// Does not apply the predicate to the first element in the list - will not split to empty first list
  /// e.g. [1; 2; 3; 1; 2; 3; 1; 2] split on 1 becomes [1; 2; 3], [1; 2; 3; 1; 2]
  let splitOnceOnInclVx (isRear : 'a -> bool) l : List<'a> * List<'a> =
    let rec inner front' possiblyRear = // front' because it is reversed
      match possiblyRear with
      | []    ->                                     [],  List.rev front'
      | [x]   -> if isRear x then    front' |> List.rev, [x]
                             else x::front' |> List.rev, []
      | x::xs -> if isRear x then    front' |> List.rev, possiblyRear
                             else inner (x::front') xs
    inner [] l

  /// Splits a list into two at the element where the predicate is first true.
  /// Element which causes true predicate is placed in second list
  /// Does not apply the predicate to the first element in the list - will not split to empty first list
  /// e.g. [1; 2; 3; 1; 2; 3; 1; 2] split on 1 becomes [1; 2; 3], [1; 2; 3; 1; 2]
  let splitOnceOnIncl (isRear : 'a -> bool) l : List<'a> * List<'a> =
    let rec inner front' possiblyRear = // front' because it is reversed
      match possiblyRear with
      | []    ->                                     [],  List.rev front'
      | [x]   -> if isRear x then    front' |> List.rev, [x]
                             else x::front' |> List.rev, []
      | x::xs -> if isRear x then    front' |> List.rev, possiblyRear
                             else inner (x::front') xs

    match l with
    | []  -> [] , []
    | [x] -> [x], []
    | x::xs -> inner [] xs |> fun (front, rear) -> x::front, rear


  let private testSplitOnceOnExcl () =
      []           |> splitOnceOnIncl ((=) 1) = ([] ,          []          ) |> ps "True?: "
      [1]          |> splitOnceOnIncl ((=) 2) = ([1],          []          ) |> ps "True?: "
      [2]          |> splitOnceOnIncl ((=) 2) = ([2],          []          ) |> ps "True?: "
      [1; 2]       |> splitOnceOnIncl ((=) 2) = ([1],          [2]         ) |> ps "True?: "
      [1; 2; 2; 3] |> splitOnceOnIncl ((=) 2) = ([1],          [2; 2; 3]   ) |> ps "True?: "
      [1; 2; 2; 3] |> splitOnceOnIncl ((=) 4) = ([1; 2; 2; 3], []          ) |> ps "True?: "
      [1; 2; 2; 3] |> splitOnceOnIncl ((=) 1) = ([1; 2; 2; 3], []          ) |> ps "True?: "
      [1; 2; 1; 2] |> splitOnceOnIncl ((=) 1) = ([1; 2],       [1; 2]      ) |> ps "True?: "

  /// Splits a list into multiple lists at each position where an element matches the predicate
  /// The element which matches the predicate is included as the head of the list after each split
  let splitMultipleOnIncl (f : 'a -> bool) l : List<List<'a>> =
    let rec inner acc remaining =
      match remaining with
      | [] -> acc |> List.filter (List.isEmpty >> not) |> List.rev
      | something ->
          let (front, remaining) = splitOnceOnIncl f something
          inner (front::acc) remaining

    inner [[]] l


module NonEmptyList =
  type NonEmptyList<'a> = 'a * List<'a>

  let map (mapF : 'a -> 'b) ((first, restL) : NonEmptyList<'a>)  =
    let first' = mapF first
    let restL' = List.map mapF restL
    first', restL'

module Map =

  let toListOfValuesSortedBy (fO : Option<'key -> 'value -> 'sorter>) m =
    m
    |> Map.toList
    |> fun l ->
        match fO with
        | Some f -> l |> List.sortBy (fun (key, value) -> f key value)
        | None -> l
    |> List.map snd

  let toListSortedBy (fO : Option<'key -> 'value -> 'sorter> ) m : List<'key * 'value>
    = m
      |> Map.toList
      |> fun l ->
          match fO with
          | Some f -> l |> List.sortBy (fun (key, value) -> f key value)
          | None -> l

  let singleton (key, value) =
    [ (key, value) ] |> Map.ofList

  /// Same as Map.map, but without requiring you to accept the key in the functor
  let map' f = Map.map (fun _ -> f)

  let choose' (chooser : 'value -> Option<'mappedValue> ) (m : Map<'key, 'value>) =
    Map.toList m
    |> List.choose (fun (key, value) -> chooser value |> Option.map (fun value2 -> key, value2))
    |> Map.ofList

  /// Same as Map.filter, but without requiring you to accept the key in the predicate
  let filter' (f: 'value -> bool) m : Map<'key, 'value> =
    Map.filter (fun _ -> f) m

  /// Checks if all values in the Map is Result.isOK else returns false if any errors are found
  let allOk (m : Map<'key, Result<'a, 'b>>) = m |> filter' Result.isError |> Map.isEmpty

  /// Same as Map.exists, but without requiring you to accept the key in the predicate
  let exists' pred = Map.exists (fun _ -> pred)

  /// Same as Map.add, but throws on duplicate key
  let addX key value map =
    match Map.tryFind key map with
    | Some k -> failwith $"Duplicate key with value [{{%A{k}}}] being added to map"
    | None -> Map.add key value map

  /// Returns a new map which is the merge of two maps
  /// If key is present in only one of the maps, the new map will contain the value in the map where the key is present
  /// If key is present in both maps, exception will be thrown
  let mergeX (m1 : Map<'key, 'a>) (m2 : Map<'key, 'a>) : Map<'key, 'a> =
    let combine m3 key v2 : Map<'key, 'a> =
      match Map.tryFind key m3 with
      | Some x -> failwithf $"Duplicate key with value [{{%A{x}}}] in maps being merged"
      | None -> Map.add key v2 m3

    Map.fold combine m1 m2

  let unzipValues (m : Map<'key, 'v1 * 'v2>) =
    let m1 = m |> Map.map (fun _ (v1, _) -> v1)
    let m2 = m |> Map.map (fun _ (_, v2) -> v2)
    (m1, m2)

  let toListOfKeys m = m |> Map.toList |> List.map fst
  let toSeqOfKeys  m = m |> Map.toSeq |> Seq.map fst
  let toSetOfKeys  m = m |> toSeqOfKeys |> Seq.toSet

  /// Will check if the maps contain the same keys and throw if they don't
  let zipValuesX (m1 : Map<'key, 'v1>) (m2 : Map<'key, 'v2>) : Map<'key, 'v1 * 'v2> =
    if toSetOfKeys m1 = toSetOfKeys m2
    then
      m1
      |> Map.map (fun key v1 ->
          let v2 = Map.find key m2
          (v1, v2))
    else failwith "Can't zip maps which don't contain the same sets of keys"

  let groupMapWithTupledKeyByFst
      (sourceMap: Map<'key1 * 'key2 ,'value>)
      =
      let folder (state: Map<'key1, Map<'key2,'value>>) (key1, key2) value =
        let myMap =
          state
          |> Map.tryFind key1
          |> Option.defaultValue Map.empty
          |> Map.add key2 value
        Map.add key1 myMap state

      Map.fold folder Map.empty sourceMap

  let groupMapWithTupledKeyBySnd
      (sourceMap: Map<'key1 * 'key2 ,'value>)
      =
      let folder (state: Map<'key2, Map<'key1,'value>>) (key1, key2) value =
        let myMap =
          state
          |> Map.tryFind key2
          |> Option.defaultValue Map.empty
          |> Map.add key1 value
        Map.add key2 myMap state

      Map.fold folder Map.empty sourceMap

  let sequenceToAsyncOfUnit (m : Map<'key, Async<unit>>) : Async<unit> =
    m
    |> Map.toList
    |> List.map snd
    |> List.sequenceToAsyncOfUnit

  /// Returns a new map which is the merge of two maps, where the values of the maps are lists
  /// If key is present in only one of the maps, the new map will contain the value in the map where the key is present
  /// If key is present in both maps, the list in m1 will be merged before the list in m2, i.e. l1 @ l2
  let mergeByListConcat (m1 : Map<'key, List<'a>>) (m2 : Map<'key, List<'a>>) : Map<'key, List<'a>> =
    let combine m3 key l2 : Map<'key, List<'a>> =
      match Map.tryFind key m3 with
      | Some l1 -> Map.add key (l1 @ l2) m3
      | None -> Map.add key l2 m3

    Map.fold combine m1 m2

  let sequenceAsyncValue (m : Map<'key, Async<'value>>) =
    m
    |> Map.toList
    |> List.unzip
    |> fun (keyL, valueLA) -> async {
        let! valueL = valueLA |> List.sequenceAsync
        return (List.zip keyL valueL |> List.toMap)
       }

  /// Changes map of results to of Ok map or Error first error value encountered.
  /// To make error deterministic you can pass in a function to sort the order in which the map is processed.
  let sequenceResultM sortingFO (m : Map<'key, Result<'a, 'b>>)
    : Result<Map<'key, 'a>, 'key * 'b>
    =
      toListSortedBy sortingFO m
      |> List.map (fun (key, r) ->
          match r with
          | Ok a -> Ok (key, a)
          | Error b -> Error (key, b))
      |> List.sequenceResultM
      |> function
          | Ok l -> Ok (List.toMap l)
          | Error e -> Error e

  /// Folds the map by only looking at the values
  let fold'
    (folder : 'state -> 'value -> 'state)
    (state : 'state)
    (table : Map<'key, 'value>)
    : 'state
      =
        Map.fold (fun state _ value -> folder state value) state table

  let innerJoinOnKey (map1 : Map<'key,'value1>) (map2 : Map<'key,'value2>)
    : Map<'key, 'value1 * 'value2>
    =
      map1
      |> Map.map (fun key1 value1 ->
          let value2O = Map.tryFind key1 map2
          match value2O with
          | Some value2 -> Some (value1, value2)
          | None -> None
          )
      |> choose' id

  let choose (chooser : 'key -> 'value -> Option<'value2>) (map : Map<'key, 'value>)  =
    map
    |> Map.map chooser
    |> Map.filter (fun _ value -> value |> Option.isSome)
    |> Map.map (fun _ value -> value |> Option.get)

  let innerJoinOnSubKey
    (getKey : 'key1 -> 'key2)
    (mapValues : 'value1 -> 'value2 -> 'value3)
    (map2 : Map<'key2,'value2>)
    (map1 : Map<'key1,'value1>)
    : Map<'key1, 'value3>
    =
      map1
      |> Map.map (fun key1 value1 ->
          let value2O = Map.tryFind (getKey key1) map2
          match value2O with
          | Some value2 -> Some (mapValues value1 value2)
          | None -> None
          )
      |> choose (fun _ b -> b)

  let innerJoinOnKeyWithMap
    (mapValues : 'value1 -> 'value2 -> 'value3)
    (map2 : Map<'key,'value2>)
    (map1 : Map<'key,'value1>)
    : Map<'key, 'value3>
    =
      map1
      |> Map.map (fun key1 value1 ->
          let value2O = Map.tryFind key1 map2
          match value2O with
          | Some value2 -> Some (mapValues value1 value2)
          | None -> None
          )
      |> choose (fun _ b -> b)

module SeqOfChar =
  let toString (cs : seq<char>) = cs |> Array.ofSeq |> System.String

module ListOfChar =
  let toString (cl : char list) = cl |> Array.ofList |> System.String


module Set =


  let choose (chooser:'T -> 'U option) s =
    s
    |> Set.toList
    |> List.choose chooser
    |> List.toSet

  let toList = Set.toList : Set<'a> -> List<'a>

module String =
  let splitOnceOnChar splitChar s =
    List.ofSeq s
    |> List.splitOnceOnExcl ((=) splitChar)
    |> fun (a, b) -> ListOfChar.toString a, ListOfChar.toString b

  let splitMultipleOnCharExcl splitChar (s : string) =
    List.ofSeq s
    |> List.splitMultipleOnExcl ((=) splitChar)
    |> List.map ListOfChar.toString

  let toCharList = List.ofSeq
  let indexOfChar (c : char) (s : string) = s.IndexOf c
  let indexOfString (sub : string) (s : string) = s.IndexOf sub

  // convenient, functional TryParse wrappers returning option<'a>
  let private tryParseWith (tryParseFunc: string -> bool * _) x =
      match tryParseFunc x with
      | true, v    -> Some v
      | false, _   -> None

  let parseToDateO   s = tryParseWith System.DateTime.TryParse s
  let parseToIntO    s = tryParseWith System.Int32.TryParse   s
  let parseToInt64O  s = tryParseWith System.Int64.TryParse   s
  let parseToSingleO s = tryParseWith System.Single.TryParse  s
  let parseToDoubleO s = tryParseWith System.Double.TryParse  s
  // etc.
  let parseToDateX   s = tryParseWith System.DateTime.TryParse s |> Option.getOrFailWith $"Parsing failed on: {s}"
  let parseToIntX    s = tryParseWith System.Int32.TryParse    s |> Option.getOrFailWith $"Parsing failed on: {s}"
  let parseToInt64X  s = tryParseWith System.Int64.TryParse    s |> Option.getOrFailWith $"Parsing failed on: {s}"
  let parseToSingleX s = tryParseWith System.Single.TryParse   s |> Option.getOrFailWith $"Parsing failed on: {s}"
  let parseToDoubleX s = tryParseWith System.Double.TryParse   s |> Option.getOrFailWith $"Parsing failed on: {s}"


[<AutoOpen>]
module ParsingActivePatterns =
     // active patterns for try-parsing strings
  let (|DateO|_|)   = String.parseToDateO
  let (|IntO|_|)    = String.parseToIntO
  let (|Int64O|_|)  = String.parseToInt64O
  let (|SingleO|_|) = String.parseToSingleO
  let (|DoubleO|_|) = String.parseToDoubleO

  let (|IntX|) = System.Int32.Parse


module Tuple2 =
  let sequenceAsync ((v1A, v2A) : Async<'v1> * Async<'v2>) = async {
      let! v1 = v1A
      let! v2 = v2A
      return v1, v2
    }

  let sequenceFstAsync ((v1A, v2) : Async<'v1> * 'v2) = async {
      let! v1 = v1A
      return v1, v2
    }

  let sequenceSndAsync ((v1, v2A) : 'v1 * Async<'v2>) = async {
      let! v2 = v2A
      return v1, v2
    }

  let sequenceSndList (a : 'a, l : List<'b>) : List<'a * 'b> = l |> List.map (fun b -> a,b)

module Tuple3 =
  let inline fst (a, _, _) = a
  let inline snd (_, b, _) = b
  let inline third (_, _, c) = c

  let inline discardThird (a,b,_)  = a, b

module Tuple4 =
  let inline fst (a, _, _, _)  = a
  let inline snd (_, b, _, _ ) = b
  let inline third (_, _, c ,_) = c
  let inline fourth (_, _, _, d) = d

  let inline discardThird (a,b,_)  = a, b

module MapOfMap =
  // Returns a list of nested tuples representing the map
  // No sorting is applied, order is presumably undefined
  let toList (mm : Map<'key1, Map<'key2, 'v>>) : List<'key1 * ('key2 * 'v)> =
    mm
    |> Map.toList
    |> List.collect (fun (key1, m2) ->
        m2
        |> Map.toList
        |> List.map (fun (key2, v) -> key1, (key2, v)))

  // Returns a list of nested tuples representing the map
  // No sorting is applied, order is presumably undefined
  let toListSortedBy outerSortF innerSortF (mm : Map<'key1, Map<'key2, 'value>>) : List<'key1 * ('key2 * 'value)> =
    mm
    |> Map.toListSortedBy outerSortF
    |> List.collect (fun (key1, m2) ->
        m2
        |> Map.toListSortedBy innerSortF
        |> List.map (fun (key2, v) -> key1, (key2, v)))

  /// Transform a map of maps
  let map ( f : 'key1 -> 'key2 -> 'a -> 'b)  (mm : Map<'key1, Map<'key2, 'a>>) =
    mm
    |> Map.map (fun key1 m2 ->
        m2
        |> Map.map ( f key1 )) // note eta reduction (linter was complaining)

  /// Map but without passing the keys to the mapping
  let map' ( f : 'a -> 'b)  (mm : Map<'key1, Map<'key2, 'a>>) =
    mm
    |> Map.map' (Map.map' f) // crazy double eta reduction

  // returns the values of the inner map as a list
  // no sorting is applied, order is presumably undefined
  let toListOfValuesWithoutKeys mm =
    mm
    |> toList
    |> List.map (snd >> snd)

  ///
  let mergeByListConcatSortingByAndDiscardingFirstKey (mm : Map<'key1, Map<'key2, List<'v>>>)
    : Map<'key2, List<'v>> =
      mm
      |> Map.toList
      |> List.sortBy fst
      |> List.map snd
      |> List.fold Map.mergeByListConcat Map.empty

  let fold
    (folder : 'state -> 'key1 -> 'key2 -> 'value -> 'state)
    (state : 'state)
    (mm : Map<'key1, Map<'key2, 'value>>)
    =
      mm
      |> toList
      |> List.fold (fun state (key1, (key2, value)) -> folder state key1 key2 value) state

  /// Fold while disregarding keys
  let fold'
    (folder : 'state -> 'value -> 'state)
    (state : 'state)
    (mm : Map<'key1, Map<'key2, 'value>>)
    =
      mm
      |> toList
      |> List.fold (fun state (_, (_, value)) -> folder state value) state

module Result =
  let bind2 f r1 r2 =
    match r1, r2 with
    | Ok v1, Ok v2 -> f v1 v2
    | Error e1, _ -> Error e1
    | _, Error e2 -> Error e2


  let getOrFailWith s = function
    | Ok x -> x
    | Error _ -> failwith s

  /// Did you map instead of binding?
  let flatten vRR =
    match vRR with
    | Ok vR ->
      match vR with
      | Ok v -> Ok v
      | Error e -> Error e
    | Error e -> Error e

  /// On success, return the value. On error, throw. Naughty.
  /// Avoid in production code
  let get = function
    | Ok x -> x
    | Error e -> failwith $"%A{e} |> Result.get failed"

  let getError = function
    | Ok x -> failwith $"%A{x} |> Result.getError failed"
    | Error e -> e

  let getErrorOrFailwith s = function
    | Ok _ -> failwith s
    | Error e -> e

  // combine a list of results, monadically (from S. Wlaschin)
  let sequence (aLR : List<Result<'a,'b>>) : Result<List<'a>,'b> =

    let cons head tail = head::tail
    let consR headR tailR = cons <!> headR <*> tailR
    let initialValue = Ok [] // empty list inside Result

    // loop through the list, prepending each element
    // to the initial value
    List.foldBack consR aLR initialValue

  let map4 fn x1 x2 x3 x4 =
    fn <!> x1 <*> x2 <*> x3 <*> x4

module Async =
  // This standard naming should be duplicated
  //let retn = Async.singleton
  //let retn2 x = async { return x }
  //let retn3 x = async.Return x

  /// Did you use map instead of bind?
  let flatten (xAA : Async<Async<'a>>) : Async<'a> = async {
    let! xA = xAA
    return! xA }

  let concatLists (aALL : Async<List<List<'a>>>) : Async<List<'a>> =
    aALL
    |> Async.map List.concat

  let unit = async.Return ()

module AsyncResult =

  type AsyncResult<'a,'b> = Async<Result<'a,'b>>


  let ofResult x : AsyncResult<_,_> = async { return x }

  let getOrFailWith s xAR =
    xAR |> Async.map (Result.getOrFailWith s)

  let eitherMap okf ef ar =
    ar |> Async.map (Result.eitherMap okf ef)

type AsyncValidation<'Success, 'Failure> = Async<Validation<'Success,'Failure>>

module AsyncValidation =

    let ofAsyncResult (asyncResult : AsyncResult<_,_>) : AsyncValidation<_,_> =
      asyncResult |> Async.map Validation.ofResult

    let ofResult (result : Result<_,_>) : AsyncValidation<_,_> =
      async { return result |> Validation.ofResult }

    let mapErrors fn asyncValidation =
      asyncValidation |> Async.map (Validation.mapErrors fn)

    let requireSome (noneError: 'a) (option : 'b option)  : AsyncValidation<_,_> =
      async { return option |> Result.requireSome noneError |> Validation.ofResult }

    let requireTrue (falseError: 'a) (myBool : bool) : AsyncValidation<_,_> =
      async { return myBool |> Result.requireTrue falseError |> Validation.ofResult }

    let requireNone (someError: 'a) (option : 'b option) : AsyncValidation<_,_> =
      async { return option |> Result.requireNone someError |> Validation.ofResult }

module Reflection =
  open System

  module Record =
    /// Returns a sequence containing the fields names in the record
    let getHeadingNameS (t : Type) =
      FSharp.Reflection.FSharpType.GetRecordFields(t) |> Array.toSeq |> Seq.map (fun x -> x.Name)

  module RecordS =

    /// Convert a sequence of records to a seq<seq<obj>>, where each object is a field in a record
    let getObjSS (r : 'record) = r |> FSharp.Reflection.FSharpValue.GetRecordFields |> Array.toSeq

    /// Convert a sequence of records to a seq<seq<obj>> with a header row
    /// Made INLINE for Fable Compiler
    let inline getHeadingAndObjectSS (recordS : seq<'record>) =
      let headingS =  Record.getHeadingNameS typeof<'record> |> Seq.map box
      let valueSS = recordS |> Seq.map getObjSS
      Seq.concat [seq { headingS }; valueSS]
