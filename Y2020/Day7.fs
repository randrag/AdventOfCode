namespace AdventOfCode

module Day7 =

  type BagColour = string
  type Quantity = int
  type MustContain = BagColour * Quantity
  type Rule = { ParentColour : BagColour ; MustContainL : List<MustContain> }

  // parsing of input
  let input = System.IO.File.ReadLines("/Users/roland/Code/AdventOfCode/Y2020/Day7Input.txt") |> List.ofSeq

  let getMustContain (s : string) =
    let firstSpacePos = s.IndexOf(" ")
    let qtyString = s.Substring(0,firstSpacePos)
    let endPos = s.IndexOf(" bag")
    let colourString = s.Substring(firstSpacePos, (endPos - firstSpacePos)).Trim(' ')

    colourString, qtyString |> int : MustContain

  let parseLine (s : string) =
    let splitPos = s.IndexOf(" bags contain ")
    let parentColour = s.Substring (0 , splitPos)
    let mustContainString = s.Substring( splitPos + Seq.length(" bags contain ") )
    let mustContainStringL =
      match mustContainString with
      | "no other bags." -> []
      | otherBags -> otherBags.Split(",") |> List.ofArray |> List.map (fun s -> s.Trim(' '))

    let mayContainL = mustContainStringL |> List.map getMustContain
    { ParentColour = parentColour; MustContainL = mayContainL}


  // Inverse tree (part 1)
  type InverseTree =
    | Outermost of Colour : BagColour
    | Enclosed of Colour : BagColour * PossibleParentColourL : List<InverseTree>

  let ruleRequiresChild childName (rule : Rule) =
    rule.MustContainL |> List.exists (fun (bagColour, _) -> bagColour = childName)

  let getPossibleParents s (ruleL : List<Rule>) =
    ruleL |> List.filter (ruleRequiresChild s) |> List.map (fun e -> e.ParentColour)

  let rec buildInverseTree bagColour (ruleL : List<Rule>) =
    let possibleParents = getPossibleParents bagColour ruleL
    match possibleParents with
    | [] -> Outermost bagColour
    | l -> Enclosed (bagColour, l |> List.map (fun e -> buildInverseTree e ruleL) )

  let rec getAllBagColoursInTree (tree : InverseTree) =
    match tree with
    | Outermost bagColour -> [bagColour]
    | Enclosed (c, l) -> List.concat ([c]::List.map getAllBagColoursInTree l)

  // Conventional tree (part 2)
  type Tree =
    | Branch of BagColour * List<Tree * Quantity>
    | Leaf of BagColour

  let rec buildTree (parentColour) (ruleL : List<Rule>) : Tree =
    let parentRule = ruleL |> List.find (fun rule -> rule.ParentColour = parentColour)
    let requiredChildL = parentRule.MustContainL
    match requiredChildL with
    | [] -> Leaf (parentColour)
    | l -> Branch ( parentColour, l |> List.map (fun (colour, qty) -> buildTree colour ruleL, qty ) )

  let rec sumTree tree =
    match tree with
    | Leaf _ -> 0
    | Branch (_, l) -> l |> List.sumBy (fun (tree, quantity) -> quantity * (1 + sumTree tree))

  // runner
  let run () =

    let ruleL = input |> List.map parseLine |> List.sortBy (fun r -> r.ParentColour)

    buildInverseTree "shiny gold" ruleL
    |> getAllBagColoursInTree
    |> List.distinct
    |> List.filter ( (<>) "shiny gold")
    |> List.length
    |> ps "Part 1, Count: "

    buildTree "shiny gold" ruleL |> sumTree |> ps "Part 2, Tree inside summed: "
