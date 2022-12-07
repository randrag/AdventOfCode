namespace AdventOfCode.Y2022

open Helpers

module Day07 =

   type FileItem = { Name : string; Size : int64 }

   type Item = File of FileItem | FolderItem of name : string

   type Command =
      | CD of dirName : string
      | CDRoot
      | CDParent
      | LS
      | ListedFolder of dirName : string
      | ListedFile of file : FileItem


   // This is a tree recursive tree structure, though it does not look it.
   // The leafs have empty SubFolder lists.
   type Folder = {
      Name : string
      FileM : Map<string, int64>
      SubFolderM : Map<string, Folder>
   }

   type FolderWithSize = {
      Name : string
      FileM : Map<string, int64>
      SubFolderM : Map<string, FolderWithSize>
      Size : int64
   }

   let createEmptyFolder name = {
      Name = name; FileM = Map[]; SubFolderM = Map[]
   }

   let parse (lines : seq<string>) =
      lines
      |> Seq.map (fun s -> s.Split(' '))
      |> Seq.map Seq.toList
      |> Seq.toList
      |> List.map (function
         | ["$"; "cd"; "/"] -> CDRoot
         | ["$"; "cd"; ".."] -> CDParent
         | ["$"; "cd"; dirName] -> CD dirName
         | ["$"; "ls"] -> LS
         | ["dir"; dirName] -> ListedFolder dirName
         | [fileSize; fileName] -> ListedFile { Name = fileName ; Size =  fileSize |> String.parseToInt64X }
         | _ -> Unreachable ()
      )
      |> Seq.toList
      |> List.tail // throw away initial cd to root


   module Part1 =

      let addEmptySubFolder  subFolderName (parentFolder : Folder) =
         let subFolderM' =
            Map.add subFolderName (createEmptyFolder subFolderName) parentFolder.SubFolderM
         {parentFolder with SubFolderM = subFolderM'}

      let addFile name size (parentFolder : Folder) =
         let fileM' =
            parentFolder.FileM
            |> Map.add name size
         {parentFolder with FileM = fileM'}

      let addFileOrFolder (folder: Folder) fileOrFolder =
         match fileOrFolder with
         | File file ->
            { folder with FileM = Map.add file.Name file.Size folder.FileM}
         | FolderItem folderName ->
            let subFolder = {Name = folderName; FileM = Map []; SubFolderM = Map []}
            { folder with SubFolderM = Map.add folderName subFolder folder.SubFolderM }

      // run commands against the folder or its children until we cd.. out of it
      let rec run folder commandList
         : Folder * List<Command>
         =
         match commandList with
         | [] -> folder, []
         | [command] -> // this is the last command
            match command with
            | CD dirName -> folder, []
            | CDParent -> folder, []
            | CDRoot -> Unreachable ()
            | LS -> folder, []
            | ListedFolder folderName -> // need to add folder to current folder
               addFileOrFolder folder (FolderItem folderName), []
            | ListedFile file -> // add file to current folder
               addFileOrFolder folder (File file), []

         | command::remainingCommands ->
            match command with
            | CD folderName -> // we want to run the next command in the subfolder
               let subFolder = Map.find folderName folder.SubFolderM // fetch it
               let subFolder', remainingCommands' =
                  // run some commands in subfolder tree and return us the new tree and
                  // remaining commands
                  run subFolder remainingCommands
               let SubFolderM' =
                  // replace the subfolder in our folder
                  Map.add subFolder'.Name subFolder' folder.SubFolderM
               // update our folder with the new subfolder map
               let folder'  = { folder with SubFolderM = SubFolderM' }
               // do the remaining commands
               run folder' remainingCommands'

            | CDRoot -> Unreachable () // I deleted it during parse since it is only at the start
            | CDParent -> folder, remainingCommands // done here - return one level higher
            | LS -> run folder remainingCommands // nothing to do, keep going with next commands
            | ListedFolder folderName ->
               let folder' = addEmptySubFolder folderName folder
               run folder' remainingCommands
            | ListedFile file ->
               let folder' = folder |> addFile file.Name file.Size
               run folder' remainingCommands

      let rec addSize (folder : Folder) : FolderWithSize =
         let fileSizes =
            folder.FileM
            |> Map.toList
            |> List.map snd
            |> List.sum

         if Map.isEmpty folder.SubFolderM then
            { Name = folder.Name
              FileM = folder.FileM
              SubFolderM = Map.empty
              Size = fileSizes }
         else
            let sizeSum, subfolderM =
               folder.SubFolderM
               |> Map.fold'
                     (fun (sizeSum, newSubfolderM) subfolder ->
                        let fws = addSize subfolder
                        let size = fws.Size
                        (sizeSum + size, newSubfolderM |> Map.add fws.Name fws)
                     )
                     (0L, Map.empty)
            { Name = folder.Name
              FileM = folder.FileM
              SubFolderM = subfolderM
              Size = sizeSum + fileSizes }

      let rec findSmallFolders (folder : FolderWithSize) : List<string * int64> =
         print $"Scoring folder {folder.Name} with size {folder.Size}"

         let qualifyingSubFolders =
            folder.SubFolderM
            |> Map.toList
            |> List.map snd
            |> List.collect findSmallFolders

         if folder.Size <= 100000 then (folder.Name, folder.Size)::qualifyingSubFolders
         else qualifyingSubFolders


      let go (year, day) runMode =
         let startingTree = { Name = "/"; SubFolderM = Map []; FileM = Map [] }

         let l =
            getInput (year, day) runMode
            |> parse
            |> List.map (fun x -> printfn $"Line: %A{x}"; x)

         run startingTree l
         |> pso "Tree: "
         |> fst
         |> addSize
         |> pso "\n\nTree with size:\n"
         |> findSmallFolders
         |> pso "\n\Small folders: "
         |> List.sumBy snd
         |> pso "\n\nAnswer part 1: "


   module Part2 =

      let rec flattenFolders (folder : FolderWithSize) : List<string * int64> =

         let subFolders =
            folder.SubFolderM
            |> Map.toList
            |> List.map snd
            |> List.collect flattenFolders

         (folder.Name, folder.Size)::subFolders

      let go (year, day) runMode =
         let startingTree = { Name = "/"; SubFolderM = Map []; FileM = Map [] }

         let l =
            getInput (year, day) runMode
            |> parse
            |> List.map (fun x -> printfn $"Line: %A{x}"; x)

         let folderWithSize =
            Part1.run startingTree l
            |> pso "Tree: "
            |> fst
            |> Part1.addSize
            |> pso "\n\nTree with size:\n"

         let totalSpace = 70000000L
         let requiredSpace = 30000000L

         let remainingSpace = totalSpace - folderWithSize.Size |> pso "Remaining space: "
         let mustFreeSpace = requiredSpace - remainingSpace |> pso "Must free space: "

         let flatFolders =
            folderWithSize
            |> flattenFolders
            |> pso "Flat folders: "
            |> List.filter (fun (s, i) -> i >= mustFreeSpace)
            |> pso "Deletion candidates: "
            |> List.sortBy snd
            |> pso "Sorted: "
            |> List.head
            |> pso "Answer part 2: "

         ()


   let run (year, day) =
      //Full |> Part1.go (year, day) //|> printn
      Full |> Part2.go (year, day) |> printn
