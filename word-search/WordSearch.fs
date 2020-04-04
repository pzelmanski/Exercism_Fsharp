module WordSearch

open System

let TryToGetWordsCoordinatesHorizontallyLeftToRight (line: string) (vertialIndex: int) (wordToSearch: string) =
    match (line).IndexOf(wordToSearch) with
    | -1 -> wordToSearch, None
    | index -> wordToSearch, Some((index + 1, vertialIndex), (index + String.length wordToSearch, vertialIndex))

let TryToGetWordsCoordinatesHorizontallyRightToLeft (line: string) (vertialIndex: int) (wordToSearch: string) =
    let wordToSearchReversed =
        wordToSearch.ToCharArray()
        |> Array.rev
        |> String

    match (line).IndexOf(wordToSearchReversed) with
    | -1 -> wordToSearch, None
    | index -> wordToSearch, Some((index + String.length wordToSearch, vertialIndex), (index + 1, vertialIndex))

let binder (wordsToSearchFor: string list) (verticalIndex: int) (gridElement: string) =
    let list1 =
        wordsToSearchFor |> List.map (TryToGetWordsCoordinatesHorizontallyRightToLeft gridElement (verticalIndex + 1))
    let list2 =
        wordsToSearchFor |> List.map (TryToGetWordsCoordinatesHorizontallyLeftToRight gridElement (verticalIndex + 1))
    
    let combinedLists = List.append list1 list2
    let combinedListsSorted = combinedLists
                              |> List.sortByDescending (fun item -> (snd item).IsSome)
                              |> List.distinctBy fst
    combinedListsSorted

let search (grid: string list) (wordsToSearchFor: string list) =
    let a = grid
            |> List.mapi (binder wordsToSearchFor)
            
    let b = a |> List.collect id
            
    let c = b |> Map.ofList
    c
