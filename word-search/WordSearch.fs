module WordSearch

open System
let TryToGetWordsCoordinatesHorizontallyRightToLeft
    (wordToSearch: string) (verticalIndex: int) (line: string) =
    let wordToSearchReversed =
        wordToSearch.ToCharArray()
        |> Array.rev
        |> String

    match (line).IndexOf(wordToSearchReversed) with
    | -1 -> wordToSearch, None
    | index -> wordToSearch,
                Some((index + String.length wordToSearch, verticalIndex + 1),
                     (index + 1, verticalIndex + 1))

let TryToGetWordsCoordinatesHorizontallyLeftToRight
    (wordToSearch: string) (verticalIndex: int) (line: string)  =
    match (line).IndexOf(wordToSearch) with
    | -1 -> wordToSearch, None
    | index -> wordToSearch,
                Some((index + 1, verticalIndex + 1),
                     (index + String.length wordToSearch, verticalIndex + 1))

let binder (grid: string list) (wordToSearchFor: string) =
    let list1 =
        grid
        |> List.mapi (TryToGetWordsCoordinatesHorizontallyRightToLeft wordToSearchFor)
    let list2 =
        grid
        |> List.mapi (TryToGetWordsCoordinatesHorizontallyLeftToRight wordToSearchFor)
    
    let combinedLists = List.append list1 list2
    let combinedListsSorted = combinedLists
                              |> List.sortByDescending (fun item -> (snd item).IsSome)
                              |> List.distinctBy fst
    combinedListsSorted

let search (grid: string list) (wordsToSearchFor: string list) =
    let a = wordsToSearchFor
            |> List.map (binder grid)
    let b = a |> List.collect id
    let c = b |> Map.ofList
    c
