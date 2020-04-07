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
    
    List.append list1 list2
        |> List.sortByDescending (fun item -> (snd item).IsSome)
        |> List.distinctBy fst
    

let search (grid: string list) (wordsToSearchFor: string list) =
    wordsToSearchFor
            |> List.map (binder grid)
            |> List.collect id
            |> Map.ofList
