module Year2022.Day07 exposing (..)

import List.Extra as LE
import Maybe exposing (withDefault)
import Parser exposing ((|.), (|=), Parser, chompWhile, getChompedString, int, keyword, map, oneOf, spaces, succeed, symbol)
import Performance
import Result.Extra as Result
import Set
import Tree as T
import Tree.Zipper as TZ
import Util.Parser


solution =
    { solve = solve
    , title = "No Space Left On Device"
    , subtitle = "Fix the handheld communication device... again... by cleaning up some files"
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        file_system =
            input
                |> String.split "\n"
                |> List.tail
                |> Maybe.withDefault []

        r1 =
            Err "--empty--"

        r2 =
            Err "--empty--"
    in
    ( r1
    , r2
    )


type Action
    = AddFile Int String
    | AddDir String
    | ChangeDir String
    | ChangeUpDir
    | ListDir


actionParser : Parser Action
actionParser =
    oneOf
        [ succeed AddFile
            |= int
            |. spaces
            |= filename
        , succeed AddDir
            |. symbol "dir"
            |. spaces
            |= filename
        , succeed ChangeUpDir
            |. symbol "$ cd .."
            |. spaces
        , succeed ListDir
            |. symbol "$ ls"
            |. spaces
        , succeed ChangeDir
            |. keyword "$"
            |. spaces
            |. keyword "cd"
            |. spaces
            |= filename
        ]


filename : Parser String
filename =
    getChompedString <|
        chompWhile (\c -> Char.isAlphaNum c || c == '.')


type alias FileDir =
    { name : String
    , size : Int
    , dir : Bool
    }


buildDirTree : List Action -> T.Tree FileDir
buildDirTree actions =
    let
        top_dir =
            T.singleton (FileDir "/" 0 True) |> TZ.fromTree
    in
    List.foldl applyAction top_dir actions |> TZ.toTree



-- This could be more robust by returning a Result (TZ.Zipper FileDir) since the
-- application of an action could fail in a variety of ways, and that should be
-- passed through.


applyAction : Action -> TZ.Zipper FileDir -> TZ.Zipper FileDir
applyAction action zipper =
    case action of
        AddFile size name ->
            addLeaf zipper (FileDir name size False)

        AddDir name ->
            addLeaf zipper (FileDir name 0 True)

        ChangeDir name ->
            case TZ.firstChild zipper of
                Just first_child ->
                    changeToChildDir first_child name

                Nothing ->
                    zipper

        ChangeUpDir ->
            withDefault zipper <| TZ.parent zipper

        ListDir ->
            zipper


addLeaf : TZ.Zipper FileDir -> FileDir -> TZ.Zipper FileDir
addLeaf zipper filedir =
    let
        -- Using appendChild will be quite inefficient and should only be used
        -- if the current focus doesn't have any children. Could easily add a
        -- call to `Tree.Zipper.lastChild` and then add a case for Nothing that
        -- replicates the below.
        -- There should also be a check the parent (i.e. current root focus)
        -- is a directory because that is the only thing that can have leaves added
        new_focus_tree =
            TZ.tree zipper |> T.appendChild (T.singleton filedir)
    in
    TZ.replaceTree new_focus_tree zipper


changeToChildDir : TZ.Zipper FileDir -> String -> TZ.Zipper FileDir
changeToChildDir zipper dir_name =
    let
        name =
            TZ.label zipper |> .name
    in
    if name == dir_name then
        zipper

    else
        case TZ.nextSibling zipper of
            Just sibling ->
                changeToChildDir sibling dir_name

            -- This should be an error...
            Nothing ->
                TZ.parent zipper |> withDefault zipper


calculateDirectorySizeTree : T.Tree FileDir -> T.Tree Int
calculateDirectorySizeTree t =
    let
        dir_info =
            T.children t |> LE.mapAccuml sumFileDirSize 0

        new_children =
            Tuple.second dir_info |> List.filterMap identity
    in
    T.tree (Tuple.first dir_info) new_children


sumFileDirSize : Int -> T.Tree FileDir -> ( Int, Maybe (T.Tree Int) )
sumFileDirSize sum t =
    let
        dir =
            T.label t |> .dir

        elem =
            if dir then
                calculateDirectorySizeTree t

            else
                T.singleton <| .size (T.label t)

        new_sum =
            T.label elem + sum
    in
    if dir then
        ( new_sum, Just elem )

    else
        ( new_sum, Nothing )
