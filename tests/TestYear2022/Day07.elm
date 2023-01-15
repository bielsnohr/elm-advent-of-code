module TestYear2022.Day07 exposing
    ( test_AocTestCase_buildDirTree
    , test_AocTestCase_sumFileSystem
    , test_actionParser
    , test_addLeaf
    , test_applyAction
    , test_calculateDirectorySizeTree
    , test_sumFileDirSize
    )

import Expect
import Maybe exposing (withDefault)
import Parser exposing (Parser)
import Test exposing (Test, describe, test)
import Tree exposing (Tree, appendChild, singleton, tree)
import Tree.Zipper as TZ
import Year2022.Day07 exposing (..)



-- Test Data --


singleFileTree : Tree FileDir
singleFileTree =
    tree { name = "a.txt", size = 1, dir = False } []


fileD : FileDir
fileD =
    FileDir "d.txt" 4 False


dirD : FileDir
dirD =
    FileDir "d" 0 True


onlyFilesTree : Tree FileDir
onlyFilesTree =
    tree { name = "/", size = 0, dir = True }
        [ tree { name = "a.txt", size = 1, dir = False } []
        , tree { name = "b.txt", size = 2, dir = False } []
        , tree { name = "c.txt", size = 3, dir = False } []
        ]


onlyFilesTree2 : Tree FileDir
onlyFilesTree2 =
    tree { name = "/", size = 0, dir = True }
        [ tree { name = "a.txt", size = 1, dir = False } []
        , tree { name = "b.txt", size = 2, dir = False } []
        , tree { name = "c.txt", size = 3, dir = False } []
        , singleton fileD
        ]


oneSubDirTree : Tree FileDir
oneSubDirTree =
    tree { name = "/", size = 0, dir = True }
        [ tree { name = "a.txt", size = 1, dir = False } []
        , tree { name = "b.txt", size = 2, dir = False } []
        , tree { name = "c.txt", size = 3, dir = False } []
        , singleton dirD
        ]


onlyFilesZipper : TZ.Zipper FileDir
onlyFilesZipper =
    TZ.fromTree onlyFilesTree


onlyFilesZipper2 : TZ.Zipper FileDir
onlyFilesZipper2 =
    TZ.fromTree onlyFilesTree2


oneSubDirZipper : TZ.Zipper FileDir
oneSubDirZipper =
    TZ.fromTree oneSubDirTree


nestedDir : Tree FileDir
nestedDir =
    appendChild onlyFilesTree onlyFilesTree


nestedDirResult : Tree Int
nestedDirResult =
    tree 12 [ singleton 6 ]


aocTestCaseFileSystem : Tree FileDir
aocTestCaseFileSystem =
    tree { name = "/", size = 0, dir = True }
        [ tree (FileDir "a" 0 True)
            [ tree (FileDir "e" 0 True)
                [ singleton (FileDir "i" 584 False)
                ]
            , singleton (FileDir "f" 29116 False)
            , singleton (FileDir "g" 2557 False)
            , singleton (FileDir "h.lst" 62596 False)
            ]
        , singleton (FileDir "b.txt" 14848514 False)
        , singleton (FileDir "c.dat" 8504156 False)
        , tree (FileDir "d" 0 True)
            [ singleton (FileDir "j" 4060174 False)
            , singleton (FileDir "d.log" 8033020 False)
            , singleton (FileDir "d.ext" 5626152 False)
            , singleton (FileDir "k" 7214296 False)
            ]
        ]


aocTestCaseActionList : List Action
aocTestCaseActionList =
    [ ListDir
    , AddDir "a"
    , AddFile 14848514 "b.txt"
    , AddFile 8504156 "c.dat"
    , AddDir "d"
    , ChangeDir "a"
    , ListDir
    , AddDir "e"
    , AddFile 29116 "f"
    , AddFile 2557 "g"
    , AddFile 62596 "h.lst"
    , ChangeDir "e"
    , ListDir
    , AddFile 584 "i"
    , ChangeUpDir
    , ChangeUpDir
    , ChangeDir "d"
    , ListDir
    , AddFile 4060174 "j"
    , AddFile 8033020 "d.log"
    , AddFile 5626152 "d.ext"
    , AddFile 7214296 "k"
    ]


aocTestCaseDirSizeTree : Tree Int
aocTestCaseDirSizeTree =
    tree 48381165 [ tree 94853 [ tree 584 [] ], tree 24933642 [] ]



-- Tests --


test_sumFileDirSize : Test
test_sumFileDirSize =
    describe "Test the sumFileDirSize function"
        [ test "A single file" <|
            \_ -> sumFileDirSize 0 singleFileTree |> Expect.equal ( 1, Nothing )
        , test "A single directory with only files under" <|
            \_ -> sumFileDirSize 0 onlyFilesTree |> Expect.equal ( 6, Just (singleton 6) )
        ]


test_calculateDirectorySizeTree : Test
test_calculateDirectorySizeTree =
    describe "Test the calculateDirectorySizeTree function"
        [ test "A directory with only files under" <|
            \_ -> calculateDirectorySizeTree onlyFilesTree |> Expect.equal (singleton 6)
        , test "A directory with one nested directory under" <|
            \_ -> calculateDirectorySizeTree nestedDir |> Expect.equal nestedDirResult
        ]


test_actionParser : Test
test_actionParser =
    describe "Test the actionParser Parser"
        [ test "Parse a file line and return AddFile variant" <|
            \_ ->
                Parser.run actionParser "14848514 b.txt"
                    |> Expect.equal (Ok (AddFile 14848514 "b.txt"))
        , test "Parse a directory line and return AddDirectory variant" <|
            \_ ->
                Parser.run actionParser "dir a"
                    |> Expect.equal (Ok (AddDir "a"))
        , test "Parse a change directory line and return ChangeDir variant" <|
            \_ ->
                Parser.run actionParser "$ cd b.dir"
                    |> Expect.equal (Ok (ChangeDir "b.dir"))
        , test "Parse a change up directory line and return ChangeUpDir variant" <|
            \_ ->
                Parser.run actionParser "$ cd .."
                    |> Expect.equal (Ok ChangeUpDir)
        , test "Parse a ls line and return ListDir variant" <|
            \_ ->
                Parser.run actionParser "$ ls"
                    |> Expect.equal (Ok ListDir)
        ]


test_addLeaf : Test
test_addLeaf =
    describe "Test the addLeaf function"
        [ test "Adding a file" <|
            \_ ->
                addLeaf onlyFilesZipper fileD
                    |> Expect.equal onlyFilesZipper2
        , test "Adding a directory" <|
            \_ ->
                addLeaf onlyFilesZipper dirD
                    |> Expect.equal oneSubDirZipper
        ]


test_applyAction : Test
test_applyAction =
    describe "Test the applyAction function"
        [ test "Adding a file" <|
            \_ ->
                applyAction (AddFile 4 "d.txt") onlyFilesZipper
                    |> Expect.equal onlyFilesZipper2
        , test "Adding a directory" <|
            \_ ->
                applyAction (AddDir "d") onlyFilesZipper
                    |> Expect.equal oneSubDirZipper
        , test "Change directory" <|
            \_ ->
                let
                    changed_dir_focus =
                        withDefault oneSubDirZipper <| TZ.lastChild oneSubDirZipper
                in
                applyAction (ChangeDir "d") oneSubDirZipper
                    |> Expect.equal changed_dir_focus
        ]


test_AocTestCase_buildDirTree : Test
test_AocTestCase_buildDirTree =
    test "Use the Action list from the AoC test case to build the file tree" <|
        \_ ->
            buildDirTree aocTestCaseActionList
                |> Expect.equal aocTestCaseFileSystem


test_AocTestCase_sumFileSystem : Test
test_AocTestCase_sumFileSystem =
    test "Use the FileDir tree from the AoC test case to find the sum of the dirs below 100000" <|
        \_ ->
            sumFilteredDirSizes 100000 aocTestCaseDirSizeTree
                |> Expect.equal 95437
