module TestYear2022.Day07 exposing (test_actionParser, test_addLeaf, test_calculateDirectorySizeTree, test_sumFileDirSize)

import Expect
import Html.Attributes exposing (action, dir)
import Parser exposing (Parser)
import Test exposing (Test, describe, test)
import Tree exposing (Tree, appendChild, singleton, tree)
import Tree.Zipper as TZ
import Year2022.Day07 exposing (..)


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
    tree { name = "top", size = 0, dir = True }
        [ tree { name = "a.txt", size = 1, dir = False } []
        , tree { name = "b.txt", size = 2, dir = False } []
        , tree { name = "c.txt", size = 3, dir = False } []
        ]


onlyFilesTree2 : Tree FileDir
onlyFilesTree2 =
    tree { name = "top", size = 0, dir = True }
        [ tree { name = "a.txt", size = 1, dir = False } []
        , tree { name = "b.txt", size = 2, dir = False } []
        , tree { name = "c.txt", size = 3, dir = False } []
        , singleton fileD
        ]


oneSubDirTree : Tree FileDir
oneSubDirTree =
    tree { name = "top", size = 0, dir = True }
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
