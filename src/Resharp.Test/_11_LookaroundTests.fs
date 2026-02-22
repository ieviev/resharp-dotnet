// [<Xunit.Collection("Sequential")>]
module Resharp.Test._11_LookaroundTests

open Resharp
open Resharp.Types
open Xunit
open Common

#if DEBUG

[<Fact>]
let ``c intersect 1.2b`` () =
    assertMatchEnd """(?<=author).*&.*and.*""" "author: abc and def" 6 19


let bibtexEntry =
    @"@article{de2000thyroid,
  title={Thyroid cancer in French Polynesia between 1985 and 1995: influence of atmospheric nuclear bomb tests performed at Mururoa and Fangataufa between 1966 and 1974},
  author={De Vathaire, Florent and Le Vu, B{\'e}atrice and Challeton-de Vathaire, C{\'e}cile},
  journal={Cancer Causes \& Control},
  volume={11},
  number={1},
  pages={59--63},
  year={2000},
  publisher={Springer}
}"


let bibtextEntry2 =
    """
@article{cunliffe1999rotavirus,
  title={Rotavirus G and P types in children with acute diarrhea in Blantyre, Malawi, from 1997 to 1998: predominance of novel P [6] G8 strains},
  author={Cunliffe, Nigel A and Gondwe, Jailosi S and Broadhead, Robin L and Molyneux, Malcolm E and Woods, Patricia A and Bresee, Joseph S and Glass, Roger I and Gentsch, Jon R and Hart, C Anthony},
  journal={Journal of medical virology},
  volume={57},
  number={3},
  pages={308--312},
  year={1999},
  publisher={Wiley Online Library}
}
"""

[<Fact>]
let ``g bibtex extraction 1.3`` () =
    assertAllLLmatchTexts
        @"(?<=or=(\{|.*\W))(~(.*and.*)&\S[\w-{}\\' ,]+\w)(?=(\W.*|)\},)"
        bibtexEntry
        [
            "De Vathaire, Florent"
            "Le Vu, B{\\'e}atrice"
            "Challeton-de Vathaire, C{\\'e}cile"
        ]

[<Fact>]
let ``g bibtex extraction 1.4`` () =
    assertAllLLmatchTexts
        @"(?<=or=\{.*)(?<=\W)(~(.*and.*)&[A-Z][\w-{}\\' ,]+)(?=.*\},)(?=\W)"
        bibtexEntry
        [
            "De Vathaire, Florent"
            "Le Vu, B{\\'e}atrice"
            "Challeton-de Vathaire, C{\\'e}cile"
        ]

[<Fact>]
let ``g bibtex extraction 1.5`` () =
    assertAllLLmatchTexts
        (String.concat "&" [
            """~(.*and.*)"""
            """[A-Z][\w-{}\\' ,]+"""
            @"(?<=or=\{.*).*"
            @"(?<=\W).*"
            @".*(?=.*\},)"
            @".*(?=\W)"
        ])
        bibtexEntry
        [
            "De Vathaire, Florent"
            "Le Vu, B{\\'e}atrice"
            "Challeton-de Vathaire, C{\\'e}cile"
        ]


let learnOpts =
    let o = ResharpOptions()
    o.MaxDfaCapacity <- 1_000_000
    o.MinimizePattern <- false
    o.FindLookaroundPrefix <- false
    o.UseDotnetUnicode <- false
    o


[<Fact>]
let ``g bibtex extraction set nullability `` () =
    assertAllLLmatchesO
        learnOpts
        @"(?<=(or\=\{)|(and )).*&(?<=or\=\{.*).*&(?<=[\{ ]).*&(?<=\{.*).*&.{2,37} .{1,24}&.*[\)\.\}1A-z]&~(.*or\=\{.*)&~(.* and.*)&[A-Zd].*&\w.*&.*(?=((\},\n)? (and)?)|( and))&.*(?=.*(\},\n)? (and)?)&.*(?=.*[\} ])&.*(?=[ \}])"
        bibtextEntry2
        [
            (189, 17)
            (211, 17)
            (233, 18)
            (256, 19)
            (280, 17)
            (302, 16)
            (323, 14)
            (342, 14)
            (361, 15)
        ]

[<Fact>]
let ``testing anchors 1.1`` () =
    assertRawDerivative """\ba""" "a " [ "⊥*"; @"(⊥*|(?<=⊥)a)" ]


let lookSample =
    """

        In other words, our intention is for Soar to support all the
        capabilities required of a general intelligent agent.
        http://wwwis.cs.utwente.nl:8080/ tcm/index.html



     TCM


     �  Web site: wwwis.cs.utwente.nl:8080/~tcm/index.html

     �  FTP site: ftp.cs.vu.nl/pub/tcm/


"""

[<Fact>]
let ``lookback 01`` () =
    let bytes = (System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/data/howto.txt"))
    let opts = ResharpOptions.HighThroughputDefaults

    let r12 =
        Regex(@"(?<=([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/[^ ]*)?).*", opts)
            .ValueMatches(bytes)

    assertEqual 52 r12.size


#endif
