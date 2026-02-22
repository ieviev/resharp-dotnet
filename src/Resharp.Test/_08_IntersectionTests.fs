// [<Xunit.Collection("Sequential")>]
module Resharp.Test._08_IntersectionTests

open Resharp
open Xunit
open Common

#if DEBUG


[<Fact>]
let ``conjunction match tests 1`` () =
    assertFirstMatchText """c...&...s""" "raining cats and dogs" "cats"
    assertFirstMatchText """c.*&.*s""" "cats blah blah blah" "cats"

    assertFirstMatchText
        """.*rain.*&.*dogs.*"""
        "raining cats and dogs"
        "raining cats and dogs"

    assertFirstMatchText """and.*&.*dogs.*""" "raining cats and dogs" "and dogs"


[<Fact>]
let ``conjunction match tests 2`` () = assertNoMatch """_*A_*&_*B""" "B  A"


let twainExampleShort =
    """

    Thursday, April 16.  Went ashore in the forenoon at Port Louis, a little
    town, but with the largest variety of nationalities and complexions we
    have encountered yet.  French, English, Chinese, Arabs, Africans with
    wool, blacks with straight hair, East Indians, half-whites, quadroons--
    and great varieties in costumes and colors.

    """

[<Fact>]
let ``more than 3 cases short`` () =
    let pattern = """_*Thursday_*&_*April_*&_*Went_*&_*ashore_*""" // [\s\S]*

    let input = twainExampleShort
    assertFirstMatchText pattern input input

[<Fact>]
let ``more than 3 cases short 2`` () =
    let pattern =
        """[\s\S]*French[\s\S]*&[\s\S]*English[\s\S]*&[\s\S]*Chinese[\s\S]*&[\s\S]*Arabs[\s\S]*""" // [\s\S]*

    let input = "French English Chinese Arabs asdasd"
    assertFirstMatchText pattern input input


[<Fact>]
let ``twain match test 1`` () =
    let pattern = """[\s\S]*English[\s\S]*&[\s\S]*French"""

    let input = twainExampleShort
    assertNoMatch pattern input


[<Fact>]
let ``twain match test more than 3 cases 1`` () =
    let pattern =
        """[\s\S]*French[\s\S]*&[\s\S]*English[\s\S]*&[\s\S]*Chinese[\s\S]*&[\s\S]*Arabs[\s\S]*"""

    let input = twainExampleShort
    assertFirstMatchText pattern input input




let ``twain match test 2`` () =
    let pattern = """[\s\S]*French[\s\S]*&[\s\S]*English"""
    let input = twainExampleShort
    assertFirstMatch pattern input (0, 195)


[<Fact>]
let ``twain match test 3`` () =
    let pattern = """[\s\S]*French[\s\S]*&[\s\S]*English""" // [\s\S]*
    let input = twainExampleShort
    let result = getFirstLLmatchText pattern input
    assertTrue (result.EndsWith("English"))

[<Fact>]
let ``twain match test 4`` () =
    let pattern = """[\s\S]*French[\s\S]*&English[\s\S]*""" // [\s\S]*
    let input = "English, French asdasd"
    assertFirstMatchText pattern input input



let twainExample3 =
    """

"I think, m'm, they--"

"Now, Tom Sawyer, what kind of a lie are you fixing YOUR
mouth to contribit to this mess of rubbage? Speak out--and
I warn you before you begin, that I don't believe a word
of it.  You and Huck's been up to something you no business
to--I know it perfectly well; I know you, BOTH of you.
Now you explain that dog, and them blackberries,
and the lantern, and the rest of that rot--and mind you
talk as straight as a string--do you hear?"

Tom he looked considerable hurt, and says, very dignified:

"It is a pity if Huck is to be talked to that way,
just for making a little bit of a mistake that anybody
could make."

"What mistake has he made?"

"Why, only the mistake of saying blackberries when
of course he meant strawberries."

"Tom Sawyer, I lay if you aggravate me a little more, I'll--"

"Aunt Sally, without knowing it--and of course without
intending it--you are in the wrong.  If you'd 'a' studied
natural history the way you ought, you would know that
all over the world except just here in Arkansaw they
ALWAYS hunt strawberries with a dog--and a lantern--"

But she busted in on him there and just piled into him
and snowed him under.  She was so mad she couldn't get
the words out fast enough, and she gushed them out
in one everlasting freshet.  That was what Tom Sawyer
was after.  He allowed to work her up and get her started
and then leave her alone and let her burn herself out.
Then she would be so aggravated with that subject
that she wouldn't say another word about it, nor let AAAAAAAAAAA
anybody else.  Well, it happened just so.  When she
was tuckered out and had to hold up, he says, quite ca'm:

"And yet, all the same, Aunt Sally--"

"Shet up!" she says, "I don't want to hear another word
out of you."


"""

[<Fact>]
let ``twain paragraph test 5`` () =
    let pattern = @"\n\n~(_*\n\n_*)\n\n&_*(Arkansaw)_*"
    let input = twainExample3

    let expectedParagraph =
        """

"Aunt Sally, without knowing it--and of course without
intending it--you are in the wrong.  If you'd 'a' studied
natural history the way you ought, you would know that
all over the world except just here in Arkansaw they
ALWAYS hunt strawberries with a dog--and a lantern--"

"""

    assertFirstMatchText pattern input expectedParagraph





let shortPg =
    """

"What mistake has he made?"

"mistake of saying strawberries."

"""


let shortPg2 =
    """
"honor of
name "
"""

[<Fact>]
let ``implication 1 `` () =

    let pattern =
        @"(?<=\n\n|\A)(~(_*\n\n_*)&(~(_*mistake_*)|(_*strawberries_*)))(?=\n\n)"

    assertAllLLmatchTexts pattern shortPg [ ""; ""; "\"mistake of saying strawberries.\"" ]


[<Fact>]
let ``implication 2 `` () =
    let pattern = @"\n~(_*\n\n_*)\n&~(_*honor_*)"
    let matcher1 = Regex(pattern)
    let result1 = matcher1.ValueMatches(shortPg2).ToArray()
    Assert.Equal(1, result1.Length)

[<Fact>]
let ``implication 3 `` () =
    let pattern = @"~(_*\n\n_*)\n&~(_*honor_*)"
    let result1 = getAllLLmatches pattern shortPg2
    Assert.Equal(2, result1.Count)

[<Fact>]
let ``script test 1`` () =
    let pattern = [ "THE.*LIFE"; @".*FIVE.*" ] |> String.concat "&"

    let input =
        @"
      EDWARD MILLS AND GEORGE BENTON:  A TALE
      THE FIVE BOONS OF LIFE
      THE FIRST WRITING-MACHINES
"

    let result = getFirstLLmatchText pattern input
    Assert.Equal(result, "THE FIVE BOONS OF LIFE")






#endif
