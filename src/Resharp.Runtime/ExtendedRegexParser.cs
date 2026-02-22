// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Text;
// ReSharper disable ConvertIfStatementToConditionalTernaryExpression
using System.Text.RegularExpressions;

namespace Resharp.Runtime;

/// <summary>Builds a tree of RegexNodes from a regular expression.</summary>
internal ref struct ExtendedRegexParser
{
    // Implementation notes:
    // It would be nice to get rid of the comment modes, since the
    // ScanBlank() calls are just kind of duct-taped in.
    private const int EscapeMaxBufferSize = 256;
    private const int OptionStackDefaultSize = 32;
    private const int MaxValueDiv10 = int.MaxValue / 10;
    private const int MaxValueMod10 = int.MaxValue % 10;
    private RegexNode? _stack;
    private RegexNode? _group;
    private RegexNode? _alternation;
    private RegexNode? _conjunction;
    private RegexNode? _concatenation;
    private RegexNode? _unit;
    private readonly string _pattern;
    private int _currentPos;

    private readonly CultureInfo _culture;
    private RegexCaseBehavior _caseBehavior;
    private bool _hasIgnoreCaseBackreferenceNodes;
    private int _autocap;
    private int _capcount;
    private int _captop;
    private readonly int _capsize;
    private readonly Hashtable _caps;
    private Hashtable? _capnames;
    private int[]? _capnumlist;
    private List<string>? _capnamelist;

    private RegexOptions _options;

    // NOTE: _optionsStack is ValueListBuilder<int> to ensure that
    //       ArrayPool<int>.Shared, not ArrayPool<RegexOptions>.Shared,
    //       will be created if the stackalloc'd capacity is ever exceeded.
    private ValueListBuilder<int> _optionsStack;
    private bool _ignoreNextParen; // flag to skip capturing a parentheses group
    private bool _wasPrevNegation;  // NEGATION

    private ExtendedRegexParser(string pattern, RegexOptions options, CultureInfo culture, Hashtable caps, int capsize,
        Hashtable? capnames, Span<int> optionSpan)
    {
        Debug.Assert(pattern != null, "Pattern must be set");
        Debug.Assert(culture != null, "Culture must be set");
        _pattern = pattern;
        _options = options;
        _culture = culture;
        _caseBehavior = default;
        _hasIgnoreCaseBackreferenceNodes = false;
        _caps = caps;
        _capsize = capsize;
        _capnames = capnames;
        _optionsStack = new ValueListBuilder<int>(optionSpan);
        _stack = null;
        _group = null;
        _alternation = null;
        _concatenation = null;
        _unit = null;
        _currentPos = 0;
        _autocap = 0;
        _capcount = 0;
        _captop = 0;
        _capnumlist = null;
        _capnamelist = null;
        _ignoreNextParen = false;
    }


    public static Resharp.Runtime.RegexTree Parse(string pattern, RegexOptions options, CultureInfo culture)
    {
        using var parser = new ExtendedRegexParser(pattern, options, culture, new Hashtable(), 0, null,
            stackalloc int[OptionStackDefaultSize]);
        parser.CountCaptures(out _);
        parser.Reset(options);
        var root = parser.ScanRegex();
        var captureNumberList = parser._capnumlist;
        var sparseMapping = parser._caps;
        var captop = parser._captop;
        int captureCount;
        if (captureNumberList == null || captop == captureNumberList.Length)
        {
            // The capture list isn't sparse.  Null out the capture mapping as it's not necessary,
            // and store the number of captures.
            captureCount = captop;
            sparseMapping = null;
        }
        else
        {
            // The capture list is sparse.  Store the number of captures, and populate the number-to-names-list.
            captureCount = captureNumberList.Length;
            for (var i = 0; i < captureNumberList.Length; i++) sparseMapping[captureNumberList[i]] = i;
        }

        return new RegexTree(root, captureCount, parser._capnamelist?.ToArray(), parser._capnames!, sparseMapping,
            options, parser._hasIgnoreCaseBackreferenceNodes ? culture : null);
    }


    /// <summary>
    /// Escapes all metacharacters (including |,(,),[,{,|,^,$,*,+,?,\, spaces and #)
    /// </summary>
    public static string Escape(string input)
    {
        var indexOfMetachar = IndexOfMetachar(input.AsSpan());
        return indexOfMetachar < 0 ? input : EscapeImpl(input.AsSpan(), indexOfMetachar);
    }

    private static string EscapeImpl(ReadOnlySpan<char> input, int indexOfMetachar)
    {
        // For small inputs we allocate on the stack. In most cases a buffer three
        // times larger the original string should be sufficient as usually not all
        // characters need to be encoded.
        // For larger string we rent the input string's length plus a fixed
        // conservative amount of chars from the ArrayPool.
        var vsb = input.Length <= EscapeMaxBufferSize / 3
            ? new ValueStringBuilder(stackalloc char[EscapeMaxBufferSize])
            : new ValueStringBuilder(input.Length + 200);
        while (true)
        {
            vsb.Append(input.Slice(0, indexOfMetachar));
            input = input.Slice(indexOfMetachar);
            if (input.IsEmpty) break;

            var ch = input[0];
            switch (ch)
            {
                case '\n':
                    ch = 'n';
                    break;
                case '\r':
                    ch = 'r';
                    break;
                case '\t':
                    ch = 't';
                    break;
                case '\f':
                    ch = 'f';
                    break;
            }

            vsb.Append('\\');
            vsb.Append(ch);
            input = input.Slice(1);
            indexOfMetachar = IndexOfMetachar(input);
            if (indexOfMetachar < 0) indexOfMetachar = input.Length;
        }

        return vsb.ToString();
    }

    /// <summary>
    /// Unescapes all metacharacters (including (,),[,],{,},|,^,$,*,+,?,\, spaces and #)
    /// </summary>
    public static string Unescape(string input)
    {
        var i = input.IndexOf('\\');
        return i >= 0 ? UnescapeImpl(input, i) : input;
    }

    private static string UnescapeImpl(string input, int i)
    {
        using var parser = new ExtendedRegexParser(input, RegexOptions.None, CultureInfo.InvariantCulture,
            new Hashtable(), 0, null, stackalloc int[OptionStackDefaultSize]);
        // In the worst case the escaped string has the same length.
        // For small inputs we use stack allocation.
        var vsb = input.Length <= EscapeMaxBufferSize
            ? new ValueStringBuilder(stackalloc char[EscapeMaxBufferSize])
            : new ValueStringBuilder(input.Length);
        vsb.Append(input.AsSpan(0, i));
        do
        {
            i++;
            parser.Textto(i);
            if (i < input.Length) vsb.Append(parser.ScanCharEscape());

            i = parser.Textpos();
            var lastpos = i;
            while (i < input.Length && input[i] != '\\') i++;

            vsb.Append(input.AsSpan(lastpos, i - lastpos));
        } while (i < input.Length);

        return vsb.ToString();
    }

    /// <summary>
    /// Resets parsing to the beginning of the pattern.
    /// </summary>
    private void Reset(RegexOptions options)
    {
        _currentPos = 0;
        _autocap = 1;
        _ignoreNextParen = false;
        _optionsStack.Length = 0;
        _options = options;
        _stack = null;
    }

    public void Dispose()
    {
        _optionsStack.Dispose();
    }

    /*
     * The main parsing function.
     */
    private RegexNode ScanRegex()
    {
        char ch;
        var isQuantifier = false;
        var isNegation = false;
        // For the main Capture object, strip out the IgnoreCase option. The rest of the nodes will strip it out depending on the content
        // of each node.
        StartGroup(new RegexNode(RegexNodeKind.Capture, _options & ~RegexOptions.IgnoreCase, 0, -1));
        while (CharsRight() > 0)
        {
            var wasPrevQuantifier = isQuantifier;
            isQuantifier = false;
            _wasPrevNegation = isNegation;
            isNegation = false;
            ScanBlank();
            var startpos = Textpos();
            // move past all of the normal characters.  We'll stop when we hit some kind of control character,
            // or if IgnorePatternWhiteSpace is on, we'll stop when we see some whitespace.
            if (UseOptionX())
            {
                while (CharsRight() > 0 && (!IsStopperX(ch = RightChar()) || (ch == '{' && !IsTrueQuantifier())))
                    MoveRight();
            }
            else
            {
                if (_wasPrevNegation && (!IsSpecial(ch = RightChar()) || (ch == '{' && !IsTrueQuantifier()))) { MoveRight(); }
                else
                {
                    while (CharsRight() > 0 &&
                           (!IsSpecial(ch = RightChar()) || (ch == '{' && !IsTrueQuantifier()))) MoveRight();
                }
            }


            var endpos = Textpos();
            ScanBlank();
            if (CharsRight() == 0)
            {
                ch = '!'; // nonspecial, means at end
            }
            else if (IsSpecial(ch = RightChar()))
            {
                isQuantifier = IsQuantifier(ch);
                MoveRight();
            }
            else
            {
                ch = ' '; // nonspecial, means at ordinary char
            }

            if (startpos < endpos)
            {
                var cchUnquantified = endpos - startpos - (isQuantifier ? 1 : 0);
                wasPrevQuantifier = false;
                // NEG: single char ~b
                if (_wasPrevNegation)
                {
                    var negatedNode =
                        RegexNode.CreateOneWithCaseConversion(_pattern[startpos], (_options | RegexOptions.Negated), _culture,
                            ref _caseBehavior);
                    _concatenation!.AddChild(negatedNode);

                    _wasPrevNegation = false;
                }
                // NEG: entire concat is negated
                else if (_options.HasFlag(RegexOptions.Negated))
                {
                    _concatenation!.Options |= RegexOptions.Negated;
                    _options &= ~RegexOptions.Negated;
                    if (cchUnquantified > 0) AddConcatenate(startpos, cchUnquantified, false);

                    if (isQuantifier) AddUnitOne(CharAt(endpos - 1));
                }
                else
                {
                    if (cchUnquantified > 0) AddConcatenate(startpos, cchUnquantified, false);

                    if (isQuantifier) AddUnitOne(CharAt(endpos - 1));
                }

            }


            switch (ch)
            {
                case '!':
                    goto BreakOuterScan;
                case ' ':
                    goto ContinueOuterScan;

                case '[':
                    {
                        var setString = ScanCharClass(UseOptionI(), false)!.ToStringClass();
                        _unit = new RegexNode(RegexNodeKind.Set, _options & ~RegexOptions.IgnoreCase, setString);
                    }

                    break;
                case '(':
                    PushOptions();
                    if (ScanGroupOpen() is RegexNode grouper)
                    {
                        PushGroup();
                        StartGroup(grouper);
                        // NEG: negate started group
                        // if (_wasPrevNegation)
                        // {
                        //     _options |= RegexOptions.Negated;
                        // }
                    }
                    else
                    {
                        PopKeepOptions();
                    }
                    continue;
                case '|':
                    // NEG: entire alternation
                    if (
                        (_concatenation!.Options & RegexOptions.Negated) != 0
                        && _alternation!.ChildCount() == 0
                    )
                    {
                        _concatenation.Options &= ~RegexOptions.Negated;
                        _alternation!.Options |= RegexOptions.Negated;
                    }
                    AddAlternate();
                    goto ContinueOuterScan;
                case '&':
                    AddConjunction();
                    goto ContinueOuterScan;
                case '~':
                    isNegation = true;
                    goto ContinueOuterScan;
                case ')':
                    if (EmptyStack())
                        throw MakeException(RegexParseError.InsufficientOpeningParentheses,
                            "InsufficientOpeningParentheses");

                    AddGroup();
                    PopGroup();
                    PopOptions();
                    if (Unit() == null) goto ContinueOuterScan;

                    break;
                case '\\':


                    if (CharsRight() == 0)
                        throw MakeException(RegexParseError.UnescapedEndingBackslash, "UnescapedEndingBackslash");

                    AddUnitNode(ScanBackslash(false)!);

                    // NEGATION: extend
                    if (_wasPrevNegation)
                        _unit!.Options |= RegexOptions.Negated;

                    break;
                case '^':
                    AddUnitType(UseOptionM() ? RegexNodeKind.Bol : RegexNodeKind.Beginning);
                    break;
                case '$':
                    AddUnitType(UseOptionM() ? RegexNodeKind.Eol : RegexNodeKind.EndZ);
                    break;
                case '.':
                    // NEG: negate the concat instead
                    if (_options.HasFlag(RegexOptions.Negated))
                    {
                        _concatenation!.Options |= RegexOptions.Negated;
                        _options &= ~RegexOptions.Negated;
                    }
                    _unit = UseOptionS()
                        ? new RegexNode(RegexNodeKind.Set, _options & ~RegexOptions.IgnoreCase, RegexCharClass.AnyClass)
                        : new RegexNode(RegexNodeKind.Notone, _options & ~RegexOptions.IgnoreCase, '\n');
                    break;
                case '_':
                    // NEG: negate the concat instead
                    if (_options.HasFlag(RegexOptions.Negated))
                    {
                        _concatenation!.Options |= RegexOptions.Negated;
                        _options &= ~RegexOptions.Negated;
                    }
                    _unit = new RegexNode(RegexNodeKind.Set, _options & ~RegexOptions.IgnoreCase,
                        RegexCharClass.AnyClass);
                    break;
                case '{':
                case '*':
                case '+':
                case '?':


                    if (Unit() == null)
                        throw wasPrevQuantifier
                            ? MakeException(RegexParseError.NestedQuantifiersNotParenthesized,
                                $"SR.NestedQuantifiersNotParenthesized:{ch}")
                            : MakeException(RegexParseError.QuantifierAfterNothing,
                                "QuantifierAfterNothing");

                    MoveLeft();
                    break;
                default:
                    Debug.Fail($"Unexpected char {ch}");
                    break;
            }

            ScanBlank();
            if (CharsRight() == 0 || !(isQuantifier = IsTrueQuantifier()))
            {
                AddConcatenate();
                goto ContinueOuterScan;
            }

            ch = RightCharMoveRight();


            // Handle quantifiers
            while (Unit() != null)
            {
                int min = 0, max = 0;
                switch (ch)
                {
                    case '*':
                        max = int.MaxValue;
                        break;
                    case '?':
                        max = 1;
                        break;
                    case '+':
                        min = 1;
                        max = int.MaxValue;
                        break;
                    case '{':
                        startpos = Textpos();
                        max = min = ScanDecimal();
                        if (startpos < Textpos())
                            if (CharsRight() > 0 && RightChar() == ',')
                            {
                                MoveRight();
                                max = CharsRight() == 0 || RightChar() == '}' ? int.MaxValue : ScanDecimal();
                            }

                        if (startpos == Textpos() || CharsRight() == 0 || RightCharMoveRight() != '}')
                        {
                            AddConcatenate();
                            Textto(startpos - 1);
                            goto ContinueOuterScan;
                        }

                        break;
                    default:
                        Debug.Fail($"Unexpected char {ch}");
                        break;
                }

                ScanBlank();
                var lazy = false;
                if (CharsRight() != 0 && RightChar() == '?')
                {
                    MoveRight();
                    lazy = true;
                }

                if (min > max)
                    throw MakeException(RegexParseError.ReversedQuantifierRange, "ReversedQuantifierRange");

                AddConcatenate(lazy, min, max);
            }



        ContinueOuterScan:;
            _wasPrevNegation = false;
            // bad negation impl
            // if (isNegation)
            // {
            //     _concatenation!.Options |= RegexOptions.Negated;
            // }
        }

    BreakOuterScan:;
        if (!EmptyStack())
            throw MakeException(RegexParseError.InsufficientClosingParentheses, "InsufficientClosingParentheses");

        AddGroup();
        return Unit()!.FinalOptimize();
    }

    /*
     * Simple parsing for replacement patterns
     */
    private RegexNode ScanReplacement()
    {
        _concatenation = new RegexNode(RegexNodeKind.Concatenate, _options);
        while (true)
        {
            var c = CharsRight();
            if (c == 0) break;

            var startpos = Textpos();
            while (c > 0 && RightChar() != '$')
            {
                MoveRight();
                c--;
            }

            AddConcatenate(startpos, Textpos() - startpos, true);
            if (c > 0)
            {
                if (RightCharMoveRight() == '$')
                {
                    var node = ScanDollar();
                    AddUnitNode(node);
                }

                AddConcatenate();
            }
        }

        return _concatenation;
    }

    /*
     * Scans contents of [] (not including []'s), and converts to a
     * RegexCharClass.
     */
    private RegexCharClass? ScanCharClass(bool caseInsensitive, bool scanOnly)
    {
        char ch;
        var chPrev = '\0';
        var inRange = false;
        var firstChar = true;
        var closed = false;
        var charClass = scanOnly ? null : new RegexCharClass();
        if (CharsRight() > 0 && RightChar() == '^')
        {
            MoveRight();
            if (!scanOnly) charClass!.Negate = true;

            if ((_options & RegexOptions.ECMAScript) != 0 && CharAt(_currentPos) == ']') firstChar = false;
        }

        for (; CharsRight() > 0; firstChar = false)
        {
            var translatedChar = false;
            ch = RightCharMoveRight();
            if (ch == ']')
            {
                if (!firstChar)
                {
                    closed = true;
                    break;
                }
            }
            else if (ch == '\\' && CharsRight() > 0)
            {
                switch (ch = RightCharMoveRight())
                {
                    case 'D':
                    case 'd':
                        if (!scanOnly)
                        {
                            if (inRange)
                                throw MakeException(RegexParseError.ShorthandClassInCharacterRange,
                                    "ShorthandClassInCharacterRange");

                            charClass!.AddDigit(UseOptionE(), ch == 'D', _pattern, _currentPos);
                        }

                        continue;
                    case 'S':
                    case 's':
                        if (!scanOnly)
                        {
                            if (inRange)
                                throw MakeException(RegexParseError.ShorthandClassInCharacterRange,
                                    "ShorthandClassInCharacterRange");

                            charClass!.AddSpace(UseOptionE(), ch == 'S');
                        }

                        continue;
                    case 'W':
                    case 'w':
                        if (!scanOnly)
                        {
                            if (inRange)
                                throw MakeException(RegexParseError.ShorthandClassInCharacterRange,
                                    "ShorthandClassInCharacterRange");

                            charClass!.AddWord(UseOptionE(), ch == 'W');
                        }

                        continue;
                    case 'p':
                    case 'P':
                        if (!scanOnly)
                        {
                            if (inRange)
                                throw MakeException(RegexParseError.ShorthandClassInCharacterRange,
                                    "ShorthandClassInCharacterRange");

                            charClass!.AddCategoryFromName(ParseProperty(), ch != 'p', caseInsensitive, _pattern,
                                _currentPos);
                        }
                        else
                        {
                            ParseProperty();
                        }

                        continue;
                    case '-':
                        if (!scanOnly)
                        {
                            if (inRange)
                            {
                                if (chPrev > ch)
                                    throw MakeException(RegexParseError.ReversedCharacterRange,
                                        "ReversedCharacterRange");

                                charClass!.AddRange(chPrev, ch);
                                inRange = false;
                                chPrev = '\0';
                            }
                            else
                            {
                                charClass!.AddRange(ch, ch);
                            }
                        }

                        continue;
                    default:
                        MoveLeft();
                        ch = ScanCharEscape(); // non-literal character
                        translatedChar = true;
                        break; // this break will only break out of the switch
                }
            }
            else if (ch == '[')
            {
                // This is code for Posix style properties - [:Ll:] or [:IsTibetan:].
                // It currently doesn't do anything other than skip the whole thing!
                if (CharsRight() > 0 && RightChar() == ':' && !inRange)
                {
                    var savePos = Textpos();
                    MoveRight();
                    if (CharsRight() < 2 || RightCharMoveRight() != ':' || RightCharMoveRight() != ']') Textto(savePos);
                }
            }

            if (inRange)
            {
                inRange = false;
                if (!scanOnly)
                {
                    if (ch == '[' && !translatedChar && !firstChar)
                    {
                        // We thought we were in a range, but we're actually starting a subtraction.
                        // In that case, we'll add chPrev to our char class, skip the opening [, and
                        // scan the new character class recursively.
                        charClass!.AddChar(chPrev);
                        charClass.AddSubtraction(ScanCharClass(caseInsensitive, scanOnly)!);
                        if (CharsRight() > 0 && RightChar() != ']')
                            throw MakeException(RegexParseError.ExclusionGroupNotLast, "ExclusionGroupNotLast");
                    }
                    else
                    {
                        // a regular range, like a-z
                        if (chPrev > ch)
                            throw MakeException(RegexParseError.ReversedCharacterRange, "ExclusionGroupNotLast");

                        charClass!.AddRange(chPrev, ch);
                    }
                }
            }
            else if (CharsRight() >= 2 && RightChar() == '-' && RightChar(1) != ']')
            {
                // this could be the start of a range
                chPrev = ch;
                inRange = true;
                MoveRight();
            }
            else if (CharsRight() >= 1 && ch == '-' && !translatedChar && RightChar() == '[' && !firstChar)
            {
                // we aren't in a range, and now there is a subtraction.  Usually this happens
                // only when a subtraction follows a range, like [a-z-[b]]
                MoveRight();
                var rcc = ScanCharClass(caseInsensitive, scanOnly);
                if (!scanOnly)
                {
                    charClass!.AddSubtraction(rcc!);
                    if (CharsRight() > 0 && RightChar() != ']')
                        throw MakeException(RegexParseError.ExclusionGroupNotLast, "ExclusionGroupNotLast");
                }
            }
            else
            {
                if (!scanOnly) charClass!.AddRange(ch, ch);
            }
        }

        if (!closed) throw MakeException(RegexParseError.UnterminatedBracket, "UnterminatedBracket");

        if (!scanOnly && caseInsensitive) charClass!.AddCaseEquivalences(_culture);

        return charClass;
    }

    /*
     * Scans chars following a '(' (not counting the '('), and returns
     * a RegexNode for the type of group scanned, or null if the group
     * simply changed options (?cimsx-cimsx) or was a comment (#...).
     */
    private RegexNode? ScanGroupOpen()
    {
        // just return a RegexNode if we have:
        // 1. "(" followed by nothing
        // 2. "(x" where x != ?
        // 3. "(?)"
        if (CharsRight() == 0 || RightChar() != '?' || (RightChar() == '?' && CharsRight() > 1 && RightChar(1) == ')'))
        {
            if (_wasPrevNegation)
            {
                return new RegexNode(RegexNodeKind.Capture, _options | RegexOptions.Negated);
            }
            if (UseOptionN() || _ignoreNextParen)
            {
                _ignoreNextParen = false;
                return new RegexNode(RegexNodeKind.Group, _options);
            }
            else
            {
                return new RegexNode(RegexNodeKind.Capture, _options, _autocap++, -1);
            }
        }

        MoveRight();
        while (true)
        {
            if (CharsRight() == 0) break;

            RegexNodeKind nodeType;
            var close = '>';
            var ch = RightCharMoveRight();
            switch (ch)
            {
                case ':':
                    // noncapturing group
                    nodeType = RegexNodeKind.Group;
                    break;
                case '=':
                    // lookahead assertion
                    _options &= ~RegexOptions.RightToLeft;
                    nodeType = RegexNodeKind.PositiveLookaround;
                    break;
                case '!':
                    // negative lookahead assertion
                    _options &= ~RegexOptions.RightToLeft;
                    nodeType = RegexNodeKind.NegativeLookaround;
                    break;
                case '>':
                    // atomic subexpression
                    nodeType = RegexNodeKind.Atomic;
                    break;
                case '\'':
                    close = '\'';
                    goto case '<'; // fallthrough
                case '<':
                    if (CharsRight() == 0) goto BreakRecognize;

                    switch (ch = RightCharMoveRight())
                    {
                        case '=':
                            if (close == '\'') goto BreakRecognize;

                            // lookbehind assertion
                            _options |= RegexOptions.RightToLeft;
                            nodeType = RegexNodeKind.PositiveLookaround;
                            break;
                        case '!':
                            if (close == '\'') goto BreakRecognize;

                            // negative lookbehind assertion
                            _options |= RegexOptions.RightToLeft;
                            nodeType = RegexNodeKind.NegativeLookaround;
                            break;
                        default:
                            MoveLeft();
                            var capnum = -1;
                            var uncapnum = -1;
                            var proceed = false;
                            // grab part before -
                            if ((uint)(ch - '0') <= 9)
                            {
                                capnum = ScanDecimal();
                                if (!IsCaptureSlot(capnum)) capnum = -1;

                                // check if we have bogus characters after the number
                                if (CharsRight() > 0 && !(RightChar() == close || RightChar() == '-'))
                                    throw MakeException(RegexParseError.CaptureGroupNameInvalid,
                                        "CaptureGroupNameInvalid");

                                if (capnum == 0)
                                    throw MakeException(RegexParseError.CaptureGroupOfZero, "CaptureGroupOfZero");
                            }
                            else if (RegexCharClass.IsBoundaryWordChar(ch))
                            {
                                var capname = ScanCapname();
                                if (IsCaptureName(capname)) capnum = CaptureSlotFromName(capname);

                                // check if we have bogus character after the name
                                if (CharsRight() > 0 && !(RightChar() == close || RightChar() == '-'))
                                    throw MakeException(RegexParseError.CaptureGroupNameInvalid,
                                        "CaptureGroupNameInvalid");
                            }
                            else if (ch == '-')
                            {
                                proceed = true;
                            }
                            else
                            {
                                // bad group name - starts with something other than a word character and isn't a number
                                throw MakeException(RegexParseError.CaptureGroupNameInvalid,
                                    "CaptureGroupNameInvalid");
                            }

                            // grab part after - if any
                            if ((capnum != -1 || proceed) && CharsRight() > 1 && RightChar() == '-')
                            {
                                MoveRight();
                                ch = RightChar();
                                if ((uint)(ch - '0') <= 9)
                                {
                                    uncapnum = ScanDecimal();
                                    if (!IsCaptureSlot(uncapnum))
                                        throw MakeException(RegexParseError.UndefinedNumberedReference,
                                            "UndefinedNumberedReference");

                                    // check if we have bogus characters after the number
                                    if (CharsRight() > 0 && RightChar() != close)
                                        throw MakeException(RegexParseError.CaptureGroupNameInvalid,
                                            "CaptureGroupNameInvalid");
                                }
                                else if (RegexCharClass.IsBoundaryWordChar(ch))
                                {
                                    var uncapname = ScanCapname();
                                    if (IsCaptureName(uncapname))
                                        uncapnum = CaptureSlotFromName(uncapname);
                                    else
                                        throw MakeException(RegexParseError.UndefinedNamedReference,
                                            "UndefinedNamedReference");

                                    // check if we have bogus character after the name
                                    if (CharsRight() > 0 && RightChar() != close)
                                        throw MakeException(RegexParseError.CaptureGroupNameInvalid,
                                            "CaptureGroupNameInvalid");
                                }
                                else
                                {
                                    // bad group name - starts with something other than a word character and isn't a number
                                    throw MakeException(RegexParseError.CaptureGroupNameInvalid,
                                        "CaptureGroupNameInvalid");
                                }
                            }

                            // actually make the node
                            if ((capnum != -1 || uncapnum != -1) && CharsRight() > 0 && RightCharMoveRight() == close)
                                return new RegexNode(RegexNodeKind.Capture, _options, capnum, uncapnum);

                            goto BreakRecognize;
                    }

                    break;
                case '(':
                    // conditional alternation construct (?(...) | )
                    var parenPos = Textpos();
                    if (CharsRight() > 0)
                    {
                        ch = RightChar();
                        // check if the alternation condition is a backref
                        if (ch >= '0' && ch <= '9')
                        {
                            var capnum = ScanDecimal();
                            if (CharsRight() > 0 && RightCharMoveRight() == ')')
                            {
                                if (IsCaptureSlot(capnum))
                                    return new RegexNode(RegexNodeKind.BackreferenceConditional, _options, capnum);

                                throw MakeException(RegexParseError.AlternationHasUndefinedReference,
                                    "AlternationHasUndefinedReference");
                            }

                            throw MakeException(RegexParseError.AlternationHasMalformedReference,
                                "AlternationHasMalformedReference");
                        }
                        else if (RegexCharClass.IsBoundaryWordChar(ch))
                        {
                            var capname = ScanCapname();
                            if (IsCaptureName(capname) && CharsRight() > 0 && RightCharMoveRight() == ')')
                                return new RegexNode(RegexNodeKind.BackreferenceConditional, _options,
                                    CaptureSlotFromName(capname));
                        }
                    }

                    // not a backref
                    nodeType = RegexNodeKind.ExpressionConditional;
                    Textto(parenPos - 1); // jump to the start of the parentheses
                    _ignoreNextParen = true; // but make sure we don't try to capture the insides
                    var charsRight = CharsRight();
                    if (charsRight >= 3 && RightChar(1) == '?')
                    {
                        var rightchar2 = RightChar(2);
                        // disallow comments in the condition
                        if (rightchar2 == '#')
                            throw MakeException(RegexParseError.AlternationHasComment, "AlternationHasComment");

                        // disallow named capture group (?<..>..) in the condition
                        if (rightchar2 == '\'')
                            throw MakeException(RegexParseError.AlternationHasNamedCapture,
                                "AlternationHasNamedCapture");

                        if (charsRight >= 4 && rightchar2 == '<' && RightChar(3) != '!' && RightChar(3) != '=')
                            throw MakeException(RegexParseError.AlternationHasNamedCapture,
                                "AlternationHasNamedCapture");
                    }

                    break;
                default:
                    MoveLeft();
                    nodeType = RegexNodeKind.Group;
                    // Disallow options in the children of a testgroup node
                    if (_group!.Kind != RegexNodeKind.ExpressionConditional) ScanOptions();

                    if (CharsRight() == 0) goto BreakRecognize;

                    if ((ch = RightCharMoveRight()) == ')') return null;

                    if (ch != ':') goto BreakRecognize;

                    break;
            }

            return new RegexNode(nodeType, _options);
        }

    BreakRecognize:;
        // break Recognize comes here
        throw MakeException(RegexParseError.InvalidGroupingConstruct, "InvalidGroupingConstruct");
    }

    /*
     * Scans whitespace or x-mode comments.
     */
    private void ScanBlank()
    {
        if (UseOptionX())
            while (true)
            {
                while (CharsRight() > 0 && IsSpace(RightChar())) MoveRight();

                if (CharsRight() == 0) break;

                if (RightChar() == '#')
                {
                    while (CharsRight() > 0 && RightChar() != '\n') MoveRight();
                }
                else if (CharsRight() >= 3 && RightChar(2) == '#' && RightChar(1) == '?' && RightChar() == '(')
                {
                    while (CharsRight() > 0 && RightChar() != ')') MoveRight();

                    if (CharsRight() == 0)
                        throw MakeException(RegexParseError.UnterminatedComment, "UnterminatedComment");

                    MoveRight();
                }
                else
                {
                    break;
                }
            }
        else
            while (true)
            {
                if (CharsRight() < 3 || RightChar(2) != '#' || RightChar(1) != '?' || RightChar() != '(') return;

                // skip comment (?# ...)
                while (CharsRight() > 0 && RightChar() != ')') MoveRight();

                if (CharsRight() == 0) throw MakeException(RegexParseError.UnterminatedComment, "UnterminatedComment");

                MoveRight();
            }
    }

    /// <summary>
    /// Scans chars following a '\' (not counting the '\'), and returns
    /// a RegexNode for the type of atom scanned.
    /// </summary>
    private RegexNode? ScanBackslash(bool scanOnly)
    {
        Debug.Assert(CharsRight() > 0, "The current reading position must not be at the end of the pattern");
        char ch;
        switch (ch = RightChar())
        {
            case 'b':
            case 'B':
            case 'A':
            case 'G':
            case 'Z':
            case 'z':
                MoveRight();
                return scanOnly ? null : new RegexNode(TypeFromCode(ch), _options);
            case 'w':
                MoveRight();
                return scanOnly
                    ? null
                    : new RegexNode(RegexNodeKind.Set, _options & ~RegexOptions.IgnoreCase,
                        UseOptionE() ? RegexCharClass.ECMAWordClass : RegexCharClass.WordClass);
            case 'T': // \T as TRUE
                MoveRight();
                return scanOnly
                    ? null
                    : new RegexNode(RegexNodeKind.Set, _options & ~RegexOptions.IgnoreCase,
                        RegexCharClass.NotSpaceSpaceClass);
            case 'W':
                MoveRight();
                return scanOnly
                    ? null
                    : new RegexNode(RegexNodeKind.Set, _options & ~RegexOptions.IgnoreCase,
                        UseOptionE() ? RegexCharClass.NotECMAWordClass : RegexCharClass.NotWordClass);
            case 's':
                MoveRight();
                return scanOnly
                    ? null
                    : new RegexNode(RegexNodeKind.Set, _options & ~RegexOptions.IgnoreCase,
                        UseOptionE() ? RegexCharClass.ECMASpaceClass : RegexCharClass.SpaceClass);
            case 'S':
                MoveRight();
                return scanOnly
                    ? null
                    : new RegexNode(RegexNodeKind.Set, _options & ~RegexOptions.IgnoreCase,
                        UseOptionE() ? RegexCharClass.NotECMASpaceClass : RegexCharClass.NotSpaceClass);
            case 'd':
                MoveRight();
                return scanOnly
                    ? null
                    : new RegexNode(RegexNodeKind.Set, _options & ~RegexOptions.IgnoreCase,
                        UseOptionE() ? RegexCharClass.ECMADigitClass : RegexCharClass.DigitClass);
            case 'D':
                MoveRight();
                return scanOnly
                    ? null
                    : new RegexNode(RegexNodeKind.Set, _options & ~RegexOptions.IgnoreCase,
                        UseOptionE() ? RegexCharClass.NotECMADigitClass : RegexCharClass.NotDigitClass);
            case 'p':
            case 'P':
                MoveRight();
                if (scanOnly) return null;

                var cc = new RegexCharClass();
                cc.AddCategoryFromName(ParseProperty(), ch != 'p', UseOptionI(), _pattern, _currentPos);
                if (UseOptionI()) cc.AddCaseEquivalences(_culture);

                return new RegexNode(RegexNodeKind.Set, _options & ~RegexOptions.IgnoreCase, cc.ToStringClass());
            default:
                var result = ScanBasicBackslash(scanOnly);
                if (result != null && result.Kind == RegexNodeKind.Backreference &&
                    (result.Options & RegexOptions.IgnoreCase) != 0) _hasIgnoreCaseBackreferenceNodes = true;

                return result;
        }
    }

    /// <summary>Scans \-style backreferences and character escapes</summary>
    private RegexNode? ScanBasicBackslash(bool scanOnly)
    {
        Debug.Assert(CharsRight() > 0, "The current reading position must not be at the end of the pattern");
        var backpos = Textpos();
        var close = '\0';
        var angled = false;
        var ch = RightChar();
        // allow \k<foo> instead of \<foo>, which is now deprecated
        if (ch == 'k')
        {
            if (CharsRight() >= 2)
            {
                MoveRight();
                ch = RightCharMoveRight();
                if (ch is '<' or '\'')
                {
                    angled = true;
                    close = ch == '\'' ? '\'' : '>';
                }
            }

            if (!angled || CharsRight() <= 0)
                throw MakeException(RegexParseError.MalformedNamedReference, "MalformedNamedReference");

            ch = RightChar();
        }
        // Note angle without \g
        else if ((ch == '<' || ch == '\'') && CharsRight() > 1)
        {
            angled = true;
            close = ch == '\'' ? '\'' : '>';
            MoveRight();
            ch = RightChar();
        }

        // Try to parse backreference: \<1>
        if (angled && ch >= '0' && ch <= '9')
        {
            var capnum = ScanDecimal();
            if (CharsRight() > 0 && RightCharMoveRight() == close)
                return scanOnly ? null :
                    IsCaptureSlot(capnum) ? new RegexNode(RegexNodeKind.Backreference, _options, capnum) :
                    throw MakeException(RegexParseError.UndefinedNumberedReference,
                        "UndefinedNumberedReference");
        }
        // Try to parse backreference or octal: \1
        else if (!angled && ch >= '1' && ch <= '9')
        {
            if (UseOptionE())
            {
                var capnum = -1;
                var newcapnum = ch - '0';
                var pos = Textpos() - 1;
                while (newcapnum <= _captop)
                {
                    if (IsCaptureSlot(newcapnum) && (_caps == null || (int)_caps[newcapnum]! < pos)) capnum = newcapnum;

                    MoveRight();
                    if (CharsRight() == 0 || (ch = RightChar()) < '0' || ch > '9') break;

                    newcapnum = newcapnum * 10 + (ch - '0');
                }

                if (capnum >= 0) return scanOnly ? null : new RegexNode(RegexNodeKind.Backreference, _options, capnum);
            }
            else
            {
                var capnum = ScanDecimal();
                if (scanOnly) return null;

                if (IsCaptureSlot(capnum)) return new RegexNode(RegexNodeKind.Backreference, _options, capnum);

                if (capnum <= 9) throw new Exception($"{RegexParseError.UndefinedNumberedReference}, {capnum}");
            }
        }
        // Try to parse backreference: \<foo>
        else if (angled && RegexCharClass.IsBoundaryWordChar(ch))
        {
            var capname = ScanCapname();
            if (CharsRight() > 0 && RightCharMoveRight() == close)
                return scanOnly
                    ? null
                    :
                    IsCaptureName(capname)
                        ?
                        new RegexNode(RegexNodeKind.Backreference, _options, CaptureSlotFromName(capname))
                        : throw MakeException(RegexParseError.UndefinedNamedReference,
                            "UndefinedNamedReference");
        }

        // Not backreference: must be char code
        Textto(backpos);
        ch = ScanCharEscape();
        return !scanOnly ? RegexNode.CreateOneWithCaseConversion(ch, _options, _culture, ref _caseBehavior) : null;
    }

    /*
     * Scans $ patterns recognized within replacement patterns
     */
    private RegexNode ScanDollar()
    {
        if (CharsRight() == 0) return RegexNode.CreateOneWithCaseConversion('$', _options, _culture, ref _caseBehavior);

        var ch = RightChar();
        bool angled;
        var backpos = Textpos();
        var lastEndPos = backpos;
        // Note angle
        if (ch == '{' && CharsRight() > 1)
        {
            angled = true;
            MoveRight();
            ch = RightChar();
        }
        else
        {
            angled = false;
        }

        // Try to parse backreference: \1 or \{1} or \{cap}
        if (ch >= '0' && ch <= '9')
        {
            if (!angled && UseOptionE())
            {
                var capnum = -1;
                var newcapnum = ch - '0';
                MoveRight();
                if (IsCaptureSlot(newcapnum))
                {
                    capnum = newcapnum;
                    lastEndPos = Textpos();
                }

                while (CharsRight() > 0 && (ch = RightChar()) >= '0' && ch <= '9')
                {
                    var digit = ch - '0';
                    if (newcapnum > MaxValueDiv10 || (newcapnum == MaxValueDiv10 && digit > MaxValueMod10))
                        throw MakeException(RegexParseError.QuantifierOrCaptureGroupOutOfRange,
                            "QuantifierOrCaptureGroupOutOfRange");

                    newcapnum = newcapnum * 10 + digit;
                    MoveRight();
                    if (IsCaptureSlot(newcapnum))
                    {
                        capnum = newcapnum;
                        lastEndPos = Textpos();
                    }
                }

                Textto(lastEndPos);
                if (capnum >= 0) return new RegexNode(RegexNodeKind.Backreference, _options, capnum);
            }
            else
            {
                var capnum = ScanDecimal();
                if (!angled || (CharsRight() > 0 && RightCharMoveRight() == '}'))
                    if (IsCaptureSlot(capnum))
                        return new RegexNode(RegexNodeKind.Backreference, _options, capnum);
            }
        }
        else if (angled && RegexCharClass.IsBoundaryWordChar(ch))
        {
            var capname = ScanCapname();
            if (CharsRight() > 0 && RightCharMoveRight() == '}')
                if (IsCaptureName(capname))
                    return new RegexNode(RegexNodeKind.Backreference, _options, CaptureSlotFromName(capname));
        }
        else if (!angled)
        {
            var capnum = 1;
            switch (ch)
            {
                case '$':
                    MoveRight();
                    return RegexNode.CreateOneWithCaseConversion('$', _options, _culture, ref _caseBehavior);
                case '&':
                    capnum = 0;
                    break;
                case '`':
                    capnum = -1;
                    break;
                case '\'':
                    capnum = -2;
                    break;
                case '+':
                    capnum = -3;
                    break;
                case '_':
                    capnum = -4;
                    break;
            }

            if (capnum != 1)
            {
                MoveRight();
                return new RegexNode(RegexNodeKind.Backreference, _options, capnum);
            }
        }

        // unrecognized $: literalize
        Textto(backpos);
        return RegexNode.CreateOneWithCaseConversion('$', _options, _culture, ref _caseBehavior);
    }

    /*
     * Scans a capture name: consumes word chars
     */
    private string ScanCapname()
    {
        var startpos = Textpos();
        while (CharsRight() > 0)
            if (!RegexCharClass.IsBoundaryWordChar(RightCharMoveRight()))
            {
                MoveLeft();
                break;
            }

        return _pattern.Substring(startpos, Textpos() - startpos);
    }

    /*
     * Scans up to three octal digits (stops before exceeding 0377).
     */
    private char ScanOctal()
    {
        // Consume octal chars only up to 3 digits and value 0377
        var c = 3;
        if (c > CharsRight()) c = CharsRight();

        int d;
        int i;
        for (i = 0; c > 0 && (uint)(d = RightChar() - '0') <= 7; c -= 1)
        {
            MoveRight();
            i = i * 8 + d;
            if (UseOptionE() && i >= 0x20) break;
        }

        // Octal codes only go up to 255.  Any larger and the behavior that Perl follows
        // is simply to truncate the high bits.
        i &= 0xFF;
        return (char)i;
    }

    /*
     * Scans any number of decimal digits (pegs value at 2^31-1 if too large)
     */
    private int ScanDecimal()
    {
        var i = 0;
        int d;
        while (CharsRight() > 0 && (uint)(d = (char)(RightChar() - '0')) <= 9)
        {
            MoveRight();
            if (i > MaxValueDiv10 || (i == MaxValueDiv10 && d > MaxValueMod10))
                throw MakeException(RegexParseError.QuantifierOrCaptureGroupOutOfRange,
                    "QuantifierOrCaptureGroupOutOfRange");

            i = i * 10 + d;
        }

        return i;
    }

    /*
     * Scans exactly c hex digits (c=2 for \xFF, c=4 for \uFFFF)
     */
    private char ScanHex(int c)
    {
        var i = 0;
        int d;
        if (CharsRight() >= c)
            for (; c > 0 && (d = HexDigit(RightCharMoveRight())) >= 0; c -= 1)
                i = i * 0x10 + d;

        if (c > 0)
            throw MakeException(RegexParseError.InsufficientOrInvalidHexDigits, "InsufficientOrInvalidHexDigits");

        return (char)i;
    }

    /*
     * Returns n <= 0xF for a hex digit.
     */
    private static int HexDigit(char ch)
    {
        int d;
        if ((uint)(d = ch - '0') <= 9)
            return d;
        if ((uint)(d = ch - 'a') <= 5)
            return d + 0xa;
        if ((uint)(d = ch - 'A') <= 5)
            return d + 0xa;
        return -1;
    }

    /*
     * Grabs and converts an ASCII control character
     */
    private char ScanControl()
    {
        if (CharsRight() == 0) throw MakeException(RegexParseError.MissingControlCharacter, "MissingControlCharacter");

        var ch = RightCharMoveRight();
        // \ca interpreted as \cA
        if ((uint)(ch - 'a') <= 'z' - 'a') ch = (char)(ch - ('a' - 'A'));

        if ((ch = (char)(ch - '@')) < ' ') return ch;

        throw MakeException(RegexParseError.UnrecognizedControlCharacter, "UnrecognizedControlCharacter");
    }

    /// <summary>Scans cimsx-cimsx option string, stops at the first unrecognized char.</summary>
    private void ScanOptions()
    {
        for (var off = false; CharsRight() > 0; MoveRight())
        {
            var ch = RightChar();
            if (ch == '-')
            {
                off = true;
            }
            else if (ch == '+')
            {
                off = false;
            }
            else
            {
                var options = OptionFromCode(ch);
                if (options == 0) return;

                if (off)
                    _options &= ~options;
                else
                    _options |= options;
            }
        }
    }

    /// <summary>Scans \ code for escape codes that map to single Unicode chars.</summary>
    private char ScanCharEscape()
    {
        var ch = RightCharMoveRight();
        if (ch >= '0' && ch <= '7')
        {
            MoveLeft();
            return ScanOctal();
        }

        switch (ch)
        {
            case 'x':
                return ScanHex(2);
            case 'u':
                return ScanHex(4);
            case 'a':
                return '\u0007';
            case 'b':
                return '\b';
            case 'e':
                return '\u001B';
            case 'f':
                return '\f';
            case 'n':
                return '\n';
            case 'r':
                return '\r';
            case 't':
                return '\t';
            case '_':
                return '_';
            case 'v':
                return '\u000B';
            case 'c':
                return ScanControl();
            default:
                if (!UseOptionE() && RegexCharClass.IsBoundaryWordChar(ch))
                    throw MakeException(RegexParseError.UnrecognizedEscape, "UnrecognizedEscape");

                return ch;
        }
    }

    /// <summary>Scans X for \p{X} or \P{X}</summary>
    private string ParseProperty()
    {
        if (CharsRight() < 3)
            throw MakeException(RegexParseError.InvalidUnicodePropertyEscape, "InvalidUnicodePropertyEscape");

        var ch = RightCharMoveRight();
        if (ch != '{')
            throw MakeException(RegexParseError.MalformedUnicodePropertyEscape, "MalformedUnicodePropertyEscape");

        var startpos = Textpos();
        while (CharsRight() > 0)
        {
            ch = RightCharMoveRight();
            if (!(RegexCharClass.IsBoundaryWordChar(ch) || ch == '-'))
            {
                MoveLeft();
                break;
            }
        }

        var capname = _pattern.Substring(startpos, Textpos() - startpos);
        if (CharsRight() == 0 || RightCharMoveRight() != '}')
            throw MakeException(RegexParseError.InvalidUnicodePropertyEscape, "InvalidUnicodePropertyEscape");

        return capname;
    }

    /// <summary>Returns the node kind for zero-length assertions with a \ code.</summary>
    private RegexNodeKind TypeFromCode(char ch)
    {
        return ch switch
        {
            'b' => UseOptionE() ? RegexNodeKind.ECMABoundary : RegexNodeKind.Boundary,
            'B' => UseOptionE() ? RegexNodeKind.NonECMABoundary : RegexNodeKind.NonBoundary,
            'a' => RegexNodeKind.Beginning, // adding \a because why not
            'A' => RegexNodeKind.Beginning,
            'G' => RegexNodeKind.Start,
            'Z' => RegexNodeKind.EndZ,
            'z' => RegexNodeKind.End,
            _ => RegexNodeKind.Nothing
        };
    }

    /// <summary>Returns option bit from single-char (?imnsx) code.</summary>
    private static RegexOptions OptionFromCode(char ch)
    {
        return (char)(ch | 0x20) switch
        {
            'i' => RegexOptions.IgnoreCase,
            'm' => RegexOptions.Multiline,
            'n' => RegexOptions.ExplicitCapture,
            's' => RegexOptions.Singleline,
            'x' => RegexOptions.IgnorePatternWhitespace,
            _ => RegexOptions.None
        };
    }

    /// <summary>
    /// A prescanner for deducing the slots used for captures by doing a partial tokenization of the pattern.
    /// </summary>
    private void CountCaptures(out RegexOptions optionsFoundInPattern)
    {
        NoteCaptureSlot(0, 0);
        optionsFoundInPattern = RegexOptions.None;
        _autocap = 1;
        while (CharsRight() > 0)
        {
            var pos = Textpos();
            var ch = RightCharMoveRight();
            switch (ch)
            {
                case '\\':
                    if (CharsRight() > 0) ScanBackslash(true);

                    break;
                case '#':
                    if (UseOptionX())
                    {
                        MoveLeft();
                        ScanBlank();
                    }

                    break;
                case '[':
                    ScanCharClass(false, true);
                    break;
                case ')':
                    if (!EmptyOptionsStack()) PopOptions();

                    break;
                case '(':
                    if (CharsRight() >= 2 && RightChar(1) == '#' && RightChar() == '?')
                    {
                        // we have a comment (?#
                        MoveLeft();
                        ScanBlank();
                    }
                    else
                    {
                        PushOptions();
                        if (CharsRight() > 0 && RightChar() == '?')
                        {
                            // we have (?...
                            MoveRight();
                            if (CharsRight() > 1 && (RightChar() == '<' || RightChar() == '\''))
                            {
                                // named group: (?<... or (?'...
                                MoveRight();
                                ch = RightChar();
                                if (ch != '0' && RegexCharClass.IsBoundaryWordChar(ch))
                                {
                                    if ((uint)(ch - '1') <= '9' - '1')
                                        NoteCaptureSlot(ScanDecimal(), pos);
                                    else
                                        NoteCaptureName(ScanCapname(), pos);
                                }
                            }
                            else
                            {
                                // (?...
                                // get the options if it's an option construct (?cimsx-cimsx...)
                                ScanOptions();
                                optionsFoundInPattern |= _options;
                                if (CharsRight() > 0)
                                {
                                    if (RightChar() == ')')
                                    {
                                        // (?cimsx-cimsx)
                                        MoveRight();
                                        PopKeepOptions();
                                    }
                                    else if (RightChar() == '(')
                                    {
                                        // alternation construct: (?(foo)yes|no)
                                        // ignore the next paren so we don't capture the condition
                                        _ignoreNextParen = true;
                                        // break from here so we don't reset _ignoreNextParen
                                        break;
                                    }
                                }
                            }
                        }
                        else
                        {
                            // Simple (unnamed) capture group.
                            // Add unnamed parentheses if ExplicitCapture is not set
                            // and the next parentheses is not ignored.
                            if (!UseOptionN() && !_ignoreNextParen) NoteCaptureSlot(_autocap++, pos);
                        }
                    }

                    _ignoreNextParen = false;
                    break;
            }
        }

        AssignNameSlots();
    }

    /// <summary>Notes a used capture slot</summary>
    private void NoteCaptureSlot(int i, int pos)
    {
        object boxedI = i; // workaround to remove a boxed int when adding to the hashtable
        if (!_caps.ContainsKey(boxedI))
        {
            // the rhs of the hashtable isn't used in the parser
            _caps.Add(boxedI, pos);
            _capcount++;
            if (_captop <= i) _captop = i == int.MaxValue ? i : i + 1;
        }
    }

    /// <summary>Notes a used capture slot</summary>
    private void NoteCaptureName(string name, int pos)
    {
        if (_capnames == null)
        {
            _capnames = new Hashtable();
            _capnamelist = new List<string>();
        }

        if (!_capnames.ContainsKey(name))
        {
            _capnames.Add(name, pos);
            _capnamelist!.Add(name);
        }
    }

    /// <summary>Assigns unused slot numbers to the capture names.</summary>
    private void AssignNameSlots()
    {
        if (_capnames != null)
            for (var i = 0; i < _capnamelist!.Count; i++)
            {
                while (IsCaptureSlot(_autocap)) _autocap++;

                var name = _capnamelist[i];
                var pos = (int)_capnames[name]!;
                _capnames[name] = _autocap;
                NoteCaptureSlot(_autocap, pos);
                _autocap++;
            }

        // if the caps array has at least one gap, construct the list of used slots
        if (_capcount < _captop)
        {
            _capnumlist = new int[_capcount];
            var i = 0;
            // Manual use of IDictionaryEnumerator instead of foreach to avoid DictionaryEntry box allocations.
            var de = _caps.GetEnumerator();
            while (de.MoveNext()) _capnumlist[i++] = (int)de.Key;

            Array.Sort(_capnumlist);
        }

        // merge capsnumlist into capnamelist
        if (_capnames != null || _capnumlist != null)
        {
            List<string>? oldcapnamelist;
            int next;
            var k = 0;
            if (_capnames == null)
            {
                oldcapnamelist = null;
                _capnames = new Hashtable();
                _capnamelist = new List<string>();
                next = -1;
            }
            else
            {
                oldcapnamelist = _capnamelist;
                _capnamelist = new List<string>();
                next = (int)_capnames[oldcapnamelist![0]]!;
            }

            for (var i = 0; i < _capcount; i++)
            {
                var j = _capnumlist == null ? i : _capnumlist[i];
                if (next == j)
                {
                    _capnamelist.Add(oldcapnamelist![k++]);
                    next = k == oldcapnamelist.Count ? -1 : (int)_capnames[oldcapnamelist[k]]!;
                }
                else
                {
                    var str = j.ToString(_culture);
                    _capnamelist.Add(str);
                    _capnames[str] = j;
                }
            }
        }
    }

    /// <summary>Looks up the slot number for a given name.</summary>
    private int CaptureSlotFromName(string capname)
    {
        return (int)_capnames![capname]!;
    }

    /// <summary>True if the capture slot was noted</summary>
    private bool IsCaptureSlot(int i)
    {
        if (_caps != null) return _caps.ContainsKey(i);

        return i >= 0 && i < _capsize;
    }

    /// <summary>
    /// When generating code on a regex that uses a sparse set
    /// of capture slots, we hash them to a dense set of indices
    /// for an array of capture slots. Instead of doing the hash
    /// at match time, it's done at compile time, here.
    /// </summary>
    public static int MapCaptureNumber(int capnum, Hashtable? caps)
    {
        return capnum == -1 ? -1 : caps != null ? (int)caps[capnum]! : capnum;
    }

    /// <summary>Looks up the slot number for a given name</summary>
    private bool IsCaptureName(string capname)
    {
        return _capnames != null && _capnames.ContainsKey(capname);
    }

    /// <summary>True if N option disabling '(' autocapture is on.</summary>
    private bool UseOptionN()
    {
        return (_options & RegexOptions.ExplicitCapture) != 0;
    }

    /// <summary>True if I option enabling case-insensitivity is on.</summary>
    private bool UseOptionI()
    {
        return (_options & RegexOptions.IgnoreCase) != 0;
    }

    /// <summary>True if M option altering meaning of $ and ^ is on.</summary>
    private bool UseOptionM()
    {
        return (_options & RegexOptions.Multiline) != 0;
    }

    /// <summary>True if S option altering meaning of . is on.</summary>
    private bool UseOptionS()
    {
        return (_options & RegexOptions.Singleline) != 0;
    }

    /// <summary> True if X option enabling whitespace/comment mode is on.</summary>
    private bool UseOptionX()
    {
        return (_options & RegexOptions.IgnorePatternWhitespace) != 0;
    }

    /// <summary>True if E option enabling ECMAScript behavior is on.</summary>
    private bool UseOptionE()
    {
        return (_options & RegexOptions.ECMAScript) != 0;
    }

    private const byte Q = 5; // quantifier
    private const byte S = 4; // ordinary stopper
    private const byte Z = 3; // ScanBlank stopper
    private const byte X = 2; // whitespace
    private const byte E = 1; // should be escaped

    /// <summary>For categorizing ASCII characters.</summary>
    private static ReadOnlySpan<byte> Category => new byte[]
    {
     // 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F  0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
        0, 0, 0, 0, 0, 0, 0, 0, 0, X, X, 0, X, X, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     //    !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /  0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?
        X, 0, 0, Z, S, 0, S, 0, S, S, Q, Q, 0, 0, S, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, Q,
     // @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, S, S, 0, S, S,
     // '  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z  {  |  }  ~
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, Q, S, 0, S, 0
    };

    private static int IndexOfMetachar(ReadOnlySpan<char> input)
    {
        for (var i = 0; i < input.Length; i++)
            if (IsMetachar(input[i]))
                return i;

        return -1;
    }

    // #endif
    /// <summary>Returns true for those characters that terminate a string of ordinary chars.</summary>
    private static bool IsSpecial(char ch)
    {
        return ch <= '~' && Category[ch] >= S;
    }

    /// <summary>Returns true for those characters that terminate a string of ordinary chars.</summary>
    private static bool IsStopperX(char ch)
    {
        return ch <= '~' && Category[ch] >= X;
    }

    /// <summary>Returns true for those characters that begin a quantifier.</summary>
    private static bool IsQuantifier(char ch)
    {
        return ch <= '{' && Category[ch] >= Q;
    }

    private bool IsTrueQuantifier()
    {
        Debug.Assert(CharsRight() > 0, "The current reading position must not be at the end of the pattern");
        var startpos = Textpos();
        var ch = CharAt(startpos);
        if (ch != '{') return ch <= '{' && Category[ch] >= Q;

        var pos = startpos;
        var nChars = CharsRight();
        while (--nChars > 0 && (uint)((ch = CharAt(++pos)) - '0') <= 9)
            ;
        if (nChars == 0 || pos - startpos == 1) return false;

        if (ch == '}') return true;

        if (ch != ',') return false;

        while (--nChars > 0 && (uint)((ch = CharAt(++pos)) - '0') <= 9) { }

        return nChars > 0 && ch == '}';
    }

    /// <summary>Returns true for whitespace.</summary>
    private static bool IsSpace(char ch)
    {
        return ch <= ' ' && Category[ch] == X;
    }

    /// <summary>Returns true for chars that should be escaped.</summary>
    private static bool IsMetachar(char ch)
    {
        return ch <= '|' && Category[ch] >= E;
    }

    /// <summary>Add a string to the last concatenate.</summary>
    private void AddConcatenate(int pos, int cch, bool isReplacement)
    {

        switch (cch)
        {
            case 0:
                return;
            case 1:
                _concatenation!.AddChild(RegexNode.CreateOneWithCaseConversion(_pattern[pos],
                    isReplacement ? _options & ~RegexOptions.IgnoreCase : _options, _culture, ref _caseBehavior));
                break;
            case > 1 when !UseOptionI() || isReplacement ||
                          !RegexCharClass.ParticipatesInCaseConversion(_pattern.AsSpan(pos, cch)):
                _concatenation!.AddChild(new RegexNode(RegexNodeKind.Multi, _options & ~RegexOptions.IgnoreCase,
                    _pattern.Substring(pos, cch)));
                break;
            default:
                foreach (var c in _pattern.AsSpan(pos, cch))
                    _concatenation!.AddChild(
                        RegexNode.CreateOneWithCaseConversion(c, _options, _culture, ref _caseBehavior));

                break;
        }
    }

    /// <summary>Push the parser state (in response to an open paren)</summary>
    private void PushGroup()
    {
        _group!.Parent = _stack;
        _alternation!.Parent = _group;
        _conjunction!.Parent = _group;
        // handle conjunction too
        if (_concatenation!.Parent != null && _concatenation.Parent.Kind == RegexNodeKind.Conjunction)
            _concatenation!.Parent.Parent = _alternation;
        else
            _concatenation!.Parent = _alternation;
        _stack = _concatenation;
    }

    /// <summary>Remember the pushed state (in response to a ')')</summary>
    private void PopGroup()
    {
        _concatenation = _stack;

        // handle conjunction too
        if (_concatenation!.Parent != null && _concatenation.Parent.Kind == RegexNodeKind.Conjunction)
            _alternation = _concatenation!.Parent.Parent;
        else
            _alternation = _concatenation!.Parent;
        // ---
        _group = _alternation!.Parent;
        _stack = _group!.Parent;
        // The first () inside a Testgroup group goes directly to the group
        if (_group.Kind == RegexNodeKind.ExpressionConditional && _group.ChildCount() == 0)
        {
            if (_unit == null)
                throw MakeException(RegexParseError.AlternationHasMalformedCondition,
                    "AlternationHasMalformedCondition");

            _group.AddChild(_unit);
            _unit = null;
        }
    }

    /// <summary>True if the group stack is empty.</summary>
    private bool EmptyStack()
    {
        return _stack == null;
    }

    /// <summary>Start a new round for the parser state (in response to an open paren or string start)</summary>
    private void StartGroup(RegexNode openGroup)
    {
        _group = openGroup;
        _alternation = new RegexNode(RegexNodeKind.Alternate, _options);
        _conjunction = new RegexNode(RegexNodeKind.Conjunction, _options);
        _concatenation = new RegexNode(RegexNodeKind.Concatenate, _options);
    }

    /// <summary>Finish the current concatenation (in response to a |)</summary>
    private void AddAlternate()
    {
        if (_concatenation!.Parent is not null && _concatenation!.Parent!.Kind is RegexNodeKind.Conjunction)
        {
            _concatenation = _concatenation.Parent;
        }
        // The | parts inside a Testgroup group go directly to the group
        if (_group!.Kind is RegexNodeKind.ExpressionConditional or RegexNodeKind.BackreferenceConditional)
            _group.AddChild(_concatenation!.ReverseConcatenationIfRightToLeft());
        else
            _alternation!.AddChild(_concatenation!.ReverseConcatenationIfRightToLeft());


        _concatenation = new RegexNode(RegexNodeKind.Concatenate, _options);
    }

    private void AddConjunction()
    {
        // The & parts inside a Testgroup group go directly to the group
        if (_group!.Kind is RegexNodeKind.ExpressionConditional or RegexNodeKind.BackreferenceConditional)
        {
            _group.AddChild(_concatenation!.ReverseConcatenationIfRightToLeft());
        }
        else
        {
            Debug.Assert(_concatenation != null);
            switch (_concatenation.Kind)
            {
                case RegexNodeKind.Concatenate:
                    {
                        var outerConjunct =
                            _concatenation.Parent is { Kind: RegexNodeKind.Conjunction }
                                ? _concatenation.Parent
                                : new RegexNode(RegexNodeKind.Conjunction, _options);

                        outerConjunct.AddChild(_concatenation);
                        var newInnerConcat =
                            new RegexNode(RegexNodeKind.Concatenate, _options)
                            {
                                Parent = outerConjunct
                            };
                        _concatenation = newInnerConcat;
                        break;
                    }
                default:
                    {
                        throw MakeException(RegexParseError.InvalidGroupingConstruct, "InvalidGroupingConstruct");
                    }
            }
        }
    }

    /// <summary>Finish the current quantifiable (when a quantifier is not found or is not possible)</summary>
    private void AddConcatenate()
    {
        // The first (| inside a Testgroup group goes directly to the group
        _concatenation!.AddChild(_unit!);
        _unit = null;
    }

    /// <summary>Finish the current quantifiable (when a quantifier is found)</summary>
    private void AddConcatenate(bool lazy, int min, int max)
    {
        _concatenation!.AddChild(_unit!.MakeQuantifier(lazy, min, max));
        _unit = null;
    }

    private RegexNode GetOutermostConjunction(RegexNode current)
    {
        if (current!.Parent != null)
            switch (current.Parent.Kind)
            {
                case RegexNodeKind.Conjunction:
                    {
                        var outer = GetOutermostConjunction(current.Parent);
                        return outer;
                    }
                default:
                    {
                        return current;
                    }
            }
        else
            return current;
    }

    private bool HasConjunctionParent(RegexNode current)
    {
        if (current!.Parent != null)
            switch (current.Parent.Kind)
            {
                case RegexNodeKind.Conjunction:
                    {
                        return true;
                    }
                default:
                    {
                        return HasConjunctionParent(current.Parent);
                    }
            }
        else
            return false;
    }

    /// <summary>Returns the current unit</summary>
    private RegexNode? Unit()
    {
        return _unit;
    }

    /// <summary>Sets the current unit to a single char node</summary>
    private void AddUnitOne(char ch)
    {
        _unit = RegexNode.CreateOneWithCaseConversion(ch, _options, _culture, ref _caseBehavior);
    }

    /// <summary>Sets the current unit to a subtree</summary>
    private void AddUnitNode(RegexNode node)
    {
        _unit = node;
    }

    /// <summary>Sets the current unit to an assertion of the specified type</summary>
    private void AddUnitType(RegexNodeKind type)
    {
        _unit = new RegexNode(type, _options);
    }

    /// <summary>Finish the current group (in response to a ')' or end)</summary>
    private void AddGroup()
    {
        if (_group!.Kind is RegexNodeKind.ExpressionConditional or RegexNodeKind.BackreferenceConditional)
        {
            _group.AddChild(_concatenation!.ReverseConcatenationIfRightToLeft());
            if ((_group.Kind == RegexNodeKind.BackreferenceConditional && _group.ChildCount() > 2) ||
                _group.ChildCount() > 3)
                throw MakeException(RegexParseError.AlternationHasTooManyConditions,
                    "AlternationHasTooManyConditions");
        }
        else
        {
            if (_concatenation!.Parent != null)
            {
                switch (_concatenation.Parent.Kind)
                {
                    case RegexNodeKind.Conjunction:
                        {
                            var outer = GetOutermostConjunction(_concatenation.Parent);
                            outer.AddChild(_concatenation);
                            _alternation!.AddChild(outer);
                            _group.AddChild(_alternation);
                            break;
                        }
                    default:
                        {
                            _alternation!.AddChild(_concatenation!.ReverseConcatenationIfRightToLeft());
                            _group.AddChild(_alternation);
                            break;
                        }
                }
            }
            else
            {
                _alternation!.AddChild(_concatenation!.ReverseConcatenationIfRightToLeft());
                _group.AddChild(_alternation);
            }
        }

        _unit = _group;
    }

    /// <summary>Saves options on a stack.</summary>
    private void PushOptions()
    {
        _optionsStack.Append((int)_options);
    }

    /// <summary>Recalls options from the stack.</summary>
    private void PopOptions()
    {
        _options = (RegexOptions)_optionsStack.Pop();
    }

    /// <summary>True if options stack is empty.</summary>
    private bool EmptyOptionsStack()
    {
        return _optionsStack.Length == 0;
    }

    /// <summary>Pops the options stack, but keeps the current options unchanged.</summary>
    private void PopKeepOptions()
    {
        _optionsStack.Length--;
    }

    /// <summary>Fills in a RegexParseException</summary>
    private RegexParseException MakeException(RegexParseError error, string message)
    {
        return new RegexParseException(error, _currentPos, $"{_pattern}; {_currentPos}; {message}");
    }

    /// <summary>Returns the current parsing position.</summary>
    private int Textpos()
    {
        return _currentPos;
    }

    /// <summary>Zaps to a specific parsing position.</summary>
    private void Textto(int pos)
    {
        _currentPos = pos;
    }

    /// <summary>Returns the char at the right of the current parsing position and advances to the right.</summary>
    private char RightCharMoveRight()
    {
        return _pattern[_currentPos++];
    }

    /// <summary>Moves the current position to the right.</summary>
    private void MoveRight()
    {
        _currentPos++;
    }

    private void MoveRight(int i)
    {
        _currentPos += i;
    }

    /// <summary>Moves the current parsing position one to the left.</summary>
    private void MoveLeft()
    {
        --_currentPos;
    }

    /// <summary>Returns the char left of the current parsing position.</summary>
    private char CharAt(int i)
    {
        return _pattern[i];
    }

    /// <summary>Returns the char right of the current parsing position.</summary>
    private char RightChar()
    {
        return _pattern[_currentPos];
    }

    /// <summary>Returns the char i chars right of the current parsing position.</summary>
    private char RightChar(int i)
    {
        return _pattern[_currentPos + i];
    }

    /// <summary>Number of characters to the right of the current parsing position.</summary>
    private int CharsRight()
    {
        return _pattern.Length - _currentPos;
    }

    /// <summary>Gets group name from its number.</summary>
    public static string GroupNameFromNumber(Hashtable? caps, string[]? capslist, int capsize, int i)
    {
        if (capslist is null)
        {
            if ((uint)i < (uint)capsize) return ((uint)i).ToString();
        }
        else
        {
            if ((caps is null || caps.TryGetValue(i, out i)) && (uint)i < (uint)capslist.Length) return capslist[i];
        }

        return string.Empty;
    }
}