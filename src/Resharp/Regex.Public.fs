namespace Resharp

open System
open System.Globalization
open System.Runtime.CompilerServices
open Microsoft.FSharp.Core
open Resharp.Types
open System.Runtime.InteropServices
open Resharp.Runtime
open Resharp.Common

/// <summary>
/// High-performance regex engine supporting intersection (<c>&amp;</c>) and complement (<c>~</c>) operations.
/// Uses symbolic derivatives and DFA-based matching.
/// </summary>
/// <param name="pattern">The regex pattern. Supports most standard syntax plus <c>&amp;</c> (intersection) and <c>~</c> (complement).</param>
/// <param name="options">Optional configuration. See <see cref="ResharpOptions"/> for defaults.</param>
/// <example>
/// <code>
/// let re = Resharp.Regex(@"\w+")
/// re.IsMatch("hello") // true
/// re.Count("one two three") // 3
/// </code>
/// </example>
[<Sealed>]
type Regex
    (
        pattern: string,
        [<Optional; DefaultParameterValue(null: ResharpOptions)>] options: ResharpOptions
    ) =
    inherit GenericRegexMatcher<char>()
    let options = if isNull options then ResharpOptions() else options
        
    let matcher =
        let regexTree =
            Resharp.Runtime.ExtendedRegexParser.Parse(
                pattern,
                RegexOptions.ExplicitCapture
                ||| RegexOptions.NonBacktracking
                ||| RegexOptions.Multiline
                ||| RegexOptions.CultureInvariant
                ||| (if options.IgnoreCase then
                         RegexOptions.IgnoreCase
                     else
                         RegexOptions.None),
                CultureInfo.InvariantCulture
            )

        let charsetSolver = CharSetSolver()
        let converter = ResharpRegexNodeConverter(charsetSolver)
        let regexBuilder = RegexBuilder(converter, charsetSolver, charsetSolver, options)

        let symbolicBddnode: RegexNodeId =
            RegexNodeConverter.convertToSymbolicRegexNode (
                charsetSolver,
                regexBuilder,
                regexTree.Root
            )

        let minterms = Minterms.compute charsetSolver regexBuilder symbolicBddnode

        Helpers.createMatcher (
            regexBuilder,
            minterms,
            charsetSolver,
            converter,
            symbolicBddnode,
            options
        )


    /// <summary>Counts the number of non-overlapping matches in the input.</summary>
    /// <param name="input">The input text to search.</param>
    /// <returns>The number of matches found.</returns>
    override this.Count(input: ReadOnlySpan<char>) = matcher.Count(input)

    /// <summary>
    /// Returns whether the pattern matches anywhere in the input.
    /// For a full-string match, anchor the pattern with <c>\A(pattern)\z</c>.
    /// </summary>
    /// <param name="input">The input text to search.</param>
    /// <returns><c>true</c> if a match is found; otherwise <c>false</c>.</returns>
    override this.IsMatch(input) = matcher.IsMatch(input)

    /// <summary>
    /// Finds all non-overlapping matches as index/length slices, avoiding string allocations.
    /// The returned <see cref="ValueList{ValueMatch}"/> must be disposed after use.
    /// </summary>
    /// <param name="input">The input text to search.</param>
    /// <returns>A disposable list of <see cref="ValueMatch"/> values.</returns>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    override this.ValueMatches(input: ReadOnlySpan<char>) = matcher.ValueMatches(input)

    /// <summary>Finds all non-overlapping matches in the input.</summary>
    /// <param name="input">The input text to search.</param>
    /// <returns>An array of <see cref="MatchResult"/> with the matched text, index, and length.</returns>
    override this.Matches(input) = matcher.Matches(input)

    /// <summary>
    /// Finds the end index of the first (shortest) match from the beginning of the input, anchored at <c>\A</c>.
    /// </summary>
    /// <param name="input">The input text to search.</param>
    /// <returns>The end index of the first match, or <c>-1</c> if no match.</returns>
    override this.FirstEnd(input: ReadOnlySpan<char>) = matcher.FirstEnd(input)

    /// <summary>
    /// Finds the end index of the longest match from the beginning of the input, anchored at <c>\A</c>.
    /// </summary>
    /// <param name="input">The input text to search.</param>
    /// <returns>The end index of the longest match, or <c>-1</c> if no match.</returns>
    override this.LongestEnd(input: ReadOnlySpan<char>) = matcher.LongestEnd(input)

    /// <summary>Replaces all non-overlapping matches with the replacement string.</summary>
    /// <param name="input">The input text to search.</param>
    /// <param name="replacement">The replacement text.</param>
    /// <returns>A new string with all matches replaced.</returns>
    override this.Replace(input, replacement: ReadOnlySpan<char>) =
        matcher.Replace(input, replacement)

    /// <summary>Replaces all non-overlapping matches using a function that transforms each match.</summary>
    /// <param name="input">The input text to search.</param>
    /// <param name="replacement">A function that receives the matched text and returns the replacement.</param>
    /// <returns>A new string with all matches replaced by the function results.</returns>
    member this.Replace(input, replacement: Func<string, string>) =
        match matcher with
        | :? RegexMatcher<uint64> as m -> m.Replace(input, replacement)
        | :? RegexMatcher<BitVector> as m -> m.Replace(input, replacement)
        | _ -> failwith "unreachable"

    // internal regex matcher for debugging
    member internal this.Matcher: GenericRegexMatcher<char> = matcher
    member internal this.Options: ResharpOptions = options

    /// <summary>
    /// Whether the pattern was compiled to a full DFA at construction time.
    /// Raise <see cref="ResharpOptions.DfaThreshold"/> to fully compile larger DFAs.
    /// </summary>
    member this.IsFullDFA: bool =
        match matcher with
        | :? RegexMatcher<uint64> as m -> m.IsFullDFA
        | :? RegexMatcher<BitVector> as m -> m.IsFullDFA
        | _ -> failwith "unreachable"
