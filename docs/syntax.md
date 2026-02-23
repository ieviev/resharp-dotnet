# RE# Syntax Reference

RE# supports most standard .NET regex syntax with three extensions: **intersection**, **complement**, and a **universal wildcard**.

## Extensions

### `_` - universal wildcard

Matches any character including newlines. Equivalent to `[\s\S]`.

```
_       matches any single character
_*      matches any string (including empty)
_{5,10} matches any string of 5-10 characters
```

Note: `.` (dot) matches any character **except** newline, same as standard regex.

### `&` - intersection

Both sides must match. The match is the intersection of the two languages.

```
c...&...s       4-letter word starting with 'c' AND ending with 's'
_*cat_*&_*dog_* contains both "cat" and "dog"
.*a.*&.*b.*     contains both 'a' and 'b'
```

Multiple intersections can be chained:

```
.*a.*&.*b.*&.*c.*   contains 'a', 'b', and 'c'
_*cat_*&_*dog_*&_{5,30}  contains "cat" and "dog", 5-30 chars long
```

Intersection has **higher precedence** than alternation: `a|b&c` is parsed as `a|(b&c)`.

### `~(...)` - complement

Matches everything the inner pattern does **not** match. Must be written with parentheses.

```
~(_*\d\d_*)     does not contain two consecutive digits
~(_*\n\n_*)     does not contain a double newline (single paragraph)
~(_*and_*)      does not contain "and"
```

- with complement, prefer `_` over `.*` 
- complement is the main reason why we added `_` because `~(.*xyz.*)` means intuitively `does not contain xyz on the same line`, so it will just match the full input string if it does not contain "xyz", 
- `~(_*xyz_*)` means `does not contain xyz`, no ifs, ands, or buts

### Unsupported features

As a bit of trivia, the .NET regex is a **turing complete** language, through the use of back references and balancing groups. RE# is an automata engine has a much more limited feature set.

RE# does not support any of the following constructs:
- Group captures: `(...)` is non-capturing, and `(?:...)` is just an explicit non-capturing group. 
  - you technically have one capturing group
  - ex. `ab(cd)ef` is can be used as `(?<=ab)cd(?=ef)`
  - also string replace supports `$0`, but not `$1`, `$2`, etc. since there are no capture groups
  - for more groups the easiest thing to do is use another engine to extract them post-match
- Lazy quantifiers: `*?`, `+?`, `??`, `{n,m}?`
- Backreferences: `\1`, `\2`, etc.
- Balancing groups: `(?<open>...)`, `(?<-open>...)`
- Conditional patterns: `(?(condition)yes|no)`
- Nested lookarounds: `(?=(?<=a)b)` or `(?<=(?=a)b)c`

### Combining operators

RE# operators can be combined in powerful ways to express complex patterns. Here are some examples:

- `_*` = any string
- `a_*` = any string that starts with 'a'
- `_*a` = any string that ends with 'a'
- `_*a_*` = any string that contains 'a'
- `~(_*a_*)` = any string that does NOT contain 'a'
- `(_*a_*)&~(_*b_*)` = any string that contains 'a' AND does not contain 'b'
- `(?<=b)_*&_*(?=a)` = any string that is preceded by 'b' AND followed by 'a'
- .. you combine all of these with `&` to get more complex patterns

## Standard syntax

### Character classes

| Pattern    | Description                           |
|------------|---------------------------------------|
| `[abc]`    | any of a, b, c                        |
| `[^abc]`   | any character except a, b, c          |
| `[a-z]`    | range: a through z                    |
| `\d`       | digit `\d = unicode (370 digits), [0-9] = ascii (10 digits)`                         |
| `\D`       | non-digit                             |
| `\w`       | word character `(\w is unicode by default, 10000+ chars)` |
| `\W`       | non-word character                    |
| `\s`       | whitespace                            |
| `\S`       | non-whitespace                        |
| `.`        | any character except `\n`             |

### Quantifiers

| Pattern   | Description              |
|-----------|--------------------------|
| `*`       | 0 or more      |
| `+`       | 1 or more       |
| `?`       | 0 or 1          |
| `{n}`     | exactly n                |
| `{n,}`    | n or more                |
| `{n,m}`   | between n and m          |

### Anchors

| Pattern | Description            |
|---------|------------------------|
| `^`     | start of line          |
| `$`     | end of line            |
| `\A`    | start of string        |
| `\z`    | end of string          |
| `\b`    | word boundary          |

### Lookarounds

| Pattern      | Description           |
|--------------|-----------------------|
| `(?=...)`    | positive lookahead (follows)   |
| `(?!...)`    | negative lookahead (does not follow)    |
| `(?<=...)`   | positive lookbehind (precedes)   |
| `(?<!...)`   | negative lookbehind (does not precede)   |

Lookarounds combine with intersection and complement:

```
(?<=author).*&.*and.*   after "author", containing "and"
(?<=ab).*&~(_*and_*)    after "ab", not containing "and"
```

**Restriction: no nested lookarounds.** RE# normalizes all lookarounds into the form `(?<=R1)R2(?=R3)`, where R1, R2, and R3 are regular expressions that themselves **cannot contain lookarounds**. This means patterns like `(?<=(?=a)b)c` or `(?=a(?<=b))` are not supported.

This restriction is what allows RE# to encode lookaround information directly into DFA states and maintain **O(n) linear-time** matching. Engines that support arbitrary lookarounds typically incur O(m × n) matching cost or resort to backtracking.

## Matching behavior

- `IsMatch` is identical to standard .NET
- For full-string matching, anchor with `\A(pattern)\z`, `^` and `$` are always the beginning or end of a **line** (or if the input is a single line, they are equivalent to `\A` and `\z`)
- Matches returned are **leftmost-longest** (.NET is leftmost-greedy, which is subtly different)
