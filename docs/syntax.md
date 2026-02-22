# RE# Syntax Reference

RE# supports standard .NET regex syntax with three extensions: **intersection**, **complement**, and a **universal wildcard**.

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
~(.*\n\n.*)     does not contain a double newline (single paragraph)
~(.*and.*)      does not contain "and"
```

### Combining operators

Intersection and complement compose naturally:

```
F.*&~(.*Finn)                starts with 'F' but does not end with "Finn"
.*Huck.*&~(.*F.*)            contains "Huck" but not 'F'
~(_*\d\d_*)&[a-zA-Z\d]{8,}  8+ alphanumeric chars, no consecutive digits
```

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
| `*`       | 0 or more (greedy)       |
| `+`       | 1 or more (greedy)       |
| `?`       | 0 or 1 (greedy)          |
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

### Groups

| Pattern     | Description              |
|-------------|--------------------------|
| `(...)`     | group (non-capturing)    |
| `(?:...)`   | explicit non-capturing   |
| `a\|b`      | alternation              |

### Lookarounds

| Pattern      | Description           |
|--------------|-----------------------|
| `(?=...)`    | positive lookahead    |
| `(?!...)`    | negative lookahead    |
| `(?<=...)`   | positive lookbehind   |
| `(?<!...)`   | negative lookbehind   |

Lookarounds combine with intersection and complement:

```
(?<=author).*&.*and.*   after "author", containing "and"
(?<=__).*&~(.*and.*)    after "__", not containing "and"
```

**Restriction: no nested lookarounds.** RE# normalizes all lookarounds into the form `(?<=R1)R2(?=R3)`, where R1, R2, and R3 are regular expressions that themselves **cannot contain lookarounds**. This means patterns like `(?<=(?=a)b)c` or `(?=a(?<=b))` are not supported.

This restriction is what allows RE# to encode lookaround information directly into DFA states and maintain **O(n) linear-time** matching. Engines that support arbitrary lookarounds typically incur O(m × n) matching cost or resort to backtracking.

## Matching behavior

- All matching is **non-backtracking**
- `IsMatch` checks if the pattern matches anywhere in the input (standard in .NET)
- For full-string matching, anchor with `\A(pattern)\z`
- Matches returned are **non-overlapping** and **leftmost-longest**
