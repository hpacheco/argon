# Argon

Argon measures your code's cyclomatic complexity.

### About the complexity being measured

`argon` will compute the [cyclomatic
complexity](https://en.wikipedia.org/wiki/Cyclomatic_complexity) of Haskell
functions, which is the number of decisions in a block of code plus 1. For
instance the following function:

```haskell
func n = case n of
           2 -> 3
           4 -> 6
           _ -> 42
```

has a cyclomatic complexity of 3.

The boolean operators `&&` and `||` also affect the this number. For instance
the following function:

```haskell
g n = n < 68 && n `mod` 3 == 2 && n > 49
```
has a cyclomatic complexity of 3.

As a last example, the following function:

```haskell
func n = case n of
           2 -> 3
           4 -> 6
           _ -> if 0 < n
                then 7
                else 8
```

has a cyclomatic complexity of 5.

Cyclomatic complexity provides a very shallow metric of code complexity: a high
cyclomatic complexity number does not necessarily mean that the function is
complex, and conversely, a low number does not necessarily indicate that the
function is simple. However, this number it can be useful for highlighting
potential maintainability issues.

### Running

The Argon executable expects a list of file paths (files or directories):

```bash
$ argon --no-color --min 2 src
src/Argon/Formatters.hs
	42:1 coloredFunc - 2
	44:5 color - 2
	63:1 formatResult - 2
src/Argon/Types.hs
	98:3 toJSON - 2
src/Argon/Cabal.hs
	24:5 toString - 3
src/Argon/Parser.hs
	68:1 parseModuleWithCpp - 5
	37:1 analyze - 2
	44:7 analysis - 2
src/Argon/Walker.hs
	12:1 allFiles - 2
src/Argon/Results.hs
	35:1 filterNulls - 3
	56:1 exportStream - 3
	46:1 filterResults - 2
src/Argon/Loc.hs
	19:5 toRealSrcLoc - 2
src/Argon/Visitor.hs
	67:1 visitExp - 6
	75:1 visitOp - 4
	35:5 visit - 2
src/Argon/SYB/Utils.hs
	21:1 everythingStaged - 2
```

For every file, Argon sorts results with the following criteria (and in this
order):

1. complexity (descending)
2. line number (ascending)
3. alphabetically

When colors are enabled (default), Argon computes a rank associated with the
complexity score:

| Complexity | Rank |
|:----------:|:----:|
|    0..5    |   A  |
|    5..10   |   B  |
|  above 10  |   C  |


#### JSON

Results can also be exported to JSON:
```bash
$ argon --json --min 5 src | jq
[
  {
    "blocks": [
      {
        "col": 1,
        "complexity": 5,
        "lineno": 68,
        "name": "parseModuleWithCpp"
      }
    ],
    "path": "src/Argon/Parser.hs",
    "type": "result"
  },
  {
    "blocks": [
      {
        "col": 1,
        "complexity": 6,
        "lineno": 67,
        "name": "visitExp"
      }
    ],
    "path": "src/Argon/Visitor.hs",
    "type": "result"
  }
]
```
