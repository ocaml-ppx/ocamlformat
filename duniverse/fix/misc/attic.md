## Preliminaries

In order to read the following, a tiny bit of vocabulary is required.

I write `epsilon` for the regular expression that
accepts the empty word, and only the empty word.
I write `zero` for the regular expression that
accepts nothing.

A regular expression `e` is **nullable** if and only if it accepts the empty word.
It is easy to determine, by inspection of the syntax of `e`, whether this is the
case (the code appears further on).

Let `iota e` stand for `epsilon` if `e` is nullable
and for `zero` otherwise. `iota e` is a regular expression
that accepts the empty word if and only if `e` accepts the empty word,
and accepts nothing else.

Let `delta a e` stand for the **derivative** of the regular expression `e`
with respect to the symbol `a`. Thinking of `e` as a set of words, `delta a e`
is obtained by keeping only the words that begin with `a` and by crossing out
in each such word this initial letter `a`. For instance, the derivative of the
set `{ ace, amid, bar }` with respect to `a` is the set `{ ce, mid }`. The
derivative of a regular expression can also be easily computed by inspection
of its syntax (read on).

## From an RE to a DFA

The main idea behind the construction is this. First, **a regular expression
`e` can be decomposed in an infinite tree**, or an infinite-state automaton,
whose vertices correspond to the iterated derivatives of `e`. Second, because
a regular expression only has a finite number of iterated derivatives (up to a
certain equational theory), **this infinite tree must in fact be the unfolding
of a finite cyclic graph**, a finite-state automaton. Because it is possible
to effectively recognize when two regular expressions are equal, it is
possible to effectively construct this finite, cyclic data structure.

In slightly greater detail, suppose, for simplicity, that we have a two-letter
alphabet, whose symbols are `a` and `b`. A word on this alphabet, then, either
is the empty word, or begins with `a`, or begins with `b`. For this reason,
an arbitrary regular expression `e` can be decomposed as follows. `e` is equal
to:

```
iota e
  + a . delta a e
  + b . delta b e
```

Here, `+` stands for choice, while `.` stands for sequencing. This can be read
as the beginning of a tree-structured automaton. The root state corresponds to
the regular expression `e`. It is an accepting state if and only if `iota e`
is nonempty, that is, if and only if `e` is nullable. Out of this state, there
are two transitions. A transition labeled `a` leads to a subtree that
corresponds to the regular expression `delta a e`. Similarly, a transition
labeled `b` leads to a subtree that corresponds to `delta b e`.

This process can be iterated: the regular expressions `delta a e` and `delta b
e` can be decomposed, too. Thus, the regular expression `e` is also equal to:

```
iota e
  + a . (iota (delta a e)
           + a . (delta a (delta a e))
           + b . (delta b (delta a e))
        )
  + b . (iota (delta b e)
           + a . (delta a (delta b e))
           + b . (delta b (delta b e))
        )
```

and so on, down to an arbitrary depth. This gives rise to an infinite tree,
whose vertices correspond to the iterated derivatives of `e`.

Brzozowski's key remark is that this tree is in reality finite.
Provided "equality" of regular expressions
includes the following laws,

```
  0 + e = e
  e + 0 = e
  e + e = e
  e . 0 = 0
    (more laws, not shown)
```

a regular expression only has a finite
number of iterated derivatives.
(When regular expressions are viewed as semantic objects,
sets of words, this is the Myhill–Nerode theorem. Brzozowski's
insight is that, when regular expressions are viewed as syntactic objects,
an analogous result holds.)
