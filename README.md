BNF:
---

```
<var> ::= ^[a-zA-Z]$
<val> ::= ^[0-9]*$

<program> ::= <stmt>*

<stmt> ::= print <expr>
        |  assign <var> <expr>
        |  read <var>

<expr> ::= <val>
        |  <expr> + <expr>
        |  <expr> - <expr>
        |  <expr> * <expr>
        |  <expr> / <expr>
        |  if <expr> then <program> else <program>
```
