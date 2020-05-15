# hintikka-viz

Some Haskell for rendering first-order model-checking games, aka Hintikka games, as [graphviz](https://www.graphviz.org/documentation/) DOT language files using the Haskell library [graphviz](https://hackage.haskell.org/package/graphviz-2999.20.0.4). Unfortunately they are completely hideous:

![A model-checking game](/example.png)

Above is the model-checking game for formula

$$\forall x, R(x) \land (\exists y, S(x, y)) $$

on a domain with 5 variables (The graph coloration does not reflect an underlying model for determining truth-values).

Unfortunately neither Graphviz nor GitHub render LaTeX so everything looks pretty bad :pensive:
