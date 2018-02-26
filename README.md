# Computational-Tree-Logic-model

The entry-point to the model is World.hs by Dan Noar and David Rubio Vallejo. (P.hs and HRAS.hs from the book Computational Semantics by Jan van Eijck and Christina Unger).

Definitions of the CTL operators follow:

 E | There Exists at least one path starting from the current world. Ep means there
exists at least one path starting from the current world along which p is true at some
point.

 A | All paths starting from the current world. Ap means p is true at some point along
all paths starting from the current world.

 X | Next: p has to hold at the next state.

 W | A given proposition holds (Weakly) Until a different proposition becomes true. p
W q  means p is true along a given path until q becomes true. W is sometimes read as
"unless". Because this operator takes two propositions instead of one, it was necessary
to change the prop field of TProp to a list.

 F | A given proposition Finally (eventually) holds somewhere along the given path.
Fp means p is true somewhere along the subsequent path.

 G | A given proposition holds Globally along the given path. Gp means p is true at
all points along the subsequent path. This definition of G has changed from the LTL
definition.

E and A are operators over all paths, while X, W, F, and G are path-specific operators.
