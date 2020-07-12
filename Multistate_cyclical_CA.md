# Introduction

Classical cellular automata such as Conway's Game of Life can be generalized by creating multistate rule with 2 or more "parallel universes", which act by themselves as the original rule. When these "parallel universes" collide, additional rules create new, hybrid forms of behavior, which may look entirely differently that the original rule. 
Additionally, there is an enterely new, unexplored field of multistate rules, which don't resemble any classical binary rules.

I started exploring this field in June 2020.

For the sake of symmetry and consistency, state transformations must be **cyclical**.

For example, if 2 cells of state A and 1 cell of state B give birth to a cell of state C in a 3-state automation (not counting the "ground" zero state), 
then 2 cells of state B and 1 cell of state C must give birth to a cell of state A, and 2 cells of state C and 1 cell of state A must give birth to a cell of state B.

Not that this does not define the behavior of a cell surrounded by 2 cells of state B and 1 cell of state A. In a multistate universe it's a different rule, which may cause nothing or give birth to either A, B or C. If a cell is surrounded by an equal numbers of all states, it may not become alive, because it is impossible to define a cyclicallt consistent birth rule.

Thing become even more complicated, when it comes to survival rules. A surviving cell may be transformed in some cases into a different state; if a cell of state C survives or gets transformed by 2 cells of state B and 1 cell of state A, it does not imply that the same must happen with a cell of state B or A. These may be two different additional rules. The aformentioned rule does imply that if a cell of state C gets transformed into state B by 2 cells of state B and 1 cell of state A, then a cell of state A gets transformed into state C by 2 cells of state C and 1 cell of state B, and a cell of state B gets transformed into state A by 2 cells of state A and 1 cell of state C.

Writing such rule tables by hand is a laborous task prone to errors. Therefore I developed a notation for such rules and wrote a Lisp program that produced the corresponding rule files. For the time being, only "totalistic", i.e. permutable rules, in which the position of the neighbors doesn't matter, are taken into consideration.

In principle, multistate cyclical rules may be defined as generalizations of the "Generations" family of rules or rules with other symmetries, although the number of various possible state combinations may become exhaustively large. For more details, read the file **Multistate_cyclical_CA.md**.

# Notation

Multistate cyclical rules are notated as follows: [Number of living states]S[Birth rules]-[Survival rules]. The dash was choses instead of the traditional slash, because the generator program uses the rule notations by default as the corresponding file names.
2S may be abbreviated as D; 3S may be abbreviated as T.

Birth rules are written as a sequence of digits representing the number of neighbours of all in the states in the rule, 
followed by a small letter, from a to h, representing the state of the cell to be born. 

For example, 3S21b or T21b means that aab->b; bbc->c;cca->a, while 3S221a means that aabbc->a; bbcca->b;ccaab->c.

Inconsistent rules with equal numbers of all states are illegal and ignored by the generator.

For example, from T222a would follow that aabbcc->a and bbccaa->b, which is self-contradictory.

Survival rules are notated the same way, except that the state A is always used as the original state of the surviving cell.

For example, T-012b means a->bcc->b;b->caa->c;c->abb->a. 

Digit sequences with less digits than the number of states mean that the rest of states in the pattern is 0.

For example, T12a is the same as the same as T120a.

Note that the number of possible meaningful birth rules is much shorter that the number of all possible survival rules, e.g. 54 vs. 165 for 3-state rules and as many as 1618 vs. 12870 for 8-state rules.

The reason behind is that while T2a, T02b and T002c result exactly in the same birth rule, T-2b,T-02b and T-002b give three distinct survival rules:

a->a->b;b->b->c;c->c->a
a->b->b;b->c->c;c->a->a
a->c->b;b->a->c;c->b->c


