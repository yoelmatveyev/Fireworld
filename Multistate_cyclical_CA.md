# Introduction

Classical cellular automata such as Conway's Game of Life can be generalized by creating multistate rules with 2 or more "parallel universes", which act by themselves as the original rule. When these "parallel universes" collide, additional rules create new, hybrid forms of behavior, which may look entirely different that the original rule. 
Additionally, there is an entirely new, unexplored field of multistate rules, which don't resemble any classical binary rules.

I started exploring this field in June 2020.

For the sake of symmetry and consistency, state transformations must be **cyclical**.

For example, if 2 cells of state A and 1 cell of state B give birth to a cell of state C in a 3-state automation (not counting the "ground" zero state), 
then 2 cells of state B and 1 cell of state C must give birth to a cell of state A, and 2 cells of state C and 1 cell of state A must give birth to a cell of state B.

Note that this does not define the behavior of a cell surrounded by 2 cells of state B and 1 cell of state A. In a multistate universe it's a different rule, which may cause nothing or give birth to either A, B or C. If a cell is surrounded by an equal numbers of all states, it may not become alive, because it is impossible to define a cyclically consistent birth rule.

Thing become even more complicated, when it comes to survival rules. A surviving cell may be transformed in some cases into a different state; if a cell of state C survives or gets transformed by 2 cells of state B and 1 cell of state A, it does not imply that the same must happen with a cell of state B or A. These may be two different additional rules. The aforementioned rule does imply that if a cell of state C gets transformed into state B by 2 cells of state B and 1 cell of state A, then a cell of state A gets transformed into state C by 2 cells of state C and 1 cell of state B, and a cell of state B gets transformed into state A by 2 cells of state A and 1 cell of state C.

Writing such rule tables by hand is a laborious task prone to errors. Therefore I developed a notation for such rules and wrote a Lisp program that produces the corresponding rule files. For the time being, only "totalistic", i.e. permutable rules, in which the position of the neighbors doesn't matter, are taken into consideration.

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

a->c->b;b->a->c;c->b->a


The "r" abbreviation, valid only for survival rules, defines a common pattern for such similar rules. For example, T-12ar is the same as T-12a0012a201a, and denotes:

a->abb->a;b->bcc->b;c->caa->c

a->bcc->a;b->caa->b;c->abb->c

a->aac->a;b->bba->b;c->ccb->c

The "n" abbreviation,only allowed in survival situations, denotes rules in which a cell survives, when surrounded by n cells of whatever state. This abbreviation may not be used for state transformations, because it would be self-contraditory. For example, T2a-2n3n denotes an extention of Conway's Game of Life, in which a cell survives, if surrounded by 2 or 3 cells of any live state.

The "z" abbreviation is used before an "n" abbreviation to exclude certain state combinations from survival rules. It may be combined with the "r" notation. For example, T2a-2n21zr3n denotes an exension of Conway's Game of Life, in which a cell survive, if surrounded by 2 or 3 cells of any live state, unless these three states are cca, aab or bbc, while T2a-2n12zr3n similarly excludes caa, abb and bcc. When both combinations are used, any combination of 2 cells of one state and 1 cell of another state is excluded, which means that T2n-2n12zr21zr3n is a redundant notation equivalent to T2n-2n111ar. Such redundant notations should be avoided.

For usage of the **rule table generator**, read **Ruletable_genegator.md**
 
