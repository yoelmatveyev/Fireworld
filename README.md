# Fireworld

I invented this rule in 2002 while playing with Mcell (Mirek's Cellebration), a great cellular automata playground. To my knowledge, for a long while it was the only program of this kind, where you could set your own rules in various flexible ways. Now, since old Mcell files and rules are convertable to Golly (sometimes by hand or by a custom script though), I decided to start publicizing my old patterns.

Originally I called this rule "Mimimum", as it was deliberately meant to be "milimatistic" in its survival/birth mechanism. Soon I had discovered that this rule must be Turing-complete, as it exhibits in its own way the same complexity as Brian Silverman's Wireworld (hence the new name of my rule). Although I did not have time to build a full computer model in Fireworld, patterns provided in this repository should serve as s sufficient reason to believe that it is possible.

Unlike Wireworld, almost nothing in Fireworld is stable, besides stand-alone 1x1 blocks and Life-like 2x2 blocks surrounded by supporting 1x1 blocks. Gligers, glider guns and puffers are the main inhabitants of this universe, However, as these structures interact with each other in complex ways, the behavior of this rule combines Life-like and Wireworld-like features.

# Rule

Fireworld operates in the Moore neighbourhood. Cell have 3 states: dead, dying and alife.

A dead cell becomes alive only if it is surrounded by one live cell horizontally or vertically adjacent to it, and one other cell adjecent to it in diagonal. To get born, it needs two parents of opposite sex, so to speak. 

A live cell survives either if there are no other live cells in the neighbourhood or there are two live cells horizontally or vertically adjacent to it, and one live cell adjecent to it in diagonal. To make another comparison, in order to survive a cell must be completely indivisualistic or moderately collectivist

A dead cell becomes half-dead in the next turn and dies in the second turn.

# Copyright by Yoel Matveyev 2002-2009
