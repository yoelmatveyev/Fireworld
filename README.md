# Fireworld

I invented this rule in 2002 while playing with Mcell (Mirek's Cellebration), a great cellular automata playground. To my knowledge, for a long while it was the only program of its kind, where you could set your own rules in various flexible ways. Now, since old Mcell files and rules are convertable to Golly (sometimes by hand or by a custom script though), I decided to start converting and publicizing my old patterns.

Originally I called this rule "Minimum", as it was deliberately meant to be "minimalistic" in its survival/birth mechanism. Soon I had discovered that this rule must be Turing-complete, as it exhibits in its own way the same complexity as Brian Silverman's Wireworld (hence the new name of my rule). Although I did not have time to build a full computer model in Fireworld, patterns provided in this repository (logic gates, memory cells etc.) should serve as s sufficient reason to believe that it is possible.

Unlike Wireworld, almost nothing in Fireworld is stable, besides stand-alone 1x1 blocks and Life-like 2x2 blocks surrounded by supporting 1x1 blocks. Gligers, glider guns and puffers are the main inhabitants of this universe, However, as these structures interact with each other in complex ways, the behavior of this rule combines Life-like and Wireworld-like features.

# Rule

Fireworld operates in the Moore neighbourhood. Cells may have 3 states: empty, dead and alife.

A empty cell becomes alive only if it is surrounded by one live cell horizontally or vertically adjacent to it, while one other cell is adjecent to it in diagonal. To get born, it needs two parents of opposite sex, so to speak. If some other cells in its neighbourhood are dead, they count as empty.

A live cell survives either if there are no other live cells in the neighbourhood or there are two live cells horizontally or vertically adjacent to it, while one other live cell is adjecent to it in diagonal. To make another comparison, in order to survive, a cell must be completely individualistic or moderately collectivist. In terms of survival, dead cells also count as empty.

A dead cell becomes empty in the next turn.

This rule may also be abbreviated in Golly aimply as **03ajkr/2ak/3**.


# Copyright by Yoel Matveyev 2002-2009
