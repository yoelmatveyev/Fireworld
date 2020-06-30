# Fireworld

I invented this rule in 2002 while playing with Mcell (Mirek's Cellebration), a great cellular automata playground made by Mirosław (Mirek) Wójtowicz. To my knowledge, for a long while it was the only program of its kind, where you could set your own rules in various flexible ways. Now, since old Mcell files and rules are convertible to Golly (sometimes by some modification or by a custom script though), I decided to start converting and publicizing my old patterns, as well as engineering new ones. As a side note, Mcell works perfectly on Linux/Unix under Wine.

Originally I called this rule "Minimum", as it was deliberately meant to be "minimalistic" in its survival/birth mechanism. Soon I had discovered that this rule must be Turing-complete, as it exhibits in its own way the same complexity as Brian Silverman's Wireworld (hence the new name of my rule). Natural evolution of random patterns often resembles the operation of some complicated machinery or circuitry, occasionally producing fractal-like structures.

Although I did not have time to build a full computer model in Fireworld, patterns provided in this repository (logic gates, various types of ROM and RAM, binary counters, data buses etc.) may serve as a sufficient reason to believe that it must be possible.

Unlike Wireworld, almost nothing in Fireworld is stable, besides stand-alone 1x1 blocks and Life-like 2x2 blocks surrounded by supporting 1x1 blocks. Tiny orthogonal ships, guns, puffers, rakes, breeders are the common natural inhabitants of this universe. As these structures interact with each other in complex ways, the behavior of this rule combines Life-like and Wireworld-like features.

# Rule

Fireworld operates in the Moore neighbourhood. Cells may have 3 states: empty, dead and alife.

A empty cell becomes alive only if it is surrounded by one live cell horizontally or vertically adjacent to it, while one other cell is adjecent to it in diagonal. To get born, so to speak, the cell needs two parents of opposite sex. If some other cells in its neighbourhood are dead, they count as empty and don't interfere with the birth.

A live cell survives either if there are no other live cells in its neighbourhood or there are exactly three live cells adjacent to it: two adjacent horizontally or vertically, while the third one is adjecent diagonally. In terms of survival, dead cells also count as empty and don't matter.

A dead cell becomes empty in the next generation.

This rule may be abbreviated in Golly simply as **03ajkr/2ak/3**.

# Copyright by Yoel Matveyev 2002
