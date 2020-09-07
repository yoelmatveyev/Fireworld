# Fireworld

I invented this rule in 2002 while playing with Mcell (Mirek's Cellebration), a great cellular automata playground made by Mirosław (Mirek) Wójtowicz. To my knowledge, for a long while it was the only program of its kind, where you could set your own rules in various flexible ways. Now, since old Mcell files and rules are convertible to Golly (sometimes by some modification or by a custom script though), I decided to start converting and publicizing my old patterns, as well as engineering new ones. On a side note, Mcell works perfectly on Linux/Unix under Wine.

Originally I called this rule "Minimum", as it was deliberately meant to be "minimalistic" in its survival/birth mechanism. Soon I had discovered that this rule must be Turing-complete, as it exhibits in its own way the same complexity as Brian Silverman's Wireworld (hence the new name of my rule). Natural evolution of random patterns often resembles the operation of some complicated machinery or circuitry, occasionally producing fractal-like structures.

Although I did not have time to build a full computer model in Fireworld, patterns provided in this repository (logic gates, various types of ROM and RAM, binary counters, data buses etc.) may serve as a sufficient reason to believe that it must be possible.

Unlike Wireworld, almost nothing in Fireworld is stable, besides stand-alone 1x1 blocks and Life-like 2x2 blocks surrounded by supporting 1x1 blocks. Tiny orthogonal ships, guns, puffers, rakes, breeders are the common natural inhabitants of this universe. As these structures interact with each other in complex ways, the behavior of this rule combines Life-like and Wireworld-like features.

# Description

Fireworld operates in the Moore neighborhood. Cells may have 3 states: empty, live and dead.

***1. An empty cell becomes alive only if it is surrounded by one live cell horizontally or vertically adjacent to it, while one other live cell is adjacent to it in diagonal.***

***2. A live cell survives either if there are no other live cells in its neighborhood or there are exactly three live cells adjacent to it in a particular way: two adjacent orthogonally (horizontally or vertically), while the third one is adjacent in diagonal.***

***3. Dead cells count as empty and don't interfere with birth or survival, if they are present in the neighborhood. They do prevent a cell to get born in their place though, as usual in the "Generations" rules.***

***4. A dead cell becomes empty in the next generation.***

This rule may be nowadays abbreviated in Golly simply as **03ajkr/2ak/3**.

For **other rules**, especially the novel **cyclical multistate rules**, and for the **generator of rule tables** read the files **Other_rules.md**, **Multistate_cyclical_CA.md** and **Ruletable_generator.md**.

Currently, growing pattern repositories are being built for two very similar 3-color cyclical rules: Gluons and Gluonic, **T2b-0n011a3n** and **T2b102b-0a011a3n**, as well as another somewhat similar rule **Morse**.

I would like to express gratitide to LifeWiki Forums users **FWKnightship** and **Layz Boi** for important contributions to the Gluonic pattern collection and to my research of this rule.
