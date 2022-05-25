# Fireworld

I invented this rule in 2001 while playing with Mcell (Mirek's Cellebration), a great cellular automata playground made by Mirosław (Mirek) Wójtowicz. To my knowledge, for a long while it was the only program of its kind, where you could set your own rules in various flexible ways. Now, since old Mcell files and rules are convertible to Golly (sometimes by some modification or by a custom script though), I decided to start converting and publicizing my old patterns, as well as engineering new ones. On a side note, Mcell works perfectly on Linux/Unix under Wine.

Originally I called this rule "Computer". then "Minimum", as it was deliberately meant to be "minimalistic" in its survival/birth mechanism. Soon I had discovered that this rule must be Turing-complete, as it exhibits in its own way the same complexity as Brian Silverman's Wireworld, hence the final name of the rule. Natural evolution of random patterns often resembles the operation of some complicated machinery or circuitry, occasionally producing fractal-like structures. An implementation of Rule 110 was made on Nov. 3, 2020. 

Eventually, I would like to build something more conventional, like the Wireworld programmable calculator. Patterns provided in this repository (chains of logic gates, including the circuitry of the Rule 110 emulator, various types of ROM and RAM, binary counters, data buses, adder, LED-like display cells etc.) demonstrate that it must be possible with enough effort.

Unlike Wireworld, almost nothing in Fireworld is stable, besides stand-alone dots and Life-like 2x2 blocks surrounded by supporting 1x1 blocks. The only natural still lifes are the dots. Tiny orthogonal ships, guns, puffers, rakes, breeders are the common natural inhabitants of this universe. As these structures interact with each other in complex ways, the behavior of this rule combines Life-like and Wireworld-like features.

# Description

Fireworld operates in the Moore neighborhood. Cells may have 3 states: empty, live and dead.

***1. An empty cell becomes alive only if it is surrounded by one live cell horizontally or vertically adjacent to it, while one other live cell is adjacent to it in diagonal.***

***2. A live cell survives either if there are no other live cells in its neighborhood or there are exactly three live cells adjacent to it in a particular way: two adjacent orthogonally (horizontally or vertically), while the third one is adjacent in diagonal.***

***3. Dead cells count as empty and don't interfere with birth or survival, if they are present in the neighborhood. They do prevent a cell to get born in their place though, as usual in the "Generations" rules.***

***4. A dead cell becomes empty in the next generation.***

This rule may be nowadays abbreviated in Golly simply as **03ajkr/2ak/3**.

For **other rules**, especially the novel **cyclical multistate rules**, and for the **generator of rule tables** read the files **Other_rules.md**, **Multistate_cyclical_CA.md** and **Ruletable_generator.md**.

Currently, growing pattern repositories are being built for several somewhat similar Life-like, 2-color and 3-color cyclical rules: expecially GluonGuns, Gluons and Gluonic, **T2b-0n2n111a4**, **T2b-0n011a3n** and **T2b102b-0a011a3n**, as well a dozen of other rules. GluonGuns, Gluons and Gluonic act in many ways similar to Fireworld and are, most likely, suitable for universal computation.

I would like to express gratitide to LifeWiki Forums users **FWKnightship** and **Layz Boi** for important contributions to pattern collections and to my research of these rules.

Additional Lisp tools are provided for emulating 1D cellular automata with larger neighborhoods in Golly (starting from 4.0) and for 1D cyciical symmetrical multistate 1D CA. Many such rules exhibit behavior as complex or more complex than the famous Rule 110.

Something else worth noticing in this repository are Rule 110 units for **Brian's Brain**. Based on the Fireworld design, they use toggle flip-flops instead of XOR gates. Each unit preserves its status and emits it by a spaceship stream.  

# Fireworld2

This rule is the next generation of Fireworld, invented in December 2020, has been proven very successful for computation modelling. A fully operational computer in it was constructed and tested in October 2021. See my [Izhora project](https://github.com/yoelmatveyev/Izhora).

It is a hybrid rule combining 03ajkr7/2ak/3 and a Wireworld-like wiring system.

It is fully backward compatible with Fireworld, being a superset of Fireworld and thus Turing-complete. The "electrons" are running on the surface of the wires, which are implemented as the fourth, immutable state. The new rules are as follows:

**A cell is born if surrounded by 1 live (State 1) cell and 2 or 3 wire cells.**

**A cell is born if surrounded by 2 horizontally adjacent living cells and 2 or 3 wire cells.**

Importantly, a cell **can not survive next to wire cells**. 

The last rule helps to "ignite" electrons on the wires by free photons.

# FireWorld

The same as above except that

**A cell is born, if surrounded by 1 orthogonal, 1 diagonal neighboring live cells and 2 or 3 wire cells.**

This meant to match the birth rule of the original Fireworld. 

There is variety called **FireWorld7**, which has the following additional element:

**A cell survives if surrounded by 7 live cells**

This turns 2x2 blocks into guns of period 3. All FireWorld patterns are expected to work identically under this rule, since a natural clash of 7 live cells is highly unlikely.

# FireWorld+

An experimental superset of FireWorld with 3 additional states. Two of them enable diagonal spaceships moving at the speed of light, which turn into regular electrons when they come next to 2 or 3 wire cells. One more state converts regular photons into diagonal ones. 100% backward compatible with FireWorld.

Likewise, there is a variety called **FireWorld7+**
