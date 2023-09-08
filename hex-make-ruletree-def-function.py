# A tool for emulating isotropic non-totalistic hexagonal rules in Golly

# Copyright by Yoel Matveyev, 2023
# The GNU General Public License v3.0

# Modify and copy this Python code to the Golly's clipboard, 
# Then run Golly's make-ruletree.py script.

# If prefix = True, add your personal conventional prefix for the rule name, e.g. Hex-B2oS23

prefix = False

# Enter your (conditionally prefixed) rulestring

name = "B02-m3-o4-m56S2m3o4m56H"

b0 = b1 = b2o = b2m = b2p = b3o = b3m = b3p = b4o = b4m = b4p = b5 = b6 = False
s0 = s1 = s2o = s2m = s2p = s3o = s3m = s3p = s4o = s4m = s4p = s5 = s6 = False
n_states = 2
n_neighbors = 8
if prefix: rule_string = name.split("-",1)[1].lower()
else: rule_string = name.lower()
birth_rules = rule_string.split("s")[0]
survival_rules = rule_string.split("s")[1]

# Determining birth rules from the rulestring

if (birth_rules.find('0') != -1): b0 = True
if (birth_rules.find('1') != -1): b1 = True
if (birth_rules.find('2') != -1):
    if (birth_rules.find('2o') != -1): b2o = True
    if (birth_rules.find('2m') != -1): b2m = True
    if (birth_rules.find('2p') != -1): b2p = True
    if (birth_rules.find('2-o') != -1):
        b2m = True
        b2p = True
    if (birth_rules.find('2-m') != -1):
        b2o = True
        b2p = True
    if (birth_rules.find('2-p') != -1):
        b2o = True
        b2m = True
    if (b2o or b2m or b2p) == False:
        b2o = True
        b2m = True
        b2p = True
if (birth_rules.find('3') != -1):
    if (birth_rules.find('3o') != -1): b3o = True
    if (birth_rules.find('3m') != -1): b3m = True
    if (birth_rules.find('3p') != -1): b3p = True
    if (birth_rules.find('3-o') != -1):
        b3m = True
        b3p = True
    if (birth_rules.find('3-m') != -1):
        b3o = True
        b3p = True
    if (birth_rules.find('3-p') != -1):
        b3o = True
        b3m = True
    if (b3o or b3m or b3p) == False:
        b3o = True
        b3m = True
        b3p = True
if (birth_rules.find('4') != -1):
    if (birth_rules.find('4o') != -1): b4o = True
    if (birth_rules.find('4m') != -1): b4m = True
    if (birth_rules.find('4p') != -1): b4p = True
    if (birth_rules.find('4-o') != -1):
        b4m = True
        b4p = True
    if (birth_rules.find('4-m') != -1):
        b4o = True
        b4p = True
    if (birth_rules.find('4-p') != -1):
        b4o = True
        b4m = True
    if (b4o or b4m or b4p) == False:
        b4o = True
        b4m = True
        b4p = True
if (birth_rules.find('5') != -1):
    b5 = True
if (birth_rules.find('6') != -1):
    b6 = True

# Determining survival rules from the rulestring

if (survival_rules.find('0') != -1): s0 = True
if (survival_rules.find('1') != -1): s1 = True
if (survival_rules.find('2') != -1):
    if (survival_rules.find('2o') != -1): s2o = True
    if (survival_rules.find('2m') != -1): s2m = True
    if (survival_rules.find('2p') != -1): s2p = True
    if (survival_rules.find('2-o') != -1):
        s2m = True
        s2p = True
    if (survival_rules.find('2-m') != -1):
        s2o = True
        s2p = True
    if (survival_rules.find('2-p') != -1):
        s2o = True
        s2m = True
    if (s2o or s2m or s2p) == False:
        s2o = True
        s2m = True
        s2p = True
if (survival_rules.find('3') != -1):
    if (survival_rules.find('3o') != -1): s3o = True
    if (survival_rules.find('3m') != -1): s3m = True
    if (survival_rules.find('3p') != -1): s3p = True
    if (survival_rules.find('3-o') != -1):
        s3m = True
        s3p = True
    if (survival_rules.find('3-m') != -1):
        s3o = True
        s3p = True
    if (survival_rules.find('3-p') != -1):
        s3o = True
        s3m = True
    if (s3o or s3m or s3p) == False:
        s3o = True
        s3m = True
        s3p = True
if (survival_rules.find('4') != -1):
    if (survival_rules.find('4o') != -1): s4o = True
    if (survival_rules.find('4m') != -1): s4m = True
    if (survival_rules.find('4p') != -1): s4p = True
    if (survival_rules.find('4-o') != -1):
        s4m = True
        s4p = True
    if (survival_rules.find('4-m') != -1):
        s4o = True
        s4p = True
    if (survival_rules.find('4-p') != -1):
        s4o = True
        s4m = True
    if (s4o or s4m or s4p) == False:
        s4o = True
        s4m = True
        s4p = True
if (survival_rules.find('5') != -1):
    s5 = True
if (survival_rules.find('6') != -1):
    s6 = True

def transition_function(s):
    # s[0..8] are cell states in the order NW, NE, SW, SE, N, W, E, S, C
    # but we ignore the NE and SW corners to emulate a hexagonal grid:
    #             NW N NE         NW  N
    #             W  C  E   ->   W   C  E
    #             SW S SE         S  SE
    NW = s[0]
    # NE = s[1]   (ignored)
    # SW = s[2]   (ignored)
    SE = s[3]
    N  = s[4]
    W  = s[5]
    E  = s[6]
    S  = s[7]
    C  = s[8]
    nc = NW + N + E + SE + S + W   # neighbor count

    # Births

    if b0:
        if C==0 and nc==0:
            return 1
    if b1:
         if C==0 and nc==1:
             return 1
    if b2o:
        if C==0 and nc==2 and (NW+N==2 or N+E==2 or E+SE==2 or
                           SE+S==2 or S+W==2 or W+NW==2): return 1
    if b2m:
        if C==0 and nc==2 and (NW+E==2 or N+SE==2 or E+S==2 or
                           SE+W==2 or S+NW==2 or W+N==2): return 1
    if b2p:
        if C==0 and nc==2 and (NW+SE==2 or N+S==2 or E+W==2): return 1
    if b3o:
        if C==0 and nc==3 and (NW+N+E==3 or N+E+SE==3 or E+SE+S==3 or
                           SE+S+W==3 or S+W+NW==3 or W+NW+N==3): return 1
    if b3m:
        if C==0 and nc==3 and (NW+N+SE==3 or N+E+S==3 or E+SE+W==3 or
                           SE+S+NW==3 or S+W+N==3 or W+NW+E==3 or
                           NW+N+S==3 or N+E+W==3 or E+SE+NW==3 or
                           SE+S+N==3 or S+W+E==3 or W+NW+SE==3): return 1
    if b3p:
        if C==0 and nc==3 and (NW+E+S==3 or N+SE+W==3): return 1
    if b4o:
        if C==0 and nc==4 and (NW+N+E+SE==4 or N+E+SE+S==4 or E+SE+S+W==4 or
                           SE+S+W+NW==4 or S+W+NW+N==4 or W+NW+N+E==4): return 1
    if b4m:
        if C==0 and nc==4 and (NW+N+E+S==4 or N+E+SE+W==4 or E+SE+S+NW==4 or
                           SE+S+W+N==4 or S+W+NW+E==4 or W+NW+N+SE==4): return 1
    if b4p:
        if C==0 and nc==4 and (NW+N+SE+S==4 or N+E+S+W==4 or E+SE+W+NW==4): return 1
    if b5:
        if C==0 and nc==5:
            return 1
    if b6:
        if C==0 and nc==6:
             return 1
    
    # Survivals
    
    if s0:
        if C==1 and nc==0:
            return 1
    if s1:
         if C==1 and nc==1:
             return 1
    if s2o:
        if C==1 and nc==2 and (NW+N==2 or N+E==2 or E+SE==2 or
                           SE+S==2 or S+W==2 or W+NW==2): return 1
    if s2m:
        if C==1 and nc==2 and (NW+E==2 or N+SE==2 or E+S==2 or
                           SE+W==2 or S+NW==2 or W+N==2): return 1
    if s2p:
        if C==1 and nc==2 and (NW+SE==2 or N+S==2 or E+W==2): return 1
    if s3o:
        if C==1 and nc==3 and (NW+N+E==3 or N+E+SE==3 or E+SE+S==3 or
                           SE+S+W==3 or S+W+NW==3 or W+NW+N==3): return 1
    if s3m:
        if C==1 and nc==3 and (NW+N+SE==3 or N+E+S==3 or E+SE+W==3 or
                           SE+S+NW==3 or S+W+N==3 or W+NW+E==3 or
                           NW+N+S==3 or N+E+W==3 or E+SE+NW==3 or
                           SE+S+N==3 or S+W+E==3 or W+NW+SE==3): return 1
    if s3p:
        if C==1 and nc==3 and (NW+E+S==3 or N+SE+W==3): return 1
    if s4o:
        if C==1 and nc==4 and (NW+N+E+SE==4 or N+E+SE+S==4 or E+SE+S+W==4 or
                           SE+S+W+NW==4 or S+W+NW+N==4 or W+NW+N+E==4): return 1
    if s4m:
        if C==1 and nc==4 and (NW+N+E+S==4 or N+E+SE+W==4 or E+SE+S+NW==4 or
                           SE+S+W+N==4 or S+W+NW+E==4 or W+NW+N+SE==4): return 1
    if s4p:
        if C==1 and nc==4 and (NW+N+SE+S==4 or N+E+S+W==4 or E+SE+W+NW==4): return 1
    if s5:
        if C==1 and nc==5:
            return 1
    if s6:
        if C==1 and nc==6:
             return 1
    
    # death in all other cases
    return 0
