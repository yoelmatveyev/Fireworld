The ruletable generator is written in Common Lisp. Using it does not require familiarity with the language. Just install Common Lisp, run it and write the function for rule table generation in the REPL, Lisp's interactive system.

This code was tested on SBCL, but should work with any other Common Lisp implementation.

To load the code, write in REPL 

(load "./golly-rule-generator.lisp")

Change the path to point to your location of that file.

To preview your rule table, write, for example:

(print-rule-table "3D2a-2n21ar111ar")

You can add your comment and the name to the rule. By default, it's named by its notation:

(print-rule-table "3D2a-2n21ar111ar" :comment "I discovered this rule today" :rulename "Myrule1")

To save your rule into your Golly rules directory ~/.golly/Rules/ write:

(save-rule-table "3D2a-2n21ar111ar" :comment "I discovered this rule today" :rulename "Myrule1")

Your rule table file will be called Myrule1.rule. You can provide a different path with the :path key.

The function (gen-rules n) gives the list of all distinct and consistent birth rules for an automation with n states. (gen-rules n :b nil) gives the longer list of survival and transformation rules.

About the notation of multistate cyclical rules, read **Multistate_cyclical_CA.md**
