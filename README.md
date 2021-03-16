
To run the training, do the following.

1. Pick a language name ("MYLANG" in the following).

2. Create two files (*):

  corpus/MYLANG-ud-train.conllu
  corpus/MYLANG-ud-test.conllu
  
3. Edit the variables at the beginning of UDSimp.hs

- lang: set this to "MYLANG"

The defaults of the other variables should be OK. But:

- maxTraining: set this to "Just n" if you want to only use the first n sentences in the training corpus (e.g. when training takes too long time or when you want to see how much training data is actually needed).

- minTesting: this is only used for when there is only one corpus. The single available corpus is then automatically split into a training corpus and a testing corpus. This number specifies how many sentences you want to have then in the testing corpus.

- onlyMainTag: should used tags be only NOUN, VERB, PRON (True) or also Number=pl, Gender=fem (False). True leads to much faster training but possibly worse rules (but it's not clear if they actually become worse).

- preciseRules: default True. False is experimental (and very promising!). Still working on this.

Output of the program:

SCORE_L | ...rule... | SCORE_T

SCORE is a% - b% where a% is the percentage of correctly drawn arrows so far, and b% is the maximum possible percentage of correctly drawn arrows. (The final % of correct arrows will be between a% and b%).

SCORE_L is for the training corpus, before the displayed rule. Expected is that b% is always 100% because the rules don't do the wrong thing in the training corpus (unless preciseRules = False).

SCORE_T is for the testing corpus, after the displayed rule.

