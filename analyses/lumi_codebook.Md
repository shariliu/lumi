# Codebook: Liu et al. (in prep)

## Column headings, `lumi_data_deid.csv`

1. reliability: Was this subject coded a second time for reliability purposes? (0 = no, 1 = yes)
2. paper: Which paper contributed this data (SCS = Skerry Carey Spelke 2013, LBS = Liu Brooks Spelke)
3. subj_id: de-identified subject ID
4. experiment: name of experiment (Exp.1 - Exp.5 from current paper, and SCS.Exp.1-SCS.Exp.5 from SCS)
5. stimuli: Hab and test stimuli (see Fig 1)
6. training: What kind of training infants underwent (none, ineffective nonsticky mittens, or effective sticky mittens)
7. hab: Whether infants saw constrained or unconstrained habituation
8. first.test: Whether infants saw inefficient or efficient reach first at test
9. goal: Whether infants saw person pick up or change the object's state
10. mitten: Whether infants saw the person act with a mitten on
11. causal: Whether infants saw the person act in a spatiotemporally continuous manner
12. ageday: Age of infants on day of experiment
13. sex: Sex of infant
14. H1-H12: Looking times, trials 1-12 of habituation
15. n_hab: Number of habituation trials infant saw
16. total_hab: Total attention to habituation trials
17. firstthree: Total attention to first 3 habituation trials
18. lastthree: Total attention to last 3 habituation trials
19. habbed: Did infant habituate in 12 trials?
20. T1-T6: Looking times, trials 1-6 of test phase
21. ineff.1 - eff.3: Looks to T1-T6 arranged by first to third efficient and inefficient events
22. prop.ineff1 - prop.ineff3: Proportion looking to inefficient reach, test pairs 1 through 3
23. prop.ineff.all: Average proportion looking to inefficient reach across 3 test pairs
24. avg.ineff - avg.eff: Average raw looking to inefficient and efficient reach across 3 test pairs
25. pref: Did infant look overall longer at efficient or inefficient reach during test?

## Column headings, `lumi_reliabiity.csv`

1. experiment: Experiment 1-5
2. coder: Initials of reliability coder (BP, NG, or DE)
3. trial: Test trial number (1-6)
4. newlook: Looking time produced by reliability coder
5. look.orig: Looking time produced by original coder
6. gap: newlook - look.orig
7. agree: did the two coders agree on trial cutoff? 
