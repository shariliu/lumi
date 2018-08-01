
### load required packages
ipak <- function (pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "Hmisc", "lattice", "multcomp", "lsmeans", "schoRsch", "influence.ME", "irr", "devtools", "skimr", "simr", "lme4", "sjPlot", "effects", "lmerTest", "ICC", "compute.es", "pwr", "stringr")

ipak(packages)

subj_codebook <- read.csv("/Users/shariliu/Documents/HarvardLDS/Studies/LUMI/github/analyses/subj_codebook.csv", header = TRUE) # change to your local directory
incomplete_rel <- read.csv("/Users/shariliu/Documents/HarvardLDS/Studies/LUMI/github/analyses/lumi_reliability.csv", header = TRUE)
lumi.wide <- read.csv("/Users/shariliu/Documents/HarvardLDS/Studies/LUMI/github/analyses/lumi_data_deid.csv", header = TRUE)

lumi.long <- lumi.wide %>%
  filter(reliability == 1) %>%
  gather(trial, look.orig, T1:T6) %>%
  select(subj_id, trial, look.orig)

lumi.long$trial <- as.numeric(str_replace_all(lumi.long$trial, 'T', ''))
lumi.long$subj_id <- factor(lumi.long$subj_id)

write.csv(lumi.long, file = "lumi_data_subset_reliability.csv")
lumi.long$check <- ! lumi.long$subj_id %in% incomplete_rel$deid 

incomplete_rel$match <- incomplete_rel$subj %in% subj_codebook$id

incomplete_rel$subj_id <- subj_codebook$subj_id[match(incomplete_rel$subj, subj_codebook$id)]
incomplete_rel$deid <- factor(incomplete_rel$deid)
incomplete_rel$orig_id <- subj_codebook$id[match(incomplete_rel$subj, subj_codebook$id)]
complete_rel <- merge(incomplete_rel, lumi.long, by=c("subj_id", "trial", "experiment"))

complete_rel <- complete_rel %>%
  mutate(newlook = replace(look, look > 45, 45)) %>%
  mutate(gap = newlook - look.orig) %>%
  select(experiment, subj_id, coder, orig_id, trial, newlook, look.orig, gap)

str(complete_rel)

ggplot(data = complete_rel, aes(experiment, gap, fill = experiment))+
  geom_point()+
  geom_boxplot(alpha=0.5)+
  facet_wrap(~coder)

write.csv(complete_rel, file = "reliability_data_1aug18.csv")
plot(complete_rel$newlook, complete_rel$look.orig)

find_index <- function(x,y) x==y

incomplete_rel$look_orig <- lumi.long$look[match(c(incomplete_rel$deid,incomplete_rel$trial), c(lumi.long$subj_id,lumi.long$trial))]

ICCest(complete_rel$newlook, complete_rel$look.orig)
icc(cbind(filter(complete_rel, experiment == "Exp.5")$newlook, filter(complete_rel, experiment == "Exp.5")$look.orig), model = "twoway", type = "agreement", unit = "single") # Study 1: ICC = 0.994, agreement = 95%


