library(psych)
library(tidyverse)
library(pROC)

# Read in the Colloff High- and Low-similarity data ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/Experiment1.csv") %>% 
    filter(include == "yes") %>% 
    select(condition, targetPresent, participantIDdecision, confidence) %>% 
    rename(cond = "condition",
           culprit_present = "targetPresent",
           id_type = "participantIDdecision",
           conf_level1 = "confidence") %>% 
    mutate(id_type = ifelse(grepl("reject", id_type, ignore.case = TRUE), "reject",
                            ifelse(grepl("filler", id_type, ignore.case = TRUE), "filler", "suspect")),
           culprit_present = ifelse(culprit_present == 0, "absent", "present")) %>% 
    ungroup() %>% 
    mutate(conf_level = round(conf_level1 / 10)+1) %>% 
    mutate(conf_level_rev = max(conf_level)+1 - conf_level) %>% 
    mutate(cond = ifelse(cond == 1, "high_similarity",
                         ifelse(cond == 2, "med_similarity", "low_similarity"))) %>% 
    filter(cond != "med_similarity") %>% 
    mutate(cond = as.character(cond))

TP_high_n = colloff %>% 
    filter(cond == "high_similarity" & culprit_present == "present") %>% 
    nrow()

TP_high_sim = colloff %>% 
    filter(cond == "high_similarity" & culprit_present == "present" & id_type == "suspect") %>% 
    select(conf_level) %>% 
    pull()

TP_high_sim = append(TP_high_sim, rep(0, times = TP_high_n - length(TP_high_sim)))

TA_high_n = colloff %>% 
    filter(cond == "high_similarity" & culprit_present == "absent") %>% 
    nrow()

TA_high_sim = colloff %>% 
    filter(cond == "high_similarity" & culprit_present == "absent" & id_type == "suspect") %>% 
    select(conf_level) %>% 
    pull()

TA_high_sim = append(TA_high_sim, rep(0, times = TA_high_n - length(TA_high_sim)))

partial_threshold = .95

high_sim_ROC = roc(controls = TA_high_sim, 
                   cases = TP_high_sim[!is.na(TP_high_sim)],
                   direction = "<",
                   ci = T,
                   partial.auc = c(1, partial_threshold))

high_sim_ROC
plot(high_sim_ROC)

## low-similarity condition ----
TP_low_n = colloff %>% 
    filter(cond == "low_similarity" & culprit_present == "present") %>% 
    nrow()

TP_low_sim = colloff %>% 
    filter(cond == "low_similarity" & culprit_present == "present" & id_type == "suspect") %>% 
    select(conf_level) %>% 
    pull()

TP_low_sim = append(TP_low_sim, rep(0, times = TP_low_n - length(TP_low_sim)))

TA_low_n = colloff %>% 
    filter(cond == "low_similarity" & culprit_present == "absent") %>% 
    nrow()

TA_low_sim = colloff %>% 
    filter(cond == "low_similarity" & culprit_present == "absent" & id_type == "suspect") %>% 
    select(conf_level) %>% 
    pull()

TA_low_sim = append(TA_low_sim, rep(0, times = TA_low_n - length(TA_low_sim)))

partial_threshold = .95

low_sim_ROC = roc(controls = TA_low_sim, 
                  cases = TP_low_sim[!is.na(TP_low_sim)],
                  direction = "<",
                  ci = T,
                  partial.auc = c(1, partial_threshold))

low_sim_ROC
plot(low_sim_ROC)

roc.test(high_sim_ROC,
         low_sim_ROC)

power.roc.test(high_sim_ROC,
               low_sim_ROC)

data(aSAH)

#### One ROC curve ####

# Build a roc object:
rocobj <- roc(aSAH$outcome, aSAH$s100b)

# Determine power of one ROC curve:
power.roc.test(rocobj)
# Same as:
power.roc.test(ncases=41, ncontrols=72, auc=0.73, sig.level=0.05)
# sig.level=0.05 is implicit and can be omitted:
power.roc.test(ncases=41, ncontrols=72, auc=0.73)

# Determine ncases & ncontrols:
power.roc.test(auc=rocobj$auc, sig.level=0.05, power=0.95, kappa=1.7)
power.roc.test(auc=0.73, sig.level=0.05, power=0.95, kappa=1.7)

# Determine sig.level:
power.roc.test(ncases=41, ncontrols=72, auc=0.73, power=0.95, sig.level=NULL)

# Derermine detectable AUC:
power.roc.test(ncases=41, ncontrols=72, sig.level=0.05, power=0.95)


#### Two ROC curves ####

###  Full AUC
roc1 <- roc(aSAH$outcome, aSAH$ndka)
roc2 <- roc(aSAH$outcome, aSAH$wfns)

## Sample size
# With DeLong variance (default)
power.roc.test(roc1, roc2, power=0.9)
# With Obuchowski variance
power.roc.test(roc1, roc2, power=0.9, method="obuchowski")

## Power test
# With DeLong variance (default)
power.roc.test(roc1, roc2)
# With Obuchowski variance
power_test_store = power.roc.test(roc1, roc2, method="obuchowski")
power_test_store$power
