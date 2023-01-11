library(psych)
library(tidyverse)
library(pROC)
library(lubridate)
library(here)

source("ROC_power_app/scripts/func.R") # helper functions

# Colloff et al., 2021 (Interactive lineups) ----
interactive = read.csv("./Dataset testing and reports/Data/Colloff et al., 2020/Exp1_osf_data.csv") %>% 
    dplyr::select(Confidence, Include, TargetPresent, IDResponse, OwnRace, LineupType) %>% 
    filter(Include == "yes") %>% 
    dplyr::group_by(Confidence, TargetPresent, IDResponse, OwnRace, LineupType) %>% 
    count() %>% 
    mutate(total = ifelse(IDResponse == "foil" & TargetPresent == "no", round(n * 5/6), n))

interactive_suspects = interactive %>% 
    filter(IDResponse == "foil" & TargetPresent == "no") %>% 
    mutate(IDResponse = "perpetrator",
           total = round(n * 1/6))

interactive = rbind(interactive,
                    interactive_suspects) %>% 
    mutate(IDResponse = ifelse(IDResponse == "foil", "filler",
                               ifelse(IDResponse == "perpetrator", "suspect", "reject")),
           TargetPresent = ifelse(TargetPresent == "no", "absent", "present"))

conf_store = NA
presence_store = NA
response_store = NA
race_store = NA
cond_store = NA

for (i in 1:nrow(interactive)) {
    conf_store = append(conf_store, rep(interactive$Confidence[i], times = interactive$total[i]))
    presence_store = append(presence_store, rep(interactive$TargetPresent[i], times = interactive$total[i]))
    response_store = append(response_store, rep(interactive$IDResponse[i], times = interactive$total[i]))
    race_store = append(race_store, rep(interactive$OwnRace[i], times = interactive$total[i]))
    cond_store = append(cond_store, rep(interactive$LineupType[i], times = interactive$total[i]))
}

interactive_expand = data.frame(cond = cond_store,
                                conf_level = conf_store,
                                culprit_present = presence_store,
                                id_type = response_store,
                                race = race_store) %>% 
    filter(!is.na(cond)) %>% 
    mutate(conf_level1 = conf_level,
           conf_level = round(conf_level1 / 10)+1)

write.csv(interactive_expand,
          "./Dataset testing and reports/Data/Colloff et al., 2020/Exp1_osf_data_processed.csv",
          row.names = FALSE,
          na = "")

# Experiment 2
interactive = read.csv("./Dataset testing and reports/Data/Colloff et al., 2020/Exp2_osf_data.csv") %>% 
    dplyr::select(Confidence, IncludeFinalSample, TargetPresent, IDResponse, OwnRace, LineupType) %>% 
    filter(IncludeFinalSample == "yes") %>% 
    dplyr::group_by(Confidence, TargetPresent, IDResponse, OwnRace, LineupType) %>% 
    count() %>% 
    mutate(total = ifelse(IDResponse == "foil" & TargetPresent == "no", round(n * 5/6), n))

interactive_suspects = interactive %>% 
    filter(IDResponse == "foil" & TargetPresent == "no") %>% 
    mutate(IDResponse = "perpetrator",
           total = round(n * 1/6))

interactive = rbind(interactive,
                    interactive_suspects) %>% 
    mutate(IDResponse = ifelse(IDResponse == "foil", "filler",
                               ifelse(IDResponse == "perpetrator", "suspect", "reject")),
           TargetPresent = ifelse(TargetPresent == "no", "absent", "present"))

conf_store = NA
presence_store = NA
response_store = NA
race_store = NA
cond_store = NA

for (i in 1:nrow(interactive)) {
    conf_store = append(conf_store, rep(interactive$Confidence[i], times = interactive$total[i]))
    presence_store = append(presence_store, rep(interactive$TargetPresent[i], times = interactive$total[i]))
    response_store = append(response_store, rep(interactive$IDResponse[i], times = interactive$total[i]))
    race_store = append(race_store, rep(interactive$OwnRace[i], times = interactive$total[i]))
    cond_store = append(cond_store, rep(interactive$LineupType[i], times = interactive$total[i]))
}

interactive_expand = data.frame(cond = cond_store,
                                conf_level = conf_store,
                                culprit_present = presence_store,
                                id_type = response_store,
                                race = race_store) %>% 
    filter(!is.na(cond)) %>% 
    mutate(conf_level1 = conf_level,
           conf_level = round(conf_level1 / 10)+1)

interactive_joint_simultaneous = interactive_expand %>% 
    filter(cond == "joint_simultaneous")

write.csv(interactive_joint_simultaneous,
          "./Dataset testing and reports/Data/Colloff et al., 2020/Exp2_osf_data_processed_joint_simultaneous.csv",
          row.names = FALSE,
          na = "")

# Dataset for testing a null effect
## Colloff et al., 2021 data ----
### Exp. 1 ----
#### Medium similarity ----
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
    filter(cond == "med_similarity") %>% 
    mutate(cond = as.character(cond))

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/Null Effect Testing/colloff_2021_processed_med.csv",
          row.names = FALSE,
          na = "")


if (!c("id_type") %in% colnames(colloff) | !c("culprit_present") %in% colnames(colloff) | !c("conf_level") %in% colnames(colloff)) {
    print("Failure!")
} else {
    print("Success")
}

data_test = read.csv("./Dataset testing and reports/Data/ROCeg.csv", fileEncoding = "UTF-8-BOM")

if (!("cond") %in% colnames(data_test)) {
    data_test$cond = "A"
} else {
    data_test$cond = data_test$cond
}

length(unique(data_test$cond))

# Processing of other datasets ----
## Colloff et al., 2021 data ----
### Exp. 1 ----
#### High similarity vs. Low similarity ----
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
    mutate(cond = as.character(cond),
           suspect_position = NA,
           lineup_size = 6)

sum(table(colloff$cond))

density(colloff$suspect_prob[!is.na(colloff$suspect_prob)]) %>%
    plot()

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/colloff_2021_processed_high_low.csv",
          row.names = FALSE,
          na = "")

#### High similarity vs. Medium similarity ----
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
    filter(cond != "low_similarity") %>% 
    mutate(cond = as.character(cond),
           suspect_position = NA,
           lineup_size = 6)

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/colloff_2021_processed_high_med.csv",
          row.names = FALSE,
          na = "")

#### Low similarity vs. Medium similarity ----
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
    filter(cond != "high_similarity") %>% 
    mutate(cond = as.character(cond),
           suspect_position = NA,
           lineup_size = 6)

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/colloff_2021_processed_low_med.csv",
          row.names = FALSE,
          na = "")

### Exp. 2 ----
#### High similarity vs. Low similarity ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 2/Experiment2.csv") %>% 
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

sum(table(colloff$cond))

density(colloff$suspect_prob[!is.na(colloff$suspect_prob)]) %>%
    plot()

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 2/colloff_2021_processed_high_low.csv",
          row.names = FALSE,
          na = "")

#### High similarity vs. Medium similarity ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 2/Experiment2.csv") %>% 
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
    filter(cond != "low_similarity") %>% 
    mutate(cond = as.character(cond))

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 2/colloff_2021_processed_high_med.csv",
          row.names = FALSE,
          na = "")

## Colloff et al., 2021b ----
### Exp 1 ----
#### Same vs. Different ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 1/Exp1_Data.csv") %>%
    mutate(suspect_position = ifelse(lineup.1 == "perp", 1,
                                     ifelse(lineup.2 == "perp", 2,
                                            ifelse(lineup.3 == "perp", 3,
                                                   ifelse(lineup.4 == "perp", 4,
                                                          ifelse(lineup.5 == "perp", 5,
                                                                 ifelse(lineup.6 == "perp", 6,
                                                                        ifelse(lineup.7 == "perp", 7,
                                                                               ifelse(lineup.8 == "perp", 8,
                                                                                      ifelse(lineup.9 == "perp", 9, NA)))))))))) %>% 
    dplyr::select(conditionSameDiff, 
                  targetPresentRaw,
                  participantSelection,
                  confidence,
                  suspect_position) %>% 
    mutate(cond = ifelse(conditionSameDiff == 1, "Same",
                         ifelse(conditionSameDiff == 2, "Different", "Same + Different")),
           culprit_present = ifelse(grepl("Present", targetPresentRaw), "present", "absent"),
           conf_level = as.numeric(confidence),
           id_type = ifelse(participantSelection == 1, "suspect",
                            ifelse(participantSelection == 2, "filler", "reject")),
           id_type = ifelse(culprit_present == "absent" & id_type == "filler", "suspect", id_type)) %>% 
    filter(cond != "Same + Different") %>% 
    select(id_type, conf_level, culprit_present, cond, suspect_position) %>% 
    mutate(exp = "Colloff et al. (2021b): Exp 1: Same-pose vs. Different-pose encoding-test",
           conf_level = (conf_level/10)+1,
           lineup_size = 9)

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 1/Exp1_Data_SameVsDiff.csv",
          row.names = FALSE,
          na = "")

write.csv(colloff,
          "./Dataset testing and reports/Data/01 - All data files for combining/Exp1_Data_SameVsDiff.csv",
          row.names = FALSE,
          na = "")

# AUC specificity = .618

#### Same + Different vs. Different ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 1/Exp1_Data.csv") %>%
    mutate(suspect_position = ifelse(lineup.1 == "perp", 1,
                                     ifelse(lineup.2 == "perp", 2,
                                            ifelse(lineup.3 == "perp", 3,
                                                   ifelse(lineup.4 == "perp", 4,
                                                          ifelse(lineup.5 == "perp", 5,
                                                                 ifelse(lineup.6 == "perp", 6,
                                                                        ifelse(lineup.7 == "perp", 7,
                                                                               ifelse(lineup.8 == "perp", 8,
                                                                                      ifelse(lineup.9 == "perp", 9, NA)))))))))) %>% 
    dplyr::select(conditionSameDiff, 
                  targetPresentRaw,
                  participantSelection,
                  confidence,
                  suspect_position) %>% 
    mutate(cond = ifelse(conditionSameDiff == 1, "Same",
                         ifelse(conditionSameDiff == 2, "Different", "Same + Different")),
           culprit_present = ifelse(grepl("Present", targetPresentRaw), "present", "absent"),
           conf_level = as.numeric(confidence),
           id_type = ifelse(participantSelection == 1, "suspect",
                            ifelse(participantSelection == 2, "filler", "reject")),
           id_type = ifelse(culprit_present == "absent" & id_type == "filler", "suspect", id_type)) %>%
    select(id_type, conf_level, culprit_present, cond, suspect_position) %>% 
    mutate(exp = "Colloff et al. (2021b): Exp 1: Same-plus-additional-pose vs. Different-pose encoding-test",
           conf_level = (conf_level/10)+1,
           lineup_size = 9) %>% 
    filter(cond != "Same")

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 1/Exp1_Data_SamePlusDiffVsDiff.csv",
          row.names = FALSE,
          na = "")

write.csv(colloff,
          "./Dataset testing and reports/Data/01 - All data files for combining/Exp1_Data_SamePlusDiffVsDiff.csv",
          row.names = FALSE,
          na = "")

# AUC specificity = .618

#### Same + Different vs. Same ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 1/Exp1_Data.csv") %>%
    mutate(suspect_position = ifelse(lineup.1 == "perp", 1,
                                     ifelse(lineup.2 == "perp", 2,
                                            ifelse(lineup.3 == "perp", 3,
                                                   ifelse(lineup.4 == "perp", 4,
                                                          ifelse(lineup.5 == "perp", 5,
                                                                 ifelse(lineup.6 == "perp", 6,
                                                                        ifelse(lineup.7 == "perp", 7,
                                                                               ifelse(lineup.8 == "perp", 8,
                                                                                      ifelse(lineup.9 == "perp", 9, NA)))))))))) %>% 
    dplyr::select(conditionSameDiff, 
                  targetPresentRaw,
                  participantSelection,
                  confidence,
                  suspect_position) %>% 
    mutate(cond = ifelse(conditionSameDiff == 1, "Same",
                         ifelse(conditionSameDiff == 2, "Different", "Same + Different")),
           culprit_present = ifelse(grepl("Present", targetPresentRaw), "present", "absent"),
           conf_level = as.numeric(confidence),
           id_type = ifelse(participantSelection == 1, "suspect",
                            ifelse(participantSelection == 2, "filler", "reject")),
           id_type = ifelse(culprit_present == "absent" & id_type == "filler", "suspect", id_type)) %>%
    select(id_type, conf_level, culprit_present, cond, suspect_position) %>% 
    mutate(exp = "Colloff et al. (2021b): Exp 1: Same-plus-additional-pose vs. Same-pose encoding-test",
           conf_level = (conf_level/10)+1,
           lineup_size = 9) %>% 
    filter(cond != "Different")

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 1/Exp1_Data_SamePlusDiffVsSame.csv",
          row.names = FALSE,
          na = "")

write.csv(colloff,
          "./Dataset testing and reports/Data/01 - All data files for combining/Exp1_Data_SamePlusDiffVsSame.csv",
          row.names = FALSE,
          na = "")

#### Only Same ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 1/Exp1_Data.csv") %>% 
    dplyr::select(conditionSameDiff, 
                  targetPresentRaw,
                  participantSelection,
                  confidence) %>% 
    mutate(cond = ifelse(conditionSameDiff == 1, "Same",
                         ifelse(conditionSameDiff == 2, "Different", "Same + Different")),
           culprit_present = ifelse(grepl("Present", targetPresentRaw), "present", "absent"),
           conf_level = as.numeric(confidence),
           id_type = ifelse(participantSelection == 1, "suspect",
                            ifelse(participantSelection == 2, "filler", "reject")),
           id_type = ifelse(culprit_present == "absent" & id_type == "filler", "suspect", id_type)) %>% 
    filter(cond == "Same")

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 1/Exp1_Data_Same.csv",
          row.names = FALSE,
          na = "")

### Exp 2 ----
#### High vs. Low pose reinstatement ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 2/Exp2_Data.csv") %>% 
    dplyr::select(MouseGroup, 
                  TargetPresent,
                  ParticipantSelection,
                  Confidence,
                  contains("Lineup")) %>% 
    mutate(culprit_present = ifelse(TargetPresent == 0, "absent", "present"),
           conf_level = as.numeric(Confidence),
           id_type = ifelse(ParticipantSelection == "target", "suspect",
                            ifelse(ParticipantSelection == "filler", "filler", "reject")),
           cond = MouseGroup) %>% 
    mutate(suspect_position = ifelse(Lineup1 == 6, 1,
                                     ifelse(Lineup2 == 6, 2,
                                            ifelse(Lineup3 == 6, 3,
                                                   ifelse(Lineup4 == 6, 4,
                                                          ifelse(Lineup5 == 6, 5,
                                                                 ifelse(Lineup6 == 6, 6, NA))))))) %>% 
    select(id_type, conf_level, culprit_present, cond, suspect_position) %>% 
    mutate(exp = "Colloff et al. (2021b): Exp 2: High vs. Low pose reinstatement",
           conf_level = (conf_level/10)+1,
           lineup_size = 6)

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 2/Exp2_Data_HighLow.csv",
          row.names = FALSE,
          na = "")

write.csv(colloff,
          "./Dataset testing and reports/Data/01 - All data files for combining/Exp2_Data_HighLow.csv",
          row.names = FALSE,
          na = "")

# AUC specificity = .59

## Colloff et al., 2018 ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al., 2018/ColloffWadeStrangeWixtedData.csv") %>% 
    filter(include == "yes") %>% 
    mutate(cond = treatmentLabel,
           conf_level = confidence,
           id_type = ifelse(faceSelected == "yes", "suspect", "reject"),
           culprit_present = targetLabel) %>% 
    dplyr::select(cond,
                  conf_level,
                  id_type,
                  culprit_present)

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al., 2018/ColloffWadeStrangeWixtedData_Processed.csv",
          row.names = FALSE,
          na = "")

# AUC specificity = .77

## Morgan et al., 2019 ----
### Sleep vs. Wake ----
morgan = read.csv("./Dataset testing and reports/Data/Morgan et al., 2019/RSOS_Data.csv") %>% 
    dplyr::select(Confidence, Correct.ID, Filler.ID, Target.Present.or.Target.Absent.Lineup, Condition) %>% 
    filter(Condition != "AM" & Condition != "PM") %>% 
    rename(cond = "Condition",
           conf_level = "Confidence",
           culprit_present = "Target.Present.or.Target.Absent.Lineup") %>% 
    mutate(culprit_present = ifelse(culprit_present == "Target Present", "present", "absent"),
           id_type = ifelse(culprit_present == "present" & Correct.ID == 1, "suspect",
                            ifelse(culprit_present == "present" & Filler.ID == 1, "filler",
                                   ifelse(culprit_present == "absent" & Filler.ID == 1, "filler",
                                          "reject"))),
           conf_level = str_remove(conf_level, "%")) %>% 
    mutate(conf_level = (as.numeric(conf_level)/10)+1,
           lineup_size = 6,
           exp = "Morgan et al. (2019): Exp 1: Sleep vs. Wake pre-lineup")

write.csv(morgan,
          "./Dataset testing and reports/Data/Morgan et al., 2019/morgan_processed_sleep_wake.csv",
          row.names = FALSE,
          na = "")

write.csv(morgan,
          "./Dataset testing and reports/Data/01 - All data files for combining/morgan_processed_sleep_wake.csv",
          row.names = FALSE,
          na = "")

### AM vs. PM ----
morgan = read.csv("./Dataset testing and reports/Data/Morgan et al., 2019/RSOS_Data.csv") %>% 
    dplyr::select(Confidence, Correct.ID, Filler.ID, Target.Present.or.Target.Absent.Lineup, Condition) %>% 
    filter(Condition != "Sleep" & Condition != "Wake") %>% 
    rename(cond = "Condition",
           conf_level = "Confidence",
           culprit_present = "Target.Present.or.Target.Absent.Lineup") %>% 
    mutate(culprit_present = ifelse(culprit_present == "Target Present", "present", "absent"),
           id_type = ifelse(culprit_present == "present" & Correct.ID == 1, "suspect",
                            ifelse(culprit_present == "present" & Filler.ID == 1, "filler",
                                   ifelse(culprit_present == "absent" & Filler.ID == 1, "filler",
                                          "reject"))),
           conf_level = str_remove(conf_level, "%")) %>% 
    mutate(conf_level = (as.numeric(conf_level)/10)+1,
           lineup_size = 6,
           exp = "Morgan et al. (2019): AM vs. PM lineup")

write.csv(morgan,
          "./Dataset testing and reports/Data/Morgan et al., 2019/morgan_processed_AM_PM.csv",
          row.names = FALSE,
          na = "")

write.csv(morgan,
          "./Dataset testing and reports/Data/01 - All data files for combining/morgan_processed_AM_PM.csv",
          row.names = FALSE,
          na = "")

## Akan et al., 2021 ----
### Exp 1 ----
akan = read.csv("./Dataset testing and reports/Data/Akan et al., 2021/Exp 1/Experiment1.csv") %>% 
    select(TP.TA, 
           Condition,
           Target.Image,
           Response,
           Confidence) %>% 
    rename(culprit_present = "TP.TA",
           cond = "Condition",
           conf_level = "Confidence") %>% 
    mutate(culprit_present = ifelse(culprit_present == 1, "present", "absent"),
           id_type = ifelse(grepl("Perpetrator", Response), "reject",
                            ifelse(Target.Image == Response & !is.na(Target.Image), "suspect", "filler")))

# Manually change some TA filler IDs to false IDs
#for (i in 1:nrow(akan)) {
#    if (akan$culprit_present[i] == "absent" & akan$id_type[i] == "filler") {
#        akan$id_type2[i] = sample(c("suspect", "filler"),
#                                  size = 1,
#                                  prob = c(akan$false_prob[i], 1 - akan$false_prob[i]))
#    } else {
#        akan$id_type2[i] = NA
#    }
#} 

akan = akan %>% 
    #mutate(id_type = ifelse(!is.na(id_type2), id_type2, id_type))  %>% 
    filter(cond == 1 | cond == 6) %>% 
    mutate(lineup_size = cond,
           cond = as.factor(cond),
           conf_level = (conf_level/10)+1,
           exp = "Akan et al. (2021): Exp 1: Showup vs. 6-person")

write.csv(akan,
          "./Dataset testing and reports/Data/Akan et al., 2021/Exp 1/Experiment1_1vs6.csv",
          na = "",
          row.names = FALSE)

write.csv(akan,
          "./Dataset testing and reports/Data/01 - All data files for combining/Experiment1_1vs6.csv",
          na = "",
          row.names = FALSE)

1 - .065

### Exp 2 ----
#### Showup vs. 3 ----
akan = read.csv("./Dataset testing and reports/Data/Akan et al., 2021/Exp 2/Experiment2.csv") %>% 
    select(ï..SubjectNo,
            LineupSize,
            TargetPresence..1.absent..2.present.,
            AnswerCoded,
            Confidence) %>% 
    rename(cond = "LineupSize",
           culprit_present = "TargetPresence..1.absent..2.present.",
           id_type = "AnswerCoded",
           conf_level = "Confidence",
           subj = "ï..SubjectNo") %>% 
    mutate(culprit_present = ifelse(culprit_present == 1, "absent", "present"),
           id_type = ifelse(id_type == "TargetPick" | id_type == "InnocentPick", "suspect",
                            ifelse(id_type == "FoilPick", "filler", "reject"))) %>% 
    filter(cond == 1 | cond == 3) %>% 
    mutate(lineup_size = cond,
           cond = as.factor(cond),
           exp = "Akan et al. (2021): Exp 2: Showup vs. 3-person")

write.csv(akan,
          "./Dataset testing and reports/Data/Akan et al., 2021/Exp 2/Experiment2_1vs3.csv",
          row.names = FALSE,
          na = "")

write.csv(akan,
          "./Dataset testing and reports/Data/01 - All data files for combining/Experiment2_1vs3.csv",
          row.names = FALSE,
          na = "")

#### Showup vs. 6 ----
akan = read.csv("./Dataset testing and reports/Data/Akan et al., 2021/Exp 2/Experiment2.csv") %>% 
    select(ï..SubjectNo,
            LineupSize,
            TargetPresence..1.absent..2.present.,
            AnswerCoded,
            Confidence) %>% 
    rename(cond = "LineupSize",
           culprit_present = "TargetPresence..1.absent..2.present.",
           id_type = "AnswerCoded",
           conf_level = "Confidence",
           subj = "ï..SubjectNo") %>% 
    mutate(culprit_present = ifelse(culprit_present == 1, "absent", "present"),
           id_type = ifelse(id_type == "TargetPick" | id_type == "InnocentPick", "suspect",
                            ifelse(id_type == "FoilPick", "filler", "reject"))) %>% 
    filter(cond == 1 | cond == 6) %>% 
    mutate(lineup_size = cond,
           cond = as.factor(cond),
           exp = "Akan et al. (2021): Exp 2: Showup vs. 6-person")

write.csv(akan,
          "./Dataset testing and reports/Data/Akan et al., 2021/Exp 2/Experiment2_1vs6.csv",
          row.names = FALSE,
          na = "")

write.csv(akan,
          "./Dataset testing and reports/Data/01 - All data files for combining/Experiment2_1vs6.csv",
          row.names = FALSE,
          na = "")

# Smith et al., 2022 ----
## Experiment 1 ----
smith_exp1 = read.csv("./Dataset testing and reports/Data/Smith et al., 2022/exp1.csv") %>% 
    select(fawarning,
           parade,
           resp_expec,
           target,
           conf,
           suspect_id,
           filler_id,
           no_id) %>% 
    mutate(id_type = ifelse(suspect_id == TRUE, "suspect",
                            ifelse(filler_id == TRUE, "filler", "reject")),
           suspect_position = ifelse(resp_expec == 7, 7,
                                     ifelse(resp_expec == 3, 3, NA)),
           lineup_size = 9) %>% 
    rename("conf_level" = conf,
           "culprit_present" = target) %>% 
    select(-c(suspect_id, filler_id, no_id, resp_expec))

### Serial warning vs. no-warning ----
smith_exp1_serial = smith_exp1 %>% 
    filter(parade == "serial") %>% 
    rename("cond" = fawarning) %>% 
    select(-parade) %>% 
    mutate(exp = "Smith et al. (2022): Exp 1: Serial parade warning vs. no-warning")

write.csv(smith_exp1_serial,
          "./Dataset testing and reports/Data/Smith et al., 2022/exp1_serial_processed.csv",
          row.names = FALSE,
          na = "")

### Sequential warning vs. no-warning ----
smith_exp1_sequential = smith_exp1 %>% 
    filter(parade == "sequential") %>% 
    rename("cond" = fawarning) %>% 
    select(-parade) %>% 
    mutate(exp = "Smith et al. (2022): Exp 1: Sequential parade warning vs. no-warning")

write.csv(smith_exp1_sequential,
          "./Dataset testing and reports/Data/Smith et al., 2022/exp1_sequential_processed.csv",
          row.names = FALSE,
          na = "")

## Experiment 2 ----
smith_exp2 = read.csv("./Dataset testing and reports/Data/Smith et al., 2022/exp2_serial.csv") %>% 
    select(pass,
           resp_expec,
           target,
           conf,
           suspect_id,
           filler_id,
           no_id) %>% 
    mutate(id_type = ifelse(suspect_id == TRUE, "suspect",
                            ifelse(filler_id == TRUE, "filler", "reject")),
           suspect_position = ifelse(resp_expec == 7, 7,
                                     ifelse(resp_expec == 3, 3, NA)),
           lineup_size = 9) %>% 
    rename("conf_level" = conf,
           "culprit_present" = target,
           "cond" = pass) %>% 
    select(-c(suspect_id, filler_id, no_id, resp_expec)) %>% 
    mutate(exp = "Smith et al. (2022): Exp 2: Serial parade 1-pass vs. 2-passes")

write.csv(smith_exp2,
          "./Dataset testing and reports/Data/Smith et al., 2022/exp2_serial_processed.csv",
          row.names = FALSE,
          na = "")

# Kaesler et al. (2020) Simultaneous vs. Sequential ----
kaesler = read.csv("./Dataset testing and reports/Data/Kaesler et al., 2020/Kaesler_2020data.csv") %>% 
    rename("cond" = Lineup_Method,
           "culprit_present" = Lineup_Type,
           "conf_level" = id_confidence,
           "suspect_position" = Target_Position) %>% 
    mutate(culprit_present = ifelse(culprit_present == "Target Present", "present", "absent"),
           conf_level = round(conf_level/10)+1,
           lineup_size = 6,
           id_type = ifelse(id_selection == "Silhouette", "reject",
                            ifelse(target == id_selection, "suspect", "filler"))) %>% 
    select(cond, culprit_present, id_type, conf_level, suspect_position, lineup_size) %>% 
    mutate(exp = "Kaesler et al. (2020): Exp 1: Simultaneous vs. Sequential")

write.csv(kaesler,
          "./Dataset testing and reports/Data/Kaesler et al., 2020/Kaesler_2020data_processed.csv",
          row.names = FALSE,
          na = "")

write.csv(kaesler,
          "./Dataset testing and reports/Data/01 - All data files for combining/Kaesler_2020data_processed.csv",
          row.names = FALSE,
          na = "")

## Generating a dataset to use for estimation (i.e., creating designated innocent suspects) ----
kaesler = read.csv("./Dataset testing and reports/Data/Kaesler et al., 2020/Kaesler_2020data.csv") %>% 
    rename("cond" = Lineup_Method,
           "culprit_present" = Lineup_Type,
           "conf_level" = id_confidence,
           "suspect_position" = Target_Position) %>% 
    mutate(culprit_present = ifelse(culprit_present == "Target Present", "present", "absent"),
           conf_level = round(conf_level/10)+1,
           lineup_size = 6,
           id_type = ifelse(id_selection == "Silhouette", "reject",
                            ifelse(target == id_selection, "suspect", "filler"))) %>% 
    select(cond, culprit_present, id_type, conf_level, suspect_position, lineup_size) %>% 
    mutate(exp = "Kaesler et al. (2020): Exp 1: Simultaneous vs. Sequential",
           suspect_prob = 1/6,
           filler_prob = 5/6) %>% 
    rowwise() %>% 
    mutate(id_type = ifelse(culprit_present == "absent" & id_type == "filler",
                            sample(c("suspect", "filler"), 1, prob = c(suspect_prob, filler_prob)), id_type),
           suspect_position = ifelse(suspect_position == 0 & cond == "Sequential",
                                     sample(c(1:6), 1), suspect_position))

write.csv(kaesler,
          "./Dataset testing and reports/Data/Kaesler et al., 2020/Kaesler_2020data_processed_for_estimation.csv",
          row.names = FALSE,
          na = "")

# Seale-Carlisle & Mickes (2016) US vs. UK lineups ----
## Image 3 is the culprit ----
## Generating a dataset to use for estimation (i.e., creating designated innocent suspects) ----
seale = read.csv("./Dataset testing and reports/Data/Seale-Carlisle & Mickes, 2016/USvUKdata.csv") %>% 
    `colnames<-` (c("expnumber", "subj", "age", "gender", "cond",
                    "culprit_present", "img1", "img2", "img3", "img4", "img5", "img6", "img7", "img8", "img9",
                    "decision", "conf_level", "accuracy", "q1", "q2", "q3", "q4", "q5",
                    "rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9",
                    "rep_sum", "repeated", "absent_present", "include")) %>% 
    filter(include == 1) %>% 
    select(cond, culprit_present, contains("img"), decision, conf_level, accuracy) %>% 
    mutate(culprit_present = ifelse(culprit_present == "Target-present", "present", "absent"))

seale_replaced = data.frame(lapply(seale, function(x) {
    y = gsub("images.*/", "", x) 
    gsub(".jpg", "", y)
}))

seale_replaced = seale_replaced %>% 
    rowwise() %>% 
    mutate(random_column = ifelse(cond == "UK", sample(c(3:11), size = 1),
                                  sample(c(3:8), size = 1)))

for (i in 1:nrow(seale_replaced)) {
    curr_suspect = seale_replaced[i, seale_replaced$random_column[i]]
    
    seale_replaced$random_suspect[i] = curr_suspect
}

seale = seale_replaced %>% 
    ungroup() %>% 
    mutate(suspect = ifelse(culprit_present == "absent", random_suspect, 3),
           suspect_position = ifelse(img1 == suspect, 1,
                                     ifelse(img2 == suspect, 2,
                                            ifelse(img3 == suspect, 3,
                                                   ifelse(img4 == suspect, 4,
                                                          ifelse(img5 == suspect, 5,
                                                                 ifelse(img6 == suspect, 6,
                                                                        ifelse(img7 == suspect, 7,
                                                                               ifelse(img8 == suspect, 8, 9))))))))) %>%  
mutate(suspect_position = ifelse(cond == "US", NA, suspect_position),
       id_type = ifelse(grepl("present", decision), "reject",
                        ifelse(decision == suspect, "suspect", "filler")),
       lineup_size = ifelse(cond == "US", 6, 9)) %>% 
    select(cond, culprit_present, id_type, conf_level, lineup_size, suspect_position) %>% 
    mutate(conf_level = (as.numeric(conf_level)/10)+1)

write.csv(seale,
          "./Dataset testing and reports/Data/Seale-Carlisle & Mickes, 2016/seale_processed_for_estimation.csv",
          row.names = FALSE,
          na = "")

## Generating a dataset without modifications ----
seale = read.csv("./Dataset testing and reports/Data/Seale-Carlisle & Mickes, 2016/USvUKdata.csv") %>% 
    `colnames<-` (c("expnumber", "subj", "age", "gender", "cond",
                    "culprit_present", "img1", "img2", "img3", "img4", "img5", "img6", "img7", "img8", "img9",
                    "decision", "conf_level", "accuracy", "q1", "q2", "q3", "q4", "q5",
                    "rep1", "rep2", "rep3", "rep4", "rep5", "rep6", "rep7", "rep8", "rep9",
                    "rep_sum", "repeated", "absent_present", "include")) %>% 
    filter(include == 1) %>% 
    select(cond, culprit_present, contains("img"), decision, conf_level, accuracy) %>% 
    mutate(culprit_present = ifelse(culprit_present == "Target-present", "present", "absent"))

seale_replaced = data.frame(lapply(seale, function(x) {
    y = gsub("images.*/", "", x) 
    gsub(".jpg", "", y)
}))

seale = seale_replaced %>% 
    ungroup() %>% 
    mutate(suspect = ifelse(culprit_present == "present", 3, NA),
           suspect_position = ifelse(img1 == suspect, 1,
                                     ifelse(img2 == suspect, 2,
                                            ifelse(img3 == suspect, 3,
                                                   ifelse(img4 == suspect, 4,
                                                          ifelse(img5 == suspect, 5,
                                                                 ifelse(img6 == suspect, 6,
                                                                        ifelse(img7 == suspect, 7,
                                                                               ifelse(img8 == suspect, 8, 
                                                                                      ifelse(img9 == suspect, 9, NA)))))))))) %>%  
    mutate(suspect_position = ifelse(cond == "US", NA, suspect_position),
           id_type = ifelse(grepl("present", decision), "reject",
                            ifelse(decision == suspect, "suspect", "filler")),
           lineup_size = ifelse(cond == "US", 6, 9)) %>% 
    select(cond, culprit_present, id_type, conf_level, lineup_size, suspect_position) %>% 
    mutate(conf_level = (as.numeric(conf_level)/10)+1,
           id_type = ifelse(is.na(id_type), "filler", id_type),
           exp = "Seale-Carlisle & Mickes (2015): US vs. UK lineups")

write.csv(seale,
          "./Dataset testing and reports/Data/Seale-Carlisle & Mickes, 2016/seale_processed.csv",
          row.names = FALSE,
          na = "")

# Palmer et al. (2013) Exp. 1: Long- vs. Short-delay ----
palmer = read.csv("./Dataset testing and reports/Data/Palmer et al., 2013/palmer_delay_2013.csv") %>% 
    mutate(conf_level = (max(conf_level)+1) - conf_level)

write.csv(palmer,
          "./Dataset testing and reports/Data/Palmer et al., 2013/palmer_delay_2013_processed.csv",
          row.names = FALSE,
          na = "")

# Wilson et al. (2017) Exp. 2: Verbal descriptions vs. Control ----
wilson = read.csv("./Dataset testing and reports/Data/Wilson et al., 2017/wilson_exp1_exp2.csv",
                  fileEncoding = "UTF-8-BOM") %>% 
    filter(Exp == 2) %>% 
    rename("cond" = Group,
           "culprit_present" = Target.Absent.or.Present,
           "conf_level" = Confidence) %>% 
    mutate(culprit_present = ifelse(grepl("absent", culprit_present), "absent", "present"),
           exp = "Wilson et al. (2017): Exp 2: Verbal description vs. Control",
           id_type = ifelse(Present.or.Absent.Response == "Absent", "reject",
                            ifelse(Present.or.Absent.Response == "Present" & Accuracy == 0, "filler", "suspect")),
           lineup_size = 6,
           suspect_position = NA) %>% 
    select(exp, cond, culprit_present, id_type, conf_level, lineup_size, suspect_position)

write.csv(wilson,
          "./Dataset testing and reports/Data/Wilson et al., 2017/wilson_exp2_processed.csv",
          row.names = FALSE,
          na = "")

write.csv(wilson,
          "./Dataset testing and reports/Data/01 - All data files for combining/wilson_exp2_processed.csv",
          row.names = FALSE,
          na = "")
