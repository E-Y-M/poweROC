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
    mutate(cond = as.character(cond))

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
    mutate(cond = as.character(cond))

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/colloff_2021_processed_high_med.csv",
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
    filter(cond != "Same + Different")

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 1/Exp1_Data_SameVsDiff.csv",
          row.names = FALSE,
          na = "")

# AUC specificity = .618

#### Same + Different vs. Different ----
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
    filter(cond != "Same")

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 1/Exp1_Data_SamePlusDiffVsDiff.csv",
          row.names = FALSE,
          na = "")

# AUC specificity = .618

#### Same + Different vs. Same ----
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
    filter(cond != "Different")

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 1/Exp1_Data_SamePlusDiffVsSame.csv",
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
#### Left encoding vs. Right encoding ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 2/Exp2_Data.csv") %>% 
    dplyr::select(EncodingCondition, 
                  TargetPresent,
                  ParticipantSelection,
                  Confidence) %>% 
    mutate(culprit_present = ifelse(TargetPresent == 0, "absent", "present"),
           conf_level = as.numeric(Confidence),
           id_type = ifelse(ParticipantSelection == "target", "suspect",
                            ifelse(ParticipantSelection == "filler", "filler", "reject")),
           id_type = ifelse(culprit_present == "absent" & id_type == "filler", "suspect", id_type),
           cond = EncodingCondition)

write.csv(colloff,
          "./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 2/Exp2_Data_LeftRight.csv",
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
                                   ifelse(culprit_present == "absent" & Filler.ID == 1, "suspect",
                                          "reject"))),
           conf_level = str_remove(conf_level, "%")) %>% 
    mutate(conf_level = (as.numeric(conf_level)/10)+1)

write.csv(morgan,
          "./Dataset testing and reports/Data/Morgan et al., 2019/morgan_processed_sleep_wake.csv",
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
                                   ifelse(culprit_present == "absent" & Filler.ID == 1, "suspect",
                                          "reject"))),
           conf_level = str_remove(conf_level, "%")) %>% 
    mutate(conf_level = (as.numeric(conf_level)/10)+1)

write.csv(morgan,
          "./Dataset testing and reports/Data/Morgan et al., 2019/morgan_processed_AM_PM.csv",
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
                            ifelse(Target.Image == Response & !is.na(Target.Image), "suspect", "filler")),
           false_prob = 1/cond)

# Manually change some TA filler IDs to false IDs
for (i in 1:nrow(akan)) {
    if (akan$culprit_present[i] == "absent" & akan$id_type[i] == "filler") {
        akan$id_type2[i] = sample(c("suspect", "filler"),
                                  size = 1,
                                  prob = c(akan$false_prob[i], 1 - akan$false_prob[i]))
    } else {
        akan$id_type2[i] = NA
    }
} 

akan = akan %>% 
    mutate(id_type = ifelse(!is.na(id_type2), id_type2, id_type))  %>% 
    filter(cond == 1 | cond == 6) %>% 
    mutate(cond = as.factor(cond))

write.csv(akan,
          "./Dataset testing and reports/Data/Akan et al., 2021/Exp 1/Experiment1_1vs6.csv",
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
    mutate(cond = as.factor(cond))

write.csv(akan,
          "./Dataset testing and reports/Data/Akan et al., 2021/Exp 2/Experiment2_1vs3.csv",
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
    mutate(cond = as.factor(cond))

write.csv(akan,
          "./Dataset testing and reports/Data/Akan et al., 2021/Exp 2/Experiment2_1vs6.csv",
          row.names = FALSE,
          na = "")
