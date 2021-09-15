library(psych)
library(tidyverse)
library(pROC)
library(lubridate)

end_time = 800
start_time = 500

time_test = paste("Time taken: ", (end_time - start_time)/60, " minutes", sep = "")
parse_number(time_test)
source("ROC_power_app/scripts/func.R") # helper functions


(2000*2000)/1000

test = "1, 1, 1.25, 1.5"
test_extract = unique(extract(test))

# Processing of other datasets ----
## Colloff et al., 2021 data ----
### Exp. 1 ----
#### High similarity vs. Low similarity ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021/Exp 1/Experiment1.csv") %>% 
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
          "./Dataset testing and reports/Data/Colloff et al. 2021/Exp 1/colloff_2021_processed_high_low.csv",
          row.names = FALSE,
          na = "")

#### High similarity vs. Medium similarity ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021/Exp 1/Experiment1.csv") %>% 
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
          "./Dataset testing and reports/Data/Colloff et al. 2021/Exp 1/colloff_2021_processed_high_med.csv",
          row.names = FALSE,
          na = "")

### Exp. 2 ----
#### High similarity vs. Low similarity ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021/Exp 2/Experiment2.csv") %>% 
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
          "./Dataset testing and reports/Data/Colloff et al. 2021/Exp 2/colloff_2021_processed_high_low.csv",
          row.names = FALSE,
          na = "")

#### High similarity vs. Medium similarity ----
colloff = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021/Exp 2/Experiment2.csv") %>% 
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
          "./Dataset testing and reports/Data/Colloff et al. 2021/Exp 2/colloff_2021_processed_high_med.csv",
          row.names = FALSE,
          na = "")

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

# doing basic ROC to confirm that it works ----
## high-similarity condition ----
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

processed_data = colloff

TP_data = colloff %>% 
    filter(culprit_present == "present")

TA_data = colloff %>% 
    filter(culprit_present == "absent")

table(TA_data$id_type)
table(TA_data$id_type)/sum(table(TA_data$id_type))
40/6

# generating hypothetical ROCs ----
data_props = processed_data %>%
    group_by(id_type, culprit_present, cond) %>% 
    count() %>% 
    ungroup() %>% 
    group_by(culprit_present, cond) %>% 
    mutate(total = sum(n),
           prop = n/total) %>% 
    ungroup()

message("Processed proportion data")

##### Getting TA & TP suspect proportions for Condition 1 ----
cond1_TA_susp_prop = data_props %>% 
    filter(cond == unique(data_props$cond)[1] &
               culprit_present == "absent" & 
               id_type == "suspect") %>% 
    dplyr::select(prop) %>% 
    as.numeric()

cond1_TP_susp_prop = data_props %>% 
    filter(cond == unique(data_props$cond)[1] &
               culprit_present == "present" & 
               id_type == "suspect") %>% 
    dplyr::select(prop) %>% 
    as.numeric()

##### Getting TA & TP suspect proportions for Condition 2 ----
cond2_TA_susp_prop = data_props %>% 
    filter(cond == unique(data_props$cond)[2] &
               culprit_present == "absent" & 
               id_type == "suspect") %>% 
    dplyr::select(prop) %>% 
    as.numeric()

cond2_TP_susp_prop = data_props %>% 
    filter(cond == unique(data_props$cond)[2] &
               culprit_present == "present" & 
               id_type == "suspect") %>% 
    dplyr::select(prop) %>% 
    as.numeric()

message("Processed data for both conditions")

#### Getting responses at each confidence level for both conditions ----
data_original = processed_data %>%
    #mutate(conf_level = as.factor(conf_level)) %>% 
    group_by(id_type, conf_level_rev, culprit_present, cond) %>% 
    count() %>% 
    ungroup() %>% 
    group_by(culprit_present, cond) %>% 
    mutate(total = sum(n),
           prop = n/total) %>% 
    ungroup() %>%
    filter(id_type == "suspect")

message("Data processing complete")
message(data_original)

ROC_data = data.frame(prop = rep(NA, times = length(unique(data_original$cond))*
                                     length(unique(data_original$culprit_present))*
                                     length(unique(data_original$conf_level_rev))*
                                     length(effs)),
                      cond = NA,
                      presence = NA,
                      criteria = NA,
                      eff = NA)

row = 1

cond1 = "low_similarity"

cond_1_greater = paste0(cond1, " >")

test = data.frame(`testing spaces` = NA,
                  "Testing spaces" = NA)

test = tibble("Testing spaces" = NA,
              `Testing spaces 2` = NA)

message("Created empty ROC store object for hypothetical plot")

for (g in 1:length(effs)) {
    data = data_original
    eff = effs[g]
    for (h in 1:nrow(data)) {
        if (data$culprit_present[h] == "present" & data$cond[h] == unique(data$cond)[2]) {
            data$n[h] = round(data$n[h]*eff)
        } else {
            data$n[h] = data$n[h]
        }
    }
    
    data$prop = data$n / data$total
    
    for (i in 1:length(unique(data$cond))) {
        curr_cond = unique(data$cond)[i]
        for (j in 1:length(unique(data$culprit_present))) {
            curr_present = unique(data$culprit_present)[j]
            for (k in 1:length(unique(data$conf_level_rev))) {
                curr_conf = unique(data$conf_level_rev)[k]
                curr_resps = sum(data$prop[data$cond == curr_cond &
                                               data$culprit_present == curr_present &
                                               data$conf_level_rev %in% c(1:curr_conf)])
                
                ROC_data$cond[row] = curr_cond
                ROC_data$presence[row] = curr_present
                ROC_data$prop[row] = curr_resps
                ROC_data$criteria[row] = curr_conf
                ROC_data$eff[row] = eff
                row = row + 1
            }
        }
    }
}

ROC_data = ROC_data %>% 
    mutate(criteria_rev = max(criteria)+1 - criteria)

message("Populated ROC store object")

if (max(ROC_data$prop) > 1) {
    showModal(modalDialog(
        title = "Warning",
        "One or more effect sizes results in a correct ID proportion > 1. 
                Change the maximum effect size(s) or the direction of the effect sizes to be tested"
    ))
    
    hide("sim_start")
} else {
    show("sim_start")
}

ROC_data_wide = spread(ROC_data,
                       key = "presence",
                       value = "prop")  %>% 
    rbind(data.frame(cond = rep(c(cond1, 
                                  cond2), 
                                each = length(effs)),
                     criteria = NA,
                     eff = rep(effs, times = length(unique(data_original$cond))),
                     present = 0,
                     absent = 0)) #%>% 
#mutate(present = ifelse(present < 0, 0,
#                        ifelse(present > 1, 1, present)))


max_criteria = as.numeric(length(unique(ROC_data_wide$criteria[!is.na(ROC_data_wide$criteria)])))

if (input$roc_trunc == "Lowest false ID rate") {
    partial_threshold = ROC_data_wide %>% 
        filter(criteria == max_criteria) %>% 
        select(absent) %>% 
        min() 
} else {
    partial_threshold = ROC_data_wide %>% 
        filter(criteria == max_criteria) %>% 
        select(absent) %>% 
        max()
}

message("Created data for plotting")
message(ROC_data_wide)


ROC_data_plot = ROC_data_wide %>% 
    ggplot(aes(x = absent, y = present, color = cond, linetype = as.factor(eff)))+
    geom_point(alpha = .5)+
    geom_line()+
    geom_vline(xintercept = partial_threshold)+
    apatheme+
    labs(x = "\nFalse ID rate",
         y = "Correct ID rate\n",
         linetype = "Effect",
         color = "Condition")+
    theme(text = element_text(size = 20))

ROC_data_plot
# testing the simulation loop ----
start_time = Sys.time()

ns = 6981
effs = 1
nsims = 1000
nboot_iter = 2000
test_tails = "low_similarity > high_similarity"
alpha_level = .05
sim_total = nsims * length(ns) * length(effs)
n_TA_lineups = 0.5
n_TP_lineups = 0.5
roc_trunc = "Lowest false ID"

showModal(modalDialog(HTML(sprintf("Start time: %s <br/>With %s simulations @ ~3-5s each, estimated completion time is between %s and %s <br/>Do not close this tab/window",
                                   start_time,
                                   sim_total,
                                   start_time + (sim_total * 3),
                                   start_time + (sim_total * 5))),
                      fade = FALSE,
                      easyClose = FALSE,
                      size = "l"))

sim_store = data.frame(auc_p = rep(NA, times = nsims))
pwr_store = matrix(nrow = length(ns),
                   ncol = length(effs))
auc_store = matrix(nrow = length(ns),
                   ncol = length(effs))
auc_1_store = matrix(nrow = length(ns),
                     ncol = length(effs))
auc_2_store = matrix(nrow = length(ns),
                     ncol = length(effs))

g = 1
h = 1
i = 1
### loop over effect sizes ----
for (g in 1:length(effs)) {
    eff = effs[g]
    
    data_original = processed_data %>%
        #mutate(conf_level = as.factor(conf_level)) %>%
        group_by(id_type, conf_level, culprit_present, cond) %>%
        count() %>%
        ungroup() %>%
        group_by(culprit_present, cond) %>%
        mutate(total = sum(n),
               prop = n / total) %>%
        ungroup() %>%
        mutate(conf_level_rev = (max(as.numeric(conf_level)) + 1) - as.numeric(conf_level)) %>%
        filter(id_type == "suspect")
    
    data = data_original
    
    ### Create the root data file to sample from across the Ns and sims ----
    for (z in 1:nrow(data)) {
        if (data$culprit_present[z] == "present" &
            data$cond[z] == unique(data$cond)[2]) {
            data$n[z] = round(data$n[z] * eff)
        } else {
            data$n[z] = data$n[z]
        }
    }
    
    data$prop = data$n / data$total
    
    ### Get the ROC data to sample from across Ns and sims ----
    #### For Condition 1 ----
    ##### TA ----
    TA_data_cond1_root = filter(data,
                                culprit_present == "absent" &
                                    cond == unique(data$cond)[1])
    
    # Empty the root vector
    #TA_ROC_cond1_root = vector()
    #
    #for (y in 1:nrow(TA_data_cond1)) {
    #  TA_ROC_cond1_root = c(TA_ROC_cond1_root, rep(TA_data_cond1$conf_level_rev[y], times = TA_data_cond1$n[y]))
    #}
    
    ##### TP ----
    TP_data_cond1_root = filter(data,
                                culprit_present == "present" &
                                    cond == unique(data$cond)[1])
    
    #### For Condition 2 ----
    ##### TA ----
    TA_data_cond2_root = filter(data,
                                culprit_present == "absent" &
                                    cond == unique(data$cond)[2])
    
    ##### TP ----
    TP_data_cond2_root = filter(data,
                                culprit_present == "present" &
                                    cond == unique(data$cond)[2])
    
    ### loop over Ns ####
    for (h in 1:length(ns)) {
        curr_n = ns[h]
        #if (parameters$roc_paired == FALSE) {
        #    curr_n = ns[h]
        #} else {
        #    curr_n = ns[h] * 2
        #}
        
        #### Loop over sims ----
        for (i in 1:nsims) {
            ##### For each sim, sample ROC data from the root ----
            ###### For Condition 1 ----
            ####### TA ----
            TA_data_cond1 = sample(
                c(
                    as_vector(TA_data_cond1_root$conf_level),
                    0
                ),
                size = round((curr_n/2) * n_TA_lineups),
                replace = TRUE,
                prob = c(
                    as_vector(TA_data_cond1_root$prop),
                    1 - sum(TA_data_cond1_root$prop)
                )
            )
            
            TA_data_cond1 = TA_data_cond1[!is.na(TA_data_cond1)]
            
            cond1_partial = length(TA_data_cond1[TA_data_cond1 > 0]) / round((curr_n/2) * n_TA_lineups)
            
            ####### TP ----
            TP_data_cond1 = sample(
                c(
                    as_vector(TP_data_cond1_root$conf_level),
                    0
                ),
                size = round((curr_n/2) * n_TP_lineups),
                replace = TRUE,
                prob = c(
                    as_vector(TP_data_cond1_root$prop),
                    1 - sum(TP_data_cond1_root$prop)
                )
            )
            
            TP_data_cond1 = TP_data_cond1[!is.na(TP_data_cond1)]
            
            
            ###### For Condition 1 ----
            ####### TA ----
            TA_data_cond2 = sample(
                c(
                    as_vector(TA_data_cond2_root$conf_level),
                    0
                ),
                size = round((curr_n/2) * n_TA_lineups),
                replace = TRUE,
                prob = c(
                    as_vector(TA_data_cond2_root$prop),
                    1 - sum(TA_data_cond2_root$prop)
                )
            )
            
            TA_data_cond2 = TA_data_cond2[!is.na(TA_data_cond2)]
            
            cond2_partial = length(TA_data_cond2[TA_data_cond2 > 0]) / round((curr_n/2) * n_TA_lineups)
            
            ####### TP ----
            TP_data_cond2 = sample(
                c(
                    as_vector(TP_data_cond2_root$conf_level),
                    0
                ),
                size = round((curr_n/2) * n_TP_lineups),
                replace = TRUE,
                prob = c(
                    as_vector(TP_data_cond2_root$prop),
                    1 - sum(TP_data_cond2_root$prop)
                )
            )
            
            TP_data_cond2 = TP_data_cond2[!is.na(TP_data_cond2)]
            
            ##### Generate the ROCs ----
            ###### Condition 1 ----
            #if (length(TP_data_cond1) > length(TA_data_cond1)) {
            #    TA_data_cond1 = append(TA_data_cond1,
            #                           rep(
            #                               0,
            #                               length(TP_data_cond1) - length(TA_data_cond1)
            #                           ))
            #} else if (length(TA_data_cond1) > length(TP_data_cond1)) {
            #    TP_data_cond1 = append(TP_data_cond1,
            #                           rep(
            #                               0,
            #                               length(TA_data_cond1) - length(TP_data_cond1)
            #                           ))
            #} else {
            #    TA_data_cond1 = TA_data_cond1
            #    TP_data_cond1 = TP_data_cond1
            #}
            
            roc_cond1 = roc(
                controls = TA_data_cond1,
                cases = TP_data_cond1[!is.na(TP_data_cond1)],
                direction = "<",
                ci = F
                #partial.auc = c(1, 1 - min(
                #    cond1_partial, cond2_partial
            )
            
            ###### Condition 2 ----
            #if (length(TP_data_cond2) > length(TA_data_cond2)) {
            #    TA_data_cond2 = append(TA_data_cond2,
            #                           rep(
            #                               0,
            #                               length(TP_data_cond2) - length(TA_data_cond2)
            #                           ))
            #} else if (length(TA_data_cond2) > length(TP_data_cond2)) {
            #    TP_data_cond2 = append(TP_data_cond2,
            #                           rep(
            #                               0,
            #                               length(TA_data_cond2) - length(TP_data_cond2)
            #                           ))
            #} else {
            #    TA_data_cond2 = TA_data_cond2
            #    TP_data_cond2 = TP_data_cond2
            #}
            
            roc_cond2 = roc(
                controls = TA_data_cond2,
                cases = TP_data_cond2[!is.na(TP_data_cond2)],
                direction = "<",
                ci = F
                #partial.auc = c(1, 1 - min(
                #    cond1_partial, cond2_partial
            )
            
            ### ROC test ----
            
            if (roc_trunc == "Lowest false ID rate") {
                #### If truncating at lowest false ID rate ----
                roc_test = roc.test(
                    roc_cond1,
                    roc_cond2,
                    reuse.auc = FALSE,
                    partial.auc = c(1, 1 - min(
                        cond1_partial, cond2_partial
                    )),
                    partial.auc.focus = "sp",
                    method = "bootstrap",
                    paired = FALSE,
                    boot.n = nboot_iter,
                    progress = "none"
                )
            } else {
                #### If truncating at highest false ID rate ----
                roc_test = roc.test(
                    roc_cond1,
                    roc_cond2,
                    reuse.auc = FALSE,
                    partial.auc = c(1, 1 - max(
                        cond1_partial, cond2_partial
                    )),
                    partial.auc.focus = "sp",
                    method = "bootstrap",
                    paired = FALSE,
                    boot.n = nboot_iter,
                    progress = "none"
                ) 
            }
            
            roc_test
            
            sim_store$auc_diff[i] = roc_test$estimate[1] - roc_test$estimate[2]
            sim_store$auc_1[i] = roc_test$estimate[1]
            sim_store$auc_2[i] = roc_test$estimate[2]
            sim_store$auc_p[i] = roc_test$p.value
            
            if (test_tails == "2_tail") {
                sim_store$sig[i] = ifelse(sim_store$auc_p[i] < alpha_level, 1, 0)
            } else if (test_tails == sprintf("%s > %s",
                                                   cond1,
                                                   cond2)) {
                sim_store$sig[i] = ifelse(sim_store$auc_p[i] < alpha_level * 2 &
                                              sim_store$auc_diff[i] > 0, 1, 0)
            } else {
                sim_store$sig[i] = ifelse(sim_store$auc_p[i] < alpha_level * 2 &
                                              sim_store$auc_diff[i] < 0, 1, 0)
            }
        }
        pwr_store[h, g] = mean(sim_store$sig)
        auc_store[h, g] = mean(sim_store$auc_diff)
        auc_1_store[h, g] = mean(sim_store$auc_1)
        auc_2_store[h, g] = mean(sim_store$auc_2)
    }
}
### generate resuts dataframes ----
auc_store = auc_store %>% 
    as.data.frame() %>% 
    `colnames<-`(effs) %>% 
    mutate(N = ns) %>% 
    gather(key = "Effect size",
           value = "Avg. AUC difference",
           -N)

auc_1_store = auc_1_store %>% 
    as.data.frame() %>% 
    `colnames<-`(effs) %>% 
    mutate(N = ns) %>% 
    gather(key = "Effect size",
           value = !!paste("Avg. AUC in", cond1, sep = " "),
           -N)

auc_2_store = auc_2_store %>% 
    as.data.frame() %>% 
    `colnames<-`(effs) %>% 
    mutate(N = ns) %>% 
    gather(key = "Effect size",
           value = !!paste("Avg. AUC in", cond2, sep = " "),
           -N)

pwr_store = as.data.frame(pwr_store) %>% 
    `colnames<-`(effs) %>% 
    mutate(N = ns) %>% 
    gather(key = "Effect size",
           value = "Power",
           -N) %>% 
    select(N, `Effect size`, `Power`) %>% 
    left_join(auc_1_store) %>% 
    left_join(auc_2_store) %>% 
    left_join(auc_store)

end_time = Sys.time()

# comparing sim proportions to actual proportions ----
table(TA_low_sim)/sum(table(TA_low_sim))
table(TA_data_cond1)/sum(table(TA_data_cond1))

table(TP_low_sim)/sum(table(TP_low_sim))
table(TP_data_cond1)/sum(table(TP_data_cond1))
TP_simmed_props = as.vector(table(TP_data_cond1)/sum(table(TP_data_cond1)))
TP_actual_props = TP_data_cond1_root %>% 
    mutate(simmed_props = TP_simmed_props[2:length(TP_simmed_props)])
