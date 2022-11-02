library(psych)
library(tidyverse)
library(pROC)
library(data.table)

# Seeing what statistics can be obtained from pROC (i.e., D/Z, sd of bootstrap estimates) ----
## Test dataset: Colloff et al., 2018 ----
data = read.csv("./Dataset testing and reports/Data/Colloff et al., 2018/ColloffWadeStrangeWixtedData_Processed.csv") %>% 
    mutate(conf_level = (conf_level/10)+1) %>% 
    mutate(conf_level = ifelse(id_type != "suspect", 0, conf_level)) %>% 
    group_by(cond, conf_level, culprit_present) %>% 
    count()

### Get partial AUC truncation ----
cond1_partial = data %>% 
    filter(cond == unique(data$cond)[1] & culprit_present == "absent") %>% 
    ungroup() %>% 
    mutate(total = sum(n),
           prop = n/total) %>% 
    slice(1) %>% 
    select(prop) %>% 
    as.numeric()

cond2_partial = data %>% 
    filter(cond == unique(data$cond)[2] & culprit_present == "absent") %>% 
    ungroup() %>% 
    mutate(total = sum(n),
           prop = n/total) %>% 
    slice(1) %>% 
    select(prop) %>% 
    as.numeric()

partial_cutoff = max(cond1_partial,
                     cond2_partial)

### Condition 1 ROC ----
data_cond1 = data %>% 
    filter(cond == unique(data$cond)[1])

data_cond1_ta = data_cond1 %>% 
    filter(culprit_present == "absent")

data_cond1_tp = data_cond1 %>% 
    filter(culprit_present == "present")

cond1_controls = vector()

for (i in 1:nrow(data_cond1_ta)) {
    curr_append = rep(data_cond1_ta$conf_level[i],
                      times = data_cond1_ta$n[i])
    
    cond1_controls = append(cond1_controls, curr_append)
}

cond1_cases = vector()

for (i in 1:nrow(data_cond1_tp)) {
    curr_append = rep(data_cond1_tp$conf_level[i],
                      times = data_cond1_tp$n[i])
    
    cond1_cases = append(cond1_cases, curr_append)
}

cond1_roc = roc(controls = cond1_controls,
                        cases = cond1_cases,
                        direction = "<",
                        ci = T,
                        partial.auc = c(1, partial_cutoff))

### Condition 2 ROC ----
data_cond2 = data %>% 
    filter(cond == unique(data$cond)[2])

data_cond2_ta = data_cond2 %>% 
    filter(culprit_present == "absent")

data_cond2_tp = data_cond2 %>% 
    filter(culprit_present == "present")

cond2_controls = vector()

for (i in 1:nrow(data_cond2_ta)) {
    curr_append = rep(data_cond2_ta$conf_level[i],
                      times = data_cond2_ta$n[i])
    
    cond2_controls = append(cond2_controls, curr_append)
}

cond2_cases = vector()

for (i in 1:nrow(data_cond2_tp)) {
    curr_append = rep(data_cond2_tp$conf_level[i],
                      times = data_cond2_tp$n[i])
    
    cond2_cases = append(cond2_cases, curr_append)
}

cond2_roc = roc(controls = cond2_controls,
                cases = cond2_cases,
                direction = "<",
                ci = T,
                partial.auc = c(1, partial_cutoff))

### ROC test ----
roc_test = roc.test(cond1_roc,
                    cond2_roc,
                    method = "bootstrap")

roc_test$estimate[1]

eff = NA
auc1 = as.numeric(roc_test$roc1$auc)
auc2 = as.numeric(roc_test$roc2$auc)
D_stat = as.numeric(roc_test$statistic)
sd_bootstrap = (auc1 - auc2) / D_stat

# Getting AUC values from the test datasets ----
## Colloff et al. 2021a, Exp. 1 "Medium-similarity fillers" ----
colloff_med = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/Null Effect Testing/colloff_2021_processed_med.csv") %>% 
    #mutate(conf_level = (conf_level/10)+1) %>% 
    mutate(conf_level = ifelse(id_type != "suspect", 0, conf_level)) %>% 
    group_by(cond, conf_level, culprit_present) %>% 
    count()

colloff_med_partial = colloff_med %>% 
    filter(culprit_present == "absent") %>% 
    ungroup() %>% 
    mutate(total = sum(n),
           prop = n/total) %>% 
    slice(1) %>% 
    select(prop) %>% 
    as.numeric()

data_colloff_med_ta = colloff_med %>% 
    filter(culprit_present == "absent")

data_colloff_med_tp = colloff_med %>% 
    filter(culprit_present == "present")

colloff_med_controls = vector()

for (i in 1:nrow(data_colloff_med_ta)) {
    curr_append = rep(data_colloff_med_ta$conf_level[i],
                      times = data_colloff_med_ta$n[i])
    
    colloff_med_controls = append(colloff_med_controls, curr_append)
}

colloff_med_cases = vector()

for (i in 1:nrow(data_colloff_med_tp)) {
    curr_append = rep(data_colloff_med_tp$conf_level[i],
                      times = data_colloff_med_tp$n[i])
    
    colloff_med_cases = append(colloff_med_cases, curr_append)
}

colloff_med_roc = roc(controls = colloff_med_controls,
                cases = colloff_med_cases,
                direction = "<",
                ci = T,
                partial.auc = c(1, colloff_med_partial))

colloff_med_roc # pAUC = 0.02256

### Write datafile for the app ----
colloff_med_write = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/Null Effect Testing/colloff_2021_processed_med.csv")

write.csv(colloff_med_write,
          "./Dataset testing and reports/Examination of D statistics/colloff_med.csv",
          row.names = FALSE,
          na = "")

## Colloff et al., 2018 "Fair showup" ----
colloff_showup = read.csv("./Dataset testing and reports/Data/Colloff et al., 2018/ColloffWadeStrangeWixtedData_Processed.csv") %>% 
    mutate(conf_level = (conf_level/10)+1) %>% 
    filter(cond == "Block") %>% 
    mutate(conf_level = ifelse(id_type != "suspect", 0, conf_level)) %>% 
    group_by(conf_level, culprit_present) %>% 
    count()

colloff_showup_partial = colloff_showup %>% 
    filter(culprit_present == "absent") %>% 
    ungroup() %>% 
    mutate(total = sum(n),
           prop = n/total) %>% 
    slice(1) %>% 
    select(prop) %>% 
    as.numeric()

data_colloff_showup_ta = colloff_showup %>% 
    filter(culprit_present == "absent")

data_colloff_showup_tp = colloff_showup %>% 
    filter(culprit_present == "present")

colloff_showup_controls = vector()

for (i in 1:nrow(data_colloff_showup_ta)) {
    curr_append = rep(data_colloff_showup_ta$conf_level[i],
                      times = data_colloff_showup_ta$n[i])
    
    colloff_showup_controls = append(colloff_showup_controls, curr_append)
}

colloff_showup_cases = vector()

for (i in 1:nrow(data_colloff_showup_tp)) {
    curr_append = rep(data_colloff_showup_tp$conf_level[i],
                      times = data_colloff_showup_tp$n[i])
    
    colloff_showup_cases = append(colloff_showup_cases, curr_append)
}

colloff_showup_roc = roc(controls = colloff_showup_controls,
                      cases = colloff_showup_cases,
                      direction = "<",
                      ci = T,
                      partial.auc = c(1, colloff_showup_partial))

colloff_showup_roc # pAUC = 0.09985

### Write datafile for the app ----
colloff_showup_write = read.csv("./Dataset testing and reports/Data/Colloff et al., 2018/ColloffWadeStrangeWixtedData_Processed.csv") %>% 
    mutate(conf_level = (conf_level/10)+1) %>% 
    filter(cond == "Block")

write.csv(colloff_showup_write,
          "./Dataset testing and reports/Examination of D statistics/colloff_showup_fair.csv",
          row.names = FALSE,
          na = "")

## Determining effect sizes to use for the different ROCs ----
auc1 = 0.02256
auc2 = 0.09985

# Let's double the first pAUC, then add the same to the second
eff1 = .6
new_auc2 = auc2 + (auc1 * eff1)
eff2 = (new_auc2 / auc2) - 1

auc2 * (1 + eff2)

# Analyzing simulation results ----
## Same pAUC difference, different effect sizes ----
D_sim_results = read.csv("./Dataset testing and reports/Examination of D statistics/Same AUC difference, different effect sizes/colloff_med_raw_sim_results.csv") %>% 
    mutate(exp = "colloff_med") %>% 
    rbind(read.csv("./Dataset testing and reports/Examination of D statistics/Same AUC difference, different effect sizes/colloff_showup_fair_raw_sim_results.csv") %>% 
              mutate(exp = "colloff_showup_fair"))

D_sim_summary = D_sim_results %>% 
    select(exp, sim, D_stat) %>% 
    distinct()

D_sim_summary %>% 
    ggplot(aes(x = D_stat, color = exp))+
    geom_density()

# D & Power are different--crucially, the same pAUC difference results in different power estimates given the same N

## Different pAUC difference, same effect sizes ----
D_sim_results = read.csv("./Dataset testing and reports/Examination of D statistics/Different AUC difference, same effect sizes/colloff_med_raw_sim_results.csv") %>% 
    mutate(exp = "colloff_med") %>% 
    rbind(read.csv("./Dataset testing and reports/Examination of D statistics/Different AUC difference, same effect sizes/colloff_showup_fair_raw_sim_results.csv") %>% 
              mutate(exp = "colloff_showup_fair"))

D_sim_summary = D_sim_results %>% 
    select(exp, sim, D_stat) %>% 
    distinct()

D_sim_summary %>% 
    ggplot(aes(x = D_stat, color = exp))+
    geom_density()

# D & Power are different

## Different pAUC difference, same effect sizes (.2) ----
D_sim_results = read.csv("./Dataset testing and reports/Examination of D statistics/Different AUC difference, same effect sizes (.2)/colloff_med_raw_sim_results.csv") %>% 
    mutate(exp = "colloff_med") %>% 
    rbind(read.csv("./Dataset testing and reports/Examination of D statistics/Different AUC difference, same effect sizes (.2)/colloff_showup_fair_raw_sim_results.csv") %>% 
              mutate(exp = "colloff_showup_fair"))

D_sim_summary = D_sim_results %>% 
    select(exp, sim, D_stat) %>% 
    distinct()

D_sim_summary %>% 
    ggplot(aes(x = D_stat, color = exp))+
    geom_density()

# D values quite similar, power different

## Trying out the same ROC curve, shifted upwards or downwards ----
eff = .2

colloff_med_original = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/Null Effect Testing/colloff_2021_processed_med.csv") %>% 
    mutate(conf_level = ifelse(id_type != "suspect", 0, conf_level)) %>% 
    group_by(cond, conf_level, culprit_present) %>% 
    count()

colloff_med_original_ca = colloff_med_original %>% 
    filter(culprit_present == "absent")

colloff_med_modified = colloff_med_original %>% 
    filter(culprit_present == "present") %>% 
    ungroup() %>% 
    mutate(new_n = round(n * (1 + eff)))

colloff_med_total_cp = colloff_med_modified %>% 
    ungroup() %>% 
    select(n) %>% 
    summarize(total = sum(n)) %>% 
    as.numeric()

colloff_new_total_cp = colloff_med_modified %>% 
    ungroup() %>% 
    filter(conf_level != 0) %>% 
    summarize(total = sum(new_n)) %>% 
    as.numeric()

colloff_new_rejections = colloff_med_total_cp - colloff_new_total_cp

colloff_med_modified = colloff_med_modified %>% 
    mutate(n = ifelse(conf_level == 0, colloff_new_rejections, new_n))

colloff_modified_final = colloff_med_modified %>% 
    select(-new_n) %>% 
    rbind(colloff_med_original_ca)

### Get data back into trial-level data ----
colloff_modified_trial = data.frame()

for (i in 1:nrow(colloff_modified_final)) {
    curr_data = data.frame(cond = rep("med_similarity", times = colloff_modified_final$n[i]),
                           conf_level = colloff_modified_final$conf_level[i],
                           culprit_present = colloff_modified_final$culprit_present[i],
                           id_type = ifelse(colloff_modified_final$conf_level[i] == 0, "reject", "suspect"))
    
    colloff_modified_trial = rbind(colloff_modified_trial,
                                   curr_data)
}

colloff_modified_trial = colloff_modified_trial %>% 
    mutate(conf_level = ifelse(conf_level == 0, 1, conf_level))

write.csv(colloff_modified_trial,
          "./Dataset testing and reports/Examination of D statistics/colloff_med_upwards_eff2.csv",
          row.names = FALSE,
          na = "")

### Reading in the simulation results ----
D_sim_results = read.csv("./Dataset testing and reports/Examination of D statistics/Same ROC curve, shifted upward (.2)/colloff_med_raw_sim_results_upward.csv") %>% 
    mutate(exp = "upward") %>% 
    rbind(read.csv("./Dataset testing and reports/Examination of D statistics/Same ROC curve, shifted upward (.2)/colloff_med_raw_sim_results.csv") %>% 
              mutate(exp = "unadjusted"))

D_sim_summary = D_sim_results %>% 
    select(exp, sim, D_stat) %>% 
    distinct()

D_sim_summary %>% 
    ggplot(aes(x = D_stat, color = exp))+
    geom_density()

# D values quite similar, power different

# Getting single-condition data from other open datasets to test the consistency of power using the same effect size with different base data ----
## Akan et al., 2021 Experiment 1: 6-person lineup ----
akan_exp1 = read.csv("./Dataset testing and reports/Data/Akan et al., 2021/Exp 1/Experiment1_1vs6.csv",
                     fileEncoding = "UTF-8-BOM") %>% 
    filter(cond == "6-person") %>% 
    mutate(conf_level = conf_level/10 + 1)

write.csv(akan_exp1,
          "./Dataset testing and reports/Examination of D statistics/akan_exp1_6person.csv",
          row.names = FALSE,
          na = "")

## Akan et al., 2021 Experiment 2: 3-person lineup ----
akan_exp2 = read.csv("./Dataset testing and reports/Data/Akan et al., 2021/Exp 2/Experiment2_1vs3.csv",
                     fileEncoding = "UTF-8-BOM") %>% 
    filter(cond == "3-person")

write.csv(akan_exp2,
          "./Dataset testing and reports/Examination of D statistics/akan_exp2_3person.csv",
          row.names = FALSE,
          na = "")

## Colloff et al., 2021a Experiment 2: High-similarity fillers lineup ----
colloff_exp2 = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 2/colloff_2021_processed_high_low.csv",
                     fileEncoding = "UTF-8-BOM") %>% 
    filter(cond == "High-similarity fillers")

colloff_exp2_test = colloff_exp2 %>% 
    group_by(id_type,
             culprit_present,
             conf_level) %>% 
    count()

write.csv(colloff_exp2,
          "./Dataset testing and reports/Examination of D statistics/colloff_2021a_high.csv",
          row.names = FALSE,
          na = "")

## Colloff et al., 2018: Do-nothing lineups ----
colloff_exp2 = read.csv("./Dataset testing and reports/Data/Colloff et al., 2018/ColloffWadeStrangeWixtedData_Processed.csv",
                        fileEncoding = "UTF-8-BOM") %>% 
    mutate(conf_level = (conf_level/10)+1) %>% 
    filter(cond == "Do-nothing")

write.csv(colloff_exp2,
          "./Dataset testing and reports/Examination of D statistics/colloff_2018_do_nothing.csv",
          row.names = FALSE,
          na = "")

## Colloff et al., 2020 Experiment 1: Interactive lineups ----
colloff_exp1 = read.csv("./Dataset testing and reports/Data/Colloff et al., 2020/Exp1_osf_data_processed.csv",
                        fileEncoding = "UTF-8-BOM") %>% 
    #mutate(conf_level = (conf_level/10)+1) %>% 
    filter(cond == "interactive")

write.csv(colloff_exp1,
          "./Dataset testing and reports/Examination of D statistics/colloff_2020_interactive.csv",
          row.names = FALSE,
          na = "")

## Colloff et al., 2020 Experiment 2: Joint-simultaneous ----
colloff_exp2 = read.csv("./Dataset testing and reports/Data/Colloff et al., 2020/Exp2_osf_data_processed_joint_simultaneous.csv",
                        fileEncoding = "UTF-8-BOM") #%>% 
    #mutate(conf_level = (conf_level/10)+1) %>% 
    #filter(cond == "interactive")

write.csv(colloff_exp2,
          "./Dataset testing and reports/Examination of D statistics/colloff_2020_joint_simultaneous.csv",
          row.names = FALSE,
          na = "")

## Colloff et al., 2021b Experiment 1: Same-pose ----
colloff_exp1 = read.csv("./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 1/Exp1_Data_SameVsDiff.csv",
                        fileEncoding = "UTF-8-BOM") %>% 
mutate(conf_level = (conf_level/10)+1) %>% 
filter(cond == "Same")

write.csv(colloff_exp1,
          "./Dataset testing and reports/Examination of D statistics/colloff_2021b_same.csv",
          row.names = FALSE,
          na = "")

## Colloff et al., 2021b Experiment 2: Low pose reinstatement ----
colloff_exp2 = read.csv("./Dataset testing and reports/Data/Colloff et al., 2021b/Exp 2/Exp2_Data_HighLow.csv",
                        fileEncoding = "UTF-8-BOM") %>% 
    mutate(conf_level = (conf_level/10)+1) %>% 
    filter(cond == "Low")

colloff_exp2_test = colloff_exp2 %>% 
    group_by(id_type,
             culprit_present,
             conf_level) %>% 
    count()

write.csv(colloff_exp2,
          "./Dataset testing and reports/Examination of D statistics/colloff_2021b_low.csv",
          row.names = FALSE,
          na = "")

## Morgan et al., 2019: Sleep ----
morgan = read.csv("./Dataset testing and reports/Data/Morgan et al., 2019/morgan_processed.csv",
                  fileEncoding = "UTF-8-BOM") %>% 
    filter(cond == "Sleep")

write.csv(morgan,
          "./Dataset testing and reports/Examination of D statistics/morgan_2019_sleep.csv",
          row.names = FALSE,
          na = "")

# Reading/writing ALL conditions from the full open dataset file ----
all_data = read.csv("./ROC_power_app/www/combined_open_data.csv",
                    fileEncoding = "UTF-8-BOM") %>% 
    mutate(exp_cond = paste(exp, cond, sep = " "))

conds_list = all_data %>% 
    select(exp_cond) %>% 
    distinct() %>% 
    as_vector()

write.csv(conds_list,
          "./Dataset testing and reports/Examination of D statistics/List of experimental conditions.csv",
          row.names = FALSE,
          na = "")

i = 1

n_store = NULL

for (i in 1:length(conds_list)) {
    curr_cond = conds_list[i]
    
    curr_data = filter(all_data, exp_cond == curr_cond)
    
    if (max(curr_data$conf_level) > 50) {
        curr_data = curr_data %>% 
            mutate(conf_level = conf_level / 10)
    } else {
        curr_data = curr_data
    }
    
    if (min(curr_data$conf_level) == 0) {
        curr_data = curr_data %>% 
            mutate(conf_level = conf_level + 1)
    } else {
        curr_data = curr_data
    }
    
    curr_filename = paste0("./Dataset testing and reports/Examination of D statistics/Individual conditions to test/", i, ".csv")
    
    write.csv(curr_data,
              curr_filename,
              row.names = FALSE,
              na = "")
    
    n_store[i] = nrow(curr_data)
}

n_store = n_store %>% 
    as.data.frame() %>% 
    mutate(cond = 1:n())

# Examining power for the same effect size across different datasets ----
fileList = list.files("./Dataset testing and reports/Examination of D statistics/Different AUC difference, same effect sizes")

fileList = fileList[fileList != "Old sim results"]

fileList_conds = fileList %>% 
    as.data.frame() %>% 
    mutate(cond = parse_number(.))

n_store_conds = n_store %>% 
    filter(cond %in% fileList_conds$cond)

examine_pwr_store = data.frame()

for (i in 1:length(fileList)) {
    curr_data = fread(paste0("./Dataset testing and reports/Examination of D statistics/Different AUC difference, same effect sizes/", fileList[i])) %>% 
        slice(1) %>% 
        mutate(auc_diff = abs(auc1 - auc2)) %>% 
        select(auc_diff, `Power (pAUC)`)
    
    examine_pwr_store = rbind(examine_pwr_store,
                              curr_data)
}

examine_pwr_store %>% 
    cbind(n_store_conds) %>% 
    ggplot(aes(x = auc_diff, y = `Power (pAUC)`, color = .))+
    geom_point()+
    labs(x = "Absolute pAUC difference")+
    apatheme+
    theme(text = element_text(size = 25))+
    scale_color_continuous(name = "Number of trials in\nbase dataset")

ggsave("./Dataset testing and reports/Examination of D statistics/Power for the same effect and sample sizes - Differing base data.png",
       dpi = 300,
       height = 10,
       width = 12,
       units = "in")
