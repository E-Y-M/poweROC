library(psych)
library(tidyverse)
library(pROC)

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

### Condition 1 ROC ----
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
