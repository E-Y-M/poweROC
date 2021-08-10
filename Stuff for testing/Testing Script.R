library(psych)
library(tidyverse)

data = read.csv("palmer_delay_data.csv") 

cond1 = as.character(unique(data$cond)[1])
cond2 = as.character(unique(data$cond)[2])

if (c("id_type", "condition") %in% colnames(data)) {
    print("TRUE")
} else {
    print("FALSE")
}

data_props = data %>%
    group_by(id_type, culprit_present, cond) %>% 
    count() %>% 
    ungroup() %>% 
    group_by(culprit_present, cond) %>% 
    mutate(total = sum(n),
           prop = n/total) %>% 
    ungroup()

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
data_original = data %>%
    mutate(conf_level = as.factor(conf_level)) %>% 
    group_by(id_type, conf_level, culprit_present, cond) %>% 
    count() %>% 
    ungroup() %>% 
    group_by(culprit_present, cond) %>% 
    mutate(total = sum(n),
           prop = n/total) %>% 
    ungroup() %>% 
    mutate(conf_level_rev = (max(as.numeric(conf_level))+1) - as.numeric(conf_level)) %>% 
    filter(id_type == "suspect")

message("Data processing complete")

effs = 1

ROC_data = data.frame(prop = rep(NA, times = length(unique(data_original$cond))*
                                     length(unique(data_original$culprit_present))*
                                     length(unique(data_original$conf_level))*
                                     length(effs)),
                      cond = NA,
                      presence = NA,
                      criteria = NA,
                      eff = NA)

row = 1

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
            for (k in 1:length(unique(data$conf_level))) {
                curr_conf = unique(data$conf_level)[k]
                curr_resps = sum(data$prop[data$cond == curr_cond &
                                               data$culprit_present == curr_present &
                                               data$conf_level %in% c(1:curr_conf)])
                
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

message("Populated ROC store object")

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


message("Created data for plotting")

ROC_data_plot = ROC_data_wide %>% 
    ggplot(aes(x = absent, y = present, color = cond, linetype = as.factor(eff)))+
    geom_point(alpha = .5)+
    geom_line()+
    #geom_vline(xintercept = partial_threshold)+
    apatheme+
    labs(x = "\nFalse ID rate",
         y = "Correct ID rate\n",
         linetype = "Effect",
         color = "Condition")+
    theme(text = element_text(size = 20))

ROC_data_plot
