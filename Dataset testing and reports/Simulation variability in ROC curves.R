library(psych)
library(tidyverse)
library(here)

setwd(here::here())

`%nin%` = Negate(`%in%`)

# Read in data ----
data = read.csv(here("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/colloff_2021_processed_high_med.csv"), fileEncoding = "UTF-8-BOM")

TA_data_cond1_root = read.csv(here("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/colloff_2021_processed_high_med.csv"), fileEncoding = "UTF-8-BOM") %>% 
    filter(cond == "High-similarity fillers" & culprit_present == "absent") %>%
    group_by(conf_level, id_type) %>% 
    summarize(resps = length(id_type)) %>% 
    ungroup() %>% 
    mutate(total = sum(resps)) %>% 
    filter(id_type == "suspect") %>% 
    mutate(prop = resps/total,
           culprit_present = "TA_prop")

TP_data_cond1_root = read.csv(here("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/colloff_2021_processed_high_med.csv"), fileEncoding = "UTF-8-BOM") %>% 
    filter(cond == "High-similarity fillers" & culprit_present == "present") %>%
    group_by(conf_level, id_type) %>% 
    summarize(resps = length(id_type)) %>% 
    ungroup() %>% 
    mutate(total = sum(resps)) %>% 
    filter(id_type == "suspect") %>% 
    mutate(prop = resps/total,
           culprit_present = "TP_prop")

# Set simulation parameters ----
nsims = 200
n_TA_lineups = 1
n_TP_lineups = 1
ns = c(250, 500, 750)
conf_levels = unique(data$conf_level)

ROC_data_store = data.frame(prop = rep(NA, times = 1*
                                     length(unique(data$culprit_present))*
                                     length(unique(data$conf_level))*
                                     1),
                      sim = NA,
                      n = NA,
                      presence = NA,
                      criteria = NA)

# Get actual ROC curves from data ----
TA_TP_data_actual = rbind(TA_data_cond1_root,
                          TP_data_cond1_root) %>% 
    mutate(conf_level_rev = max(conf_level)+1 - conf_level)

ROC_data_actual = data.frame(prop = rep(NA, times = 1*
                                            length(unique(TA_TP_data_actual$culprit_present))*
                                            length(unique(TA_TP_data_actual$conf_level_rev))*
                                            1),
                             sim = "actual",
                             n = NA,
                             presence = NA,
                             criteria = NA)

row = 1
for (j in 1:length(unique(TA_TP_data_actual$culprit_present))) {
    curr_present = unique(TA_TP_data_actual$culprit_present)[j]
    for (k in 1:length(unique(TA_TP_data_actual$conf_level_rev))) {
        curr_conf = unique(TA_TP_data_actual$conf_level_rev)[k]
        curr_resps = sum(TA_TP_data_actual$prop[TA_TP_data_actual$culprit_present == curr_present &
                                                    TA_TP_data_actual$conf_level_rev %in% c(1:curr_conf)])
        
        ROC_data_actual$presence[row] = curr_present
        ROC_data_actual$prop[row] = curr_resps
        ROC_data_actual$criteria[row] = curr_conf
        row = row + 1
    }
}

ROC_data_actual_append = data.frame(presence = unique(TA_TP_data_actual$culprit_present),
                             criteria = 0,
                             sim = "actual",
                             n = NA,
                             prop = 0)


ROC_data_actual_wide = ROC_data_actual %>% 
    rbind(ROC_data_actual_append) %>% 
    pivot_wider(names_from = presence,
                values_from = prop)

ROC_data_actual_wide_ns = ROC_data_actual_wide %>% 
    rbind(ROC_data_actual_wide) %>% 
    rbind(ROC_data_actual_wide) %>% 
    mutate(n = rep(ns, each = length(unique(data$conf_level))+1))


# Main simulation loop ----
for (h in 1:length(ns)) {
    curr_n = ns[h]
    for (i in 1:nsims) {
        ##### For each sim, sample ROC data from the root ----
        ###### For Condition 1 ----
        ####### TA ----
        TA_data_cond1 = sample(
            c(as_vector(TA_data_cond1_root$conf_level),
              0),
            size = round((curr_n / 1) * n_TA_lineups),
            replace = TRUE,
            prob = c(
                as_vector(TA_data_cond1_root$prop),
                1 - sum(TA_data_cond1_root$prop)
            )
        )
        
        ####### TP ----
        TP_data_cond1 = sample(
            c(as_vector(TP_data_cond1_root$conf_level),
              0),
            size = round((curr_n / 1) * n_TP_lineups),
            replace = TRUE,
            prob = c(
                as_vector(TP_data_cond1_root$prop),
                1 - sum(TP_data_cond1_root$prop)
            )
        )
        
        TA_data_cond1 = TA_data_cond1 %>%
            as.data.frame() %>%
            mutate(conf_level = as.numeric(.)) %>%
            select(-.) %>%
            group_by(conf_level) %>%
            summarize(resps = length(conf_level)) %>%
            ungroup()
        
        TP_data_cond1 = TP_data_cond1 %>%
            as.data.frame() %>%
            mutate(conf_level = as.numeric(.)) %>%
            select(-.) %>%
            group_by(conf_level) %>%
            summarize(resps = length(conf_level)) %>%
            ungroup()
        
        TA_TP_data = left_join(TP_data_cond1,
                               TA_data_cond1,
                               by = "conf_level")
        
        missing_confs = conf_levels %>%
            as.data.frame() %>%
            filter(conf_levels %nin% TA_TP_data$conf_level)
        
        if (nrow(missing_confs) > 0) {
            missing_confs = missing_confs %>%
                rowwise() %>%
                mutate(
                    conf_level = .[[1]],
                    resps.y = 0,
                    resps.x = 0
                ) %>%
                select(-.) %>%
                ungroup()
        } else {
            missing_confs = data.frame()
        }
        
        TA_TP_data = TA_TP_data %>%
            rbind(missing_confs) %>%
            ungroup() %>%
            mutate(
                TA_total = sum(resps.y, na.rm = TRUE),
                TP_total = sum(resps.x, na.rm = TRUE)
            ) %>%
            filter(conf_level != 0) %>%
            mutate(
                conf_level_rev = max(conf_level) + 1 - conf_level,
                TA_prop = resps.y / TA_total,
                TP_prop = resps.x / TP_total
            ) %>%
            select(conf_level, conf_level_rev, TA_prop, TP_prop) %>%
            pivot_longer(
                !c(conf_level, conf_level_rev),
                names_to = c("culprit_present"),
                values_to = c("prop")
            ) %>%
            mutate(prop = ifelse(is.na(prop), 0, prop))
        
        TA_TP_data = TA_TP_data[order(as.numeric(as.character(TA_TP_data$conf_level_rev))),]
        
        ROC_data = data.frame(
            prop = rep(NA, times = 1 *
                           length(
                               unique(TA_TP_data$culprit_present)
                           ) *
                           length(
                               unique(TA_TP_data$conf_level_rev)
                           ) *
                           1),
            sim = i,
            n = curr_n,
            presence = NA,
            criteria = NA
        )
        
        row = 1
        for (j in 1:length(unique(TA_TP_data$culprit_present))) {
            curr_present = unique(TA_TP_data$culprit_present)[j]
            for (k in 1:length(unique(TA_TP_data$conf_level_rev))) {
                curr_conf = unique(TA_TP_data$conf_level_rev)[k]
                curr_resps = sum(TA_TP_data$prop[TA_TP_data$culprit_present == curr_present &
                                                     TA_TP_data$conf_level_rev %in% c(1:curr_conf)])
                
                ROC_data$presence[row] = curr_present
                ROC_data$prop[row] = curr_resps
                ROC_data$criteria[row] = curr_conf
                ROC_data$n[row] = curr_n
                row = row + 1
            }
        }
        
        ROC_data_append = data.frame(presence = unique(TA_TP_data$culprit_present),
                                     criteria = 0,
                                     sim = i,
                                     n = curr_n,
                                     prop = 0)
        
        ROC_data_store = rbind(ROC_data_store,
                               ROC_data,
                               ROC_data_append)
    }
}

# Simulation data processing & plotting ----
ROC_data_wide = ROC_data_store %>% 
    filter(!is.na(sim)) %>% 
    pivot_wider(names_from = presence,
                values_from = prop)

ROC_sim_plot = ROC_data_wide %>%
    arrange(sim, n, criteria) %>% 
    ggplot(aes(x = TA_prop, y = TP_prop, color = as.factor(sim)))+
    facet_grid(cols = vars(n))+
    geom_point(alpha = .5)+
    geom_line(alpha = .5)+
    apatheme+
    geom_point(data = ROC_data_actual_wide_ns,
               aes(x = TA_prop, y = TP_prop), color = "black")+
    geom_line(data = ROC_data_actual_wide_ns,
              aes(x = TA_prop, y = TP_prop), color = "black", size = 1.5)+
    labs(x = "False ID rate",
         y = "Correct ID rate",
         color = "Sim")+
    scale_color_discrete(guide = "none")+
    theme(text = element_text(size = 30),
          axis.title.y = element_text(size = 30,
                                      margin = margin(t = 0, r = 30, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 0, l = 0)))
ROC_sim_plot

ggsave("./Dataset testing and reports/ROC_sim_variability.png",
       dpi = 300,
       height = 10,
       width = 22,
       units = "in")
