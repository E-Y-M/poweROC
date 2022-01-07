library(psych)
library(tidyverse)
library(pROC)
library(lubridate)
library(here)
library(DirichletReg)

source("ROC_power_app/scripts/func.R") # helper functions

#APA theme for plots
apatheme <-
    theme_bw()+                                      #apply ggplot2() black and white theme
    theme(panel.grid.major = element_blank(),        #eliminate major grid lines
          panel.grid.minor = element_blank(),        #eliminate minor grid lines
          panel.background = element_blank(),        #eliminate the square panel background
          panel.border = element_blank(),            #eliminate the square panel border
          text=element_text(family="Arial"),         #use arial font
          #legend.title=element_blank(),              #eliminate lengend title
          legend.position= "right",                  #position legend to the right of the plot
          axis.line.x = element_line(color="black"), #include a black border along the x-axis
          axis.line.y = element_line(color="black")) #include a black border along the y-axis

#** Looking at ratios of hits/FAs across confidence levels for the open datasets ----
setwd(here::here())

open_data = read.csv(here("./Dataset testing and reports/Data/combined_open_data.csv"),
                     fileEncoding = "UTF-8-BOM")

exps_list = open_data %>% 
    select(exp) %>% 
    distinct() %>% 
    unlist() %>% 
    as.character()

max_confs = open_data %>% 
    group_by(exp) %>% 
    summarize(confs = length(unique(conf_level))) %>% 
    select(confs) %>% 
    max() %>% 
    as.numeric()

#hits_ratios = matrix(nrow = length(exps_list),
#                     ncol = max_confs) %>% 
#    as.data.frame()

#fa_ratios = matrix(nrow = length(exps_list),
#                   ncol = max_confs) %>% 
#    as.data.frame()

combined_data = data.frame()

nboots = 500
i = 1
for (i in 1:length(exps_list)) {
    curr_exp = exps_list[i]
    
    TP_valid = open_data %>% 
        filter(exp == curr_exp & culprit_present == "present" & id_type == "suspect") %>% 
        nrow() %>% 
        as.numeric()
    
    TA_valid = open_data %>% 
        filter(exp == curr_exp & culprit_present == "absent" & id_type == "suspect") %>% 
        nrow() %>% 
        as.numeric()
    
    n_lineups = open_data %>% 
        filter(exp == curr_exp) %>% 
        nrow() %>% 
        as.numeric()
    
    if (TA_valid > 0) {
        data = open_data %>% 
            filter(exp == curr_exp) %>% 
            group_by(cond, conf_level, id_type, culprit_present) %>% 
            summarize(resps = length(id_type)) %>% 
            filter(id_type == "suspect") %>% 
            pivot_wider(names_from = c(cond, culprit_present),
                        values_from = resps, values_fill = NA) %>% 
            ungroup() %>% 
            mutate(TA_ratio = .[[3]] / .[[5]],
                   TP_ratio = .[[4]] / .[[6]],
                   TA_upr = NA,
                   TA_lwr = NA,
                   TP_upr = NA,
                   TP_lwr = NA)
        
        #*** Bootstrapping CIs on the ratios ----
        boot_data = open_data %>% 
            filter(exp == curr_exp)
        
        boot_store_TP = matrix(nrow = nboots,
                               ncol = max_confs)
        
        boot_store_TA = matrix(nrow = nboots,
                               ncol = max_confs)
        
        boot_SD_TP = NA
        boot_SD_TA = NA
        
        for (h in 1:nboots) {
            boot_data_sim = sample_n(boot_data, size = nrow(boot_data), replace = TRUE) %>% 
                group_by(cond, conf_level, id_type, culprit_present) %>% 
                summarize(resps = length(id_type)) %>% 
                filter(id_type == "suspect") %>% 
                pivot_wider(names_from = c(cond, culprit_present),
                            values_from = resps, values_fill = NA) %>% 
                ungroup() %>% 
                mutate(TA_ratio = .[[3]] / .[[5]],
                       TP_ratio = .[[4]] / .[[6]],
                       TA_upr = NA,
                       TA_lwr = NA,
                       TP_upr = NA,
                       TP_lwr = NA)
            
            for (l in 1:nrow(boot_data_sim)) {
                boot_store_TP[h, l] = boot_data_sim$TP_ratio[l]
                boot_store_TA[h, l] = boot_data_sim$TA_ratio[l]
            }
            
            boot_SD_TA[h] = sd(boot_store_TA[h,], na.rm = TRUE)
            boot_SD_TP[h] = sd(boot_store_TP[h,], na.rm = TRUE)
        }
        
        for (m in 1:nrow(data)) {
            data$TA_upr[m] = quantile(boot_store_TA[,m], probs = c(.25, .75), na.rm = TRUE)[2]
            data$TA_lwr[m] = quantile(boot_store_TA[,m], probs = c(.25, .75), na.rm = TRUE)[1]
            data$TP_upr[m] = quantile(boot_store_TP[,m], probs = c(.25, .75), na.rm = TRUE)[2]
            data$TP_lwr[m] = quantile(boot_store_TP[,m], probs = c(.25, .75), na.rm = TRUE)[1]
            
            data$TA_sd[m] = mean(boot_SD_TA, na.rm = TRUE)
            data$TA_sd_lwr[m] = quantile(boot_SD_TA, probs = c(.025, .975), na.rm = TRUE)[1]
            data$TA_sd_upr[m] = quantile(boot_SD_TA, probs = c(.025, .975), na.rm = TRUE)[2]
            
            data$TP_sd[m] = mean(boot_SD_TP, na.rm = TRUE)
            data$TP_sd_lwr[m] = quantile(boot_SD_TP, probs = c(.025, .975), na.rm = TRUE)[1]
            data$TP_sd_upr[m] = quantile(boot_SD_TP, probs = c(.025, .975), na.rm = TRUE)[2]
        }
        
    } else {
        data = open_data %>% 
            filter(exp == curr_exp) %>% 
            group_by(cond, conf_level, id_type, culprit_present) %>% 
            summarize(resps = length(id_type)) %>% 
            filter((id_type == "suspect" & culprit_present == "present") |
                       (id_type == "filler" & culprit_present == "absent")) %>% 
            mutate(id_type = "suspect") %>% 
            pivot_wider(names_from = c(cond, culprit_present),
                        values_from = resps, values_fill = NA) %>% 
            ungroup() %>% 
            mutate(TA_ratio = .[[3]] / .[[5]],
                   TP_ratio = .[[4]] / .[[6]],
                   TA_upr = NA,
                   TA_lwr = NA,
                   TP_upr = NA,
                   TP_lwr = NA)
        
        #*** Bootstrapping CIs on the ratios ----
        boot_data = open_data %>% 
            filter(exp == curr_exp)
        
        boot_store_TP = matrix(nrow = nboots,
                               ncol = max_confs)
        
        boot_store_TA = matrix(nrow = nboots,
                               ncol = max_confs)
        
        boot_SD_TP = NA
        boot_SD_TA = NA
        
        for (h in 1:nboots) {
            boot_data_sim = sample_n(boot_data, size = nrow(boot_data), replace = TRUE) %>% 
                group_by(cond, conf_level, id_type, culprit_present) %>% 
                summarize(resps = length(id_type)) %>% 
                filter((id_type == "suspect" & culprit_present == "present") |
                           (id_type == "filler" & culprit_present == "absent")) %>% 
                mutate(id_type = "suspect") %>% 
                pivot_wider(names_from = c(cond, culprit_present),
                            values_from = resps, values_fill = NA) %>% 
                ungroup() %>% 
                mutate(TA_ratio = .[[3]] / .[[5]],
                       TP_ratio = .[[4]] / .[[6]],
                       TA_upr = NA,
                       TA_lwr = NA,
                       TP_upr = NA,
                       TP_lwr = NA)
            
            for (l in 1:nrow(boot_data_sim)) {
                boot_store_TP[h, l] = boot_data_sim$TP_ratio[l]
                boot_store_TA[h, l] = boot_data_sim$TA_ratio[l]
            }
            
            boot_SD_TA[h] = sd(boot_store_TA[h,], na.rm = TRUE)
            boot_SD_TP[h] = sd(boot_store_TP[h,], na.rm = TRUE)
        }
        
        for (m in 1:nrow(data)) {
            data$TA_upr[m] = quantile(boot_store_TA[,m], probs = c(.25, .75), na.rm = TRUE)[2]
            data$TA_lwr[m] = quantile(boot_store_TA[,m], probs = c(.25, .75), na.rm = TRUE)[1]
            data$TP_upr[m] = quantile(boot_store_TP[,m], probs = c(.25, .75), na.rm = TRUE)[2]
            data$TP_lwr[m] = quantile(boot_store_TP[,m], probs = c(.25, .75), na.rm = TRUE)[1]
            
            data$TA_sd[m] = median(boot_SD_TA, na.rm = TRUE)
            data$TA_sd_lwr[m] = quantile(boot_SD_TA, probs = c(.025, .975), na.rm = TRUE)[1]
            data$TA_sd_upr[m] = quantile(boot_SD_TP, probs = c(.025, .975), na.rm = TRUE)[2]
            
            data$TP_sd[m] = median(boot_SD_TP, na.rm = TRUE)
            data$TP_sd_lwr[m] = quantile(boot_SD_TP, probs = c(.025, .975), na.rm = TRUE)[1]
            data$TP_sd_upr[m] = quantile(boot_SD_TP, probs = c(.025, .975), na.rm = TRUE)[2]
        }
    }
    
    nconfs = nrow(data)
    
    data = data %>% 
        mutate(exp = curr_exp,
               conf_level = c(1:nconfs),
               n_lineups = n_lineups) %>% 
        dplyr::select(-c(3:6))
    
    combined_data = rbind(combined_data, data)
    
    #hits_ratios$exp[i] = curr_exp
    #    
    #hits_ratios$culprit_present[i] = "hit"
    #    
    #hits_ratios$n_lineups[i] = n_lineups
    #    
    #    
    #fa_ratios$exp[i] = curr_exp
    #    
    #fa_ratios$culprit_present[i] = "fa"
    #    
    #fa_ratios$n_lineups[i] = n_lineups
    #    
    #    
    #for (j in 1:nrow(data)) {
    #        
    #    hits_ratios[i, j] = data[j, 7]
    #        
    #    fa_ratios[i, j] = data[j, 8]
    #    
    #}
}

#*** Plotting hits and FA ratios by confidence and experiment ----
combined_data2 = combined_data %>% 
    mutate(TP_sd_stats = sprintf("SD = %.2f [95%% CI: %.2f, %.2f]",
                                 TP_sd,
                                 TP_sd_lwr,
                                 TP_sd_upr),
           n_lineups_text = sprintf("%g lineups",
                                    n_lineups)) %>% 
    mutate(exp = ifelse(grepl("Same-plus-additional-pose", exp), "Colloff et al. (2021b): Exp 1: Same-plus-additional vs. Different", exp))

combined_data2$exp = reorder(combined_data2$exp, -combined_data2$n_lineups)

combined_data2$TP_sd_stats = reorder(combined_data2$TP_sd_stats, -combined_data2$n_lineups)

unique_exps = combined_data2 %>% 
    select(exp, n_lineups) %>% 
    distinct()

ratio_names = unique_exps$exp
names(ratio_names) = unique_exps$n_lineups

ratio_plot = combined_data2 %>% 
    mutate(TP_upr = ifelse(is.na(TP_ratio), NA, TP_upr),
           TP_lwr = ifelse(is.na(TP_ratio), NA, TP_lwr)) %>% 
    
    #filter(culprit_present == "hit") %>% 
    ggplot(aes(x = conf_level,
               y = TP_ratio))+
    facet_grid(cols = NULL,
               rows = vars(exp, n_lineups_text),
               scales = "free_y",
               labeller = label_wrap_gen(20))+
    geom_line(size = 1.5)+
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_ribbon(aes(ymax = TP_upr,
                    ymin = TP_lwr),
                alpha = .5)+
    guides(color = FALSE)+
    apatheme+
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 35),
          axis.title = element_text(size = 35),
          panel.spacing = unit(3, "lines"))+
    scale_x_continuous(breaks = seq(1, max_confs, by = 1))+
    labs(x = " 
         Confidence (Lowest-to-highest)",
         y = "Condition ratio of correct IDs
         ")

ratio_plot
ggsave("./Dataset testing and reports/HitRatios_Large.png",
       dpi = 300,
       height = 40,
       width = 20,
       units = "in")

#** Getting the proportion of hits and FAs at each confidence level ----
designated = open_data %>% 
    filter(culprit_present == "absent" & id_type == "suspect") %>% 
    select(exp) %>% 
    distinct()

exps_list = open_data %>% 
    select(exp) %>% 
    distinct()

conf_data = open_data %>% 
    mutate(designated = ifelse(exp %in% designated$exp, "Yes", "No")) %>% 
    mutate(id_type = ifelse(designated == "No" & culprit_present == "absent" & id_type == "filler", "suspect", id_type)) %>% 
    group_by(exp, cond, culprit_present, id_type, conf_level) %>% 
    summarize(n_resps = length(id_type)) %>% 
    ungroup() %>% 
    group_by(exp, cond, culprit_present, id_type) %>% 
    mutate(n_total = sum(n_resps)) %>% 
    ungroup() %>% 
    distinct(cond, culprit_present, id_type, conf_level, n_resps, n_total, .keep_all = TRUE) %>% 
    group_by(exp) %>% 
    mutate(max_conf = max(conf_level)) %>% 
    ungroup() %>% 
    mutate(conf_new = ifelse(max_conf > 20, (conf_level/10)+1, conf_level),
           prop = n_resps/n_total) %>% 
    filter(id_type == "suspect" & max_conf > 3) %>% 
    rowwise() %>% 
    mutate(exp_label = paste(exp, cond, sep = " "),
           culprit_present = ifelse(culprit_present == "present", 
                                    "Culprit-present",
                                    "Culprit-absent"))

conf_plot = conf_data %>% 
    mutate(conf_new = (conf_new - 1)*10) %>% 
    ggplot(aes(x = conf_new, y = prop, color = exp, group = exp_label))+
    facet_grid(cols = vars(culprit_present))+
    geom_point()+
    geom_smooth(aes(x = conf_new, y = prop, color = NULL, group = NULL), 
                color = "black",
                size = 1.5)+
    geom_line()+
    apatheme+
    scale_color_discrete(guide = FALSE)+
    labs(x = "Confidence",
         y = "Proportion of suspect IDs")+
    theme(text = element_text(size = 30),
          axis.title.y = element_text(size = 30,
                                      margin = margin(t = 0, r = 30, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 0, l = 0)))+
    scale_x_continuous(breaks = seq(0, 100, by = 10))
    
conf_plot
ggsave("./Dataset testing and reports/SuspectIDsByConfidence.png",
       dpi = 300,
       height = 12,
       width = 15,
       units = "in")

#** Dirichlet distribution ----
nsims = 10
draw = 10
alpha = c(1,2,3)
dimension = 3
x = rdirichlet(nsims, c(1, 1, 2, 2, 2, 3, 3, 6, 6, 7, 8))

dirichlet_data = x %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(conf_level = seq(0, 100, by = 10)) %>% 
    pivot_longer(!conf_level,
                 names_to = "sim", 
                 values_to = "prop")

dirichlet_data %>% 
    ggplot(aes(x = conf_level, y = prop, color = sim, group = sim))+
    geom_point()+
    geom_line()+
    apatheme+
    labs(x = "Confidence",
         y = "Proportion of IDs")+
    theme(text = element_text(size = 30),
          axis.title.y = element_text(size = 30,
                                      margin = margin(t = 0, r = 30, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 0, l = 0)))+
    scale_x_continuous(breaks = seq(0, 100, by = 10))+
    scale_color_discrete(name = "none",
                         guide = "none")
ggsave("./Dataset testing and reports/Dirichlet_Example.png",
       dpi = 300,
       height = 12,
       width = 15,
       units = "in")
