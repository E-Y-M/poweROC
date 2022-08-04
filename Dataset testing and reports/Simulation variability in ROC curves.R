library(psych)
library(tidyverse)
library(here)
library(sdtlu)

# APA theme for plots
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

# Simulations from data ----
## Set simulation parameters ----
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

h = 1
i = 1
## Main simulation loop ----
for (h in 1:length(ns)) {
    curr_n = ns[h]
    for (i in 1:nsims) {
        ### For each sim, sample ROC data from the root ----
        #### For Condition 1 ----
        ##### TA ----
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
        
        ##### TP ----
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

## Simulation data processing & plotting ----
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

# Simulations of sdtlu-generated data ----
data = read.csv(here("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/colloff_2021_processed_high_med.csv"), fileEncoding = "UTF-8-BOM") %>% 
    filter(cond == "High-similarity fillers") %>% 
    mutate(conf_level = (max(conf_level) + 1) - conf_level,
           lineup_size = 6) %>% 
    select(id_type,
           conf_level,
           culprit_present,
           lineup_size)

data_processed = sdtlu_process_data(data)

## Model the data ----
options = list(model_type = "sim",
               fit_fcn = "G2",
               fix_p = "data",
               fix_sigma_t = "free",
               use_restr_data = FALSE,
               run_bootstrap = FALSE,
               n_bootstrap_samps = 10)

data_fit = sdtlu_fit(data_processed,
                     options = options)

params = data_fit$best_params_full

p = params[1]
mu_t = params[2]
sigma_t = params[3]
cs = params[4:length(params)]

## Set simulation parameters ----
ROC_data_store = data.frame(prop = rep(NA, times = 1*
                                           length(unique(data$culprit_present))*
                                           length(unique(data$conf_level))*
                                           1),
                            sim = NA,
                            n = NA,
                            presence = NA,
                            criteria = NA)


nsims = 200
n_TA_lineups = 1
n_TP_lineups = 1
ns = c(250, 500, 750)
lineup_sizes = 6

## Simulation loop ----
for (h in 1:length(ns)) {
    curr_n = ns[h]
    
    n_trials = (curr_n * n_TA_lineups) + (curr_n * n_TP_lineups)
    
    n_sims = 1
    
    for (i in 1:nsims) {
        ### Simulate data from the model ----
        params = c(p, mu_t, sigma_t, cs)
        simmed_data = as.data.frame(t(as.data.frame(sdtlu_sim_sim(params, lineup_sizes, n_trials, n_sims))))
        simmed_data$id_type = rep(c(rep("suspect", length(cs)),
                                      rep("filler", length(cs)),
                                      "reject"), times = 2)
        simmed_data$conf_level_rev = rep(c(1:length(cs), 1:length(cs), NA),
                                           times = 2)
        simmed_data$conf_level = rep(c(length(cs):1, length(cs):1, NA),
                                       times = 2)
        simmed_data$presence = c(rep("present", times = length(cs)*2+1),
                                   rep("absent", times = length(cs)*2+1))
        
        simmed_data_TP_rej = data.frame()
        
        for (j in 1:length(cs)) {
            simmed_data_TP_rej = rbind(simmed_data_TP_rej,
                                         filter(simmed_data, id_type == "reject" & presence == "present"))
        }
        
        simmed_data_TP_rej$conf_level_rev = 1:length(cs)
        simmed_data_TP_rej$conf_level = length(cs):1
        simmed_data_TP_rej$V1 = round(simmed_data_TP_rej$V1/length(cs))
        
        simmed_data_TA_rej = data.frame()
        
        for (k in 1:length(cs)) {
            simmed_data_TA_rej = rbind(simmed_data_TA_rej,
                                       filter(simmed_data, id_type == "reject" & presence == "absent"))
        }
        
        simmed_data_TA_rej$conf_level_rev = 1:length(cs)
        simmed_data_TA_rej$conf_level = length(cs):1
        simmed_data_TA_rej$V1 = round(simmed_data_TA_rej$V1/length(cs))
        
        simmed_data_final = filter(rbind(simmed_data,
                                           simmed_data_TP_rej,
                                           simmed_data_TA_rej),
                                     !is.na(conf_level))
        
        simmed_data = simmed_data_final
        
        conf_levels = as.numeric(unique(simmed_data$conf_level))
        
        simmed_data_trial = data.frame()
        
        for (l in 1:nrow(simmed_data)) {
            simmed_data_slice = simmed_data[l,]
            simmed_data_append = do.call("rbind", replicate(simmed_data_slice$V1, simmed_data_slice, simplify = FALSE))
            simmed_data_trial = rbind(simmed_data_trial,
                                      simmed_data_append)    
        }
        
        simmed_data_trial$conf_level = ifelse(simmed_data_trial$id_type == "filler" |
                                                  simmed_data_trial$id_type == "reject", 0,
                                              simmed_data_trial$conf_level)
        
        ##### For each sim, sample ROC data from the root ----
        ###### For Condition 1 ----
        ####### TA ----
        TA_data_cond1 = as.vector(simmed_data_trial$conf_level[simmed_data_trial$presence == "absent"])
        
        ####### TP ----
        TP_data_cond1 = as.vector(simmed_data_trial$conf_level[simmed_data_trial$presence == "present"])
        
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
        for (m in 1:length(unique(TA_TP_data$culprit_present))) {
            curr_present = unique(TA_TP_data$culprit_present)[m]
            for (n in 1:length(unique(TA_TP_data$conf_level_rev))) {
                curr_conf = unique(TA_TP_data$conf_level_rev)[n]
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

ggsave("./Dataset testing and reports/ROC_sim_variability_sdtlu.png",
       dpi = 300,
       height = 10,
       width = 22,
       units = "in")
