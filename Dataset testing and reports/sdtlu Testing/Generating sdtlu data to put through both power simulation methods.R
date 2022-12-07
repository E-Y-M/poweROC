library(psych)
library(tidyverse)
library(sdtlu)

# Specify data names ----
datafile_names = c("Colloff (2021) High vs Low similarity",
                   "Palmer (2013) Long vs Short delay")

datafiles = list.files("./Dataset testing and reports/sdtlu Testing/Datafiles to use")
datafiles = datafiles[grepl("colloff", datafiles) | grepl("palmer", datafiles)]

lineup_types = c("sim", "sim")
ns = c(500, 1000, 2000, 3000)


for (i in 1:length(datafile_names)) {
    # Read in data ----
    
    data = read.csv(paste0("./Dataset testing and reports/sdtlu Testing/Datafiles to use/", datafiles[i])) %>% 
        #mutate(conf_level = (max(conf_level)+1) - conf_level,
        #       lineup_size = 6) %>% 
        select(id_type,
               culprit_present,
               conf_level,
               cond,
               lineup_size,
               suspect_position
        ) %>% 
        mutate(cond = as.factor(cond),
               conf_level = (max(conf_level)+1) - conf_level)
    
    conds = levels(data$cond)
    
    data = data %>% 
        mutate(cond = ifelse(cond == conds[1], "A", "B"))
    
    # Model the data ----
    options_A = list(model_type = lineup_types[i],
                   fit_fcn = "G2",
                   fix_p = "data",
                   fix_sigma_t = "free",
                   use_restr_data = FALSE,
                   run_bootstrap = FALSE,
                   n_bootstrap_samps = 10,
                   n_fits = 5)
    
    ## If manually setting the condition ----
    options_A = list(model_type = "sim",
                   fit_fcn = "G2",
                   fix_p = "data",
                   fix_sigma_t = "free",
                   use_restr_data = FALSE,
                   run_bootstrap = FALSE,
                   n_bootstrap_samps = 10,
                   n_fits = 5)
    
    ## Condition A ----
    data_A = data %>% 
        filter(cond == "A")
    
    lineup_size_A = as.numeric(unique(data_A$lineup_size))
    
    data_A_processed = sdtlu_process_data(data_A)

    A_fit = sdtlu_fit(data_A_processed,
                        options = options_A)
    
    params_A = A_fit$best_params_full
    
    p_A = params_A[1]
    mu_t_A = params_A[2]
    sigma_t_A = params_A[3]
    cs_A = params_A[4:length(params_A)]
    
    if (options_A$model_type == "seq") {
        pos_prop_A = as.vector(data_A_processed$pos_prop)
    } else {
        pos_prop_A = NA
    }
    
    ## Condition B ----
    options_B = list(model_type = lineup_types[i],
                     fit_fcn = "G2",
                     fix_p = "data",
                     fix_sigma_t = "free",
                     use_restr_data = FALSE,
                     run_bootstrap = FALSE,
                     n_bootstrap_samps = 10,
                     n_fits = 5)
    
    ## If manually setting the condition ----
    options_B = list(model_type = "sim",
                   fit_fcn = "G2",
                   fix_p = "data",
                   fix_sigma_t = "free",
                   use_restr_data = FALSE,
                   run_bootstrap = FALSE,
                   n_bootstrap_samps = 10,
                   n_fits = 5)
    
    data_B = data %>% 
        filter(cond == "B")
    
    lineup_size_B = as.numeric(unique(data_B$lineup_size))
    
    data_B_processed = sdtlu_process_data(data_B)
    
    B_fit = sdtlu_fit(data_B_processed,
                      options = options_B)
    
    params_B = B_fit$best_params_full
    
    p_B = params_B[1]
    mu_t_B = params_B[2]
    sigma_t_B = params_B[3]
    cs_B = params_B[4:length(params_B)]
    
    if (options_B$model_type == "seq") {
        pos_prop_B = as.vector(data_B_processed$pos_prop)
    } else {
        pos_prop_B = NA
    }
    
    # Simulate data ----
    for (n in 1:length(ns)) {
        n_trials = ns[n]
        n_sims = 1
        
        ## Condition A ----
        params = c(p_A, mu_t_A, sigma_t_A, cs_A)
        lineup_sizes = as.numeric(data_A$lineup_size[1])
        
        if (options_A$model_type == "seq") {
            simmed_data = as.data.frame(t(as.data.frame(sdtlu_seq_sim(params, lineup_sizes, n_trials, n_sims, pos_prop = pos_prop_A))))
        } else {
            simmed_data = as.data.frame(t(as.data.frame(sdtlu_sim_sim(params, lineup_sizes, n_trials, n_sims))))
        }
        
        simmed_data$id_type = rep(c(rep("suspect", length(cs_A)),
                                    rep("filler", length(cs_A)),
                                    "reject"), times = 2)
        simmed_data$conf_level_rev = rep(c(1:length(cs_A), 1:length(cs_A), NA),
                                         times = 2)
        simmed_data$conf_level = rep(c(length(cs_A):1, length(cs_A):1, NA),
                                     times = 2)
        simmed_data$presence = c(rep("present", times = length(cs_A)*2+1),
                                 rep("absent", times = length(cs_A)*2+1))
        
        simmed_data_TP_rej = data.frame()
        
        for (j in 1:length(cs_A)) {
            simmed_data_TP_rej = rbind(simmed_data_TP_rej,
                                       filter(simmed_data, id_type == "reject" & presence == "present"))
        }
        
        simmed_data_TP_rej$conf_level_rev = 1:length(cs_A)
        simmed_data_TP_rej$conf_level = length(cs_A):1
        simmed_data_TP_rej$V1 = round(simmed_data_TP_rej$V1/length(cs_A))
        
        simmed_data_TA_rej = data.frame()
        
        for (k in 1:length(cs_A)) {
            simmed_data_TA_rej = rbind(simmed_data_TA_rej,
                                       filter(simmed_data, id_type == "reject" & presence == "absent"))
        }
        
        simmed_data_TA_rej$conf_level_rev = 1:length(cs_A)
        simmed_data_TA_rej$conf_level = length(cs_A):1
        simmed_data_TA_rej$V1 = round(simmed_data_TA_rej$V1/length(cs_A))
        
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
        
        simmed_data_A = simmed_data_trial %>% 
            mutate(cond = "A")
        
        ## Condition B ----
        params = c(p_B, mu_t_B, sigma_t_B, cs_B)
        lineup_sizes = as.numeric(data_B$lineup_size[1])
        
        if (options_B$model_type == "seq") {
            simmed_data = as.data.frame(t(as.data.frame(sdtlu_seq_sim(params, lineup_sizes, n_trials, n_sims, pos_prop = pos_prop_B))))
        } else {
            simmed_data = as.data.frame(t(as.data.frame(sdtlu_sim_sim(params, lineup_sizes, n_trials, n_sims))))
        }
        
        simmed_data$id_type = rep(c(rep("suspect", length(cs_B)),
                                    rep("filler", length(cs_B)),
                                    "reject"), times = 2)
        simmed_data$conf_level_rev = rep(c(1:length(cs_B), 1:length(cs_B), NA),
                                         times = 2)
        simmed_data$conf_level = rep(c(length(cs_B):1, length(cs_B):1, NA),
                                     times = 2)
        simmed_data$presence = c(rep("present", times = length(cs_B)*2+1),
                                 rep("absent", times = length(cs_B)*2+1))
        
        simmed_data_TP_rej = data.frame()
        
        for (x in 1:length(cs_B)) {
            simmed_data_TP_rej = rbind(simmed_data_TP_rej,
                                       filter(simmed_data, id_type == "reject" & presence == "present"))
        }
        
        simmed_data_TP_rej$conf_level_rev = 1:length(cs_B)
        simmed_data_TP_rej$conf_level = length(cs_B):1
        simmed_data_TP_rej$V1 = round(simmed_data_TP_rej$V1/length(cs_B))
        
        simmed_data_TB_rej = data.frame()
        
        for (y in 1:length(cs_B)) {
            simmed_data_TB_rej = rbind(simmed_data_TB_rej,
                                       filter(simmed_data, id_type == "reject" & presence == "absent"))
        }
        
        simmed_data_TB_rej$conf_level_rev = 1:length(cs_B)
        simmed_data_TB_rej$conf_level = length(cs_B):1
        simmed_data_TB_rej$V1 = round(simmed_data_TB_rej$V1/length(cs_B))
        
        simmed_data_final = filter(rbind(simmed_data,
                                         simmed_data_TP_rej,
                                         simmed_data_TB_rej),
                                   !is.na(conf_level))
        
        simmed_data = simmed_data_final
        
        conf_levels = as.numeric(unique(simmed_data$conf_level))
        
        simmed_data_trial = data.frame()
        
        for (z in 1:nrow(simmed_data)) {
            simmed_data_slice = simmed_data[z,]
            simmed_data_append = do.call("rbind", replicate(simmed_data_slice$V1, simmed_data_slice, simplify = FALSE))
            simmed_data_trial = rbind(simmed_data_trial,
                                      simmed_data_append)    
        }
        
        simmed_data_B = simmed_data_trial %>% 
            mutate(cond = "B")
        
        ## Bind both simulated conditions together ----
        simmed_data_both = rbind(simmed_data_A,
                                 simmed_data_B) %>% 
            rename("culprit_present" = presence) %>% 
            rowwise() %>% 
            mutate(suspect_position = ifelse(cond == "A" & options_A$model_type == "seq", 
                                             sample(c(1:lineup_size_A), 1), 
                                             ifelse(cond == "B" & options_B$model_type == "seq",
                                                    sample(c(1:lineup_size_B, 1)), NA)),
                   lineup_size = ifelse(cond == "A", lineup_size_A, lineup_size_B)) %>% 
            mutate(cond = factor(cond, levels = c("A", "B")))
        
        write.csv(simmed_data_both,
                  paste0("./Dataset testing and reports/sdtlu Testing/Simulated data/", datafile_names[i], "_", ns[n], "trials.csv"),
                  na = "",
                  row.names = FALSE)
    }
 }

# Testing why sdtlu can't fit the generated Kaesler simultaneous data ----
kaesler_test = read.csv("./Dataset testing and reports/sdtlu Testing/Simulated data/Kaesler (2020) Simultaneous vs Sequential_500trials.csv") %>% 
    filter(cond == "A") %>% 
    mutate(conf_level_original = conf_level,
           conf_level = (max(conf_level)+1) - conf_level)

kaesler_test_processed = sdtlu_process_data(kaesler_test)

options_B = list(model_type = "seq",
                 fit_fcn = "G2",
                 fix_p = "data",
                 fix_sigma_t = "free",
                 use_restr_data = FALSE,
                 run_bootstrap = FALSE,
                 n_bootstrap_samps = 10)

kaesler_test_fit = sdtlu_fit(kaesler_test_processed,
                  options = options_B)

kaesler_test_order = read.csv("./Dataset testing and reports/sdtlu Testing/Simulated data/Kaesler (2020) Simultaneous vs Sequential_500trials.csv")

as.character(levels(kaesler_test_order$cond)[1])

# Testing why sdtlu can't fit the Seale-Carlisle simultaneous data ----
seale_test = read.csv("./Dataset testing and reports/sdtlu Testing/Simulated data/Seale-Carlisle & Mickes (2016) UK vs US lineups_500trials.csv") %>% 
    filter(cond == "B") %>% 
    mutate(conf_level_original = conf_level,
           conf_level = (max(conf_level)+1) - conf_level)

seale_test_processed = sdtlu_process_data(seale_test)

options_B = list(model_type = "sim",
                 fit_fcn = "G2",
                 fix_p = "data",
                 fix_sigma_t = "free",
                 use_restr_data = FALSE,
                 run_bootstrap = FALSE,
                 n_bootstrap_samps = 10)

seale_test_fit = sdtlu_fit(seale_test_processed,
                             options = options_B)

seale_test_order = read.csv("./Dataset testing and reports/sdtlu Testing/Simulated data/Seale-Carlisle & Mickes (2016) UK vs US lineups_500trials.csv")

as.character(levels(seale_test_order$cond))[3]
