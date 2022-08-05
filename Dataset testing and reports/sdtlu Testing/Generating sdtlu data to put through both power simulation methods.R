library(psych)
library(tidyverse)
library(sdtlu)

# Read in Colloff et al. 2021 High- vs. Low-similarity fillers ----
data = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 2/colloff_2021_processed_high_low.csv") %>% 
    mutate(conf_level = (max(conf_level)+1) - conf_level,
           lineup_size = 6) %>% 
    select(id_type,
           culprit_present,
           conf_level,
           cond,
           lineup_size
    )

# Low-similarity fillers ----
data_low = data %>% 
    filter(cond == "Low-similarity fillers")

data_low_processed = sdtlu_process_data(data_low)

## Model the data ----
options = list(model_type = "sim",
               fit_fcn = "G2",
               fix_p = "data",
               fix_sigma_t = "free",
               use_restr_data = FALSE,
               run_bootstrap = FALSE,
               n_bootstrap_samps = 10)

low_fit = sdtlu_fit(data_low_processed,
                     options = options)

params = low_fit$best_params_full

p = params[1]
mu_t = params[2]
sigma_t = params[3]
cs = params[4:length(params)]

## Simulate data ----
lineup_sizes = 6
n_trials = 6000
n_sims = 1

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

simmed_data_low = simmed_data_trial %>% 
    mutate(cond = "Low-similarity fillers")

# High-similarity fillers ----
data_high = data %>% 
    filter(cond == "High-similarity fillers")

data_high_processed = sdtlu_process_data(data_high)

## Model the data ----
options = list(model_type = "sim",
               fit_fcn = "G2",
               fix_p = "data",
               fix_sigma_t = "free",
               use_restr_data = FALSE,
               run_bootstrap = FALSE,
               n_bootstrap_samps = 10)

high_fit = sdtlu_fit(data_high_processed,
                    options = options)

params = high_fit$best_params_full

p = params[1]
mu_t = params[2]
sigma_t = params[3]
cs = params[4:length(params)]


## Simulate data ----
lineup_sizes = 6
n_trials = 6000
n_sims = 1

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

simmed_data_high = simmed_data_trial %>% 
    mutate(cond = "High-similarity fillers")

# Bind both simulated conditions together ----
simmed_data_both = rbind(simmed_data_low,
                         simmed_data_high) %>% 
    rename("culprit_present" = presence)

write.csv(simmed_data_both,
          "./Dataset testing and reports/sdtlu Testing/Simulated data/colloff_2021_simmed_data_high_low_6000trials.csv",
          na = "",
          row.names = FALSE)

# Get parameters for the sdtlu power simulation ----
## Low-similarity fillers ----
low_params = low_fit$best_params_full

p_low = low_params[1]
mu_t_low = low_params[2]
sigma_t_low = low_params[3]
cs_low = low_params[4:length(low_params)] %>% 
    paste(collapse = ",")

## Low-similarity fillers ----
high_params = high_fit$best_params_full

p_high = high_params[1]
mu_t_high = high_params[2]
sigma_t_high = high_params[3]
cs_high = high_params[4:length(high_params)] %>% 
    paste(collapse = ",")

## Summarize parameters ----
params_data = data.frame(cond = c("Low-similarity fillers",
                                  "High-similarity fillers"),
                         mu = c(mu_t_low,
                                mu_t_high),
                         sigma = c(sigma_t_low,
                                   sigma_t_high),
                         cs = c(cs_low,
                                cs_high))

write.csv(params_data,
          "./Dataset testing and reports/sdtlu Testing/colloff_2021_simmed_params_high_low.csv",
          na = "",
          row.names = FALSE)
