library(psych)
library(tidyverse)
library(sdtlu)
library(bayestestR)

#* Load in Colloff et al, (2021a) high- vs. low-similarity data ----
data = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 2/colloff_2021_processed_high_low.csv") %>% 
    mutate(lineup_size = 6) %>% 
    mutate(conf_level = 12 - conf_level)

data_low = data %>% 
    filter(cond == "Low-similarity fillers")

data_high = data %>% 
    filter(cond == "High-similarity fillers")

#* sdtlu processing data ----
sim_low = sdtlu_process_data(data_low)
sim_high = sdtlu_process_data(data_high)
sim_low
options = list(model_type = "sim",
               fit_fcn = "G2",
               fix_p = "data",
               fix_sigma_t = "free",
               use_restr_data = FALSE,
               run_bootstrap = FALSE)

fit_a = sdtlu_fit(sim_low,
                    options = options)

fit_b = sdtlu_fit(sim_high,
                     options = options)

cond1_label_test = "testtest_test"

test_data = data.frame(
    test = "test",
    cs = paste(round(cond_a_cs, digits = 3), collapse = ", ")
) %>% 
    separate(cs, as.character(c(1:12)), ", ")

colnames(test_data) = cond1_label_test
as.numeric(test_data[1,2])
print(cond_a_cs)
#* Getting relevant values from the model fits ----
#** Condition A ----
params_a = fit_a$best_params_full

cond_a_p = params_a[1]
cond_a_mu_t = params_a[2]
cond_a_sigma_t = params_a[3]
cond_a_cs = params_a[4:length(params_a)]
cond_a_data = 

cond_a_dists = data.frame(t = distribution_normal(100, 
                              mean = cond_a_mu_t,
                              sd = cond_a_sigma_t),
                          l = distribution_normal(100,
                                                  mean = 0,
                                                  sd = 1)) %>% 
    pivot_longer(names_to = "distribution",
                 values_to = "strength",
                 c(t,l)) %>% 
    mutate(cond = "A")

cond_a_fit = data.frame(data = fit_a$model_prop)


#** Condition B ----
params_b = fit_b$best_params_full

cond_b_p = params_b[1]
cond_b_mu_t = params_b[2]
cond_b_sigma_t = params_b[3]
cond_b_cs = params_b[4:length(params_b)]

cond_b_dists = data.frame(t = distribution_normal(100, 
                                                  mean = cond_b_mu_t,
                                                  sd = cond_b_sigma_t),
                          l = distribution_normal(100,
                                                  mean = 0,
                                                  sd = 1)) %>% 
    pivot_longer(names_to = "distribution",
                 values_to = "strength",
                 c(t,l))

#** Combined plot ----
cond_a_sdt = cond_a_dists %>% 
    ggplot(aes(x = strength, color = distribution))+
    geom_density()+
    geom_vline(xintercept = cond_a_cs)

cond_a_sdt

cond_b_sdt = cond_b_dists %>% 
    ggplot(aes(x = strength, color = distribution))+
    geom_density()+
    geom_vline(xintercept = cond_b_cs)

cond_a_sdt
cond_b_sdt
