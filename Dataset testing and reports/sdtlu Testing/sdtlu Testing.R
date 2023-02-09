library(psych)
library(tidyverse)
library(sdtlu)
library(bayestestR)
source("./ROC_power_app/scripts/func.R")

pchisq(6.63, df = 1, lower.tail = FALSE)

test_cs = "1, 1.5, 2"
test_cs_vector = extract(test_cs)
test_cs_vector[1]
#* Load in Palmer et al. (2013) data ----
palmer_data = read.csv("./Dataset testing and reports/sdtlu Testing/palmer_data.csv")

#* Load in Colloff et al, (2021a) high- vs. low-similarity data ----
data = read.csv("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 2/colloff_2021_processed_high_low.csv") %>% 
    mutate(lineup_size = 6) %>% 
    mutate(conf_level = 12 - conf_level)

data_low = data %>% 
    filter(cond == "Low-similarity fillers")

data_high = data %>% 
    filter(cond == "High-similarity fillers")

#* sdtlu processing data ----
sdtlu_graph_data_roc(sim_low$resp_data_full) 

sim_low = sdtlu_process_data(data_low)
sim_high = sdtlu_process_data(data_high)
sim_low
options = list(model_type = "sim",
               fit_fcn = "chi-square",
               fix_p = "data",
               fix_sigma_t = "free",
               use_restr_data = FALSE,
               run_bootstrap = FALSE,
               n_bootstrap_samps = 10)

fit_a = sdtlu_fit(sim_low,
                    options = options)

fit_b = sdtlu_fit(sim_high,
                     options = options)

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
fit_a$quants_fit

cond_a_p = params_a[1]
cond_a_mu_t = params_a[2]
cond_a_sigma_t = params_a[3]
cond_a_cs = params_a[4:length(params_a)]

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

#** Getting the model-predicted ROC ----
cond_a_roc = sdtlu_graph_model_roc(
    params = params_a,
    params_quants = NULL,
    lineup_size = 6)

params_a = c(cond_a_p,
           cond_a_mu_t,
           cond_a_sigma_t,
           cond_a_cs)

cond_b_roc = sdtlu_graph_model_roc(
    params = c(cond_b_p,
               cond_b_mu_t,
               cond_b_sigma_t,
               cond_b_cs),
    params_quants = quants_matrix,
    lineup_size = 6)

quants_matrix = data.frame(upr = c(
    NA,
    .975,
    .975,
    rep(NA, times = 11)),
    lwr = c(
        NA,
        .025,
        .025,
        rep(NA, times = 11)))


sdtlu_graph_model_roc
quants_matrix[1,1]

cond_a_roc2 = sdtlu_roc_model(
    params = params_a,
    lineup_size = 6)

cond_a_roc_data_lines = data.frame(x = cond_a_roc2[[3]],
                                   y = cond_a_roc2[[4]])

cond_a_roc_data_points = data.frame(x = unlist(cond_a_roc2[[1]]),
                                    y = unlist(cond_a_roc2[[2]]))

cond_a_rocplot = ggplot(cond_a_roc_data_lines,
                        aes(x = x,
                            y = y))+
    geom_point()+
    geom_point(data = cond_a_roc_data_points,
               aes(x = x, y = y),
               color = "red",
               size = 3)

cond_a_rocplot

#** Getting the data ROC ----
cond_a_data_roc = sdtlu_roc_data(sim_low$resp_data_full)

cond_a_data_roc = data.frame(x = cond_a_data_roc[[2]],
                             y = cond_a_data_roc[[1]])

cond_a_rocplot_complete = cond_a_rocplot +
    geom_point(data = cond_a_data_roc, 
               aes(x = x,
                   y = y),
               color = "blue",
               size = 5)+
    geom_line(data = cond_a_data_roc,
              aes(x = x,
                  y = y))

cond_a_rocplot_complete

#* Testing things on the Palmer data ----
palmer_processed = sdtlu_process_data(palmer_data)

options = list(model_type = "sim",
               fit_fcn = "chi_square",
               fix_p = "data",
               fix_sigma_t = "free",
               use_restr_data = FALSE,
               run_bootstrap = TRUE,
               n_bootstrap_samps = 20)

palmer_fit = sdtlu_fit(palmer_processed,
                       options = options)

quants_matrix = palmer_fit$quants_params

palmer_roc = sdtlu_graph_model_roc(
    params = palmer_fit$best_params_full,
    params_quants = quants_matrix,
    lineup_size = 6)

#* Testing what happens when there aren't IDs at a confidence level ----
test_data = palmer_data %>% 
    filter(!(id_type == "filler" & conf_level == 4))

n_confs = length(unique(test_data$conf_level))
max_conf = max(test_data$conf_level)
min_conf = min(test_data$conf_level)

resp_vector = rep(c(rep("suspect", n_confs),
                rep("filler", n_confs),
                "reject"), times = 2)

palmer_processed = sdtlu_process_data(test_data)

options = list(model_type = "sim",
               fit_fcn = "G2",
               fix_p = "data",
               fix_sigma_t = "free",
               use_restr_data = FALSE,
               run_bootstrap = TRUE,
               n_bootstrap_samps = 20)

palmer_fit = sdtlu_fit(palmer_processed,
                       options = options)

palmer_fit$best_fit_measure

test = palmer_fit$model_prop
palmer_processed$resp_data_full

model_fit_data = data.frame(model_prop = as.vector(palmer_fit$model_prop),
                            data_resps = as.vector(palmer_processed$resp_data_full),
                            resp_type = rep(c(rep("Suspect", n_confs),
                                              rep("Filler", n_confs),
                                              "Reject"), times = 2),
                            conf_level = rep(c(max_conf:min_conf, max_conf:min_conf, NA),
                                               times = 2),
                            Presence = c(rep("TP", times = n_confs*2+1),
                                         rep("TA", times = n_confs*2+1))) %>% 
    mutate(data_total = sum(data_resps),
           data_prop = data_resps/data_total,
           row = 1:n()) %>% 
    rowwise() %>% 
    mutate(label = paste(Presence, resp_type, conf_level, sep = "_"),
           label = str_replace(label, "_NA", "")) %>% 
    arrange(row) %>% 
    pivot_longer(names_to = "Source",
                 values_to = "Proportion",
                 c(model_prop, data_prop)) %>% 
    mutate(Source = ifelse(grepl("data", Source), "Data", "Model"),
           Condition = "A")

model_fit_data$label = reorder(model_fit_data$label, model_fit_data$row)

model_fit_data %>% 
    ggplot(aes(x = label, y = Proportion, color = Presence, shape = Source))+
    geom_point()+
    scale_shape_manual(values = c(1, 4))+
    scale_color_manual(values = c("red", "blue"))+
    apatheme+
    ggtitle("Model response predictions vs. data")+
    theme(plot.title = element_text(hjust = .5),
          axis.text.x = element_text(angle = 90,
                                     vjust = .5))

#* Simulating data from a model ----
#** Condition A ----
p_a = .5
mu_t_a = 1
sigma_t_a = 1
cs_a = c(1.5, 2, 2.2, 2.5)
lineup_sizes_a = 6
n_trials = 1000
n_sims = 1

params_a = c(p_a, mu_t_a, sigma_t_a, cs_a)
simmed_data_a = as.data.frame(t(as.data.frame(sdtlu_sim_sim(params_a, lineup_sizes_a, n_trials, n_sims))))
simmed_data_a$id_type = rep(c(rep("suspect", length(cs_a)),
                             rep("filler", length(cs_a)),
                             "reject"), times = 2)
simmed_data_a$conf_level_rev = rep(c(1:length(cs_a), 1:length(cs_a), NA),
                             times = 2)
simmed_data_a$conf_level = rep(c(length(cs_a):1, length(cs_a):1, NA),
                               times = 2)
simmed_data_a$presence = c(rep("present", times = length(cs_a)*2+1),
                           rep("absent", times = length(cs_a)*2+1))

# Test that the condition n's make sense ----
simmed_data_a_ns = simmed_data_a %>% 
    group_by(presence) %>% 
    summarize(n = sum(V1))

# sdtlu doesn't strictly enforce exactly equal assignment of 

simmed_data_a_TP_rej = data.frame()

for (i in 1:length(cs_a)) {
    simmed_data_a_TP_rej = rbind(simmed_data_a_TP_rej,
                                 filter(simmed_data_a, id_type == "reject" & presence == "present"))
}

simmed_data_a_TP_rej = rbind(filter(simmed_data_a, id_type == "reject" & presence == "present"),
                             filter(simmed_data_a, id_type == "reject" & presence == "present"),
                             filter(simmed_data_a, id_type == "reject" & presence == "present"))
simmed_data_a_TP_rej$conf_level_rev = 1:length(cs_a)
simmed_data_a_TP_rej$conf_level = length(cs_a):1
simmed_data_a_TP_rej$V1 = round(simmed_data_a_TP_rej$V1/length(cs_a))

simmed_data_a_TA_rej = rbind(filter(simmed_data_a, id_type == "reject" & presence == "absent"),
                             filter(simmed_data_a, id_type == "reject" & presence == "absent"),
                             filter(simmed_data_a, id_type == "reject" & presence == "absent"))
simmed_data_a_TA_rej$conf_level_rev = 1:length(cs_a)
simmed_data_a_TA_rej$conf_level = length(cs_a):1
simmed_data_a_TA_rej$V1 = round(simmed_data_a_TA_rej$V1/length(cs_a))

simmed_data_a_final = filter(rbind(simmed_data_a,
                      simmed_data_a_TP_rej,
                      simmed_data_a_TA_rej),
                      !is.na(conf_level))
simmed_data_a_final$cond = "A"

#** Condition B ----
p_b = .5
mu_t_b = 2
sigma_t_b = 1.4
cs_b = c(.5, 2.5, 3.5)
lineup_sizes_b = 6
n_trials = 10000
n_sims = 1

params_b = c(p_b, mu_t_b, sigma_t_b, cs_b)
simmed_data_b = as.data.frame(t(as.data.frame(sdtlu_sim_sim(params_b, lineup_sizes_b, n_trials, n_sims))))
simmed_data_b$id_type = rep(c(rep("suspect", length(cs)),
                              rep("filler", length(cs)),
                              "reject"), times = 2)
simmed_data_b$conf_level_rev = rep(c(1:length(cs_b), 1:length(cs_b), NA),
                                   times = 2)
simmed_data_b$conf_level = rep(c(length(cs_b):1, length(cs_b):1, NA),
                               times = 2)
simmed_data_b$presence = c(rep("present", times = length(cs_b)*2+1),
                           rep("absent", times = length(cs_b)*2+1))

simmed_data_b_TP_rej = rbind(filter(simmed_data_b, id_type == "reject" & presence == "present"),
                             filter(simmed_data_b, id_type == "reject" & presence == "present"),
                             filter(simmed_data_b, id_type == "reject" & presence == "present"))
simmed_data_b_TP_rej$conf_level_rev = 1:length(cs_b)
simmed_data_b_TP_rej$conf_level = length(cs_b):1
simmed_data_b_TP_rej$V1 = round(simmed_data_b_TP_rej$V1/length(cs_b))

simmed_data_b_TA_rej = rbind(filter(simmed_data_b, id_type == "reject" & presence == "absent"),
                             filter(simmed_data_b, id_type == "reject" & presence == "absent"),
                             filter(simmed_data_b, id_type == "reject" & presence == "absent"))
simmed_data_b_TA_rej$conf_level_rev = 1:length(cs_b)
simmed_data_b_TA_rej$conf_level = length(cs_b):1
simmed_data_b_TA_rej$V1 = round(simmed_data_b_TA_rej$V1/length(cs_b))

simmed_data_b_final = filter(rbind(simmed_data_b,
                                   simmed_data_b_TP_rej,
                                   simmed_data_b_TA_rej),
                             !is.na(conf_level))
simmed_data_b_final$cond = "B"

#** Combined both conditions ----
simmed_data = rbind(simmed_data_a_final,
                    simmed_data_b_final)
replicate(simmed_data, 2)

#** Generate trial-level data ----
simmed_data_trial = data.frame()

for (i in 1:nrow(simmed_data)) {
    simmed_data_slice = simmed_data[i,]
    simmed_data_append = do.call("rbind", replicate(simmed_data_slice$V1, simmed_data_slice, simplify = FALSE))
    simmed_data_trial = rbind(simmed_data_trial,
                              simmed_data_append)    
}

p = 1/3
mu_t_a = 1
sigma_t_a = 1
cs_a = extract("1, 1.5, 2")

params_a = c(p, mu_t_a, sigma_t_a, cs_a)
params_a
params_a[1]

# Testing a sequential lineup dataset ----
smith_data = read.csv("./Dataset testing and reports/Data/01 - All data files for combining/exp1_sequential_processed.csv")

## Smith et al. (2022) "warning" condition ----
smith_data_warning = smith_data %>% 
    filter(cond == "warning") %>% 
    mutate(conf_level = conf_level + 1,
           conf_level_rev = (max(smith_data$conf_level)+2)-conf_level) %>% 
    mutate(conf_level_original = conf_level,
           conf_level = conf_level_rev)

warning_processed = sdtlu_process_data(smith_data_warning)

warning_processed$resp_data_full

as.character(paste0(warning_processed$pos_prop, collapse = ","))

options = list(model_type = "seq",
               fit_fcn = "chi_square",
               fix_p = "data",
               fix_sigma_t = "free",
               use_restr_data = FALSE,
               run_bootstrap = FALSE,
               n_bootstrap_samps = 10)

fit_warning = sdtlu_fit(warning_processed,
                  options = options)

params_warning = fit_warning$best_params_full

fit_warning$best_fit_measure

pchisq(fit_warning$best_fit_measure, 1, lower.tail = FALSE)

fit_warning$model_prop
warning_processed$resp_data_full

n_confs_a = length(unique(smith_data_warning$conf_level))

## "No-warning" condition
smith_data_nowarning = smith_data %>% 
    filter(cond == "no warning") %>% 
    mutate(conf_level = conf_level + 1)

smith_data_nowarning = smith_data_nowarning %>% 
    mutate(conf_level_rev = (max(smith_data_nowarning$conf_level)+1)-conf_level) %>% 
    mutate(conf_level_original = conf_level,
           conf_level = conf_level_rev)

max(smith_data_nowarning$conf_level)

nowarning_processed = sdtlu_process_data(smith_data_nowarning)

nowarning_processed$resp_data_full

as.character(paste0(nowarning_processed$pos_prop, collapse = ","))

options = list(model_type = "seq",
               fit_fcn = "chi-square",
               fix_p = "data",
               fix_sigma_t = "free",
               use_restr_data = FALSE,
               run_bootstrap = FALSE,
               n_bootstrap_samps = 10)

fit_nowarning = sdtlu_fit(nowarning_processed,
                        options = options)

params_nowarning = fit_nowarning$best_params_full

fit_nowarning$model_prop
nowarning_processed$resp_data_full

n_confs_a = length(unique(smith_data_nowarning$conf_level))

## Missing confidence levels ----
smith_data_conf = smith_data_nowarning %>% 
    mutate(conf_level = ifelse(id_type == "reject", 99, conf_level)) %>% 
    #filter(cond == "warning") %>% 
    group_by(id_type, conf_level, cond, culprit_present) %>% 
    count() %>% 
    mutate(id_type = factor(id_type,
                            levels = c("suspect", "filler", "reject"))) %>% 
    arrange(desc(culprit_present),
            id_type,
            conf_level)

smith_data_all_confs = data.frame(
    id_type = rep(c(rep("suspect", times = 10),
                rep("filler", times = 10),
                "reject"), times = 2),
    conf_level = rep(c(1:10,
                       1:10,
                       99),
                     times = 2),
    culprit_present = rep(c("present", "absent"),
                          each = 21))

smith_data_conf_combined = smith_data_all_confs %>% 
    left_join(smith_data_conf) %>% 
    mutate(n = ifelse(is.na(n), 0, n))

max_conf_smith = max(smith_data_warning$conf_level)
min_conf_smith = min(smith_data_warning$conf_level)

length(max)

cond1_confs = sort(unique(smith_data$conf_level[smith_data$cond == unique(as.factor(smith_data$cond))[1]]),
                   decreasing = TRUE)
cond1_confs

cond2_confs = sort(unique(smith_data$conf_level[smith_data$cond == unique(as.factor(smith_data$cond))[2]]),
                   decreasing = TRUE)

cond2_confs
