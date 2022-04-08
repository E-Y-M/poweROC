library(psych)
library(tidyverse)
library(sdtlu)
library(bayestestR)
source("./ROC_power_app/scripts/func.R")

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
               fit_fcn = "G2",
               fix_p = "data",
               fix_sigma_t = "free",
               use_restr_data = FALSE,
               run_bootstrap = FALSE,
               n_bootstrap_samps = 10)

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
fit_a$quants_fit

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
