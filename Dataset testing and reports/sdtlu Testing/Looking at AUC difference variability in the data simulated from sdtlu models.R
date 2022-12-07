library(psych)
library(tidyverse)
library(pROC)

# Set the folder with all the datafiles ----
dataFolder = "./Dataset testing and reports/sdtlu Testing/Simulated data/"
fileList = list.files(dataFolder)

fileList = fileList[!grepl("variability", fileList) & !grepl("Old", fileList)]

auc_variability_store = data.frame(exp = rep(NA, times = length(fileList)))
# Loop to get statistics ----
for (i in 1:length(fileList)) {
    curr_data = read.csv(paste0(dataFolder, fileList[i]))
    
    curr_name = gsub(".csv", "", fileList[i])
    
    curr_exp = gsub("_.*", "", curr_name)
    
    curr_n = parse_number(gsub(".*_", "", curr_name))
    
    ## Condition A ----
    curr_data_A = curr_data %>% 
        filter(cond == "A")
    
    falseIDs_A = curr_data_A %>% 
        filter(culprit_present == "absent") %>% 
        mutate(conf_level = ifelse(id_type != "suspect", 0, conf_level)) %>% 
        select(conf_level) %>% 
        as.list() %>% 
        unlist() %>% 
        as.vector()
    
    correctIDs_A = curr_data_A %>% 
        filter(culprit_present == "present") %>% 
        mutate(conf_level = ifelse(id_type != "suspect", 0, conf_level)) %>% 
        select(conf_level) %>% 
        as.list() %>% 
        unlist() %>% 
        as.vector()
    
    partialAUC_A = length(falseIDs_A[falseIDs_A != 0]) / length(falseIDs_A)
    
    ### ROC curve ----
    roc_A = roc(controls = falseIDs_A,
                cases = correctIDs_A,
                direction = "<",
                ci = T)
    
    ## Condition B ----
    curr_data_B = curr_data %>% 
        filter(cond == "B")
    
    falseIDs_B = curr_data_B %>% 
        filter(culprit_present == "absent") %>% 
        mutate(conf_level = ifelse(id_type != "suspect", 0, conf_level)) %>% 
        select(conf_level) %>% 
        as.list() %>% 
        unlist() %>% 
        as.vector()
    
    correctIDs_B = curr_data_B %>% 
        filter(culprit_present == "present") %>% 
        mutate(conf_level = ifelse(id_type != "suspect", 0, conf_level)) %>% 
        select(conf_level) %>% 
        as.list() %>% 
        unlist() %>% 
        as.vector()
    
    partialAUC_B = length(falseIDs_B[falseIDs_B != 0]) / length(falseIDs_B)
    
    ### ROC curve ----
    roc_B = roc(controls = falseIDs_B,
                cases = correctIDs_B,
                direction = "<",
                ci = T)
    
    ## ROC comparison ----
    roc_A_B = roc.test(
        roc_A,
        roc_B,
        reuse.auc = FALSE,
        partial.auc = c(1, 1 - min(
            partialAUC_A, partialAUC_B
        )),
        partial.auc.focus = "sp",
        method = "bootstrap",
        paired = FALSE,
        boot.n = 1000,
        progress = "none",
        conf.level = 0.95
    )
    
    ## Save results ----
    auc_variability_store$exp[i] = curr_exp
    auc_variability_store$n[i] = curr_n
    auc_variability_store$auc1[i] = roc_A_B$estimate[1]
    auc_variability_store$auc2[i] = roc_A_B$estimate[2]
    auc_variability_store$auc_diff[i] = roc_A_B$estimate[1] - roc_A_B$estimate[2]
}

auc_variability_store_processed = auc_variability_store %>% 
    filter(n < 6000) %>% 
    group_by(exp) %>% 
    mutate(auc_diff_scaled = scale(auc_diff),
           mean_check = mean(auc_diff_scaled),
           sd_check = sd(auc_diff_scaled),
           exp = factor(exp, 
                        levels = c("Colloff (2021) High vs Low similarity",
                                   "Palmer (2013) Long vs Short delay",
                                   "Seale-Carlisle & Mickes (2016) UK vs US lineups",
                                   "Kaesler (2020) Simultaneous vs Sequential"),
                        labels = c("Colloff 2021",
                                   "Palmer 2013",
                                   "Seale-Carlisle 2016",
                                   "Kaesler 2020")))

auc_variability_store_processed %>% 
    ggplot(aes(x = exp, y = auc_diff, color = as.factor(n)))+
    geom_point(size = 3)+
    apatheme+
    labs(x = "Experiment",
         y = "pAUC difference",
         color = "Base sample size")+
    theme(text = element_text(size = 25),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     vjust = 1))

ggsave("./Dataset testing and reports/sdtlu Testing/Comparing sdtlu vs. data method/Figures/AUC variability (from sdtlu generation).png",
       dpi = 300,
       height = 10,
       width = 15,
       units = "in")
