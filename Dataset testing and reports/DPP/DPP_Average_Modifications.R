# Authors: Andrew M. Smith, James M. Lampinen, Gary L. Wells, Laura Smalarz, & Simona Mackovichova 
# Title: Deviation from Perfect Performance Measures the Diagnostic Utility of Eyewitness Lineups but Partial Area Under the ROC Curve Does Not
# Status: Accepted
# Journal: Journal of Applied Research in Memory and Cognition
# Average DPP

library(tidyverse)
library(here)

colloff_data = read.csv("./Dataset testing and reports/DPP/ColloffWadeStrangeData.csv") %>% 
    filter(exclude == "no") %>% 
    select(treatmentLabel,
           targetLabel,
           confidenceRounded,
           face0,
           face1,
           face2,
           face3,
           face4,
           face5,
           faceSelected) %>% 
    mutate(perpetrator = ifelse(grepl("perpetrator", face0) | grepl("replication", face0), "face0div",
                                             ifelse(grepl("perpetrator", face1) | grepl("replication", face1), "face1div",
                                                          ifelse(grepl("perpetrator", face2) | grepl("replication", face2), "face2div",
                                                                       ifelse(grepl("perpetrator", face3) | grepl("replication", face3), "face3div",
                                                                                    ifelse(grepl("perpetrator", face4) | grepl("replication", face4), "face4div",
                                                                                           "face5div")))))) %>% 
    mutate(response_type = case_when(faceSelected == perpetrator & targetLabel == "present" ~ "suspect",
                                     faceSelected == perpetrator & targetLabel == "absent" & treatmentLabel == "nothing" ~ "suspect",
                                     faceSelected == perpetrator & targetLabel == "absent" & treatmentLabel != "nothing" ~ "filler",
                                     faceSelected != perpetrator & faceSelected != "notpresent" ~ "filler",
                                     faceSelected == "notpresent" ~ "reject"),
           cond = treatmentLabel)

colloff_props = colloff_data %>% 
    filter(!is.na(cond)) %>% 
    mutate(confidenceRounded = as.numeric(confidenceRounded)) %>% 
    group_by(targetLabel, cond, confidenceRounded, response_type) %>% 
    count() %>% 
    ungroup() %>% 
    group_by(cond, targetLabel) %>% 
    mutate(total = sum(n)) %>% 
    mutate(n_new = case_when(targetLabel == "absent" & response_type == "filler" ~ round(n/6, digits = 0),
                         TRUE ~ as.numeric(n)))

colloff_fair = colloff_props %>% 
    filter(cond == "concealment") %>% 
    arrange(cond, targetLabel, response_type, confidenceRounded)

colloff_unfair = colloff_props %>% 
    filter(cond == "nothing") %>% 
    arrange(cond, targetLabel, response_type, confidenceRounded)

colloff_fair_TP_filler = colloff_props %>% 
    filter(cond == "concealment" & targetLabel == "present" & response_type == "filler") %>% 
    summarize(total = sum(n))

colloff_fair_TP_miss = colloff_props %>% 
    filter(cond == "concealment" & targetLabel == "present" & response_type == "miss") %>% 
    summarize(total = sum(n))

colloff_fair_TA_rej = colloff_props %>% 
    filter(cond == "concealment" & targetLabel == "absent" & response_type == "correctRej") %>% 
    summarize(total = sum(n))

colloff_data_write = colloff_data %>% 
    filter(cond == "nothing" | cond == "concealment") %>% 
    mutate(id_type = response_type,
           culprit_present = targetLabel,
           conf_level = as.numeric(confidenceRounded)) %>% 
    select(id_type,
           conf_level,
           culprit_present,
           cond) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(id_type = ifelse(cond == "concealment" & id_type == "filler" & culprit_present == "absent",
                            sample(c("suspect", "filler"), 1, prob = c(1/6, 5/6)), id_type))
           

colloff_data_test = colloff_data_write %>% 
    filter(id_type != id_type2)

write.csv(colloff_data_write,
          "./Dataset testing and reports/DPP/ColloffWadeStrange_2016_processed.csv",
          row.names = FALSE,
          na = "")

#Load or enter in data
# In this case, manual entry of Colloff et al. Fair Lineup data
Culprit1 = rep(11:0,c(38,35,39,32,36,65,21,30,17,5,5,804)) ##n reponses in each of 12 conf bins - 0 is filler or rejection
Innocent1 = rep(11:0,c(4,5,10,12,10,27,8,10,8,3,4,1036))## n responses in each of 12 conf bins - 0 is anything other than a rejection
tpORta1 = rep(1:0,c(1127,1137)) # column vector of culprit status coded as 1s (culprit present) and 0s (culprit absent)
fair <- data.frame(tpORta1,c(Culprit1,Innocent1)) #creating data frame based on variables above
colnames(fair)<-c("tpORta","conf") #labelling variables in fair lineup data set

#Load or enter in data
# In this case, manual entry of Colloff et al. Biased Lineup data
Culprit2 = rep(11:0,c(182,84,96,59,45,111,16,19,11,5,1,481)) ##n reponses in each of 12 conf bins - 0 is total miss
Innocent2 = rep(11:0,c(82,40,39,35,36,77,18,19,10,6,2,653))## n responses in each of 12 conf bins - 0 is non-innocent total
tpORta2 = rep(1:0,c(1110,1017))  # column vector of culprit status coded as 1s (culprit present) and 0s (culprit absent)
biased <- data.frame(tpORta2,c(Culprit2,Innocent2)) #creating data frame based on variables above
colnames(biased)<-c("tpORta","conf") #labelling variables in biased lineup data set

#Combining all data into a single dataset
DPP_data <-rbind(fair,biased)
DPP_data$cond <-rep(1:0,c(2264,2127))
DPP_data$cond = as.factor(DPP_data$cond)
#adding variable coding as Fair (1) or Biased (0)
##--------Everything above is just creating a dataset based on two conditions from Colloff et al.

####---------below is Deviation from Perfect Performance Function which uses
### a nonparametric bootstrapping routine to build inferential confidence intervals
### around the DPP difference for two lineup procedures

#First, we need to bin our data so that it reflects the True and False positive rates from the ROC curve
library("plyr")#load in required package plyr
library("boot")#load in required package boot

#Below is the actual function that finds the difference between average DPPs
DPP_difference = function(data, indices) {
  d=data[indices,]
  
  ### Get confidence levels (MUST ENSURE THAT MINIMUM CONFIDENCE IN THE DATASET IS 1)
  conf_breaks = c(c(0, 0.5, 1), seq(from = 2, to = max(d$conf), by = 1))
  
  ###binning cond1 true positive proportions by cumulating confidence levels
  cond1_data<-subset(d,cond==levels(d$cond)[1])
  culprit1<-subset(cond1_data,tpORta==1)
  histinfo1<-hist(culprit1$conf,breaks=conf_breaks,plot=FALSE)->h1
  h1$percents<-cumsum(h1$counts)/sum(h1$counts)
  tps1 <- 1-h1$percents
  tps1=tps1[1:length(tps1)-1] ##drop last element of vector (it equals 0) to avoid bias in performance estimates
  
  ###binning cond1_data false positive proportions by cumulating confidence levels
  innocent1<-subset(cond1_data,tpORta==0)
  histinfo1i<-hist(innocent1$conf,breaks=conf_breaks,plot=FALSE)->h1i
  h1i$percents<-cumsum(h1i$counts)/sum(h1i$counts)
  fps1 <- 1-h1i$percents
  fps1=fps1[1:length(fps1)-1] ##drop last element of vector (it equals 0) to avoid bias in performance estimates
  
  ###compute DPP for cond1_data lineup - this is our measure of performance
  DPP1<-mean((1-tps1)+fps1)
  
  ###binning cond2_data true positive proportions by cumulating confidence levels
  cond2_data<-subset(d,cond==levels(d$cond)[2])
  culprit2<-subset(cond2_data,tpORta==1)
  histinfo2<-hist(culprit2$conf,breaks=conf_breaks,plot=FALSE)->h2
  h2$percents<-cumsum(h2$counts)/sum(h2$counts)
  tps2 <- 1-h2$percents
  tps2=tps2[1:length(tps2)-1] ##drop last element of vector (it equals 0) to avoid bias in performance estimates
  
  ###binning cond2_data false positive proportions by cumulating confidence levels
  innocent2<-subset(cond2_data,tpORta==0)
  histinfo2i<-hist(innocent2$conf,breaks=conf_breaks,plot=FALSE)->h2i
  h2i$percents<-cumsum(h2i$counts)/sum(h2i$counts)
  fps2 <- 1-h2i$percents
  fps2=fps2[1:length(fps2)-1] ##drop last element of vector (it equals 0) to avoid bias in performance estimates
  
  ###compute DPP for dataset 2 - this is our measure of performance
  DPP2<-mean((1-tps2)+fps2)
  
  ###compue DPP difference...positive values mean procedure 1 is worse (it has a higher dpp or less utility)
  diff<-DPP1-DPP2
  stats_DPP = c(DPP1,DPP2,diff)
  return(stats_DPP)
}

#print bootstrap results
results = boot(data = DPP_data, statistic = DPP_difference,R=10000)

results$t0[2]

print(results)
plot(results, index=3)

#generate 95% CIs Bias Corrected and Accelerated
confidence_interval_diff = boot.ci(results,index=3,conf=0.95,type='bca')
print(confidence_interval_diff)
ci_diff=confidence_interval_diff$bca[,c(4,5)]
print(ci_diff)
ci_diff[2]
# Figure out what data format my script generates ----

processed_data = read.csv("./Dataset testing and reports/DPP/colloff_2021_processed_med.csv") %>% 
    mutate(cond = "A") %>% 
    rbind(read.csv("./Dataset testing and reports/DPP/colloff_2021_processed_med.csv") %>% 
              mutate(cond = "B")) %>% 
    mutate(cond = as.factor(cond))

for (g in 1:length(parameters$effs)) {
    eff = .3
    
    data_original = processed_data %>%
        #mutate(conf_level = as.factor(conf_level)) %>%
        group_by(id_type, conf_level, culprit_present, cond) %>%
        count() %>%
        ungroup() %>%
        group_by(culprit_present, cond) %>%
        mutate(total = sum(n),
               prop = n / total) %>%
        ungroup() %>%
        mutate(conf_level_rev = (max(as.numeric(conf_level)) + 1) - as.numeric(conf_level)) %>%
        filter(id_type == "suspect")
    
    data = data_original
    
    ### Create the root data file to sample from across the Ns and sims ----
    for (z in 1:nrow(data)) {
        if (data$culprit_present[z] == "present" &
            data$cond[z] == levels(data$cond)[2]) {
            data$n[z] = round(data$n[z] * (eff + 1))
        } else {
            data$n[z] = data$n[z]
        }
    }
    
    data$prop = data$n / data$total
    
    ### Get the ROC data to sample from across Ns and sims ----
    #### For Condition 1 ----
    ##### TA ----
    TA_data_cond1_root = filter(data,
                                culprit_present == "absent" &
                                    cond == levels(data$cond)[1])
    
    # Empty the root vector
    #TA_ROC_cond1_root = vector()
    #
    #for (y in 1:nrow(TA_data_cond1)) {
    #  TA_ROC_cond1_root = c(TA_ROC_cond1_root, rep(TA_data_cond1$conf_level_rev[y], times = TA_data_cond1$n[y]))
    #}
    
    ##### TP ----
    TP_data_cond1_root = filter(data,
                                culprit_present == "present" &
                                    cond == levels(data$cond)[1])
    
    #### For Condition 2 ----
    ##### TA ----
    TA_data_cond2_root = filter(data,
                                culprit_present == "absent" &
                                    cond == levels(data$cond)[2])
    
    ##### TP ----
    TP_data_cond2_root = filter(data,
                                culprit_present == "present" &
                                    cond == levels(data$cond)[2])
    
    ### loop over Ns ####
    for (h in 1:length(parameters$ns)) {
        curr_n = 500
        #if (parameters$roc_paired == FALSE) {
        #    curr_n = parameters$ns[h]
        #} else {
        #    curr_n = parameters$ns[h] * 2
        #}
        
        #### Loop over sims ----
        for (i in 1:input$nsims) {
            ##### For each sim, sample ROC data from the root ----
            ###### For Condition 1 ----
            ####### TA ----
            TA_data_cond1 = sample(
                c(
                    as_vector(TA_data_cond1_root$conf_level),
                    0
                ),
                size = round((curr_n/2) * 1),
                replace = TRUE,
                prob = c(
                    as_vector(TA_data_cond1_root$prop),
                    1 - sum(TA_data_cond1_root$prop)
                )
            )
            
            TA_data_cond1 = TA_data_cond1[!is.na(TA_data_cond1)]
            
            cond1_partial = length(TA_data_cond1[TA_data_cond1 > 0]) / round((curr_n/2) * input$n_TA_lineups)
            
            ####### TP ----
            TP_data_cond1 = sample(
                c(
                    as_vector(TP_data_cond1_root$conf_level),
                    0
                ),
                size = round((curr_n/2) * 1),
                replace = TRUE,
                prob = c(
                    as_vector(TP_data_cond1_root$prop),
                    1 - sum(TP_data_cond1_root$prop)
                )
            )
            
            TP_data_cond1 = TP_data_cond1[!is.na(TP_data_cond1)]
            
            
            ###### For Condition 2 ----
            ####### TA ----
            TA_data_cond2 = sample(
                c(
                    as_vector(TA_data_cond2_root$conf_level),
                    0
                ),
                size = round((curr_n/2) * 1),
                replace = TRUE,
                prob = c(
                    as_vector(TA_data_cond2_root$prop),
                    1 - sum(TA_data_cond2_root$prop)
                )
            )
            
            TA_data_cond2 = TA_data_cond2[!is.na(TA_data_cond2)]
            
            cond2_partial = length(TA_data_cond2[TA_data_cond2 > 0]) / round((curr_n/2) * input$n_TA_lineups)
            
            ####### TP ----
            TP_data_cond2 = sample(
                c(
                    as_vector(TP_data_cond2_root$conf_level),
                    0
                ),
                size = round((curr_n/2) * 1),
                replace = TRUE,
                prob = c(
                    as_vector(TP_data_cond2_root$prop),
                    1 - sum(TP_data_cond2_root$prop)
                )
            )
            
            TP_data_cond2 = TP_data_cond2[!is.na(TP_data_cond2)]
            
            
            
            #### Generate data for DPP function ----
            TA_dataframe_cond1 = data.frame(conf = TA_data_cond1,
                                            tpORta = 0,
                                            cond = levels(data$cond)[1])
            
            TP_dataframe_cond1 = data.frame(conf = TP_data_cond1,
                                            tpORta = 1,
                                            cond = levels(data$cond)[1])
            
            TA_dataframe_cond2 = data.frame(conf = TA_data_cond2,
                                            tpORta = 0,
                                            cond = levels(data$cond)[2])
            
            TP_dataframe_cond2 = data.frame(conf = TP_data_cond2,
                                            tpORta = 1,
                                            cond = levels(data$cond)[2])
            
            data_DPP = rbind(TA_dataframe_cond1,
                             TP_dataframe_cond1,
                             TA_dataframe_cond2,
                             TP_dataframe_cond2)
        }
    }
}
