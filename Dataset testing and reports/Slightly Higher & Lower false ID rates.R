library(psych)
library(tidyverse)
library(pROC)
library(lubridate)
library(here)

source("ROC_power_app/scripts/func.R") # helper functions

setwd(here::here())

# Dataset with slightly higher (1.25x) false ID rates at each level of confidence, all else constant ----
mods = c(2/3, .8, 1.25, 1.5)
titles = c("66", "80", "125", "150")

for (i in 1:length(mods)) {
    curr_mod = mods[i]
    curr_title = titles[i]
    
    data = read.csv(here("./Dataset testing and reports/Data/Colloff et al. 2021a/Exp 1/Null Effect Testing/colloff_2021_processed_med.csv"), fileEncoding = "UTF-8-BOM") %>% 
        group_by(culprit_present, id_type, conf_level) %>% 
        count() %>% 
        ungroup() %>% 
        group_by(culprit_present) %>% 
        mutate(total = sum(n),
               n_mod = ifelse(culprit_present == "absent" & id_type == "suspect", round(n * curr_mod), 
                              ifelse(culprit_present == "absent" & id_type == "reject", round(n * 1), n)),
               sum_check = sum(n_mod)) %>% 
        filter(id_type == "suspect")
    
    combined_data = data.frame()
    
    for (j in 1:nrow(data)) {
        curr_row = data[j,]
        
        combined_data = combined_data %>% 
            bind_rows(replicate(data$n_mod[j], curr_row, simplify = FALSE))
    }
    
    n_culprits = combined_data %>% 
        filter(culprit_present == "present") %>% 
        nrow() %>% 
        as.numeric()
    
    n_suspects = combined_data %>% 
        filter(culprit_present == "absent") %>% 
        nrow() %>% 
        as.numeric()
    
    fake_tp_data = data %>% 
        filter(culprit_present == "present") %>% 
        slice(1) %>% 
        mutate(id_type = "reject") %>% 
        as.data.frame()
    
    fake_ta_data = data %>% 
        filter(culprit_present == "absent") %>% 
        slice(1) %>% 
        mutate(id_type = "reject") %>% 
        as.data.frame()
    
    append_tp = fake_tp_data$total[1] - n_culprits
    
    append_ta = fake_ta_data$total[1] - n_suspects
    
    final_data = combined_data %>% 
        bind_rows(replicate(append_tp, fake_tp_data, simplify = FALSE)) %>%
        bind_rows(replicate(append_ta, fake_ta_data, simplify = FALSE))
        
    write.csv(final_data, 
              sprintf("./Dataset testing and reports/Effects of differential partial truncation on power/colloff_2021_%s_FID.csv",
                      curr_title),
              row.names = FALSE,
              na = "")
}

sprintf("./Dataset testing and reports/Effects of differential truncation on power/colloff_2021_%s_FID.csv",
        curr_title)

1/1.5
10*1.25
12.5*.8
