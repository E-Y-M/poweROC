library(psych)
library(tidyverse)

# Read in both data files and bind them together ----
data = read.csv("./Dataset testing and reports/sdtlu testing/Comparing sdtlu vs. data method/raw_sim_results_data.csv") %>% 
    mutate(method = "data") %>% 
    rbind(read.csv("./Dataset testing and reports/sdtlu testing/Comparing sdtlu vs. data method/raw_sim_results_sdtlu.csv") %>% 
              mutate(method = "sdtlu"))

# First, look at sample sizes to make sure things are working correctly ----
sample_sizes = data %>% 
    group_by(method, sim, cond, culprit_present) %>% 
    summarize(n = length(conf_level))
