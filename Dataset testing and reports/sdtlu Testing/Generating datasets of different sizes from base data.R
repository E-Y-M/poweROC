library(psych)
library(tidyverse)

datafile_names = c(#"Colloff (2021a) High- vs Low-similarity fillers",
                   "Kaesler (2020) Simultaneous vs Sequential",
                   "Palmer (2013) Long vs Short delay",
                   "Seale-Carlisle & Mickes (2016) UK vs US lineups")

datafile_selected = c(#"Colloff et al. (2021a): Exp 2: High- vs. Low-similarity fillers",
                      "Kaesler et al. (2020): Exp 1: Simultaneous vs. Sequential",
                      "Palmer et al. (2013): Exp 1: Long vs. short delay",
                      "Seale-Carlisle & Mickes (2015): US vs. UK lineups")

combined_data = read.csv("./ROC_power_app/www/combined_open_data.csv") %>% 
    filter(exp %in% datafile_selected)

datafiles = list.files("./Dataset testing and reports/sdtlu Testing/Datafiles to use/")

datafiles = datafiles[!grepl("colloff", datafiles)]

reps = c(1:4)
i = 1
for (i in 1:length(datafile_names)) {
    data = read.csv(paste0("./Dataset testing and reports/sdtlu Testing/Datafiles to use/", datafiles[i]))
    
    # Get the number of trials, and determine what 1/4 of the total trials is ----
    subset_trials = round(nrow(data)/4)
    
    for (h in 1:length(reps)) {
        data_sample = sample_n(data,
                               subset_trials,
                               replace = TRUE)
        
        write.csv(data_sample,
                  paste0("./Dataset testing and reports/sdtlu testing/Resampled data/", datafile_names[i], "_", reps[h], ".csv"),
                  row.names = FALSE,
                  na = "")
    }
}
