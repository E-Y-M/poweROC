library(psych)
library(tidyverse)

`%nin%` = negate(`%in%`)

# Set the data folder for all the processed open datafiles ----
dataFolder = "./Dataset testing and reports/Data/01 - All data files for combining/"
fileList = list.files(dataFolder)

all_exp_names = data.frame()

## Loop to get all experiment names to get lineup sizes ----
for (i in 1:length(fileList)) {
    curr_dataset = read.csv(paste0(dataFolder, fileList[i])) 
    
    if (!c("lineup_size") %in% colnames(curr_dataset)) {
        curr_dataset = curr_dataset %>% 
            select(exp, cond) %>% 
            distinct() %>% 
            mutate(lineup_size = NA)
    } else {
        curr_dataset = curr_dataset %>% 
            select(exp, cond, lineup_size) %>% 
            distinct()
    }
    
    all_exp_names = rbind(all_exp_names,
                          curr_dataset)
}

## Read in alread-entered lineup sizes ----
lineup_sizes_old = read.csv("./Dataset testing and reports/Data/Lineup sizes.csv",
                            fileEncoding = "UTF-8-BOM")

all_exp_names_coding = all_exp_names %>% 
    filter(!is.na(exp) & exp %nin% lineup_sizes_old$exp & is.na(lineup_size))

write.csv(all_exp_names_coding,
          "./Dataset testing and reports/Data/Lineup sizes (for entering).csv",
          row.names = FALSE,
          na = "")

lineup_sizes_entered = read.csv("./Dataset testing and reports/Data/Lineup sizes (entered).csv")

lineup_sizes_combined = rbind(lineup_sizes_old,
                              lineup_sizes_entered,
                              all_exp_names) %>% 
    distinct() %>% 
    filter(!is.na(lineup_size))

## Read in entered lineup sizes 

## Loop to add them to a single combined datafile ----
combined_open_data = data.frame()

for (i in 1:length(fileList)) {
    curr_dataset = read.csv(paste0(dataFolder, fileList[i])) %>% 
        mutate(cond = as.factor(cond))
    
    if (!c("suspect_position") %in% colnames(curr_dataset)) {
        curr_dataset = curr_dataset %>% 
            mutate(suspect_position = NA)
    } else {
        curr_dataset = curr_dataset
    }
    
    curr_dataset = curr_dataset %>% 
        select(id_type, conf_level, culprit_present, cond, suspect_position, exp) %>% 
        left_join(lineup_sizes_combined)
    
    combined_open_data = rbind(combined_open_data,
                               curr_dataset)
}

write.csv(combined_open_data,
          "./Dataset testing and reports/Data/combined_open_data.csv",
          row.names = FALSE,
          na = "")
