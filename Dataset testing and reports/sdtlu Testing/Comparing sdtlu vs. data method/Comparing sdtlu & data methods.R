library(psych)
library(tidyverse)

#APA theme for plots
apatheme <-
    theme_bw()+                                      #apply ggplot2() black and white theme
    theme(panel.grid.major = element_blank(),        #eliminate major grid lines
          panel.grid.minor = element_blank(),        #eliminate minor grid lines
          panel.background = element_blank(),        #eliminate the square panel background
          panel.border = element_blank(),            #eliminate the square panel border
          text=element_text(family="Arial"),         #use arial font
          #legend.title=element_blank(),              #eliminate lengend title
          legend.position= "right",                  #position legend to the right of the plot
          axis.line.x = element_line(color="black"), #include a black border along the x-axis
          axis.line.y = element_line(color="black")) #include a black border along the y-axis

# "Not in" function
`%nin%` = Negate(`%in%`)

# Names of data files to loop over ----
simulated_datafiles = list.files("./Dataset testing and reports/sdtlu Testing/Simulated data")
datafile_names = str_replace(simulated_datafiles, ".csv", "")

data_method_raw = list.files("./Dataset testing and reports/sdtlu Testing/Comparing sdtlu vs. data method/Data resampling method")
data_method_sdtlu = list.files("./Dataset testing and reports/sdtlu Testing/Comparing sdtlu vs. data method/sdtlu method")

data_method_raw = data_method_raw[!grepl("html", data_method_raw)]
data_method_sdtlu = data_method_sdtlu[!grepl("html", data_method_sdtlu)]

# Loop over datasets ----
a = 1
for (a in 1:length(datafile_names)) {
        # Get original data and ROC values ----
    
    data_original = read.csv(paste0("./Dataset testing and reports/sdtlu Testing/Simulated data/", simulated_datafiles[a])) 
    
    conds = unique(data_original$cond)
    
    data_original = data_original %>% 
        mutate(conf_level = ifelse(id_type == "filler" | id_type == "reject", 0, conf_level),
               cond = ifelse(cond == conds[1], "A", "B"),
               culprit_present = ifelse(culprit_present == "absent", "TA", "TP")) %>% 
        group_by(cond, culprit_present, conf_level) %>% 
        count() %>% 
        ungroup() %>% 
        group_by(cond, culprit_present) %>% 
        mutate(total = sum(n),
               prop = n / total,
               sim = paste0(cond, culprit_present))
    
    conf_levels = unique(data_original$conf_level)
    
    roc_data_store_original = tibble()
    
    i = 1
    for (i in 1:length(unique(data_original$sim))) {
        curr_data = filter(data_original, sim == unique(data_original$sim)[i]) %>% 
            mutate(method = "original",
                   sim_method = "original")
        
        # Check if any confidence levels are missing ----
        missing_confs = conf_levels[conf_levels %nin% curr_data$conf_level]
        
        ## If confidence levels are missing, add them in with a 0 count ----
        if (length(missing_confs) > 0) {
            missing_confs_data = data.frame(conf_level = missing_confs,
                                            method = "original",
                                            sim = curr_data$sim[1],
                                            cond = curr_data$cond[1],
                                            culprit_present = curr_data$culprit_present[1],
                                            n = 0,
                                            sim_method = "original")
            
            curr_data = rbind(curr_data,
                              missing_confs_data)
        } else {
            curr_data = curr_data
        }
        
        curr_data = rbind(curr_data,
                          data.frame(conf_level = max(conf_levels)+1,
                                     method = "original",
                                     sim_method = "original",
                                     sim = curr_data$sim[1],
                                     cond = curr_data$cond[1],
                                     culprit_present = curr_data$culprit_present[1],
                                     n = 0))
        
        curr_data = curr_data %>% 
            ungroup() %>% 
            mutate(total = sum(n),
                   prop = n / total,
                   sim = 0) %>% 
            filter(conf_level != 0) %>% 
            arrange(conf_level)
        
        for (j in 1:nrow(curr_data)) {
            curr_data$cumulative[j] = sum(curr_data$prop[j:nrow(curr_data)])
        }
        
        # Bind current data back into the main dataframe ----
        roc_data_store_original = rbind(roc_data_store_original,
                                        curr_data)
    }
    
    # Read in both data files and bind them together ----
    data = read.csv(paste0("./Dataset testing and reports/sdtlu Testing/Comparing sdtlu vs. data method/Data resampling method/", data_method_raw[a])) %>% 
        mutate(method = "data") %>% 
        rbind(read.csv(paste0("./Dataset testing and reports/sdtlu Testing/Comparing sdtlu vs. data method/sdtlu method/", data_method_sdtlu[a])) %>% 
                  mutate(method = "sdtlu"))
    
    # First, look at sample sizes to make sure things are working correctly ----
    sample_sizes = data %>% 
        group_by(method, sim, cond, culprit_present) %>% 
        summarize(n = length(conf_level))
    
    # Generate the ROC curves ----
    data_ROC = data %>% 
        group_by(method, sim, cond, culprit_present, conf_level) %>% 
        count() %>% 
        mutate(sim_method = paste0(sim, method, cond, culprit_present))
    
    ## Add missing confidence levels ----
    sims = unique(data_ROC$sim_method)
    conf_levels = unique(data_ROC$conf_level)
    
    roc_data_store = tibble()
    i = 1
    for (i in 1:length(sims)) {
        curr_data = filter(data_ROC, sim_method == sims[i])
        
        # Check if any confidence levels are missing ----
        missing_confs = conf_levels[conf_levels %nin% curr_data$conf_level]
        
        ## If confidence levels are missing, add them in with a 0 count ----
        if (length(missing_confs) > 0) {
            missing_confs_data = data.frame(conf_level = missing_confs,
                                            method = curr_data$method[1],
                                            sim = curr_data$sim[1],
                                            cond = curr_data$cond[1],
                                            culprit_present = curr_data$culprit_present[1],
                                            n = 0,
                                            sim_method = curr_data$sim_method[1])
            
            curr_data = rbind(curr_data,
                              missing_confs_data)
        } else {
            curr_data = curr_data
        }
        
        curr_data = rbind(curr_data,
                          data.frame(conf_level = max(conf_levels)+1,
                                     method = curr_data$method[1],
                                     sim = curr_data$sim[1],
                                     cond = curr_data$cond[1],
                                     culprit_present = curr_data$culprit_present[1],
                                     n = 0,
                                     sim_method = curr_data$sim_method[1]))
        
        curr_data = curr_data %>% 
            ungroup() %>% 
            mutate(total = sum(n),
                   prop = n / total) %>% 
            filter(conf_level != 0) %>% 
            arrange(conf_level)
        
        for (j in 1:nrow(curr_data)) {
            curr_data$cumulative[j] = sum(curr_data$prop[j:nrow(curr_data)])
        }
        
        # Bind current data back into the main dataframe ----
        roc_data_store = rbind(roc_data_store,
                               curr_data)
    }
    
    roc_data_plotting = roc_data_store %>% 
        rbind(roc_data_store_original %>% 
                  mutate(method = "data")) %>% 
        rbind(roc_data_store_original %>% 
                  mutate(method = "sdtlu")) %>% 
        ungroup() %>% 
        pivot_wider(names_from = culprit_present,
                    values_from = cumulative,
                    id_cols = c("sim", "method", "conf_level", "cond")) %>% 
        unnest(cols = everything()) %>% 
        mutate(sim = as.factor(sim),
               conf_level = as.numeric(conf_level)) %>% 
        mutate(color = ifelse(sim == 0, "Original", cond),
               method = ifelse(method == "data", "Data resampling", "sdtlu"))
    
    roc_data_plotting %>%
        arrange(sim, method, desc(conf_level)) %>% 
        ggplot(aes(x = TA, y = TP, group = interaction(cond, sim), color = color, alpha = color, size = color))+
        scale_color_manual(values = c("red", "blue", "black"))+
        scale_alpha_manual(values = c(.3, .3, 1))+
        scale_size_manual(values = c(1, 1, 2))+
        facet_grid(rows = vars(method))+
        geom_point()+
        geom_line()+
        labs(x = "False ID rate",
             y = "Correct ID rate",
             color = "Condition")+
        guides(alpha = "none",
               size = "none")+
        apatheme+
        ggtitle(datafile_names[a])+
        theme(text = element_text(size = 25),
              plot.title = element_text(hjust = .5))
    
    ggsave(paste0("./Dataset testing and reports/sdtlu Testing/Comparing sdtlu vs. data method/Figures/", datafile_names[a], ".png"),
           dpi = 300,
           height = 20, 
           width = 15,
           units = "in")
}

