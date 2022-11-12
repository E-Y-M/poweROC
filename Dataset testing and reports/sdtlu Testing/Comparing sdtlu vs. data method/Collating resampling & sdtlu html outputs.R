library(psych)
library(tidyverse)
library(rvest)

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

# Resampling method ----
resamplingFolder = "./Dataset testing and reports/sdtlu Testing/Comparing sdtlu vs. data method/Data resampling method/New sims/"
resamplingFileList = list.files(resamplingFolder)

pwr_table_store = data.frame()

for (i in 1:length(resamplingFileList)) {
    curr_name = gsub("_trials_.*", "", resamplingFileList[i])
    
    curr_author_year = gsub("_.*", "", curr_name)
    
    curr_samplesize = gsub(".*_", "", curr_name)
    
    curr_file = xml2::read_html(paste0(resamplingFolder, resamplingFileList[i]))
    
    pwr_table = html_table(curr_file, fill = TRUE)[[2]] %>% 
        mutate(exp = curr_author_year,
               base_sample_size = curr_samplesize,
               method = "Resampling")
    
    pwr_table_store = rbind(pwr_table_store,
                            pwr_table)
}

# sdtlu method ----
sdtluFolder = "./Dataset testing and reports/sdtlu Testing/Comparing sdtlu vs. data method/sdtlu method/New sims/"
sdtluFileList = list.files(sdtluFolder)

for (i in 1:length(sdtluFileList)) {
    curr_name = gsub("_trials_.*", "", sdtluFileList[i])
    
    curr_author_year = gsub("_.*", "", curr_name)
    
    curr_samplesize = gsub(".*_", "", curr_name)
    
    curr_file = xml2::read_html(paste0(sdtluFolder, sdtluFileList[i]))
    
    pwr_table = html_table(curr_file, fill = TRUE)[[2]] %>% 
        mutate(exp = curr_author_year,
               base_sample_size = curr_samplesize,
               method = "sdtlu")
    
    pwr_table_store = rbind(pwr_table_store,
                            pwr_table)
}

pwr_table_store = pwr_table_store %>% 
    mutate(base_sample_size = as.numeric(base_sample_size)*2) %>% 
    mutate(base_sample_size = as.factor(as.numeric(base_sample_size)),
           exp = factor(exp,
                        levels = c("Colloff 2021",
                                   "Palmer 2013",
                                   "Seale-Carlisle 2016",
                                   "Kaesler 2020")))

pwr_methods_plot = pwr_table_store %>% 
    ggplot(aes(x = N, y = `Power (pAUC)`, linetype = method, color = base_sample_size))+
    facet_grid(~exp)+
    geom_line(size = 1.5)+
    apatheme+
    theme(text = element_text(size = 25))+
    labs(color = "Base sample size",
         linetype = "Method")

pwr_methods_plot

ggsave("./Dataset testing and reports/sdtlu Testing/Comparing sdtlu vs. data method/Figures/sdtlu_resampling_power.png",
       width = 20,
       height = 10,
       dpi = 300,
       units = "in")
