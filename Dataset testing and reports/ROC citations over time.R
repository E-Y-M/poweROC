library(psych)
library(tidyverse)
library(here)
library(DescTools)

setwd(here::here())

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

# Load in data ----
citation_data = read.csv("./Dataset testing and reports/ROC citations over time.csv",
                         fileEncoding = "UTF-8-BOM")

## Get proportions & CIs ----
for (i in 1:nrow(citation_data)) {
    citation_data$Proportion[i] = BinomCI(
        citation_data$Citations[i],
        citation_data$Total[i],
        conf.level = .95)[1]
    
    citation_data$lwr[i] = BinomCI(
        citation_data$Citations[i],
        citation_data$Total[i],
        conf.level = .95)[2]
    
    citation_data$upr[i] = BinomCI(
        citation_data$Citations[i],
        citation_data$Total[i],
        conf.level = .95)[3]
    
}

## Plot raw citations ----
citation_data %>% 
    ggplot(aes(x = Year, y = Citations))+
    geom_line(size = 1.5)+
    apatheme+
    theme(text = element_text(size = 30),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     vjust = 1),
          axis.title.y = element_text(size = 30,
                                      margin = margin(t = 0, r = 30, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 0, l = 0)))+
    scale_x_continuous(breaks = seq(min(citation_data$Year),
                                    max(citation_data$Year),
                                    by = 1))+
    labs(x = "Year",
         y = "Number of citations")

ggsave("./Dataset testing and reports/ROC_Citations_Raw.png",
       dpi = 300,
       height = 10,
       width = 14,
       units = "in")

## Plot proportion citations ----
citation_data %>% 
    ggplot(aes(x = Year, y = Proportion))+
    geom_line(size = 1.5)+
    geom_ribbon(aes(ymin = lwr, ymax = upr),
                alpha = .5)+
    apatheme+
    theme(text = element_text(size = 30),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     vjust = 1),
          axis.title.y = element_text(size = 30,
                                      margin = margin(t = 0, r = 30, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 0, l = 0)))+
    scale_x_continuous(breaks = seq(min(citation_data$Year),
                                    max(citation_data$Year),
                                    by = 1))+
    labs(x = "Year",
         y = "Proportion of citations")

ggsave("./Dataset testing and reports/ROC_Citations_Proportions.png",
       dpi = 300,
       height = 10,
       width = 14,
       units = "in")

# Example ROC curves ----
curve1 = data.frame(criteria = c(1:5),
                    CID = c(0, .10, .25, .35, .40),
                    FID = c(0, .01, .03, .06, .10),
                    cond = "Condition A")

curve2 = curve1 %>% 
    mutate(CID = CID * 1.5,
           cond = "Condition B")

curves = rbind(curve1,
               curve2)

curves %>% 
    ggplot(aes(x = FID, y = CID, group = cond, color = cond))+
    geom_point(size = 2)+
    geom_line(size = 1.5)+
    geom_abline(intercept = 0, slope = 1,
                linetype = "dashed",
                size = 1.5)+
    apatheme+
    labs(x = "False ID rate",
         y = "Correct ID rate")+
    theme(text = element_text(size = 30),
          axis.title.y = element_text(size = 30,
                                      margin = margin(t = 0, r = 30, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 30, r = 0, b = 0, l = 0)))+
    scale_color_discrete(name = "Condition")

ggsave("./Dataset testing and reports/ROC_Examples.png",
       dpi = 300,
       height = 10,
       width = 14,
       units = "in")
