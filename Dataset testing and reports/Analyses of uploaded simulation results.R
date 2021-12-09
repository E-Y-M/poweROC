library(psych)
library(tidyverse)
library(here)

setwd(here::here())

# Read in data ----
data = read.csv("./Dataset testing and reports/Previous simulation data/powe(R)OC Compendium.csv",
                fileEncoding = "UTF-8-BOM")

# Look at simulation time ----
time_data = data %>% 
    select(1, 21) %>% 
    distinct() %>% 
