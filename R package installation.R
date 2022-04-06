# Run this script to install all the required packages to run a local copy of powe(R)OC
install.packages("pacman")

library(pacman)

pacman::p_load(
    shiny,
    shinyjs,
    shinydashboard,
    tidyverse,
    readr,
    pROC,
    shinyBS,
    googlesheets4,
    boot,
    sdtlu,
    bayestestR
)
