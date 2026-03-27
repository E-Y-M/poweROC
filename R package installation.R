# Run this script to install all the required packages to run a local copy of powe(R)OC
install.packages("pacman")
install.packages("renv")

library(pacman)
library(renv)

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
    BiocManager,
    optimx,
    bayestestR
)

renv::restore()

install.packages("sdtlu_0.0.0.90100.tar.gz", repos=NULL)

# powe(R)OC also uses the 'sdtlu' package (Cohen et al., 2020). To install that package, see the instructions at https://osf.io/mfk4e/wiki?wiki=unvk5
