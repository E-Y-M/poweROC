# powe(R)OC: A power simulation tool for eyewitness lineup ROC analyses

![test](https://raw.githubusercontent.com/E-Y-M/poweROC/main/Dataset%20testing%20and%20reports/Full%20Simulation%20Tests/ROC%20curves%20tested.png?token=AOPCKSBPSQIVNKTK5ZG6NN3BLWDDS)

An R Shiny app to run power simulations for two-group between-subjects eyewitness lineup experiments where data are analyzed via ROC curves.

Run the app here: http://96.54.58.71:3838/poweROC-main/ROC_power_app/. Simulations can be time-consuming (and can be disrupted by internet timeouts), and the online version is currently hosted on a free version of Shiny server that limits the number of concurrent users (if you aren't able to load the link above, it is likely that the max # of users was reached). Thus, it is recommended that users with access to R/RStudio instead download and run a local copy of the app. The steps to do so are simple:
1. Click the "Code" button and download the repository as a .zip file
2. Extract the contents to a new folder
3. Open "poweROC.rproj"
4. Open the "R package installation" R script file and run it to install all the required packages
5. In RStudio, navigate to the "ROC_power_app" folder and open "app.R"
6. Click "Run App", then "Open in Browser"

You can also download app validation/testing results here: https://github.com/E-Y-M/poweROC/blob/main/Dataset%20testing%20and%20reports/powe(R)OC%20Testing%20Results.docx

This app is in the beta stage so feedback/comments are greatly appreciated (these can be posted in "Issues")



*ROC curves depicted in the example figure constructed using data from Colloff, M. F., Wilson, B. M., Seale-Carlisle, T. M., & Wixted, J. T. (2021). Optimizing the selection of fillers in police lineups. Proceedings of the National Academy of Sciences, 118(8), e2017292118. https://doi.org/10.1073/pnas.2017292118
