# setup ----
## libraries ----
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
    library(tidyverse)
    library(readr)
    library(pROC)
    library(shinyBS)
    library(googlesheets4)
    library(boot)
    library(sdtlu)
    library(bayestestR)
})

## functions ----
source("scripts/func.R") # helper functions
source("scripts/gs4.R") # google sheets functions

## Google Sheets setup ----
#setwd("./ROC_power_app")
#gs4_deauth()
#gs4_auth(cache = ".secrets", email = "eric7mah@gmail.com")
#google_sheet_id = "12hwA8QHK7D_kgo-OJjGv9xoasgZ2ksChkPtW875aOQQ"

## read in necessary data files ----
example_data = data.frame(id_type = sample(c("suspect", "filler", "reject"),
                                           100,
                                           replace = TRUE,
                                           prob = c(1/3, 1/3, 1/3)),
                          conf_level = round(runif(100, min = 1, max = 5)),
                          culprit_present = sample(c("present", "absent"),
                                                   100,
                                                   replace = TRUE,
                                                   prob = c(.5, .5)),
                          cond = sample(c("A", "B"),
                                        100,
                                        replace = TRUE,
                                        prob = c(.5, .5))) %>% 
    mutate(lineup_size = ifelse(cond == "A", 6, 8))

### Compendium of open datasets ----
open_data = read.csv("www/combined_open_data.csv", fileEncoding = 'UTF-8-BOM') #%>% 
    #filter(exp != "Colloff et al. (2021b): Exp 2: High vs. Low pose reinstatement") # Filter out this dataset for now until I figure out problems

### AUC effect sizes ----
effect_sizes = read.csv("www/auc_ratios.csv", fileEncoding = 'UTF-8-BOM') %>% 
    rowwise() %>% 
    mutate(ratio = max(auc1, auc2) / min(auc1, auc2) - 1) %>% 
    dplyr::select(ratio, dval, pval, exp) %>% 
    dplyr::rename(`Effect size` = "ratio",
                  D = dval,
                  p = pval,
                  Experiment = exp)

#### ...and the corresponding plot ----
##### Get 33rd, 66th, and 99th quantiles ----
auc_lit_quantiles = quantile(effect_sizes$`Effect size`,
                             probs = c(.33, .66))

auc_small_x = (auc_lit_quantiles[1]) / 2
auc_med_x = (auc_lit_quantiles[1] + auc_lit_quantiles[2]) / 2
auc_large_x = (auc_lit_quantiles[2] + max(effect_sizes$`Effect size`)) / 2
auc_y = max(density(effect_sizes$`Effect size`)[["y"]]) + .20*(max(density(effect_sizes$`Effect size`)[["y"]]))

##### Plot ----
auc_lit_plot = effect_sizes %>% 
    ggplot(aes(x = `Effect size`))+
    geom_density()+
    geom_vline(xintercept = auc_lit_quantiles)+
    apatheme+
    labs(x = "Effect size",
         y = "Density")+
    annotate("text", x = auc_small_x, y = auc_y, label = "Small", vjust = .5, size = 11)+
    annotate("text", x = auc_med_x, y = auc_y, label = "Medium", vjust = .5, size = 11)+
    annotate("text", x = auc_large_x, y = auc_y, label = "Large", vjust = .5, size = 11)+
    theme(text = element_text(size = 20))+
    scale_x_continuous(breaks = seq(0, round(max(effect_sizes$`Effect size`), 1), by = .1))

# user interface ----
shinyjs::useShinyjs()

## tabs ----
### introduction ----
intro_tab <- tabItem(
    tabName = "intro_tab",
    box(width = 12,
        collapsible = FALSE,
        title = "Introduction",
        tags$p("This R Shiny app allows users to run simulation-based power analysis for ROC curve (partial AUC and deviation from perfect performance/DPP) analysis of eyewitness data (and certain recognition memory designs). 
               Use the tabs on the left to navigate. I strongly recommend checking out the 'How this app works' tab before proceeding."),
        #actionLink("explanation_tab_link", "How this app works"),
        #tags$br(),
        #actionLink("previous_tab_link", "View results of previous simulations that other users have uploaded"),
        #tags$br(),
        #actionLink("data_tab_link", "Upload data or choose from open datasets to use as the basis for simulation"),
        #tags$br(),
        #actionLink("effects_tab_link", "View effect sizes from other eyewitness lineup ROC experiments"),
        #tags$br(),
        #actionLink("parameters_tab_link", "Enter simulation parameters and run the analysis"),
        #tags$br(),
        #actionLink("results_tab_link", "View simulation analysis results"),
        #tags$br(),
        #actionLink("validation_tab_link", "View a summary of app validation/testing"),
        #tags$br(),
        tags$br(),
        tags$p("This program is licensed under the ", a(href = 'https://github.com/E-Y-M/poweROC/blob/main/LICENSE', 'GNU General Public License v3.0. ', .noWS = "outside"), 
               'Complete source code for this app can be downloaded from GitHub at ', a(href = 'https://github.com/E-Y-M/poweROC', 'https://github.com/E-Y-M/poweROC', .noWS = "outside"), ', and any issues can be reported at ', a(href = 'https://github.com/E-Y-M/poweROC/issues', 'https://github.com/E-Y-M/poweROC/issues', .noWS = "outside"), ". Preliminary app testing/validation results and best-use recommendations can be viewed in the 'App validation & testing tab'.",
               .noWS = c("after-begin", "before-end")),
        tags$br(),
        tags$p("This is beta version 1.0 of an app developed by Eric Mah, a graduate student at the University of Victoria. Feedback/suggestions/bug reports are very much appreciated, and can be added on the linked issues page or addressed to ericmah@uvic.ca. 
               Thanks to Ryan Fitzgerald, Adam Krawitz, Farouk Nathoo, and Steve Lindsay for their helpful feedback and suggestions in developing this app.")
    )
)

### app explanation ----
explanation_tab <- tabItem(
    tabName = "explanation_tab",
    box(width = 12,
        collapsible = TRUE,
        title = "What is this?",
        tags$p('This app takes as input lineup data with either one condition or two between- or within-subjects conditions (either user-uploaded or selected from a selection of open datasets), and allows users to specify various 
               simulation/design parameters (sample sizes, effect sizes, alpha level, test tails, AUC specificity, # of TA/TP lineups per participant), 
               visualize hypothetical ROC curves, simulate datasets by repeatedly sampling from the data under different conditions/effect sizes/sample sizes to provide power estimates, 
               download summary reports of power simulations, upload simulation results for other users, and view the results of previous simulations uploaded by other users (see the "Previous simulation results" tab).'),
        tags$p(strong("By using existing data as the basis for simulation, this app avoids assumptions about data-generating process that one would make using other methods (e.g., specifying an SDT model to simulate from). Of course, a critical assumption that this app does make is that the data one uses for simulation will resemble the data one plans to collect. To 
                      the extent that this assumption does not hold, the power simulation results may not be valid. Any power simulation enterprise requires certain assumptions 
                      (e.g., about underlying distributions); my aim with this app was to minimize the necessary assumptions and the amount of 
                      information required (e.g., underlying SDT parameters/processes) for power analyses.")), 
        tags$p("This app was inspired by both Boogert et al.`s (2021) ", a(href = 'https://lmickes.github.io/pyWitness/index.html', 'pyWitness', .noWS = "outside"), 
               " program and Cohen et al.`s (2021) ", a(href = "https://link.springer.com/article/10.3758%2Fs13428-020-01402-7", 'sdtlu', .noWS = 'outside'), " R package. 
               Both use pre-existing data and allow for in-depth simulation and analysis of various SDT models from eyewitness lineup data, but simulation for power is not their primary focus. 
               The goal of this app is to provide an interface for the kinds of ROC analyses commonly conducted in lineup experiments. 
               In this app, ROC curves are constructed and AUCs compared via boostrap analysis, using the ", a(href = 'https://cran.r-project.org/web/packages/pROC/pROC.pdf', 'pROC R package', .noWS = "outside"), 
               ". ROC curves are constructed using the proportion of correct/false IDs at each confidence level (i.e., filler IDs are not counted as false IDs unless recoded in the data file), as per Gronlund et al.`s (2014) ", a(href = 'http://mickeslab.com/handy/roc-tutorial/', 'ROC tutorial.', .noWS = "outside"),
               " Deviation from perfect performance is computed using the bootstrap method described in ", a(href = "https://doi.org/10.1016/j.jarmac.2018.09.003", 'Smith et al. (2019)', .noWS = 'outside'), ".",
               .noWS = c("after-begin", "before-end")),
        tags$br()
    ),
    box(width = 12,
        collapsible = TRUE,
        title = "How does it work?",
        tags$p('This app requires a data file containing lineup data (see the “Data Upload” tab for instructions). Users can either upload their own data or use an included open dataset (see the "App validation & testing" tab for references). This file can contain a single condition (e.g., pilot data) or data from two conditions (e.g., data from another experiment similar to the one being powered for). If the former, the app will automatically duplicate data from the single provided condition to use as a basis for effect size adjustment and comparison. Before simulating data, various parameters will need to be specified (e.g., effect/sample sizes to test, number of simulation samples, one- or two-tailed testing protocol, etc.). The simulations themselves operate like so:'),
        img(src = "SimulationDescription.png", align = "center"),
        #tags$ol(
        #    tags$li("For each specified effect size:"),
        #    tags$li("   Apply that effect size to the # of correct IDs for the 2nd condition in the data file", style="white-space: pre-wrap"),
        #    tags$li("	Calculate the new proportion of correct IDs at each confidence level", style="white-space: pre-wrap"),
        #    tags$li("	For each specified sample size:", style="white-space: pre-wrap"),
        #    tags$li("	  For each simulation sample:", style="white-space: pre-wrap"),
        #    tags$li("		 Sample lineup outcomes/confidence according to the newly defined proportions", style="white-space: pre-wrap"),
        #    tags$li("		 Compute ROC curves for each condition and compare partial AUCs via boostrap analysis, using the ", a(href = 'https://cran.r-project.org/web/packages/pROC/pROC.pdf', 'pROC R package', .noWS = "outside"), ". ROC curves are constructed using the proportion of correct/false IDs at each confidence level (i.e., filler IDs are not counted as false IDs unless recoded in the data file), as per Gronlund et al.`s (2014) ", a(href = 'http://mickeslab.com/handy/roc-tutorial/', 'ROC tutorial', .noWS = "outside"), style="white-space: pre-wrap", .noWS = c("after-begin", "before-end")),
        #    tags$li("		 Record test significance", style="white-space: pre-wrap"),
        #    tags$li("Record proportion of significant tests at each effect size/N", style="white-space: pre-wrap")),
        #tags$br(),
        tags$p(strong('NOTE:'), ' Due to the computationally intensive bootstrap resampling involved in ROC analyses, some simulations can potentially take a long time (e.g., upwards of an hour if several sample/effect sizes are under consideration). Thus, users with access to R/RStudio may want to ', a(href = 'https://github.com/E-Y-M/poweROC', 'download and run a local copy', .noWS = "outside"), " to avoid simulation disruption with dropped internet connections or timeouts. Whether running the web or a local version, it is also recommended that hibernation settings be temporarily disabled.", .noWS = c("after-begin", "before-end"))
    )
)

### data upload ----
data_tab <- tabItem(
    tabName = "data_tab",
    box(
        width = 12,
        collapsible = TRUE,
        title = "Instructions",
        tags$p(
            'Upload your data (.csv format) here, or choose one of the open datasets included with this app. Data files uploaded are NOT saved to the server and are only used for a given session. Data must be formatted so that each row represents a single lineup decision by a single participant. Data files must contain the following (case-sensitive) columns:'
        ),
        tags$ul(
            tags$li(
                strong('id_type'),
                ': The lineup decision, one of “suspect”, “filler”, or “reject”. If your data does not contain a designated innocent suspect, you will need to generate false IDs (using lineup size) via the option that will appear after uploading the data.'
            ),
            tags$li(
                strong('conf_level'),
                ': The confidence rating for the decision, where lower values represent lower confidence. Must be numeric, and if not already binned (e.g., a 1-100 continuous scale), should be binned as desired prior to uploading'
            ),
            tags$li(
                strong('culprit_present'),
                ': Whether or not the lineup was culprit present/absent, one of “present” or “absent”'
            ),
            tags$li(
                strong('cond'),
                ': The between- or within-subjects condition for that participant/lineup (e.g., Simultaneous vs. Sequential). Only necessary to include if you have data with two pre-existing conditions (which is recommended), otherwise the variable will be created and populated automatically. Note that the condition that comes 2nd alphabetically will be the one that effect sizes are applied to.'
            ),
            tags$li(
                strong('lineup_size'),
                ': The number of individuals in the lineup.'
            )
        ),
        tags$p("And if your data contain sequential lineups:"),
        tags$ul(
            tags$li(
                strong('suspect_position'),
                ': The position of the suspect in the lineup.'
            )
        ),
        tags$br(),
        tags$p(strong('NOTE:'), ' While this app is primarily designed for lineup data, it can also be used for general recognition memory experiments where participants study items then are tested for recognition of old/new items. In this case, each row should be a recognition trial with hits and false alarms recoded to "suspect" for id_type (rejections recoded to "reject"), and culprit_present denoting old/new status.'),
        tags$p('See the example data file below for proper formatting'),
        radioButtons(
            "data_source",
            "Do you want to upload your own data or use one of the open data files?",
            choices = c("Upload data",
                        "Use open data"),
            selected = "Upload data"
        ),
        hidden(fileInput(
            "user_data",
            "Upload your data",
            multiple = FALSE,
            accept = ".csv"
        )),
        hidden(selectInput(
               "open_dataset",
               "Choose the open dataset to use",
               choices = unique(open_data$exp),
               selected = NULL
        )),
        tags$p(strong("Once you have uploaded or chosen a dataset and specified whether your data has a designated innocent suspect, click `Check data` to check your data and generate ROC curves")),
        radioButtons(
            "designated_suspect",
            "Does your data contain a designated innocent suspect? (Automatically selected for open datasets)",
            choices = c("Yes",
                        "No"),
            selected = "Yes"
        ),
        bsTooltip("designated_suspect",
                  "If your data does not contain a designated innocent suspect, the app will automatically convert culprit-absent filler IDs to suspect IDs with a probability of 1/lineup size",
                  placement = "left",
                  trigger = "hover"),
        htmlOutput("cond1_lineup_text"),
        hidden(numericInput(
            "lineup_size_1",
            "",
            value = NA,
            min = 1
        )),
        htmlOutput("cond2_lineup_text"),
        hidden(numericInput(
            "lineup_size_2",
            "",
            value = NA,
            min = 1
        )),
        actionButton(
            "check_data",
            "Check data and generate hypothetical ROC curves",
            width = '100%',
            class = "btn-info"
        ),
        bsTooltip(
            "check_data",
            "Check whether the uploaded data is the correct format (and if so, generate hypothetical ROC curves).",
            placement = "bottom",
            trigger = "hover"
        )
    ),
    box(width = 12,
        collapsible = TRUE,
        title = "ROC Curves",
        plotOutput("hypothetical_ROC_plot")),
        #textOutput("auc_diff_text")),
    box(width = 6,
        collapsible = TRUE,
        title = "Example data",
        dataTableOutput("example_data")),
    box(width = 6,
        collapsible = TRUE,
        title = "Your data",
        div(style = 'overflow-x: scroll',dataTableOutput("processed_data")))
)

### sdtlu tab ----
sdtlu_tab = tabItem(tabName = "sdtlu_tab",
                    box(width = 12,
                        title = "SDT parameter estimates from data",
                        tags$p("If you wish to generate data for simulation from an SDT model, you may want to get an idea of what parameter values to use, either from your own data or from one of the available open datasets. This tab allows you to estimate SDT parameters from an uploaded dataset using functions from Cohen et al.`s (2021) ", a(href = "https://link.springer.com/article/10.3758%2Fs13428-020-01402-7", 'sdtlu', .noWS = 'outside'), " R package."),
                        tags$br(),
                        tags$p("To estimate parameters, fill in the following information about your dataset below and click `Get SDT parameter estimates`. Once estimation is complete, this page will display estimated parameter values, and several sdtlu plots, including: model-implied SDT distributions and model-predicted vs. data ROCs and response frequencies.")),
                    box(width = 6,
                            textOutput("cond1_label"),
                            radioButtons(
                                "simultaneous_sequential_sim_a",
                                "Simultaneous or Sequential lineup?",
                                choices = c("Simultaneous",
                                            "Sequential"),
                                selected = "Simultaneous"
                            ),
                            numericInput("lineup_size_sim_a",
                                         "Lineup size?",
                                         value = 6,
                                         min = 1
                            )),
                    box(width = 6,
                        textOutput("cond2_label"),
                        radioButtons(
                            "simultaneous_sequential_sim_b",
                            "Simultaneous or Sequential lineup?",
                            choices = c("Simultaneous",
                                        "Sequential"),
                            selected = "Simultaneous"
                        ),
                        numericInput("lineup_size_sim_b",
                                     "Lineup size?",
                                     value = 6,
                                     min = 1
                        )),
                    actionButton(
                        "get_sdtlu",
                        "Get SDT parameter estimates",
                        width = '100%',
                        class = "btn-info"
                    ),
                    box(width = 12,
                        dataTableOutput("sdtlu_estimates")),
                    box(width = 12,
                        plotOutput("sdtlu_params_sim_plot")),
                    box(width = 12,
                        plotOutput("model_fit_data_plot")),
                    box(width = 12,
                        plotOutput("hypothetical_sdtlu_plot")))

### effect sizes tab ----
effects_tab = tabItem(tabName = "effects_tab",
                      box(width = 12,
                          title = "pAUC effect sizes in the literature",
                          tags$p("The effect size metric used by this app is an adjusted ratio of two pAUCs. For example, an effect size of 0 means that two ROC curves have the same pAUC, an effect size of 0.5 means that the 2nd ROC has 50% more pAUC than the 1st, an effect size of -0.5 means that the 2nd ROC has 50% less pAUC than the 1st, etc. Effect size adjustment in this app occurs by applying the effect size multiplier (e.g., +/- 50%) to the number of hits across all confidence levels, which has the effect of multiplying the pAUC by the same amount. This effect size is more amenable to simulation than D (as D relies on knowing the standard error of the to-be-simulated data), and perhaps more intuitive."),
                          tags$br(),
                          tags$b("An important caveat/caution regarding effect sizes: The above relies on the assumption that when two ROC curves differ, the difference will be constant across confidence levels/operating points. This assumption may not be valid (indeed, an examination of hit ratios across confidence levels in the open datasets revealed substantial variability in the differences). There may also be theoretical reasons to predict varying effects at different confidence levels (e.g., one condition increases hits, but only at higher confidence levels). As such, this app also allows the specification of separate effect sizes at each confidence level in the base data. In this case, the ratio effect size does not translate directly into a ratio of AUCs, and should be interpreted as applying to the # of hits at each confidence level. This trade-off in interpretability allows additional flexibility in specifying ROC curves."),
                          tags$br(),
                          tags$br(),
                          tags$p("How to decide what effect size(s) to simulate? The plot below shows a distribution of pAUC effect sizes (absolute value) from published eyewitness experiments that used ROC analysis (see the ‘App validation & testing’ tab for references). If you do not have a similar experiment in mind when choosing an effect size, I have delineated some crude conventions for ‘small’, ‘medium’, and ‘large’ effects based on the 33rd and 66th percentiles. However, because my literature review is by no means comprehensive, I instead recommend looking through the table below the figure for an experiment with a similar manipulation to the one you are powering for, and using effect sizes similar to that in the ‘Simulation Parameters’ tab."),
                          tags$br(),
                          tags$p("My recommendations for simulation re:effect sizes are as follows, in order from most to least ideal:"),
                          tags$ol(
                              tags$li("Using data from a pilot experiment with two conditions and a decent sample size"),
                              tags$li("Using data from a similar prior experiment with two conditions and a decent sample size"),
                              tags$li("Using single-condition data from a pilot or prior experiment and applying differing effect sizes to each confidence level (requires having a good idea of what the differing effect sizes will be)",
                              tags$li("Using single-condition data from a pilot or prior experiment and applying the same effect size to each confidence level (if you don't have a good idea of how effect sizes might differ across confidence levels)")
                          )
                      ))
                      ,
                      box(width = 12,
                          collapsible = TRUE,
                          plotOutput("auc_lit_plot"))
                      ,
                      box(width = 12, 
                          collapsible = TRUE,
                          dataTableOutput("effect_sizes")))
                      

### simulation parameters tab ----
parameters_tab = tabItem(tabName = "parameters_tab",
                         box(width = 12,
                             title = "Enter simulation parameters below. Mouse over each entry box for an explanation. Once you have entered all parameters, press the `Check parameters and generate hypothetical ROC curves` button to check whether the entered parameters are valid and view 
                             hypothetical ROC curves (Note that you will need to do this any time you change a parameter value). If the check succeeds, a `Simulate` button will appear.",
                             radioButtons("empirical_theoretical",
                                          "Use uploaded data or simulate from SDT parameters?",
                                          choices = c("Data",
                                                      "SDT"),
                                          selected = "Data")),
                             #tags$p(strong("Enter simulation parameters below. Mouse over each entry box for an explanation. Once you have entered effect sizes and sample sizes, a plot of hypothetical ROCs will be generated."))),
                         fluidRow(
                             column(
                                 6,
                                 hidden(radioButtons("sim_seq_a",
                                              "Condition A: Simultaneous or Sequential?",
                                              choices = c("Simultaneous",
                                                          "Sequential"),
                                              selected = "Simultaneous")),
                                 hidden(numericInput("lineup_sizes_a",
                                                     "Condition A: Lineup size",
                                                     value = 6,
                                                     min = 1)),
                                 hidden(numericInput("mu_t_a",
                                                     "Condition A: Target distribution mean",
                                                     value = 1)),
                                 bsTooltip("mu_t_a",
                                           "Specify the target distribution mean (i.e., d). The mean of the lure distribution is 0.",
                                           placement = "bottom",
                                           trigger = "hover"),
                                 hidden(numericInput("sigma_t_a",
                                                     "Condition A: Target distribution SD",
                                                     value = 1,
                                                     min = 0)),
                                 bsTooltip("sigma_t_a",
                                           "Specify the SD of the target distribution. The SD of the lure distribution is 1.",
                                           placement = "bottom",
                                           trigger = "hover"),
                                 hidden(textInput("cs_a",
                                                  "Condition A: Criterion values",
                                                  value = "",
                                                  placeholder = "1.5, 2, 2.5")),
                                 bsTooltip("cs_a",
                                           "Specify the criterion values in a comma-separated list"),
                                 hidden(textInput("pos_prop_a",
                                                  "Condition A: Sequential position probabilities",
                                                  value = "",
                                                  placeholder = "0, .5, 0, 0, .5, 0")),
                                 bsTooltip("pos_prop_a",
                                           "Specify the probabilities of a suspect appearing in each sequential position, as a comma-separated list. Must be the same length as the lineup size.")
                                 ),
                             column(
                                 6,
                                 hidden(radioButtons("sim_seq_b",
                                                     "Condition B: Simultaneous or Sequential?",
                                                     choices = c("Simultaneous",
                                                                 "Sequential"),
                                                     selected = "Simultaneous")),
                                 hidden(numericInput("lineup_sizes_b",
                                                     "Condition B: Lineup size",
                                                     value = 6,
                                                     min = 1)),
                                 hidden(numericInput("mu_t_b",
                                                     "Condition B: Target distribution mean",
                                                     value = 1)),
                                 bsTooltip("mu_t_b",
                                           "Specify the target distribution mean (i.e., d). The mean of the lure distribution is 0.",
                                           placement = "bottom",
                                           trigger = "hover"),
                                 hidden(numericInput("sigma_t_b",
                                                     "Condition B: Target distribution SD",
                                                     value = 1,
                                                     min = 0)),
                                 bsTooltip("sigma_t_b",
                                           "Specify the SD of the target distribution. The SD of the lure distribution is 1.",
                                           placement = "bottom",
                                           trigger = "hover"),
                                 hidden(textInput("cs_b",
                                                  "Condition B: Criterion values",
                                                  value = "",
                                                  placeholder = "1.5, 2, 2.5")),
                                 bsTooltip("cs_b",
                                           "Specify the criterion values in a comma-separated list"),
                                 hidden(textInput("pos_prop_b",
                                                  "Condition B: Sequential position probabilities",
                                                  value = "",
                                                  placeholder = "0, .5, 0, 0, .5, 0")),
                                 bsTooltip("pos_prop_b",
                                           "Specify the probabilities of a suspect appearing in each sequential position, as a comma-separated list. Must be the same length as the lineup size.")
                                 
                             )
                        ),
                         fluidRow(
                             column(
                                 4,
                                 radioButtons(
                                     "between_within",
                                     "Between- or within-subjects analysis?",
                                     choices = c("Between-subjects",
                                                 "Within-subjects"),
                                     selected = "Between-subjects"
                                 ),
                                 #hidden(radioButtons(
                                 #    "eff_type",
                                 #    "Use the same effect size for all confidence levels or differing effect sizes?",
                                 #    choices = c("Same effect" = "constant",
                                 #                "Different effects" = "different"),
                                 #    selected = "constant"
                                 #)),
                                 #bsTooltip("eff_type",
                                 #          "Specify how effect sizes are to be applied to the 2nd (or only) condition in the data. Refer to the `pAUC effect sizes` tab for more information",
                                 #          placement = "bottom",
                                 #          trigger = "hover"),
                                 #hidden(textInput(
                                 #    "effs",
                                 #    "Effect sizes to test",
                                 #    value = "",
                                 #    placeholder = "0"
                                 #)),
                                 #bsTooltip("effs",
                                 #          "Specify effect sizes to test in a comma-separated list. Effect sizes are applied to the 2nd condition in your dataset (unless only one condition is present). An effect size of 0 does not change the pAUC of the 2nd condition, an effect size of 0.5 increases the pAUC of the 2nd condition by 50%, and an effect size of -0.5 decreases the pAUC of the 2nd condition by 50%. See the `pAUC effect sizes` tab for more details.",
                                 #          placement = "bottom",
                                 #          trigger = "hover"),
                                 #textInput(
                                 #    "effs_different",
                                 #    "Effect sizes to test, specified for each confidence level (lowest to highest)",
                                 #    value = "",
                                 #    placeholder = "e.g., .1, .2, .1, .4, .5, etc."
                                 #),
                                 #textOutput("n_confs_message"),
                                 #tags$br(),
                                 #bsTooltip("effs_different",
                                 #          "Specify effect sizes to test in a comma-separated list, with one effect size per confidence level in your data. Effect sizes are applied to the 2nd condition in your dataset (unless only one condition is present). An effect size of 0 does not change the # of hits at that confidence level in the 2nd condition, an effect size of 0.5 increases the # of hits in the 2nd condition by 50%, and an effect size of -0.5 decreases the # of hits in the 2nd condition by 50%. See the `pAUC effect sizes` tab for more details.",
                                 #          placement = "bottom",
                                 #          trigger = "hover"),
                                 textInput(
                                     "ns",
                                     "Sample sizes to test",
                                     value = "",
                                     placeholder = "3000, 5000, 10000"
                                 ),
                                 bsTooltip("ns",
                                           "Specify sample sizes to test in a comma-separated list. Note that these sample sizes represent the total sample size, not the per-condition sample size",
                                           placement = "bottom",
                                           trigger = "hover")
                             ),
                             #column(
                             #    3,
                             #    textInput(
                             #        "min_n",
                             #        "Minimum N",
                             #        value = 1000,
                             #        min = 0
                             #    ),
                             #    numericInput(
                             #        "max_n",
                             #        "Maximum N",
                             #        value = 3000,
                             #        min = 0
                             #    ),
                             #    numericInput(
                             #        "n_n_test",
                             #        "# of sample sizes to test",
                             #        value = 3,
                             #        min = 1
                             #    )
                             #),
                             column(
                                 4,
                                 numericInput("n_total_lineups",
                                              "# of lineups per subject per condition",
                                              value = 2,
                                              min = 1,
                                              step = 1),
                                 bsTooltip("n_total_lineups",
                                           "Specify the total # of lineups each subject will complete. If using a between-subjects design, this is the total # of lineups per subject. If using a within-subjects design, this is the # of lineups in each condition (e.g., Simultaneous vs. Sequential) that each subject will complete. Note that because lineups will be generated at the sample level, no correlation between lineup performance at the subject level is assumed.",
                                           placement = "bottom",
                                           trigger = "hover"),
                                 hidden(numericInput(
                                     "n_TA_lineups",
                                     "# of TA lineups per subject per condition",
                                     value = .5,
                                     min = 0
                                 )),
                                 bsTooltip("n_TA_lineups",
                                           "Specify the # of target-absent lineups each subject will complete (again in each condition). If you have an odd # of total lineups (e.g., each subject completes either 2 TA and 1 TP lineup or 1 TA and 2 TP lineups), then divide the total # of lineups by 2 and the program will adjust for this in sampling.",
                                           placement = "bottom",
                                           trigger = "hover"),
                                 hidden(numericInput(
                                     "n_TP_lineups",
                                     "# of TP lineups per subject per condition",
                                     value = .5,
                                     min = 0
                                 )),
                                 bsTooltip("n_TP_lineups",
                                           "Specify the # of target-present lineups each subject will complete (again in each condition). If you have an odd # of total lineups (e.g., each subject completes either 2 TA and 1 TP lineup or 1 TA and 2 TP lineups), then divide the total # of lineups by 2 and the program will adjust for this in sampling.",
                                           placement = "bottom",
                                           trigger = "hover"),
                                 numericInput(
                                     "nsims",
                                     "# of simulated samples per effect size/N",
                                     value = 100,
                                     min = 1
                                 ),
                                 bsTooltip(
                                     "nsims",
                                     "Specify the # of samples (and pAUC/DPP tests) to simulate for each combination of effect size/N. 100 provides relatively stable estimates, but if time is not a concern I recommend upping this to 200 (see the `App validation & testing` tab for more details)",
                                     placement = "bottom",
                                     trigger = "hover"
                                 ),
                                 numericInput(
                                     "nboot_iter",
                                     "# of bootstraps per AUC/DPP test",
                                     value = 1000,
                                     min = 1
                                 ),
                                 bsTooltip(
                                     "nboot_iter",
                                     "Specify the # of bootstrap iterations per pROC AUC test and/or DPP bootstrapping. For pAUC analyses, 1000 provides relatively stable estimates, but if time is not a concern or if the AUC difference in question is small, recommend upping to the pROC default of 2000. For DPP, the minimum # of iterations is equal to the total sample size X the # of lineups (this value is automatically adujusted if DPP is selected).",
                                     #"Specify the # of bootstrap iterations per pROC AUC test. 1000 provides relatively stable estimates, but if time is not a concern recommend upping to pROC's default of 2000",
                                     placement = "bottom",
                                     trigger = "hover"
                                 )
                             ),
                             #column(
                             #    3,
                             #    numericInput(
                             #        "nsims",
                             #        "# of simulated samples per effect size/N",
                             #        value = 100,
                             #        min = 1
                             #    ),
                             #    bsTooltip(
                             #        "nsims",
                             #        "Specify the # of samples (and pAUC tests) to simulate for each combination of effect size/N. 100 provides relatively stable estimates, but if time is not a concern I recommend upping this to 200 (see the `App validation & testing` tab for more details)",
                             #        placement = "bottom",
                             #        trigger = "hover"
                             #    ),
                             #    numericInput(
                             #        "nboot_iter",
                             #        "# of bootstraps per AUC test",
                             #        value = 1000,
                             #        min = 1
                             #    ),
                             #    bsTooltip(
                             #        "nboot_iter",
                             #        "Specify the # of bootstrap iterations per pROC AUC test. 1000 provides relatively stable estimates, but if time is not a concern or if the AUC difference in question is small, recommend upping to the pROC default of 2000",
                             #        #"Specify the # of bootstrap iterations per pROC AUC test. 1000 provides relatively stable estimates, but if time is not a concern recommend upping to pROC's default of 2000",
                             #        placement = "bottom",
                             #        trigger = "hover"
                             #    )
                             #), 
                             column(
                                 4,
                                 #radioButtons(
                                 #    "roc_paired",
                                 #    "Paired ROCs (i.e., within-subjects comparison)?",
                                 #    choices = c("No", "Yes"),
                                 #    selected = "No"
                                 #),
                                 radioButtons(
                                     "measure",
                                     "Measure(s) to compute power for",
                                     choices = c("pAUC",
                                                 "DPP",
                                                 "Both"),
                                     selected = "pAUC"
                                 ),
                                 bsTooltip("measure",
                                           "Specify whether to compute power for pAUC comparison, DPP (deviation from perfect performance) comparison, or both. Note that including DPP substantially increases the simulation time.",
                                           placement = "bottom",
                                           trigger = "hover"),
                                 radioButtons(
                                     "roc_trunc",
                                     "Partial AUC truncation",
                                     choices = c("Highest false ID rate",
                                                 "Lowest false ID rate",
                                                 "Custom"),
                                     selected = "Lowest false ID rate"
                                 ),
                                 hidden(numericInput("custom_trunc",
                                                     "Enter the specificity (1 - FAR)",
                                                     value = NA,
                                                     min = 0,
                                                     max = 1)),
                                 bsTooltip("roc_trunc",
                                           "Specify whether the pAUC comparison will occur at the highest or lowest false ID rate between your conditions, or at a custom specificity level",
                                           placement = "bottom",
                                           trigger = "hover"),
                                 uiOutput("test_tails"),
                                 bsTooltip("test_tails",
                                           "If the pAUC/DPP comparison will be one-sided, select the test direction. Note that because pAUC and DPP are directionally opposite `>` here means higher pAUC but lower DPP (i.e., the superior condition).",
                                           placement = "bottom",
                                           trigger = "hover"),
                                 numericInput(
                                     "alpha_level",
                                     "Type I error rate",
                                     value = 0.05,
                                     min = 0,
                                     step = 0.01
                                 ),
                                 bsTooltip("alpha_level",
                                           "The desired Type I error rate. Adjustment for one- vs. two-tailed tests occurs automatically",
                                           placement = "bottom",
                                           trigger = "hover")
                             )
                         ),
                         fluidRow(
                             actionButton(
                                 "generate_hypothetical",
                                 "Check parameters and generate hypothetical ROC curves",
                                 width = '100%',
                                 class = "btn-info"
                             ),
                             bsTooltip(
                                 "generate_hypothetical",
                                 "Check whether the entered parameters are valid and plot hypothetical ROC curves based on the parameters. If you change any parameter values, you will need to click this again before running the simulations.",
                                 #"Specify the # of bootstrap iterations per pROC AUC test. 1000 provides relatively stable estimates, but if time is not a concern recommend upping to pROC's default of 2000",
                                 placement = "bottom",
                                 trigger = "hover"
                             ),
                             hidden(actionButton(
                                 "sim_start",
                                 "Simulate",
                                 width = '100%',
                                 class = "btn-success"
                             )),
                         ),
                         fluidRow(
                             tags$br(),
                             tags$p(strong("Plot of hypothetical ROCs to test")),
                             plotOutput("ROC_data_plot")
                         ))

### App validation & testing tab ----
validation_tab = tabItem(
    tabName = "validation_tab",
    box(
        width = 12,
        title = "",
        collapsible = FALSE,
        includeHTML("./www/App-Testing-Results.html")
    )
)

### simulation results tab ----
results_tab = tabItem(
    tabName = "results_tab",
    box(id = "no_results",
        width = 12,
        tags$h4("Simulation results will appear here")),
    box(id = "results_box",
        width = 12,
        #title = "Results",
        collapsible = TRUE,
        tags$h1("Results"),
        textOutput("time_taken"),
        tags$br(),
        div(style = 'overflow-x: scroll', dataTableOutput("pwr_store")),
        tags$br(),
        tags$p(strong("You can download a summary report of the power analysis (including all simulation parameters) by clicking the button below.")),
        downloadButton("report_dl",
                       "Download summary report"),
        #actionButton("upload_results",
        #             "Upload analysis results",
        #             icon = icon("upload"))
    ),
    #bsTooltip(
    #    "upload_results",
    #    "Anonymously upload your analysis results for others to use. Note that this does not upload your data, just the power simulation results",
    #    placement = "bottom",
    #    trigger = "hover"
    #),
    box(id = "power_curves_box",
        width = 12,
        #title = "Power curves",
        tags$h1("Power curves"),
        collapsible = TRUE,
        plotOutput("pwr_plot")
    )
)

### results compendium tab ----
previous_tab = tabItem(tabName = "previous_tab",
                       useShinyjs(),
                       box(width = 12,
                           title = "Previous simulation results",
                           tags$p("This page shows power analysis results uploaded by other users. You can sort by average AUC difference, effect size, N, etc. Results were anonymously uploaded to a Google Sheet linked to this app. If you encounter an error on this page and the sheet does not display, refreshing should fix it (But, WARNING: If you have run your simulation and refresh the page, your results will be lost)"),
                           tags$br(),
                           tags$p("To view power curves and simulation parameters for a given analysis (with the option to use their simulation parameters for your own simulation), copy and paste the sim_id in the box below and press 'Show'"),
                           textInput("previous_sim_id",
                                     "Previous sim id"),
                           actionButton("show_previous_sim",
                                        "Show")
                               #        "min_n",
                               #        "Minimum N",
                               #        value = 1000,
                               #        min = 0
                           ),
                       box(width = 12,
                           div(style = 'overflow-x: scroll', dataTableOutput("power_results"))),
                       box(id = "previous_sim_box",
                           width = 12,
                           tags$h4("Previous simulation power curves"),
                           plotOutput("previous_sim_plot"),
                           tags$h4("Previous simulation AUC effect sizes"),
                           dataTableOutput("previous_sim_effs"),
                           tags$h4("Previous simulation parameters"),
                           dataTableOutput("previous_sim_params"),
                           hidden(actionButton("transfer_params",
                                               "Transfer these parameters to my simulation",
                                               width = '100%',
                                               class = "btn-success")),
                           bsTooltip("transfer_params",
                                     "Transfer these parameter values to the relevant field in the `Simulation Parameters` tab. Note that you will still need to specify some parameters, like test tails and alpha level",
                                     placement = "top",
                                     trigger = "hover")
                       ))

## UI ----
skin_color <- "black"

ui <- dashboardPage(
    skin = skin_color,
    dashboardHeader(title = "powe(R)OC: A power simulation tool for eyewitness lineup ROC analyses", 
                    titleWidth = "calc(100% - 44px)" # puts sidebar toggle on right
    ),
    dashboardSidebar(
        # https://fontawesome.com/icons?d=gallery&m=free
        sidebarMenu(
            id = "tabs",
            menuItem("Introduction", tabName = "intro_tab", icon = icon("compass")),
            menuItem("How this app works", tabName = "explanation_tab", icon = icon("info-circle")),
            #menuItem("Previous simulation results", tabName = "previous_tab", icon = icon("history")),
            menuItem("Data Upload", tabName = "data_tab", icon = icon("table")),
            menuItem("SDT parameter estimation", tabName = "sdtlu_tab", icon = icon("signal")),
            #menuItem("pAUC effect sizes", tabName = "effects_tab", icon = icon("book")),
            menuItem("Simulation Parameters", tabName = "parameters_tab", icon = icon("book")),
            menuItem("Simulation Results", tabName = "results_tab", icon = icon("poll")),
            menuItem("App validation & testing", tabName = "validation_tab", icon = icon("search"))
            #sidebarMenuOutput("results_render")
            )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
            # links to files in www/
            tags$link(rel = "stylesheet", type = "text/css", href = "basic_template.css"), 
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), 
            tags$script(src = "custom.js")
        ),
        tabItems(
            intro_tab,
            explanation_tab,
            #previous_tab,
            data_tab,
            sdtlu_tab,
            #effects_tab,
            parameters_tab,
            results_tab,
            validation_tab
        )
    )
)

# server ----
server <- function(input, output, session) {
    
    #shinyjs::hide("previous_sim_box")
    
    ## initial setup ----
    hide("effs_different")
    
    output$cond1_label = renderText({"Condition 1"})
    
    output$cond2_label = renderText({"Condition 2"})
    
    ## function for computing average DPP ----
    # Authors: Andrew M. Smith, James M. Lampinen, Gary L. Wells, Laura Smalarz, & Simona Mackovichova 
    # Title: Deviation from Perfect Performance Measures the Diagnostic Utility of Eyewitness Lineups but Partial Area Under the ROC Curve Does Not
    # Status: Accepted
    # Journal: Journal of Applied Research in Memory and Cognition
    # Average DPP
    
    DPP_difference = function(data, indices) {
        d=data[indices,]
        
        ### Get confidence levels (MUST ENSURE THAT MINIMUM CONFIDENCE IN THE DATASET IS 1)
        conf_breaks = c(c(0, 0.5, 1), seq(from = 2, to = max(d$conf), by = 1))
        
        ###binning cond1 true positive proportions by cumulating confidence levels
        cond1_data<-subset(d,cond==levels(d$cond)[1])
        culprit1<-subset(cond1_data,tpORta==1)
        histinfo1<-hist(culprit1$conf,breaks=conf_breaks,plot=FALSE)->h1
        h1$percents<-cumsum(h1$counts)/sum(h1$counts)
        tps1 <- 1-h1$percents
        tps1=tps1[1:length(tps1)-1] ##drop last element of vector (it equals 0) to avoid bias in performance estimates
        
        ###binning cond1_data false positive proportions by cumulating confidence levels
        innocent1<-subset(cond1_data,tpORta==0)
        histinfo1i<-hist(innocent1$conf,breaks=conf_breaks,plot=FALSE)->h1i
        h1i$percents<-cumsum(h1i$counts)/sum(h1i$counts)
        fps1 <- 1-h1i$percents
        fps1=fps1[1:length(fps1)-1] ##drop last element of vector (it equals 0) to avoid bias in performance estimates
        
        ###compute DPP for cond1_data lineup - this is our measure of performance
        DPP1<-mean((1-tps1)+fps1)
        
        ###binning cond2_data true positive proportions by cumulating confidence levels
        cond2_data<-subset(d,cond==levels(d$cond)[2])
        culprit2<-subset(cond2_data,tpORta==1)
        histinfo2<-hist(culprit2$conf,breaks=conf_breaks,plot=FALSE)->h2
        h2$percents<-cumsum(h2$counts)/sum(h2$counts)
        tps2 <- 1-h2$percents
        tps2=tps2[1:length(tps2)-1] ##drop last element of vector (it equals 0) to avoid bias in performance estimates
        
        ###binning cond2_data false positive proportions by cumulating confidence levels
        innocent2<-subset(cond2_data,tpORta==0)
        histinfo2i<-hist(innocent2$conf,breaks=conf_breaks,plot=FALSE)->h2i
        h2i$percents<-cumsum(h2i$counts)/sum(h2i$counts)
        fps2 <- 1-h2i$percents
        fps2=fps2[1:length(fps2)-1] ##drop last element of vector (it equals 0) to avoid bias in performance estimates
        
        ###compute DPP for dataset 2 - this is our measure of performance
        DPP2<-mean((1-tps2)+fps2)
        
        ###compue DPP difference...positive values mean procedure 1 is worse (it has a higher dpp or less utility)
        diff<-DPP1-DPP2
        stats_DPP = c(DPP1,DPP2,diff)
        return(stats_DPP)
    }
    
    ## tab links ----
    observeEvent(input$explanation_tab_link, {
        updateTabItems(session, "tabs", "explanation_tab")
    })
    
    observeEvent(input$previous_tab_link, {
        updateTabItems(session, "tabs", "previous_tab")
    })
    
    observeEvent(input$data_tab_link, {
        updateTabItems(session, "tabs", "data_tab")
    })
    
    observeEvent(input$effects_tab_link, {
        updateTabItems(session, "tabs", "effects_tab")
    })
    
    observeEvent(input$parameters_tab_link, {
        updateTabItems(session, "tabs", "parameters_tab")
    })
    
    observeEvent(input$results_tab_link, {
        updateTabItems(session, "tabs", "results_tab")
    })
    
    observeEvent(input$validation_tab_link, {
        updateTabItems(session, "tabs", "validation_tab")
    })
    
    ## data files ----
    data_files = reactiveValues(user_data = NULL,
                                processed_data = NULL,
                                ROC_data_wide = NULL,
                                sdtlu_estimates = NULL,
                                conf_effs_data = NULL,
                                saved_data = NULL,
                                upload_data = NULL,
                                pwr_store = NULL,
                                sim_params = data.frame(Parameter = rep(NA, times = 24)),
                                sdtlu_hypothetical_data = NULL,
                                compendium_data = NULL,
                                previous_sim_params = data.frame(Parameter = rep(NA, times = 8)),
                                previous_sim_effs = NULL)
    
    output$example_data = renderDataTable({
        example_data
    })
    
    output$effect_sizes = renderDataTable({
        effect_sizes
    })
    
    observeEvent(input$effects_tab_link, {
        output$effect_sizes = renderDataTable({
            effect_sizes})
    })
    
    observeEvent(input$sim_start, {
        
        
        
        if (input$between_within == "Between-subjects") {
            parameters$between_within = "Between-subjects"
        } else {
            parameters$between_within = "Within-subjects"
        }
        
        #if (input$eff_type == "constant") {
            other_vars$effs_report = input$effs
            other_vars$effs_different_report = "N/A"
        #} else {
        #    other_vars$effs_different_report = as.list(data_files$conf_effs_data$conf_effs) %>% 
        #        paste(collapse = ",")
        #    other_vars$effs_report = "N/A"
        #}
        
        
        
        if (input$empirical_theoretical == "Data") {
            data_files$sim_params = data_files$sim_params %>% 
                mutate(Parameter = c("Simulating from data or SDT parameters?",
                                     "SDT: Condition A Simultaneous or Sequential",
                                     "SDT: Condition A Lineup size",
                                     "SDT: Condition A Target distribution mean",
                                     "SDT: Condition A Target distribution SD",
                                     "SDT: Condition A Criterion values",
                                     "SDT: Condition A Sequential position probabilities",
                                     "SDT: Condition B Simultaneous or Sequential",
                                     "SDT: Condition B Lineup size",
                                     "SDT: Condition B Target distribution mean",
                                     "SDT: Condition B Target distribution SD",
                                     "SDT: Condition B Criterion values",
                                     "SDT: Condition B Sequential position probabilities",
                                     "Ns",
                                     #"Effects",
                                     "Between- or within-subjects",
                                     #"Confidence levels",
                                     #"Effects by confidence level",
                                     "# of lineups/subject per condition",
                                     "# TA lineups/subject per condition",
                                     "# TP lineups/subject per condition",
                                     "# of simulated samples per effect size/N",
                                     "# of bootstraps per AUC/DPP test",
                                     "Partial AUC truncation",
                                     "Custom specificity",
                                     "Two-tailed or one-tailed?",
                                     "Type I error rate"),
                       Value = c(input$empirical_theoretical,
                                 input$sim_seq_a,
                                 input$lineup_sizes_a,
                                 input$mu_t_a,
                                 input$sigma_t_a,
                                 input$cs_a,
                                 input$pos_prop_a,
                                 input$sim_seq_b,
                                 input$lineup_sizes_b,
                                 input$mu_t_b,
                                 input$sigma_t_b,
                                 input$cs_b,
                                 input$pos_prop_b,
                                 input$ns,
                                 #other_vars$effs_report,
                                 parameters$between_within,
                                 #parameters$n_confs,
                                 #other_vars$effs_different_report,
                                 input$n_total_lineups,
                                 input$n_TA_lineups,
                                 input$n_TP_lineups,
                                 input$nsims,
                                 input$nboot_iter,
                                 input$roc_trunc,
                                 input$custom_trunc,
                                 input$test_tails,
                                 input$alpha_level)) %>% 
                filter(!grepl("SDT: ", Parameter))
        } else {
            data_files$sim_params = data_files$sim_params %>% 
                mutate(Parameter = c("Simulating from data or SDT parameters?",
                                     "SDT: Condition A Simultaneous or Sequential",
                                     "SDT: Condition A Lineup size",
                                     "SDT: Condition A Target distribution mean",
                                     "SDT: Condition A Target distribution SD",
                                     "SDT: Condition A Criterion values",
                                     "SDT: Condition A Sequential position probabilities",
                                     "SDT: Condition B Simultaneous or Sequential",
                                     "SDT: Condition B Lineup size",
                                     "SDT: Condition B Target distribution mean",
                                     "SDT: Condition B Target distribution SD",
                                     "SDT: Condition B Criterion values",
                                     "SDT: Condition B Sequential position probabilities",
                                     "Ns",
                                     #"Effects",
                                     "Between- or within-subjects",
                                     #"Confidence levels",
                                     #"Effects by confidence level",
                                     "# of lineups/subject per condition",
                                     "# TA lineups/subject per condition",
                                     "# TP lineups/subject per condition",
                                     "# of simulated samples per effect size/N",
                                     "# of bootstraps per AUC/DPP test",
                                     "Partial AUC truncation",
                                     "Custom specificity",
                                     "Two-tailed or one-tailed?",
                                     "Type I error rate"),
                       Value = c(input$empirical_theoretical,
                                 input$sim_seq_a,
                                 input$lineup_sizes_a,
                                 input$mu_t_a,
                                 input$sigma_t_a,
                                 input$cs_a,
                                 input$pos_prop_a,
                                 input$sim_seq_b,
                                 input$lineup_sizes_b,
                                 input$mu_t_b,
                                 input$sigma_t_b,
                                 input$cs_b,
                                 input$pos_prop_b,
                                 input$ns,
                                 #other_vars$effs_report,
                                 parameters$between_within,
                                 #parameters$n_confs,
                                 #other_vars$effs_different_report,
                                 input$n_total_lineups,
                                 input$n_TA_lineups,
                                 input$n_TP_lineups,
                                 input$nsims,
                                 input$nboot_iter,
                                 input$roc_trunc,
                                 input$custom_trunc,
                                 input$test_tails,
                                 input$alpha_level))
        }
        
    })
    
    #observeEvent(input$user_data, {
    #    data_files$user_data = read_csv(input$user_data$datapath)
    #    
    #    if (!("cond") %in% colnames(data_files$user_data)) {
    #        data_files$user_data$cond = "A"
    #    } else {
    #        data_files$user_data$cond = data_files$user_data$cond
    #    }
    #})
    
    ### Choose whether to upload data or use an open dataset ----
    observeEvent(input$data_source, {
        if (input$data_source == "Upload data") {
            show("user_data")
            hide("open_dataset")
        } else {
            hide("user_data")
            show("open_dataset")
        }
    })
    
    ### If open dataset is selected, create the dataset ----
    observeEvent(input$open_dataset, {
        data_files$processed_data = open_data %>% 
            filter(exp == input$open_dataset)
        
        # Flag datasets that don't have a designated innocent suspect ----
        if (data_files$processed_data$exp[1] %in% c("Colloff et al. (2021b): Exp 1: Same-pose vs. Different-pose encoding-test", 
                       "Colloff et al. (2021b): Exp 1: Same-plus-additional-pose vs. Different-pose encoding-test", 
                       "Colloff et al. (2021b): Exp 1: Same-plus-additional-pose vs. Same-pose encoding-test", 
                       "Colloff et al. (2021b): Exp 2: High vs. Low pose reinstatement", 
                       "Akan et al. (2021): Exp 1: Showup vs. 6-person",
                       "Morgan et al. (2019): Exp 1: Sleep vs. Wake pre-lineup",
                       "Morgan et al. (2019): AM vs. PM lineup",
                       "Smith et al. (2022): Exp 1: Sequential parade warning vs. no-warning",
                       "Smith et al. (2022): Exp 1: Serial parade warning vs. no-warning",
                       "Smith et al. (2022): Exp 1: Serial parade 1-pass vs. 2-passes")) {
            updateRadioButtons(session,
                               "designated_suspect",
                               selected = "No"
            )
            
            updateNumericInput(session,
                               "lineup_size_1",
                               value = as.numeric(unique(data_files$processed_data$lineup_size[data_files$processed_data$cond == levels(as.factor(data_files$processed_data$cond))[1]])))
            
            updateNumericInput(session,
                               "lineup_size_2",
                               value = as.numeric(unique(data_files$processed_data$lineup_size[data_files$processed_data$cond == levels(as.factor(data_files$processed_data$cond))[2]])))
        } else {
            updateRadioButtons(session,
                               "designated_suspect",
                               selected = "Yes"
            )
        }
        
        
        minimum_conf = min(data_files$processed_data$conf_level)
        
        if (minimum_conf == 0) {
            data_files$processed_data = open_data %>% 
                filter(exp == input$open_dataset) %>% 
                mutate(id_type = tolower(id_type),
                       culprit_present = tolower(culprit_present),
                       cond = as.factor(cond),
                       conf_level = conf_level + 1,
                       conf_level_rev = max(conf_level)+1 - conf_level) %>% 
                arrange(cond)
        } else {
            data_files$processed_data = open_data %>% 
                filter(exp == input$open_dataset) %>% 
                mutate(id_type = tolower(id_type),
                       culprit_present = tolower(culprit_present),
                       cond = as.factor(cond),
                       conf_level = conf_level,
                       conf_level_rev = max(conf_level)+1 - conf_level) %>% 
                arrange(cond)
        }
        
        data_files$saved_data = data_files$processed_data
    })
    
    observeEvent(input$designated_suspect, {
        req(data_files$processed_data)
        if (input$designated_suspect == "Yes") {
            
            output$cond1_lineup_text = NULL
            output$cond2_lineup_text = NULL

            
            hide("lineup_size_1")
            hide("lineup_size_2")
        } else if (input$designated_suspect == "No") {
            ### lineup size ----
            output$cond1_lineup_text = renderPrint({
                HTML(paste0("<b>", "Lineup size for ", parameters$cond1, "</b>"))
            })
            
            output$cond2_lineup_text = renderText({
                sprintf("<b> Lineup size for %s </b>",
                        parameters$cond2)
            })
            
            show("lineup_size_1")
            show("lineup_size_2")
            
            updateNumericInput(session,
                               "lineup_size_1",
                               value = as.numeric(unique(data_files$processed_data$lineup_size[data_files$processed_data$cond == levels(as.factor(data_files$processed_data$cond))[1]])))
            
            updateNumericInput(session,
                               "lineup_size_2",
                               value = as.numeric(unique(data_files$processed_data$lineup_size[data_files$processed_data$cond == levels(as.factor(data_files$processed_data$cond))[2]])))
            }
    })

    observeEvent(input$user_data, {
        data_files$user_data = read_csv(input$user_data$datapath)
        
        if(!c("suspect_position") %in% colnames(data_files$user_data)) {
            data_files$user_data$suspect_position = NA
        } else {
            data_files$user_data$suspect_position = data_files$user_data$suspect_position
        }
        
        if (!c("id_type") %in% colnames(data_files$user_data) |
            !c("culprit_present") %in% colnames(data_files$user_data) |
            !c("conf_level") %in% colnames(data_files$user_data) |
            !c("lineup_size") %in% colnames(data_files$user_data)) {
            showModal(modalDialog(
                title = "Warning",
                "Uploaded data file missing required columns. 
                Check the formatting of the example data for reference."
            ))
        } else if (!is.numeric(data_files$user_data$conf_level)) {
            showModal(modalDialog(
                title = "Warning",
                "Confidence variable must be numeric."
            ))
        } else {
            if (!("cond") %in% colnames(data_files$user_data)) {
                data_files$user_data$cond = "A"
            } else {
                data_files$user_data$cond = data_files$user_data$cond
            }
            
            data_files$user_data = data_files$user_data %>% 
                filter(str_length(id_type) > 0 &
                           str_length(culprit_present) > 0 &
                           str_length(cond) > 0 &
                           !is.na(conf_level))
            
            message("Created condition variable")
            
            if (length(unique(data_files$user_data$cond)) > 1) {
                minimum_conf = min(data_files$user_data$conf_level)
                
                if (minimum_conf == 0) {
                    data_files$processed_data = data_files$user_data %>% 
                        mutate(id_type = tolower(id_type),
                               culprit_present = tolower(culprit_present),
                               cond = as.factor(cond),
                               conf_level = conf_level + 1,
                               conf_level_rev = max(conf_level)+1 - conf_level) %>% 
                        arrange(cond)
                } else {
                    data_files$processed_data = data_files$user_data %>% 
                        mutate(id_type = tolower(id_type),
                               culprit_present = tolower(culprit_present),
                               cond = as.factor(cond),
                               conf_level = conf_level,
                               conf_level_rev = max(conf_level)+1 - conf_level) %>% 
                        arrange(cond)
                }
                
            message("Processed data with two conditions")
            } else {
                minimum_conf = min(data_files$user_data$conf_level)
                
                if (minimum_conf < 1) {
                    data_files$processed_data = data_files$user_data %>% 
                        rbind(data_files$user_data) %>% 
                        mutate(cond = rep(c("A", "B"), each = nrow(data_files$user_data)),
                               id_type = tolower(id_type),
                               culprit_present = tolower(culprit_present),
                               conf_level = conf_level + 1,
                               conf_level_rev = max(conf_level)+1 - conf_level,
                               cond = as.factor(cond)) %>% 
                        arrange(cond)
                } else {
                    data_files$processed_data = data_files$user_data %>% 
                        rbind(data_files$user_data) %>% 
                        mutate(cond = rep(c("A", "B"), each = nrow(data_files$user_data)),
                               id_type = tolower(id_type),
                               culprit_present = tolower(culprit_present),
                               conf_level = conf_level,
                               conf_level_rev = max(conf_level)+1 - conf_level,
                               cond = as.factor(cond)) %>% 
                        arrange(cond)
                }
            message("Processed data with one condition")
            }
            
            data_files$saved_data = data_files$processed_data
            
            data_files$processed_data = data_files$processed_data %>% 
                mutate(specificity = NA,
                       auc_diff = NA)
            
            #show("designated_suspect")
            message("Created processed data")
        }
    })
    
    output$user_data = renderDataTable({
        data_files$user_data
    })
    
    output$processed_data = renderDataTable({
        data_files$processed_data
    })
    
    output$auc_diff_text = renderText({
        req(data_files$processed_data)
        if (("auc_diff" %nin% colnames(data_files$processed_data))) {
            ""
        } else {
            paste("pAUC difference: ", data_files$processed_data$auc_diff[1], sep = "")
        }
    })
    
    ## deal with datasets without designated innocent suspects ----
    #observeEvent(c(input$designated_suspect), {
    #    if (input$designated_suspect == "Yes") {
    #        data_files$processed_data = data_files$saved_data
    #        
    #        updateNumericInput(session,
    #                           "lineup_size_1",
    #                           value = 6)
    #        
    #        updateNumericInput(session,
    #                           "lineup_size_2",
    #                           value = 6)
    #    }
    #    })
    
    #observeEvent(c(input$lineup_size_1, input$lineup_size_2), {
    #    req(input$lineup_size_1)
    #    req(input$lineup_size_2)
    #    
    #    if (input$designated_suspect == "No") {
    #        data_files$processed_data = data_files$saved_data %>% 
    #            mutate(lineup_size = ifelse(cond == parameters$cond1, input$lineup_size_1, input$lineup_size_2),
    #                   suspect_prob = 1/lineup_size)
    #        
    #        for (i in 1:nrow(data_files$processed_data)) {
    #            if (data_files$processed_data$culprit_present[i] == "absent" & data_files$processed_data$id_type[i] == "filler") {
    #                data_files$processed_data$id_type[i] = sample(c("filler", "suspect"), 1, prob = c(1-data_files$processed_data$suspect_prob[i], data_files$processed_data$suspect_prob[i]))
    #            } else {
    #                data_files$processed_data$id_type[i] = data_files$processed_data$id_type[i]
    #            }
    #        }
    #}
    #    })
    
    ### generate hypothetical ROCs upon data load ----
    observeEvent(input$check_data, {
        req(data_files$processed_data)
        req(input$designated_suspect)
        
        if (input$designated_suspect == "Yes") {
            data_files$processed_data = data_files$saved_data
            
            #updateNumericInput(session,
            #                   "lineup_size_1",
            #                   value = parameters$cond1_lineup_size)
            #
            #updateNumericInput(session,
            #                   "lineup_size_2",
            #                   value = parameters$cond2_lineup_size)
            
        } else if (input$designated_suspect == "No") {
            data_files$processed_data = data_files$saved_data %>% 
                mutate(lineup_size = ifelse(cond == parameters$cond1, input$lineup_size_1, input$lineup_size_2),
                       suspect_prob = 1/lineup_size)
            
            for (i in 1:nrow(data_files$processed_data)) {
                if (data_files$processed_data$culprit_present[i] == "absent" & data_files$processed_data$id_type[i] == "filler") {
                    data_files$processed_data$id_type[i] = sample(c("filler", "suspect"), 1, prob = c(1-data_files$processed_data$suspect_prob[i], data_files$processed_data$suspect_prob[i]))
                } else {
                    data_files$processed_data$id_type[i] = data_files$processed_data$id_type[i]
                }
            }
        }
        
        #### getting proportion data from each condition ----
        #data_props = open_data %>% 
        #    filter(exp == "Akan et al. (2021): Exp 1: Showup vs. 6-person") %>%
        #    group_by(id_type, culprit_present, cond) %>% 
        #    count() %>% 
        #    ungroup() %>% 
        #    group_by(culprit_present, cond) %>% 
        #    mutate(total = sum(n),
        #           prop = n/total,
        #           cond = as.factor(cond)) %>% 
        #    ungroup()
        
        message("Pre-group by")
        
        data_props = data_files$processed_data %>%
            group_by(id_type, culprit_present, cond) %>% 
            count() %>% 
            ungroup() %>% 
            group_by(culprit_present, cond) %>% 
            mutate(total = sum(n),
                   prop = n/total,
                   cond = as.factor(cond)) %>% 
            ungroup()
        
        message("Processed proportion data")
        
        ##### Getting TA & TP suspect proportions for Condition 1 ----
        cond1_TA_susp_prop = data_props %>% 
            filter(cond == levels(data_props$cond)[1] &
                       culprit_present == "absent" & 
                       id_type == "suspect") %>% 
            dplyr::select(prop) %>% 
            as.numeric()
        
        cond1_TP_susp_prop = data_props %>% 
            filter(cond == levels(data_props$cond)[1] &
                       culprit_present == "present" & 
                       id_type == "suspect") %>% 
            dplyr::select(prop) %>% 
            as.numeric()
        
        ##### Getting TA & TP suspect proportions for Condition 2 ----
        cond2_TA_susp_prop = data_props %>% 
            filter(cond == levels(data_props$cond)[2] &
                       culprit_present == "absent" & 
                       id_type == "suspect") %>% 
            dplyr::select(prop) %>% 
            as.numeric()
        
        cond2_TP_susp_prop = data_props %>% 
            filter(cond == levels(data_props$cond)[2] &
                       culprit_present == "present" & 
                       id_type == "suspect") %>% 
            dplyr::select(prop) %>% 
            as.numeric()
        
        message("Processed data for both conditions")
        
        #### Getting responses at each confidence level for both conditions ----
        data_original = data_files$processed_data %>%
            #mutate(conf_level = as.factor(conf_level)) %>% 
            group_by(id_type, conf_level_rev, culprit_present, cond) %>% 
            count() %>% 
            ungroup() %>% 
            group_by(culprit_present, cond) %>% 
            mutate(total = sum(n),
                   prop = n/total) %>% 
            ungroup() %>%
            filter(id_type == "suspect")
        
        message("Data processing complete")
        message(data_original)
        
        ROC_data = data.frame(prop = rep(NA, times = length(unique(data_original$cond))*
                                             length(unique(data_original$culprit_present))*
                                             length(unique(data_original$conf_level_rev))*
                                             1),
                              cond = NA,
                              presence = NA,
                              criteria = NA,
                              eff = NA)
        
        row = 1
        
        message("Created empty ROC store object for hypothetical plot")
        
        for (g in 1:1) {
            data = data_original
            eff = 1
            for (h in 1:nrow(data)) {
                if (data$culprit_present[h] == "present" & data$cond[h] == levels(data$cond)[2]) {
                    data$n[h] = round(data$n[h]*eff)
                } else {
                    data$n[h] = data$n[h]
                }
            }
            
            data$prop = data$n / data$total
            
            for (i in 1:length(unique(data$cond))) {
                curr_cond = levels(data$cond)[i]
                for (j in 1:length(unique(data$culprit_present))) {
                    curr_present = unique(data$culprit_present)[j]
                    for (k in 1:length(unique(data$conf_level_rev))) {
                        curr_conf = unique(data$conf_level_rev)[k]
                        curr_resps = sum(data$prop[data$cond == curr_cond &
                                                       data$culprit_present == curr_present &
                                                       data$conf_level_rev %in% c(1:curr_conf)])
                        
                        ROC_data$cond[row] = curr_cond
                        ROC_data$presence[row] = curr_present
                        ROC_data$prop[row] = curr_resps
                        ROC_data$criteria[row] = curr_conf
                        #ROC_data$eff[row] = eff
                        row = row + 1
                    }
                }
            }
        }
        
        message("Populated ROC store object")
        message(ROC_data)
        
        #write.csv(ROC_data,
        #          "./test_ROC_data.csv",
        #          row.names = FALSE,
        #          na = "")
        
        ROC_data_wide = spread(ROC_data,
                               key = "presence",
                               value = "prop")  %>%
            rbind(data.frame(cond = rep(c(levels(data_files$processed_data$cond)[1], 
                                          levels(data_files$processed_data$cond)[2]), 
                                        each = 1),
                             criteria = NA,
                             eff = rep(1, times = length(unique(data_original$cond))),
                             present = 0,
                             absent = 0)) #%>% 
        #mutate(present = ifelse(present < 0, 0,
        #                        ifelse(present > 1, 1, present)))
        
        
        partial_threshold = ifelse(is.na(data_files$processed_data$specificity), 0,
                                   1 - data_files$processed_data$specificity)
        
        #max_criteria = as.numeric(length(unique(ROC_data_wide$criteria[!is.na(ROC_data_wide$criteria)])))
        
        #if (input$roc_trunc == "Lowest false ID rate") {
        #    #partial_threshold = ROC_data_wide %>% 
        #    #    filter(criteria == max_criteria) %>% 
        #    #    select(absent) %>% 
        #    #    min() 
        #    
        #    partial_threshold = min(
        #        data_props$prop[data_props$id_type == "suspect" &
        #                            data_props$culprit_present == "absent" &
        #                            data_props$cond == levels(data_props$cond)[1]],
        #        data_props$prop[data_props$id_type == "suspect" &
        #                            data_props$culprit_present == "absent" &
        #                            data_props$cond == levels(data_props$cond)[2]]
        #    )
        #    
        #} else if (input$roc_trunc == "Highest false ID rate") {
        #    #partial_threshold = ROC_data_wide %>% 
        #    #    filter(criteria == max_criteria) %>% 
        #    #    select(absent) %>% 
        #    #    max()
        #    
        #    partial_threshold = max(
        #        data_props$prop[data_props$id_type == "suspect" &
        #                            data_props$culprit_present == "absent" &
        #                            data_props$cond == levels(data_props$cond)[1]],
        #        data_props$prop[data_props$id_type == "suspect" &
        #                            data_props$culprit_present == "absent" &
        #                            data_props$cond == levels(data_props$cond)[2]]
        #    )
        #    
        #} else {
        #    partial_threshold = 1 - other_vars$custom_trunc
        #}
        
        message("Created data for plotting")
        message(ROC_data_wide)
        
        hypothetical_ROC_plot = ROC_data_wide %>% 
            ggplot(aes(x = absent, y = present, color = cond))+
            geom_point(alpha = .5)+
            geom_line()+
            geom_vline(xintercept = partial_threshold)+
            apatheme+
            labs(x = "\nFalse ID rate",
                 y = "Correct ID rate\n",
                 #linetype = "Effect",
                 color = "Condition")+
            theme(text = element_text(size = 20))
        
        data_files$ROC_data_wide = ROC_data_wide
        plots$hypothetical_ROC_plot = hypothetical_ROC_plot
        
        output$hypothetical_ROC_plot = renderPlot({
            hypothetical_ROC_plot
        })
        
    })
    
    observeEvent(input$effects_tab_link, {
        output$auc_lit_plot = renderPlot({
            auc_lit_plot
        })
    })
    
    output$auc_lit_plot = renderPlot({
        auc_lit_plot
    })
    
    ## simulation parameters ----
    parameters = reactiveValues(effs = vector(),
                                effs_different = vector(),
                                n_confs = NA,
                                n_confs_a = NA,
                                n_confs_b = NA,
                                max_conf_a = NA,
                                min_conf_a = NA,
                                max_conf_b = NA,
                                min_conf_b = NA,
                                ns = vector(),
                                n_TA_lineups = NA,
                                n_TP_lineups = NA,
                                cond1 = NA,
                                cond2 = NA,
                                between_within = NA,
                                empirical_theoretical = NA,
                                sim_seq_a = NA,
                                lineup_sizes_a = NA,
                                mu_t_a = NA,
                                sigma_t_a = NA,
                                cs_a = NA,
                                pos_prop_a = NA,
                                sim_seq_b = NA,
                                lineup_sizes_b = NA,
                                mu_t_b = NA,
                                sigma_t_b = NA,
                                cs_b = NA,
                                pos_prop_b = NA
                                )
    
    observeEvent(input$empirical_theoretical, {
        hide("sim_start")
        
        if (input$empirical_theoretical == "Data") {
            show("eff_type")
            show("effs")
            
            hide("sim_seq_a")
            hide("lineup_sizes_a")
            hide("mu_t_a")
            hide("sigma_t_a")
            hide("cs_a")
            hide("pos_prop_a")
            
            hide("sim_seq_b")
            hide("lineup_sizes_b")
            hide("mu_t_b")
            hide("sigma_t_b")
            hide("cs_b")
            hide("pos_prop_b")
            
        } else {
            show("sim_seq_a")
            show("lineup_sizes_a")
            show("mu_t_a")
            show("sigma_t_a")
            show("cs_a")
            
            show("sim_seq_b")
            show("lineup_sizes_b")
            show("mu_t_b")
            show("sigma_t_b")
            show("cs_b")
            
            hide("eff_type")
            hide("effs")
        }
    })
    
    observeEvent(input$sim_seq_a, {
        if (input$sim_seq_a == "Simultaneous") {
            hide("pos_prop_a")
        } else {
            show("pos_prop_a")
        }
    })
    
    observeEvent(input$sim_seq_b, {
        if (input$sim_seq_a == "Simultaneous") {
            hide("pos_prop_b")
        } else {
            show("pos_prop_b")
        }
    })
    
    observeEvent(data_files$processed_data, {
        #parameters$cond1 = data_files$processed_data %>% 
        #    as.data.frame() %>% 
        #    select(cond) %>% 
        #    unique() %>% 
        #    slice(1) %>% 
        #    as.character()
        
        parameters$n_confs = length(unique(data_files$processed_data$conf_level))
        
        parameters$effs = 0
        
        parameters$effs_different = rep(0, times = parameters$n_confs)
        
        parameters$cond1 = as.character(levels(data_files$processed_data$cond)[1])
        
        parameters$cond2 = as.character(levels(data_files$processed_data$cond)[2])
        
        parameters$cond1_lineup_size = as.numeric(unique(data_files$processed_data$lineup_size[data_files$processed_data$cond == levels(as.factor(data_files$processed_data$cond))[1]]))
        
        parameters$cond2_lineup_size = as.numeric(unique(data_files$processed_data$lineup_size[data_files$processed_data$cond == levels(as.factor(data_files$processed_data$cond))[2]]))
        
        parameters$n_confs_a = length(unique(data_files$processed_data$conf_level[data_files$processed_data$cond == parameters$cond1]))
        
        parameters$max_conf_a = max(data_files$processed_data$conf_level[data_files$processed_data$cond == parameters$cond1], na.rm = TRUE)
        
        parameters$min_conf_a = min(data_files$processed_data$conf_level[data_files$processed_data$cond == parameters$cond1], na.rm = TRUE)
        
        parameters$n_confs_b = length(unique(data_files$processed_data$conf_level[data_files$processed_data$cond == parameters$cond2]))
        
        parameters$max_conf_b = max(data_files$processed_data$conf_level[data_files$processed_data$cond == parameters$cond2], na.rm = TRUE)
        
        parameters$min_conf_b = min(data_files$processed_data$conf_level[data_files$processed_data$cond == parameters$cond2], na.rm = TRUE)
        
        ## Figure out if lineups in one condition are sequential ----
        cond1_lineup_position = mean(data_files$processed_data$suspect_position[data_files$processed_data$cond == parameters$cond1],
                                     na.rm = TRUE)
        
        if (is.nan(cond1_lineup_position)) {
            updateRadioButtons(session,
                               "simultaneous_sequential_sim_a",
                               selected = "Simultaneous")
            
            updateRadioButtons(session,
                               "sim_seq_a",
                               selected = "Simultaneous")
        } else {
            updateRadioButtons(session,
                               "simultaneous_sequential_sim_a",
                               selected = "Sequential")
            
            updateRadioButtons(session,
                               "sim_seq_a",
                               selected = "Sequential")
        }
        
        cond2_lineup_position = mean(data_files$processed_data$suspect_position[data_files$processed_data$cond == parameters$cond2],
                                     na.rm = TRUE)
        
        if (is.nan(cond2_lineup_position)) {
            updateRadioButtons(session,
                               "simultaneous_sequential_sim_b",
                               selected = "Simultaneous")
            
            updateRadioButtons(session,
                               "sim_seq_b",
                               selected = "Simultaneous")
        } else {
            updateRadioButtons(session,
                               "simultaneous_sequential_sim_b",
                               selected = "Sequential")
            
            updateRadioButtons(session,
                               "sim_seq_b",
                               selected = "Sequential")
        }
        
        output$cond1_label = renderText({parameters$cond1})
        
        output$cond2_label = renderText({parameters$cond2})
        
        ## Update the values for lineup size if there is no designated innocent suspect ----
        #updateNumericInput(session,
        #                   "lineup_size_1",
        #                   value = parameters$cond1_lineup_size)
        #
        #updateNumericInput(session,
        #                   "lineup_size_2",
        #                   value = parameters$cond2_lineup_size)
        
        ## Update the values for lineup size for estimating SDT parameters from data ----
        updateNumericInput(session,
                           "lineup_size_sim_a",
                           value = parameters$cond1_lineup_size)
        
        updateNumericInput(session,
                           "lineup_size_sim_b",
                           value = parameters$cond2_lineup_size)
        
        ## Update the values for lineup sizes in the simulation parameters tab ----
        updateNumericInput(session,
                           "lineup_sizes_a",
                           value = parameters$cond1_lineup_size)
        
        updateNumericInput(session,
                           "lineup_sizes_b",
                           value = parameters$cond2_lineup_size)
    })
    
    #observeEvent(input$roc_paired, {
    #    if (input$roc_paired == "Yes") {
    #        parameters$roc_paired = TRUE
    #    } else {
    #        parameters$roc_paired = FALSE
    #    }
    #})
    
    ## reactive plots ----
    plots = reactiveValues(hypothetical_plot = NULL,
                           hypothetical_ROC_plot = NULL,
                           pwr_plot = NULL,
                           previous_sim_plot = NULL)
    
    ## other reactive variables ----
    other_vars = reactiveValues(sim_counter = 0,
                                sim_total = NA,
                                time_taken = NA,
                                start_time = NA,
                                end_time = NA,
                                avg_n = NA,
                                end_time_est = NA,
                                duration_est = NA,
                                custom_trunc = NA,
                                sims_complete = 0,
                                #n_confs_report = NA,
                                effs_report = NA,
                                effs_different_report = NA)
    
    ### number of lineups ----
    observeEvent(input$n_total_lineups, {
        
        message(sprintf("%s TA lineups, %s TP lineups",
                        input$n_TA_lineups,
                        input$n_TP_lineups))
        
        if (input$n_total_lineups == 1) {
            hide("n_TA_lineups")
            hide("n_TP_lineups")
            updateNumericInput(session,
                               "n_TA_lineups",
                               value = .5)
            updateNumericInput(session,
                               "n_TP_lineups",
                               value = .5)
        } else {
            show("n_TA_lineups")
            show("n_TP_lineups")
            updateNumericInput(session,
                               "n_TA_lineups",
                               value = input$n_total_lineups/2)
            updateNumericInput(session,
                               "n_TP_lineups",
                               value = input$n_total_lineups/2)
        }
        

    })
    
    ### measure to include ----
    observeEvent(input$measure, {
        if (input$measure == "DPP") {
            hide("roc_trunc")
        } else {
            show("roc_trunc")
        }
    })
    
    ### update bootstrap iterations depending on whether DPP is to be computed ----
    observeEvent(c(input$measure, input$n_lineups, input$ns), {
        #req(input$ns)
        #req(input$n_lineups)
        
        max_n = max(unique(extract(input$ns)))
        max_boot_iter = max_n * input$n_total_lineups
        
        if (input$measure != "pAUC") {
            updateNumericInput(session,
                               "nboot_iter",
                               value = max_boot_iter)
        }
    })
    
    ### one- or two-tailed test ----
    output$test_tails = renderUI({
        
        if(!is.null(data_files$processed_data) & input$empirical_theoretical == "Data") {
            opt = c("Two-tailed" = "2_tail",
                    sprintf("%s > %s",
                            parameters$cond1,
                            parameters$cond2),
                    sprintf("%s > %s",
                            parameters$cond2,
                            parameters$cond1))
        } else {
            opt = c("Two-tailed" = "2_tail",
                    sprintf("%s > %s",
                            "A",
                            "B"),
                    sprintf("%s > %s",
                            "B",
                            "A"))
        }
        
        
                #sprintf("%s > %s",
                #        unique(data_files$processed_data$cond)[1],
                #        unique(data_files$processed_data$cond)[2]) = "A_B",
                #sprintf("%s > %s",
                #        unique(data_files$processed_data$cond)[2],
                #        unique(data_files$processed_data$cond)[1]) = "B_A")
        label = "Two-tailed or one-tailed?"
        
        radioButtons("test_tails", label, opt, 
                     selected = "2_tail")
    })
    
    observeEvent(input$test_tails, {
        message(input$test_tails)
    })
    
    ### effect sizes ----
    observeEvent(c(input$effs), {
        req(input$effs)
        
        parameters$effs = 
            unique(extract(input$effs))
    })
    
    observeEvent(c(input$effs_different), {
        req(input$effs_different)
        
        parameters$effs_different = 
            extract(input$effs_different)
    })
    
    #### show/hide effect size input based on effect size type selected ----
    observeEvent(input$eff_type, {
        
        req(data_files$processed_data)
        
        if (input$eff_type == "constant") {
            hide("effs_different")
            show("effs")
            
            parameters$effs_different = rep(0, times = parameters$n_confs)
            
            updateTextInput(session,
                            "effs_different",
                            value = parameters$effs_different)
            
        } else {
            show("effs_different")
            hide("effs")
            
            parameters$effs = 0
            
            updateTextInput(session,
                            "effs",
                            value = parameters$effs)
        }
    })
    
    output$n_confs_message = renderText({
        
        if (input$eff_type == "constant") {
            ""
        } else {
            sprintf("Your data has %i confidence levels",
                    parameters$n_confs) 
        }
    })
    
    #output$effs = renderText({
    #    parameters$effs
    #})
    
    ### ns ----
    observeEvent(c(input$ns), {
        req(input$ns)
        
        parameters$ns = 
            unique(extract(input$ns))
        
        other_vars$avg_n = mean(parameters$ns)
    })
    
    ### Custom ROC truncation ----
    observeEvent(input$roc_trunc, {
        if (input$roc_trunc == "Custom") {
            show("custom_trunc") 
        } else {
            hide("custom_trunc")
        }
    })
    
    observeEvent(input$custom_trunc, {
        other_vars$custom_trunc = input$custom_trunc
    })
    
    ### generate hypothetical ROCs before simulation ----
    observeEvent(input$generate_hypothetical, {
        
        parameters$effs = 0
        
        if (input$empirical_theoretical == "Data") {
            req(data_files$processed_data)
            req(parameters$effs)
            req(parameters$ns)
            
            if (length(parameters$effs_different) != parameters$n_confs) {
                showModal(modalDialog(
                    title = "Warning",
                    "# of entered effect sizes does not match # of confidence levels in the data. 
                Please enter the correct # of effect sizes."
                ))
                
                data_files$conf_effs_data = data.frame(
                    conf_level = unique(data_files$processed_data$conf_level)) %>% 
                    mutate(conf_level_rev = max(conf_level)+1 - conf_level) %>% 
                    arrange(conf_level) %>% 
                    mutate(conf_effs = 1)
                
                hide("sim_start")
            } else {
                data_files$conf_effs_data = data.frame(
                    conf_level = unique(data_files$processed_data$conf_level)) %>% 
                    mutate(conf_level_rev = max(conf_level)+1 - conf_level) %>% 
                    arrange(conf_level) %>% 
                    mutate(conf_effs = parameters$effs_different)
                
                show("sim_start")
            }
            
            #### getting proportion data from each condition ----
            #data_props = open_data %>% 
            #    filter(exp == "Akan et al. (2021): Exp 1: Showup vs. 6-person") %>%
            #    group_by(id_type, culprit_present, cond) %>% 
            #    count() %>% 
            #    ungroup() %>% 
            #    group_by(culprit_present, cond) %>% 
            #    mutate(total = sum(n),
            #           prop = n/total,
            #           cond = as.factor(cond)) %>% 
            #    ungroup()
            
            data_props = data_files$processed_data %>%
                group_by(id_type, culprit_present, cond) %>% 
                count() %>% 
                ungroup() %>% 
                group_by(culprit_present, cond) %>% 
                mutate(total = sum(n),
                       prop = n/total,
                       cond = as.factor(cond)) %>% 
                ungroup()
            
            message("Processed proportion data")
            
            ##### Getting TA & TP suspect proportions for Condition 1 ----
            cond1_TA_susp_prop = data_props %>% 
                filter(cond == levels(data_props$cond)[1] &
                           culprit_present == "absent" & 
                           id_type == "suspect") %>% 
                dplyr::select(prop) %>% 
                as.numeric()
            
            cond1_TP_susp_prop = data_props %>% 
                filter(cond == levels(data_props$cond)[1] &
                           culprit_present == "present" & 
                           id_type == "suspect") %>% 
                dplyr::select(prop) %>% 
                as.numeric()
            
            ##### Getting TA & TP suspect proportions for Condition 2 ----
            cond2_TA_susp_prop = data_props %>% 
                filter(cond == levels(data_props$cond)[2] &
                           culprit_present == "absent" & 
                           id_type == "suspect") %>% 
                dplyr::select(prop) %>% 
                as.numeric()
            
            cond2_TP_susp_prop = data_props %>% 
                filter(cond == levels(data_props$cond)[2] &
                           culprit_present == "present" & 
                           id_type == "suspect") %>% 
                dplyr::select(prop) %>% 
                as.numeric()
            
            message("Processed data for both conditions")
            
            #### Getting responses at each confidence level for both conditions ----
            data_original = data_files$processed_data %>%
                #mutate(conf_level = as.factor(conf_level)) %>% 
                group_by(id_type, conf_level_rev, culprit_present, cond) %>% 
                count() %>% 
                ungroup() %>% 
                group_by(culprit_present, cond) %>% 
                mutate(total = sum(n),
                       prop = n/total) %>% 
                ungroup() %>%
                filter(id_type == "suspect")
            
            message("Data processing complete")
            message(data_original)
            
            ROC_data = data.frame(prop = rep(NA, times = length(unique(data_original$cond))*
                                                 length(unique(data_original$culprit_present))*
                                                 length(unique(data_original$conf_level_rev))*
                                                 length(parameters$effs)),
                                  cond = NA,
                                  presence = NA,
                                  criteria = NA,
                                  eff = NA)
            
            row = 1
            
            message("Created empty ROC store object for hypothetical plot")
            
            for (g in 1:length(parameters$effs)) {
                data = data_original %>% 
                    left_join(data_files$conf_effs_data)
                
                eff = parameters$effs[g]
                
                for (h in 1:nrow(data)) {
                    if (data$culprit_present[h] == "present" & data$cond[h] == levels(data$cond)[2]) {
                        data$n[h] = round(data$n[h] * (eff + 1) * (data$conf_effs[h] + 1))
                    } else {
                        data$n[h] = data$n[h]
                    }
                }
                
                data$prop = data$n / data$total
                
                for (i in 1:length(unique(data$cond))) {
                    curr_cond = levels(data$cond)[i]
                    for (j in 1:length(unique(data$culprit_present))) {
                        curr_present = unique(data$culprit_present)[j]
                        for (k in 1:length(unique(data$conf_level_rev))) {
                            curr_conf = unique(data$conf_level_rev)[k]
                            curr_resps = sum(data$prop[data$cond == curr_cond &
                                                           data$culprit_present == curr_present &
                                                           data$conf_level_rev %in% c(1:curr_conf)])
                            
                            ROC_data$cond[row] = curr_cond
                            ROC_data$presence[row] = curr_present
                            ROC_data$prop[row] = curr_resps
                            ROC_data$criteria[row] = curr_conf
                            ROC_data$eff[row] = eff
                            row = row + 1
                        }
                    }
                }
            }
            
            message("Populated ROC store object")
            
            if (max(ROC_data$prop) > 1) {
                showModal(modalDialog(
                    title = "Warning",
                    "One or more effect sizes results in a correct ID proportion > 1. 
                Change the maximum effect size(s) or the direction of the effect sizes to be tested"
                ))
                
                hide("sim_start")
            } else if (min(parameters$effs) < -1) {
                showModal(modalDialog(
                    title = "Warning",
                    "One or more effect sizes is below -1. 
                Please ensure that all effect sizes are greater than or equal to -1"
                ))
                
                hide("sim_start")
            } else {
                show("sim_start")
            }
            
            ROC_data_wide = spread(ROC_data,
                                   key = "presence",
                                   value = "prop")  %>% 
                rbind(data.frame(cond = rep(c(parameters$cond1, 
                                              parameters$cond2), 
                                            each = length(parameters$effs)),
                                 criteria = NA,
                                 eff = rep(parameters$effs, times = length(unique(data_original$cond))),
                                 present = 0,
                                 absent = 0)) #%>% 
            #mutate(present = ifelse(present < 0, 0,
            #                        ifelse(present > 1, 1, present)))
            
            
            max_criteria = as.numeric(length(unique(ROC_data_wide$criteria[!is.na(ROC_data_wide$criteria)])))
            
            if (input$roc_trunc == "Lowest false ID rate") {
                #partial_threshold = ROC_data_wide %>% 
                #    filter(criteria == max_criteria) %>% 
                #    select(absent) %>% 
                #    min() 
                
                partial_threshold = min(
                    data_props$prop[data_props$id_type == "suspect" &
                                        data_props$culprit_present == "absent" &
                                        data_props$cond == levels(data_props$cond)[1]],
                    data_props$prop[data_props$id_type == "suspect" &
                                        data_props$culprit_present == "absent" &
                                        data_props$cond == levels(data_props$cond)[2]]
                )
                
            } else if (input$roc_trunc == "Highest false ID rate") {
                #partial_threshold = ROC_data_wide %>% 
                #    filter(criteria == max_criteria) %>% 
                #    select(absent) %>% 
                #    max()
                
                partial_threshold = max(
                    data_props$prop[data_props$id_type == "suspect" &
                                        data_props$culprit_present == "absent" &
                                        data_props$cond == levels(data_props$cond)[1]],
                    data_props$prop[data_props$id_type == "suspect" &
                                        data_props$culprit_present == "absent" &
                                        data_props$cond == levels(data_props$cond)[2]]
                )
                
            } else {
                partial_threshold = 1 - other_vars$custom_trunc
            }
            
            message("Created data for plotting")
            message(ROC_data_wide)
            
            
            ROC_data_plot = ROC_data_wide %>% 
                ggplot(aes(x = absent, y = present, color = cond))+
                           #linetype = as.factor(eff)))+
                geom_point(alpha = .5)+
                geom_line()+
                geom_vline(xintercept = partial_threshold)+
                apatheme+
                labs(x = "\nFalse ID rate",
                     y = "Correct ID rate\n",
                     #linetype = "Effect",
                     color = "Condition")+
                theme(text = element_text(size = 20))
            
            plots$hypothetical_plot = ROC_data_plot
            
            output$ROC_data_plot = renderPlot({
                ROC_data_plot
            })
        
        # If simulating from SDT parameters ----
        } else {
            ### Required input values for simulation ----
            req(input$sim_seq_a)
            req(input$lineup_sizes_a)
            req(input$mu_t_a)
            req(input$sigma_t_a)
            req(input$cs_a)
            
            req(input$sim_seq_b)
            req(input$lineup_sizes_b)
            req(input$mu_t_b)
            req(input$sigma_t_b)
            req(input$cs_b)
            
            req(parameters$ns)
            
            ### Set parameter values ----
            parameters$sim_seq_a = input$sim_seq_a
            parameters$lineup_sizes_a = input$lineup_sizes_a
            parameters$mu_t_a = input$mu_t_a
            parameters$sigma_t_a = input$sigma_t_a
            parameters$cs_a = extract(input$cs_a)

            parameters$sim_seq_b = input$sim_seq_b
            parameters$lineup_sizes_b = input$lineup_sizes_b
            parameters$mu_t_b = input$mu_t_b
            parameters$sigma_t_b = input$sigma_t_b
            parameters$cs_b = extract(input$cs_b)
            
            parameters$effs = 0
            
            #### Set the probability of target presence (based on # of TP and total lineups) ----
            p = input$n_TP_lineups / input$n_total_lineups
            
            #### Set the # of datasets to simulate for the hypothetical ROC curves ----
            n_sims = 1
            
            #### Set the # of trials to estimate (using the maximum N provided) ----
            n_trials = max(parameters$ns)
            
            #### Put parameter values into vectors ----
            params_a = c(p, parameters$mu_t_a, parameters$sigma_t_a, parameters$cs_a)
            params_b = c(p, parameters$mu_t_b, parameters$sigma_t_b, parameters$cs_b)
            
            message(params_a)
            message(params_b)
            
            message("Generated SDT parameter values prior to simulation")
             
            ### If one or both conditions are Sequential, require pos_prop variables (and check these against lineup size) ----
            #### Generate simulated data from SDT models ----
            ##### Condition A ----
            if (input$sim_seq_a == "Sequential") {
                req(input$pos_prop_a)
                
                parameters$pos_prop_a = extract(input$pos_prop_a)
                
                ###### Check if the probability vector is same length as lineup size ----
                if (length(parameters$pos_prop_a) != parameters$lineup_sizes_a) {
                    showModal(modalDialog(
                        title = "Warning",
                        "# of entered sequential probabilities does not match lineup size."
                    ))
                    
                    parameters$pos_prop_a = c(rep(1/parameters$lineup_sizes_a), times = lineup_sizes_a)

                    hide("sim_start")
                } else {
                    simmed_data_a = as.data.frame(t(as.data.frame(sdtlu_seq_sim(params_a, parameters$lineup_sizes_a, n_trials, n_sims, pos_prop = parameters$pos_prop_a))))
                    simmed_data_a$id_type = rep(c(rep("suspect", length(parameters$cs_a)),
                                                  rep("filler", length(parameters$cs_a)),
                                                  "reject"), times = 2)
                    simmed_data_a$conf_level_rev = rep(c(1:length(parameters$cs_a), 1:length(parameters$cs_a), NA),
                                                       times = 2)
                    simmed_data_a$conf_level = rep(c(length(parameters$cs_a):1, length(parameters$cs_a):1, NA),
                                                   times = 2)
                    simmed_data_a$culprit_present = c(rep("present", times = length(parameters$cs_a)*2+1),
                                               rep("absent", times = length(parameters$cs_a)*2+1))
                    
                    simmed_data_a_TP_rej = data.frame()
                    
                    for (i in 1:length(parameters$cs_a)) {
                        simmed_data_a_TP_rej = rbind(simmed_data_a_TP_rej,
                                                     filter(simmed_data_a, id_type == "reject" & culprit_present == "present"))
                    }
                    
                    simmed_data_a_TP_rej$conf_level_rev = 1:length(parameters$cs_a)
                    simmed_data_a_TP_rej$conf_level = length(parameters$cs_a):1
                    simmed_data_a_TP_rej$V1 = round(simmed_data_a_TP_rej$V1/length(parameters$cs_a))
                    
                    simmed_data_a_TA_rej = data.frame()
                    
                    for (i in 1:length(parameters$cs_a)) {
                        simmed_data_a_TA_rej = rbind(simmed_data_a_TA_rej,
                                                     filter(simmed_data_a, id_type == "reject" & culprit_present == "absent"))
                    }
                    
                    simmed_data_a_TA_rej$conf_level_rev = 1:length(parameters$cs_a)
                    simmed_data_a_TA_rej$conf_level = length(parameters$cs_a):1
                    simmed_data_a_TA_rej$V1 = round(simmed_data_a_TA_rej$V1/length(parameters$cs_a))
                    
                    simmed_data_a_final = filter(rbind(simmed_data_a,
                                                       simmed_data_a_TP_rej,
                                                       simmed_data_a_TA_rej),
                                                 !is.na(conf_level))
                    simmed_data_a_final$cond = "A"
                }
            } else {
                simmed_data_a = as.data.frame(t(as.data.frame(sdtlu_sim_sim(params_a, parameters$lineup_sizes_a, n_trials, n_sims))))
                simmed_data_a$id_type = rep(c(rep("suspect", length(parameters$cs_a)),
                                              rep("filler", length(parameters$cs_a)),
                                              "reject"), times = 2)
                
                simmed_data_a$conf_level_rev = rep(c(1:length(parameters$cs_a), 1:length(parameters$cs_a), NA),
                                                   times = 2)
                simmed_data_a$conf_level = rep(c(length(parameters$cs_a):1, length(parameters$cs_a):1, NA),
                                               times = 2)
                simmed_data_a$culprit_present = c(rep("present", times = length(parameters$cs_a)*2+1),
                                           rep("absent", times = length(parameters$cs_a)*2+1))
                
                simmed_data_a_TP_rej = data.frame()
                
                for (i in 1:length(parameters$cs_a)) {
                    simmed_data_a_TP_rej = rbind(simmed_data_a_TP_rej,
                                                 filter(simmed_data_a, id_type == "reject" & culprit_present == "present"))
                }
                
                simmed_data_a_TP_rej$conf_level_rev = 1:length(parameters$cs_a)
                simmed_data_a_TP_rej$conf_level = length(parameters$cs_a):1
                simmed_data_a_TP_rej$V1 = round(simmed_data_a_TP_rej$V1/length(parameters$cs_a))
                
                simmed_data_a_TA_rej = data.frame()
                
                for (i in 1:length(parameters$cs_a)) {
                    simmed_data_a_TA_rej = rbind(simmed_data_a_TA_rej,
                                                 filter(simmed_data_a, id_type == "reject" & culprit_present == "absent"))
                }
                
                simmed_data_a_TA_rej$conf_level_rev = 1:length(parameters$cs_a)
                simmed_data_a_TA_rej$conf_level = length(parameters$cs_a):1
                simmed_data_a_TA_rej$V1 = round(simmed_data_a_TA_rej$V1/length(parameters$cs_a))
                
                simmed_data_a_final = filter(rbind(simmed_data_a,
                                                   simmed_data_a_TP_rej,
                                                   simmed_data_a_TA_rej),
                                             !is.na(conf_level))
                simmed_data_a_final$cond = "A"
            }
            
            ##### Condition B ----
            if (input$sim_seq_b == "Sequential") {
                req(input$pos_prop_b)
                
                parameters$pos_prop_b = extract(input$pos_prop_b)
                
                ###### Check if the probability vector is same length as lineup size ----
                if (length(parameters$pos_prop_b) != parameters$lineup_sizes_b) {
                    showModal(modalDialog(
                        title = "Warning",
                        "# of entered sequential probabilities does not match lineup size."
                    ))
                    
                    parameters$pos_prop_b = c(rep(1/parameters$lineup_sizes_b), times = lineup_sizes_b)
                    
                    hide("sim_start")
                } else {
                    simmed_data_b = as.data.frame(t(as.data.frame(sdtlu_seq_sim(params_b, parameters$lineup_sizes_b, n_trials, n_sims, pos_prop = parameters$pos_prop_b))))
                    simmed_data_b$id_type = rep(c(rep("suspect", length(parameters$cs_b)),
                                                  rep("filler", length(parameters$cs_b)),
                                                  "reject"), times = 2)
                    simmed_data_b$conf_level_rev = rep(c(1:length(parameters$cs_b), 1:length(parameters$cs_b), NA),
                                                       times = 2)
                    simmed_data_b$conf_level = rep(c(length(parameters$cs_b):1, length(parameters$cs_b):1, NA),
                                                   times = 2)
                    simmed_data_b$culprit_present = c(rep("present", times = length(parameters$cs_b)*2+1),
                                               rep("absent", times = length(parameters$cs_b)*2+1))
                    
                    simmed_data_b_TP_rej = data.frame()
                    
                    for (i in 1:length(parameters$cs_b)) {
                        simmed_data_b_TP_rej = rbind(simmed_data_b_TP_rej,
                                                     filter(simmed_data_b, id_type == "reject" & culprit_present == "present"))
                    }
                    
                    simmed_data_b_TP_rej$conf_level_rev = 1:length(parameters$cs_b)
                    simmed_data_b_TP_rej$conf_level = length(parameters$cs_b):1
                    simmed_data_b_TP_rej$V1 = round(simmed_data_b_TP_rej$V1/length(parameters$cs_b))
                    
                    simmed_data_b_TA_rej = data.frame()
                    
                    for (i in 1:length(parameters$cs_b)) {
                        simmed_data_b_TA_rej = rbind(simmed_data_b_TA_rej,
                                                     filter(simmed_data_b, id_type == "reject" & culprit_present == "absent"))
                    }
                    
                    simmed_data_b_TA_rej$conf_level_rev = 1:length(parameters$cs_b)
                    simmed_data_b_TA_rej$conf_level = length(parameters$cs_b):1
                    simmed_data_b_TA_rej$V1 = round(simmed_data_b_TA_rej$V1/length(parameters$cs_b))
                    
                    simmed_data_b_final = filter(rbind(simmed_data_b,
                                                       simmed_data_b_TP_rej,
                                                       simmed_data_b_TA_rej),
                                                 !is.na(conf_level))
                    simmed_data_b_final$cond = "B"
                }
            } else {
                simmed_data_b = as.data.frame(t(as.data.frame(sdtlu_sim_sim(params_b, parameters$lineup_sizes_b, n_trials, n_sims))))
                simmed_data_b$id_type = rep(c(rep("suspect", length(parameters$cs_b)),
                                              rep("filler", length(parameters$cs_b)),
                                              "reject"), times = 2)
                simmed_data_b$conf_level_rev = rep(c(1:length(parameters$cs_b), 1:length(parameters$cs_b), NA),
                                                   times = 2)
                simmed_data_b$conf_level = rep(c(length(parameters$cs_b):1, length(parameters$cs_b):1, NA),
                                               times = 2)
                simmed_data_b$culprit_present = c(rep("present", times = length(parameters$cs_b)*2+1),
                                           rep("absent", times = length(parameters$cs_b)*2+1))
                
                simmed_data_b_TP_rej = data.frame()
                
                for (i in 1:length(parameters$cs_b)) {
                    simmed_data_b_TP_rej = rbind(simmed_data_b_TP_rej,
                                                 filter(simmed_data_b, id_type == "reject" & culprit_present == "present"))
                }
                
                simmed_data_b_TP_rej$conf_level_rev = 1:length(parameters$cs_b)
                simmed_data_b_TP_rej$conf_level = length(parameters$cs_b):1
                simmed_data_b_TP_rej$V1 = round(simmed_data_b_TP_rej$V1/length(parameters$cs_b))
                
                simmed_data_b_TA_rej = data.frame()
                
                for (i in 1:length(parameters$cs_b)) {
                    simmed_data_b_TA_rej = rbind(simmed_data_b_TA_rej,
                                                 filter(simmed_data_b, id_type == "reject" & culprit_present == "absent"))
                }
                
                simmed_data_b_TA_rej$conf_level_rev = 1:length(parameters$cs_b)
                simmed_data_b_TA_rej$conf_level = length(parameters$cs_b):1
                simmed_data_b_TA_rej$V1 = round(simmed_data_b_TA_rej$V1/length(parameters$cs_b))
                
                simmed_data_b_final = filter(rbind(simmed_data_b,
                                                   simmed_data_b_TP_rej,
                                                   simmed_data_b_TA_rej),
                                             !is.na(conf_level))
                simmed_data_b_final$cond = "B"
            }
            
            #### Combine Condition A & B data ----
            simmed_data = rbind(simmed_data_a_final,
                                simmed_data_b_final)
            
            #### Generate trial-level data ----
            simmed_data_trial = data.frame()
            
            for (i in 1:nrow(simmed_data)) {
                simmed_data_slice = simmed_data[i,]
                simmed_data_append = do.call("rbind", replicate(simmed_data_slice$V1, simmed_data_slice, simplify = FALSE))
                simmed_data_trial = rbind(simmed_data_trial,
                                          simmed_data_append)    
            }
            
            simmed_data_trial$cond = factor(simmed_data_trial$cond,
                                            levels = c("A", "B"))
            
            data_files$sdtlu_hypothetical_data = simmed_data_trial
            
            message("Created hypothetical sdtlu data")
            
            data_files$conf_effs_data = data.frame(
                conf_level = unique(data_files$sdtlu_hypothetical_data$conf_level)) %>% 
                mutate(conf_level_rev = max(conf_level)+1 - conf_level) %>% 
                arrange(conf_level) %>% 
                mutate(conf_effs = 0)
            
            #### getting proportion data from each condition ----
            #data_props = open_data %>% 
            #    filter(exp == "Akan et al. (2021): Exp 1: Showup vs. 6-person") %>%
            #    group_by(id_type, culprit_present, cond) %>% 
            #    count() %>% 
            #    ungroup() %>% 
            #    group_by(culprit_present, cond) %>% 
            #    mutate(total = sum(n),
            #           prop = n/total,
            #           cond = as.factor(cond)) %>% 
            #    ungroup()
            
            data_props = data_files$sdtlu_hypothetical_data %>%
                group_by(id_type, culprit_present, cond) %>% 
                count() %>% 
                ungroup() %>% 
                group_by(culprit_present, cond) %>% 
                mutate(total = sum(n),
                       prop = n/total,
                       cond = as.factor(cond)) %>% 
                ungroup()
            
            message("Processed proportion data")
            
            ##### Getting TA & TP suspect proportions for Condition 1 ----
            cond1_TA_susp_prop = data_props %>% 
                filter(cond == levels(data_props$cond)[1] &
                           culprit_present == "absent" & 
                           id_type == "suspect") %>% 
                dplyr::select(prop) %>% 
                as.numeric()
            
            cond1_TP_susp_prop = data_props %>% 
                filter(cond == levels(data_props$cond)[1] &
                           culprit_present == "present" & 
                           id_type == "suspect") %>% 
                dplyr::select(prop) %>% 
                as.numeric()
            
            ##### Getting TA & TP suspect proportions for Condition 2 ----
            cond2_TA_susp_prop = data_props %>% 
                filter(cond == levels(data_props$cond)[2] &
                           culprit_present == "absent" & 
                           id_type == "suspect") %>% 
                dplyr::select(prop) %>% 
                as.numeric()
            
            cond2_TP_susp_prop = data_props %>% 
                filter(cond == levels(data_props$cond)[2] &
                           culprit_present == "present" & 
                           id_type == "suspect") %>% 
                dplyr::select(prop) %>% 
                as.numeric()
            
            message("Processed data for both conditions")
            
            #### Getting responses at each confidence level for both conditions ----
            data_original = data_files$sdtlu_hypothetical_data %>%
                #mutate(conf_level = as.factor(conf_level)) %>% 
                group_by(id_type, conf_level_rev, culprit_present, cond) %>% 
                count() %>% 
                ungroup() %>% 
                group_by(culprit_present, cond) %>% 
                mutate(total = sum(n),
                       prop = n/total) %>% 
                ungroup() %>%
                filter(id_type == "suspect")
            
            message("Data processing complete")
            message(data_original)
            
            ROC_data = data.frame(prop = rep(NA, times = length(unique(data_original$cond))*
                                                 length(unique(data_original$culprit_present))*
                                                 length(unique(data_original$conf_level_rev))*
                                                 length(parameters$effs)),
                                  cond = NA,
                                  presence = NA,
                                  criteria = NA,
                                  eff = NA)
            
            row = 1
            
            message("Created empty ROC store object for hypothetical plot")
            
            for (g in 1:length(parameters$effs)) {
                data = data_original %>% 
                    left_join(data_files$conf_effs_data)
                
                eff = parameters$effs[g]
                
                message(data)
                
                for (h in 1:nrow(data)) {
                    data$n[h] = data$n[h]
                }
                
                message("Passed first loop")
                
                data$prop = data$n / data$total
                
                for (i in 1:length(unique(data$cond))) {
                    curr_cond = levels(data$cond)[i]
                    for (j in 1:length(unique(data$culprit_present))) {
                        curr_present = unique(data$culprit_present)[j]
                        for (k in 1:length(unique(data$conf_level_rev))) {
                            curr_conf = unique(data$conf_level_rev)[k]
                            curr_resps = sum(data$prop[data$cond == curr_cond &
                                                           data$culprit_present == curr_present &
                                                           data$conf_level_rev %in% c(1:curr_conf)])
                            
                            ROC_data$cond[row] = curr_cond
                            ROC_data$presence[row] = curr_present
                            ROC_data$prop[row] = curr_resps
                            ROC_data$criteria[row] = curr_conf
                            ROC_data$eff[row] = eff
                            row = row + 1
                        }
                    }
                }
            }
            
            message("Populated ROC store object")
            
            if (max(ROC_data$prop) > 1) {
                showModal(modalDialog(
                    title = "Warning",
                    "One or more effect sizes results in a correct ID proportion > 1. 
                Change the maximum effect size(s) or the direction of the effect sizes to be tested"
                ))
                
                hide("sim_start")
            } else if (min(parameters$effs) < -1) {
                showModal(modalDialog(
                    title = "Warning",
                    "One or more effect sizes is below -1. 
                Please ensure that all effect sizes are greater than or equal to -1"
                ))
                
                hide("sim_start")
            } else {
                show("sim_start")
            }
            
            ROC_data_wide = spread(ROC_data,
                                   key = "presence",
                                   value = "prop")  %>% 
                rbind(data.frame(cond = rep(c("A", 
                                              "B"), 
                                            each = length(parameters$effs)),
                                 criteria = NA,
                                 eff = rep(parameters$effs, times = length(unique(data_original$cond))),
                                 present = 0,
                                 absent = 0)) #%>% 
            #mutate(present = ifelse(present < 0, 0,
            #                        ifelse(present > 1, 1, present)))
            
            
            max_criteria = as.numeric(length(unique(ROC_data_wide$criteria[!is.na(ROC_data_wide$criteria)])))
            
            if (input$roc_trunc == "Lowest false ID rate") {
                #partial_threshold = ROC_data_wide %>% 
                #    filter(criteria == max_criteria) %>% 
                #    select(absent) %>% 
                #    min() 
                
                partial_threshold = min(
                    data_props$prop[data_props$id_type == "suspect" &
                                        data_props$culprit_present == "absent" &
                                        data_props$cond == levels(data_props$cond)[1]],
                    data_props$prop[data_props$id_type == "suspect" &
                                        data_props$culprit_present == "absent" &
                                        data_props$cond == levels(data_props$cond)[2]]
                )
                
            } else if (input$roc_trunc == "Highest false ID rate") {
                #partial_threshold = ROC_data_wide %>% 
                #    filter(criteria == max_criteria) %>% 
                #    select(absent) %>% 
                #    max()
                
                partial_threshold = max(
                    data_props$prop[data_props$id_type == "suspect" &
                                        data_props$culprit_present == "absent" &
                                        data_props$cond == levels(data_props$cond)[1]],
                    data_props$prop[data_props$id_type == "suspect" &
                                        data_props$culprit_present == "absent" &
                                        data_props$cond == levels(data_props$cond)[2]]
                )
                
            } else {
                partial_threshold = 1 - other_vars$custom_trunc
            }
            
            message("Created data for plotting")
            message(ROC_data_wide)
            
            
            ROC_data_plot = ROC_data_wide %>% 
                ggplot(aes(x = absent, y = present, color = cond))+ 
                           #linetype = as.factor(eff)))+
                geom_point(alpha = .5)+
                geom_line()+
                geom_vline(xintercept = partial_threshold)+
                apatheme+
                labs(x = "\nFalse ID rate",
                     y = "Correct ID rate\n",
                     #linetype = "Effect",
                     color = "Condition")+
                theme(text = element_text(size = 20))
            
            plots$hypothetical_plot = ROC_data_plot
            
            output$ROC_data_plot = renderPlot({
                ROC_data_plot
        })
    }
})
    
    ## estimating SDT parameters from uploaded data ----
    observeEvent(input$get_sdtlu, {
        req(input$simultaneous_sequential_sim_a,
            input$simultaneous_sequential_sim_b,
            input$lineup_size_sim_a,
            input$lineup_size_sim_b,
            data_files$processed_data)
        
        showModal(modalDialog(HTML(sprintf("Estimating parameters, please wait... <br/>If this takes longer than ~5 minutes it is possible that the data are not suitable for modelling.")),
                              fade = TRUE,
                              easyClose = FALSE,
                              size = "l"))
        
        ### Estimates for Condition A ----
        #### Process data ----
        data_sdtlu_a = data_files$processed_data %>% 
            filter(cond == parameters$cond1) %>% 
            mutate(lineup_size = input$lineup_size_sim_a,
                   conf_level = (max(conf_level)+1) - conf_level)
        
        data_sdtlu_processed_a = sdtlu_process_data(data_sdtlu_a)
        
        #### If lineup type is sequential, get the position probabilities ----
        #### Set fitting options ----
        if (input$simultaneous_sequential_sim_a == "Simultaneous") {
            options_a = list(model_type = "sim",
                             fit_fcn = "G2",
                             fix_p = "data",
                             fix_sigma_t = "free",
                             use_restr_data = FALSE,
                             run_bootstrap = FALSE)
            
            est_pos_prop_a = NA
        } else {
            options_a = list(model_type = "seq",
                             fit_fcn = "G2",
                             fix_p = "data",
                             fix_sigma_t = "free",
                             use_restr_data = FALSE,
                             run_bootstrap = FALSE)
            
            est_pos_prop_a = as.character(paste0(data_sdtlu_processed_a$pos_prop, collapse = ", "))
        }
        
        #### Fit the model ----
        sdtlu_fit_a = sdtlu_fit(data_sdtlu_processed_a,
                                options = options_a)
        
        #### Recover the estimates ----
        params_a = sdtlu_fit_a$best_params_full
        
        cs_a = params_a[4:length(params_a)]
        
        params_data_a = data.frame(
            Parameter = c("p",
                          "mu_t",
                          "sigma_t",
                          "cs",
                          "pos_prop"),
            vals_a = c(
                params_a[1],
                params_a[2],
                params_a[3],
                paste(cs_a, collapse = ", "),
                est_pos_prop_a
            )
        )
        
        #### Add the estimates to the simulation parameters tab ----
        updateRadioButtons(session,
                           "sim_seq_a",
                           selected = input$simultaneous_sequential_sim_a)
        
        updateNumericInput(session,
                        "lineup_sizes_a",
                        value = input$lineup_size_sim_a)
        
        updateNumericInput(session,
                           "mu_t_a",
                           value = params_a[2])
        
        updateNumericInput(session,
                           "sigma_t_a",
                           value = params_a[3])
        
        updateTextInput(session,
                        "cs_a",
                        value = paste(cs_a, collapse = ", "))
        
        updateTextInput(session,
                        "pos_prop_a",
                        value = est_pos_prop_a)
        
        ### Estimates for Condition B ----
        #### Process data ----
        data_sdtlu_b = data_files$processed_data %>% 
            filter(cond == parameters$cond2) %>% 
            mutate(lineup_size = input$lineup_size_sim_b,
                   conf_level = (max(conf_level)+1) - conf_level)
        
        data_sdtlu_processed_b = sdtlu_process_data(data_sdtlu_b)
        
        #### Set fitting options ----
        if (input$simultaneous_sequential_sim_a == "Simultaneous") {
            options_b = list(model_type = "sim",
                             fit_fcn = "G2",
                             fix_p = "data",
                             fix_sigma_t = "free",
                             use_restr_data = FALSE,
                             run_bootstrap = FALSE)
            
            est_pos_prop_b = NA
        } else {
            options_b = list(model_type = "seq",
                             fit_fcn = "G2",
                             fix_p = "data",
                             fix_sigma_t = "free",
                             use_restr_data = FALSE,
                             run_bootstrap = FALSE)
            
            est_pos_prop_b = as.character(paste0(data_sdtlu_processed_b$pos_prop, collapse = ", "))
        }
        
        #### Fit the model ----
        sdtlu_fit_b = sdtlu_fit(data_sdtlu_processed_b,
                                options = options_b)
        
        #### Recover the estimates ----
        params_b = sdtlu_fit_b$best_params_full
        
        cs_b = params_b[4:length(params_a)]
        
        params_data_b = data.frame(
            Parameter = c("p",
                          "mu_t",
                          "sigma_t",
                          "cs",
                          "pos_prop"),
            vals_b = c(
                params_b[1],
                params_b[2],
                params_b[3],
                paste(cs_b, collapse = ", "),
                est_pos_prop_b
            )
        )
        
        #### Add the estimates to the simulation parameters tab ----
        updateRadioButtons(session,
                           "sim_seq_b",
                           selected = input$simultaneous_sequential_sim_b)
        
        updateNumericInput(session,
                           "lineup_sizes_b",
                           value = input$lineup_size_sim_b)
        
        updateNumericInput(session,
                           "mu_t_b",
                           value = params_b[2])
        
        updateNumericInput(session,
                           "sigma_t_b",
                           value = params_b[3])
        
        updateTextInput(session,
                        "cs_b",
                        value = paste(cs_b, collapse = ", "))
        
        updateTextInput(session,
                        "pos_prop_b",
                        value = est_pos_prop_b)
        
        data_files$sdtlu_estimates = params_data_a %>% 
            left_join(params_data_b) %>% 
            `colnames<-`(c("Parameter", parameters$cond1, parameters$cond2))
        
        fit_measures = data.frame("Parameter" = "Fit (G2)",
                                  "Cond1" = sdtlu_fit_a$best_fit_measure,
                                  "Cond2" = sdtlu_fit_b$best_fit_measure) %>% 
            `colnames<-`(c("Parameter", parameters$cond1, parameters$cond2))
        
        data_files$sdtlu_estimates = data_files$sdtlu_estimates %>% 
            rbind(fit_measures)
        
        output$sdtlu_estimates = renderDataTable({
            data_files$sdtlu_estimates
        })
        
        message(data_files$sdtlu_estimates)
        
        ### Visualizing the parameters ----
        #### Simulated SDT distributions ----
            cond_a_dists = data.frame(
                Target = distribution_normal(100,
                                        mean = as.numeric(data_files$sdtlu_estimates[2,2]),
                                        sd = as.numeric(data_files$sdtlu_estimates[3,2])),
                Lure = distribution_normal(100,
                                        mean = 0,
                                        sd = 1)
            ) %>%
                pivot_longer(names_to = "Distribution",
                             values_to = "Strength",
                             c(Target, Lure)) %>%
                mutate(cond = parameters$cond1)
            
        cond_b_dists = data.frame(
            Target = distribution_normal(100,
                                         mean = as.numeric(data_files$sdtlu_estimates[2,3]),
                                         sd = as.numeric(data_files$sdtlu_estimates[3,3])),
            Lure = distribution_normal(100,
                                       mean = 0,
                                       sd = 1)
        ) %>%
            pivot_longer(names_to = "Distribution",
                         values_to = "Strength",
                         c(Target, Lure)) %>%
            mutate(cond = parameters$cond2)
        
        
        combined_dists = rbind(cond_a_dists,
                               cond_b_dists) %>% 
            mutate(Condition = cond)
        
        message("Generated distributions")
        message(combined_dists)
        
        #### Get the criterion values ----
        criterions_sim = data.frame(
            Condition = c(parameters$cond1,
                     parameters$cond2),
            cs = c(paste(cs_a, collapse = ", "),
                   paste(cs_b, collapse = ", "))
        ) %>% 
            separate(cs, as.character(c(1:max(length(cs_a), length(cs_b)))), ", ") %>% 
            pivot_longer(names_to = "criterion",
                         values_to = "Strength",
                         -Condition) %>% 
            mutate(Strength = as.numeric(Strength))
        
        message("Extracted criterion values for plotting")
        
        sdtlu_params_sim_plot = combined_dists %>% 
            ggplot(aes(x = Strength, color = Distribution))+
            geom_density()+
            scale_color_manual(values = c("red", "blue"))+
            facet_grid(cols = vars(Condition))+
            geom_vline(data = criterions_sim,
                       aes(xintercept = Strength),
                       alpha = .5)+
            apatheme+
            labs(y = "Density")+
            ggtitle("Model-implied SDT parameters")+
            theme(text = element_text(size = 20),
                  plot.title = element_text(hjust = .5,
                                            size = 25))
        
        output$sdtlu_params_sim_plot = renderPlot({
            sdtlu_params_sim_plot
        })
        
        #### Model vs. Data response predictions ----
        ##### Condition A ----
        #model_fit_data_a = data.frame(model_prop = as.vector(sdtlu_fit_a$model_prop),
        #                            data_resps = as.vector(data_sdtlu_processed_a$resp_data_full),
        #                            resp_type = rep(c(rep("Suspect", parameters$n_confs_a),
        #                                              rep("Filler", parameters$n_confs_a),
        #                                              "Reject"), times = 2),
        #                            conf_level = rep(c(sort(unique(data_sdtlu_a$conf_level), decreasing = TRUE), 
        #                                               sort(unique(data_sdtlu_a$conf_level), decreasing = TRUE),
        #                                               NA),
        #                                             times = 2),
        #                            Presence = c(rep("TP", times = parameters$n_confs_a*2+1),
        #                                         rep("TA", times = parameters$n_confs_a*2+1))) %>% 
        #    mutate(data_total = sum(data_resps),
        #           data_prop = data_resps/data_total,
        #           row = 1:n()) %>% 
        #    rowwise() %>% 
        #    mutate(label = paste(Presence, resp_type, conf_level, sep = "_"),
        #           label = str_replace(label, "_NA", "")) %>% 
        #    arrange(row) %>% 
        #    pivot_longer(names_to = "Source",
        #                 values_to = "Proportion",
        #                 c(model_prop, data_prop)) %>% 
        #    mutate(Source = ifelse(grepl("data", Source), "Data", "Model"),
        #           Condition = parameters$cond1)
        
        model_fit_data_a = data.frame(model_prop = as.vector(sdtlu_fit_a$model_prop),
                                      data_resps = as.vector(data_sdtlu_processed_a$resp_data_full),
                                      resp_type = rep(c(rep("Suspect", max(data_sdtlu_a$conf_level)),
                                                        rep("Filler", max(data_sdtlu_a$conf_level)),
                                                        "Reject"), times = 2),
                                      conf_level = rep(c(parameters$max_conf_a:parameters$min_conf_a, 
                                                         parameters$max_conf_a:parameters$min_conf_a, NA),
                                                       times = 2),
                                      Presence = c(rep("TP", times = (max(data_sdtlu_a$conf_level)*2)+1),
                                                   rep("TA", times = (max(data_sdtlu_a$conf_level)*2)+1))) %>% 
            mutate(data_total = sum(data_resps),
                   data_prop = data_resps/data_total,
                   row = 1:n()) %>% 
            rowwise() %>% 
            mutate(label = paste(Presence, resp_type, conf_level, sep = "_"),
                   label = str_replace(label, "_NA", "")) %>% 
            arrange(row) %>% 
            pivot_longer(names_to = "Source",
                         values_to = "Proportion",
                         c(model_prop, data_prop)) %>% 
            mutate(Source = ifelse(grepl("data", Source), "Data", "Model"),
                   Condition = parameters$cond1)
        
        model_fit_data_a$label = reorder(model_fit_data_a$label, model_fit_data_a$row)
        
        message("Condition A model vs. data summary generated")
        
        ##### Condition B ----
        #model_fit_data_b = data.frame(model_prop = as.vector(sdtlu_fit_b$model_prop),
        #                              data_resps = as.vector(data_sdtlu_processed_b$resp_data_full),
        #                              resp_type = rep(c(rep("Suspect", parameters$n_confs_b),
        #                                                rep("Filler", parameters$n_confs_b),
        #                                                "Reject"), times = 2),
        #                              conf_level = rep(c(sort(unique(data_sdtlu_b$conf_level), decreasing = TRUE), 
        #                                                 sort(unique(data_sdtlu_b$conf_level), decreasing = TRUE),
        #                                                 NA),
        #                                               times = 2),
        #                              Presence = c(rep("TP", times = parameters$n_confs_b*2+1),
        #                                           rep("TA", times = parameters$n_confs_b*2+1))) %>% 
        #    mutate(data_total = sum(data_resps),
        #           data_prop = data_resps/data_total,
        #           row = 1:n()) %>% 
        #    rowwise() %>% 
        #    mutate(label = paste(Presence, resp_type, conf_level, sep = "_"),
        #           label = str_replace(label, "_NA", "")) %>% 
        #    arrange(row) %>% 
        #    pivot_longer(names_to = "Source",
        #                 values_to = "Proportion",
        #                 c(model_prop, data_prop)) %>% 
        #    mutate(Source = ifelse(grepl("data", Source), "Data", "Model"),
        #           Condition = parameters$cond2)
        
        model_fit_data_b = data.frame(model_prop = as.vector(sdtlu_fit_b$model_prop),
                                      data_resps = as.vector(data_sdtlu_processed_b$resp_data_full),
                                      resp_type = rep(c(rep("Suspect", max(data_sdtlu_b$conf_level)),
                                                        rep("Filler", max(data_sdtlu_b$conf_level)),
                                                        "Reject"), times = 2),
                                      conf_level = rep(c(parameters$max_conf_b:1, parameters$max_conf_b:1, NA),
                                                       times = 2),
                                      Presence = c(rep("TP", times = (max(data_sdtlu_b$conf_level)*2)+1),
                                                   rep("TA", times = (max(data_sdtlu_b$conf_level)*2)+1))) %>% 
            mutate(data_total = sum(data_resps),
                   data_prop = data_resps/data_total,
                   row = 1:n()) %>% 
            rowwise() %>% 
            mutate(label = paste(Presence, resp_type, conf_level, sep = "_"),
                   label = str_replace(label, "_NA", "")) %>% 
            arrange(row) %>% 
            pivot_longer(names_to = "Source",
                         values_to = "Proportion",
                         c(model_prop, data_prop)) %>% 
            mutate(Source = ifelse(grepl("data", Source), "Data", "Model"),
                   Condition = parameters$cond2)
        
        model_fit_data_b$label = reorder(model_fit_data_b$label, model_fit_data_b$row)
        
        message("Condition B model vs. data summary generated")
        
        ##### Combine and plot ----
        model_fit_data = rbind(model_fit_data_a,
                               model_fit_data_b) %>% 
            mutate(Condition = factor(Condition,
                                      levels = c(parameters$cond1, parameters$cond2)))
        
        model_fit_data_plot = model_fit_data %>% 
            ggplot(aes(x = label, y = Proportion, color = Presence, shape = Source))+
            geom_point(size = 3)+
            facet_grid(cols = vars(Condition))+
            scale_shape_manual(values = c(1, 4))+
            scale_color_manual(values = c("red", "blue"))+
            apatheme+
            ggtitle("Model response predictions vs. data")+
            theme(text = element_text(size = 20),
                  axis.text.x = element_text(size = 8,
                                             angle = 90,
                                             vjust = .5),
                  plot.title = element_text(size = 25,
                                            hjust = .5))+
            labs(x = "Response Category",
                 y = "Proportion")
        
        output$model_fit_data_plot = renderPlot({
            model_fit_data_plot
        })
        
        message("Generated model vs. data fit plot")
        
        #### Model vs. Data ROC curves ----
        ##### Condition A ----
        if (input$simultaneous_sequential_sim_a == "Simultaneous") {
            cond_a_roc = sdtlu_roc_model(
                params = sdtlu_fit_a$best_params_full,
                lineup_size = input$lineup_size_sim_a)
        } else {
            cond_a_roc = sdtlu_roc_model(
                params = sdtlu_fit_a$best_params_full,
                lineup_size = input$lineup_size_sim_a,
                model_type = "seq")
        }
        
        cond_a_roc_data_lines = data.frame("absent" = cond_a_roc[[3]],
                                           "present" = cond_a_roc[[4]],
                                           cond = parameters$cond1)
        
        cond_a_roc_data_points = data.frame("absent" = unlist(cond_a_roc[[1]]),
                                            "present" = unlist(cond_a_roc[[2]]),
                                            cond = parameters$cond1)
        
        message(cond_a_roc_data_points)
        
        ##### Condition B ----
        if (input$simultaneous_sequential_sim_b == "Simultaneous") {
            cond_b_roc = sdtlu_roc_model(
                params = sdtlu_fit_b$best_params_full,
                lineup_size = input$lineup_size_sim_b)
        } else {
            cond_b_roc = sdtlu_roc_model(
                params = sdtlu_fit_b$best_params_full,
                lineup_size = input$lineup_size_sim_b,
                model_type = "seq")
        }
        
        cond_b_roc_data_lines = data.frame("absent"  = cond_b_roc[[3]],
                                           "present" = cond_b_roc[[4]],
                                           cond = parameters$cond2)
        
        cond_b_roc_data_points = data.frame("absent"  = unlist(cond_b_roc[[1]]),
                                            "present" = unlist(cond_b_roc[[2]]),
                                            cond = parameters$cond2)
        
        ##### Update the plot ----
        ROC_data_wide = data_files$ROC_data_wide
        
        hypothetical_sdtlu_plot = ggplot(data = ROC_data_wide, aes(x = absent, y = present, color = cond))+
            geom_point(alpha = .5)+
            geom_line()+
            #geom_vline(xintercept = partial_threshold)+
            apatheme+
            labs(x = "\nFalse ID rate",
                 y = "Correct ID rate\n",
                 #linetype = "Effect",
                 color = "Condition")+
            ggtitle("Model ROC predictions vs. data")+
            theme(text = element_text(size = 20),
                  plot.title = element_text(hjust = .5,
                                            size = 25))+
            geom_line(data = cond_a_roc_data_lines,
                       aes(x = absent,
                           y = present,
                       color = cond))+
            geom_point(data = cond_a_roc_data_points,
                       aes(x = absent,
                           y = present,
                       color = cond),
                       size = 3,
                       shape = 4)+
            geom_line(data = cond_b_roc_data_lines,
                       aes(x = absent,
                           y = present,
                       color = cond))+
            geom_point(data = cond_b_roc_data_points,
                       aes(x = absent,
                           y = present,
                       color = cond),
                       size = 3,
                       shape = 4)
        
        message("Created data vs. model ROC plot")
        
        output$hypothetical_sdtlu_plot = renderPlot({
            hypothetical_sdtlu_plot
            #sdtlu_params_sim_plot
        })
        
        showModal(modalDialog("All done! See below for parameter estimates and model fit.",
                              fade = TRUE,
                              easyClose = FALSE,
                              size = "l"))
    })
    
    ## main simulation loop ----
    observeEvent(input$sim_start, {
        
        message(data_files$conf_effs_data)
        
        
        ## CODE TO SAVE RAW SIMULATED DATA -- UNCOMMENT THIS AND ANY OTHER LINES WITH "RAW_DATA" TO ENABLE ----
        #data_files$raw_data_store = data.frame(conf_level = NA,
        #                                       cond = NA,
        #                                       culprit_present = NA,
        #                                       sim = NA,
        #                                       n = NA,
        #                                       eff = NA,
        #                                       auc1 = NA,
        #                                       auc2 = NA,
        #                                       D_stat = NA,
        #                                       sd_bootstrap = NA)
        
        other_vars$start_time = Sys.time()
        start_time = Sys.time()
        
        other_vars$sim_total = input$nsims * length(parameters$ns) * length(parameters$effs)
        #duration = (other_vars$avg_n/1000 * input$nboot_iter/1500) * other_vars$sim_total
        #other_vars$duration_est = duration/60
        #other_vars$end_time_est = start_time + duration
        #
        #message(other_vars$end_time_est)
        
        showModal(modalDialog(HTML(sprintf("Start time: %s <br/>You will be redirected to the results tab once simulations are complete.",
                            other_vars$start_time)),
                    fade = TRUE,
                    easyClose = TRUE,
                    size = "l"))
        
        pwr_store = matrix(nrow = length(parameters$ns),
                           ncol = length(parameters$effs))
        
        pwr_store_dpp = matrix(nrow = length(parameters$ns),
                               ncol = length(parameters$effs))
        
        ### matrices to store AUC values ----
        auc_store = matrix(nrow = length(parameters$ns),
                           ncol = length(parameters$effs))
        
        auc_store_ci_upr = matrix(nrow = length(parameters$ns),
                           ncol = length(parameters$effs))
        auc_store_ci_lwr = matrix(nrow = length(parameters$ns),
                           ncol = length(parameters$effs))
        
        auc_1_store = matrix(nrow = length(parameters$ns),
                             ncol = length(parameters$effs))
        
        auc_1_store_ci_upr = matrix(nrow = length(parameters$ns),
                                  ncol = length(parameters$effs))
        auc_1_store_ci_lwr = matrix(nrow = length(parameters$ns),
                                  ncol = length(parameters$effs))
        
        auc_2_store = matrix(nrow = length(parameters$ns),
                             ncol = length(parameters$effs))
        
        auc_2_store_ci_upr = matrix(nrow = length(parameters$ns),
                                  ncol = length(parameters$effs))
        auc_2_store_ci_lwr = matrix(nrow = length(parameters$ns),
                                  ncol = length(parameters$effs))
        
        ### matrices to store DPP values ----
        dpp_store = matrix(nrow = length(parameters$ns),
                           ncol = length(parameters$effs))
        
        dpp_store_ci_upr = matrix(nrow = length(parameters$ns),
                                  ncol = length(parameters$effs))
        dpp_store_ci_lwr = matrix(nrow = length(parameters$ns),
                                  ncol = length(parameters$effs))
        
        dpp_1_store = matrix(nrow = length(parameters$ns),
                             ncol = length(parameters$effs))
        
        dpp_1_store_ci_upr = matrix(nrow = length(parameters$ns),
                                    ncol = length(parameters$effs))
        dpp_1_store_ci_lwr = matrix(nrow = length(parameters$ns),
                                    ncol = length(parameters$effs))
        
        dpp_2_store = matrix(nrow = length(parameters$ns),
                             ncol = length(parameters$effs))
        
        dpp_2_store_ci_upr = matrix(nrow = length(parameters$ns),
                                    ncol = length(parameters$effs))
        dpp_2_store_ci_lwr = matrix(nrow = length(parameters$ns),
                                    ncol = length(parameters$effs))
        
        show("sim_progress")
        sim_counter = 0
        other_vars$sim_counter = 0
        
        ## If simulating from data ----
        if (input$empirical_theoretical == "Data") {
        ### loop over effect sizes ----
            for (g in 1:length(parameters$effs)) {
            eff = parameters$effs[g]
            
            data_original = data_files$processed_data %>%
                #mutate(conf_level = as.factor(conf_level)) %>%
                group_by(id_type, conf_level, culprit_present, cond) %>%
                count() %>%
                ungroup() %>%
                group_by(culprit_present, cond) %>%
                mutate(total = sum(n),
                       prop = n / total) %>%
                ungroup() %>%
                mutate(conf_level_rev = (max(as.numeric(conf_level)) + 1) - as.numeric(conf_level)) %>%
                filter(id_type == "suspect")
            
            data = data_original %>% 
                left_join(data_files$conf_effs_data)
            
            ### Create the root data file to sample from across the Ns and sims ----
            for (z in 1:nrow(data)) {
                if (data$culprit_present[z] == "present" &
                    data$cond[z] == levels(data$cond)[2]) {
                    data$n[z] = round(data$n[z] * (eff + 1) * (data$conf_effs[z] + 1))
                } else {
                    data$n[z] = data$n[z]
                }
            }
            
            data$prop = data$n / data$total
            
            ### Get the ROC data to sample from across Ns and sims ----
            #### For Condition 1 ----
            ##### TA ----
            TA_data_cond1_root = filter(data,
                                        culprit_present == "absent" &
                                            cond == levels(data$cond)[1])
            
            # Empty the root vector
            #TA_ROC_cond1_root = vector()
            #
            #for (y in 1:nrow(TA_data_cond1)) {
            #  TA_ROC_cond1_root = c(TA_ROC_cond1_root, rep(TA_data_cond1$conf_level_rev[y], times = TA_data_cond1$n[y]))
            #}
            
            ##### TP ----
            TP_data_cond1_root = filter(data,
                                        culprit_present == "present" &
                                            cond == levels(data$cond)[1])
            
            #### For Condition 2 ----
            ##### TA ----
            TA_data_cond2_root = filter(data,
                                        culprit_present == "absent" &
                                            cond == levels(data$cond)[2])
            
            ##### TP ----
            TP_data_cond2_root = filter(data,
                                        culprit_present == "present" &
                                            cond == levels(data$cond)[2])
            
            ### loop over Ns ####
            for (h in 1:length(parameters$ns)) {
                
                sim_store = data.frame(auc_diff = rep(NA, times = input$nsims),
                                       auc_1 = rep(NA, times = input$nsims),
                                       auc_2 = rep(NA, times = input$nsims),
                                       auc_p = rep(NA, times = input$nsims),
                                       sig = rep(NA, times = input$nsims),
                                       dpp_diff = rep(NA, times = input$nsims),
                                       dpp_1 = rep(NA, times = input$nsims),
                                       dpp_2 = rep(NA, times = input$nsims),
                                       sig_dpp = rep(NA, times = input$nsims))
                
                #curr_n = parameters$ns[h]
                
                if (input$between_within == "Between-subjects") {
                    curr_n = parameters$ns[h]
                } else {
                    curr_n = parameters$ns[h] * 2
                }
                
                #### Loop over sims ----
                for (i in 1:input$nsims) {
                    ##### For each sim, sample ROC data from the root ----
                    ###### For Condition 1 ----
                    ####### TA ----
                    TA_data_cond1 = sample(
                        c(
                            as_vector(TA_data_cond1_root$conf_level),
                            0
                        ),
                        size = round((curr_n/2) * input$n_TA_lineups),
                        replace = TRUE,
                        prob = c(
                            as_vector(TA_data_cond1_root$prop),
                            1 - sum(TA_data_cond1_root$prop)
                        )
                    )
                    
                    TA_data_cond1 = TA_data_cond1[!is.na(TA_data_cond1)]
                    
                    cond1_partial = length(TA_data_cond1[TA_data_cond1 > 0]) / round((curr_n/2) * input$n_TA_lineups)
                    
                    ####### TP ----
                    TP_data_cond1 = sample(
                        c(
                            as_vector(TP_data_cond1_root$conf_level),
                            0
                        ),
                        size = round((curr_n/2) * input$n_TP_lineups),
                        replace = TRUE,
                        prob = c(
                            as_vector(TP_data_cond1_root$prop),
                            1 - sum(TP_data_cond1_root$prop)
                        )
                    )
                    
                    TP_data_cond1 = TP_data_cond1[!is.na(TP_data_cond1)]
                    
                    
                    ###### For Condition 2 ----
                    ####### TA ----
                    TA_data_cond2 = sample(
                        c(
                            as_vector(TA_data_cond2_root$conf_level),
                            0
                        ),
                        size = round((curr_n/2) * input$n_TA_lineups),
                        replace = TRUE,
                        prob = c(
                            as_vector(TA_data_cond2_root$prop),
                            1 - sum(TA_data_cond2_root$prop)
                        )
                    )
                    
                    TA_data_cond2 = TA_data_cond2[!is.na(TA_data_cond2)]
                    
                    cond2_partial = length(TA_data_cond2[TA_data_cond2 > 0]) / round((curr_n/2) * input$n_TA_lineups)
                    
                    ####### TP ----
                    TP_data_cond2 = sample(
                        c(
                            as_vector(TP_data_cond2_root$conf_level),
                            0
                        ),
                        size = round((curr_n/2) * input$n_TP_lineups),
                        replace = TRUE,
                        prob = c(
                            as_vector(TP_data_cond2_root$prop),
                            1 - sum(TP_data_cond2_root$prop)
                        )
                    )
                    
                    TP_data_cond2 = TP_data_cond2[!is.na(TP_data_cond2)]
                    
                    ##### Generate the ROCs ----
                    
                    if (input$measure != "DPP") {
                    
                    ###### Condition 1 ----
                    
                    
                    #if (length(TP_data_cond1) > length(TA_data_cond1)) {
                    #    TA_data_cond1 = append(TA_data_cond1,
                    #                           rep(
                    #                               0,
                    #                               length(TP_data_cond1) - length(TA_data_cond1)
                    #                           ))
                    #} else if (length(TA_data_cond1) > length(TP_data_cond1)) {
                    #    TP_data_cond1 = append(TP_data_cond1,
                    #                           rep(
                    #                               0,
                    #                               length(TA_data_cond1) - length(TP_data_cond1)
                    #                           ))
                    #} else {
                    #    TA_data_cond1 = TA_data_cond1
                    #    TP_data_cond1 = TP_data_cond1
                    #}
                    
                    roc_cond1 = roc(
                        controls = TA_data_cond1,
                        cases = TP_data_cond1[!is.na(TP_data_cond1)],
                        direction = "<",
                        ci = F
                        #partial.auc = c(1, 1 - min(
                        #    cond1_partial, cond2_partial
                    )
                    
                    ###### Condition 2 ----
                    #if (length(TP_data_cond2) > length(TA_data_cond2)) {
                    #    TA_data_cond2 = append(TA_data_cond2,
                    #                           rep(
                    #                               0,
                    #                               length(TP_data_cond2) - length(TA_data_cond2)
                    #                           ))
                    #} else if (length(TA_data_cond2) > length(TP_data_cond2)) {
                    #    TP_data_cond2 = append(TP_data_cond2,
                    #                           rep(
                    #                               0,
                    #                               length(TA_data_cond2) - length(TP_data_cond2)
                    #                           ))
                    #} else {
                    #    TA_data_cond2 = TA_data_cond2
                    #    TP_data_cond2 = TP_data_cond2
                    #}
                    
                    roc_cond2 = roc(
                        controls = TA_data_cond2,
                        cases = TP_data_cond2[!is.na(TP_data_cond2)],
                        direction = "<",
                        ci = F
                        #partial.auc = c(1, 1 - min(
                        #    cond1_partial, cond2_partial
                    )
                    
                    ##### ROC test ----
                    if (input$between_within == "Between-subjects") {
                        if (input$roc_trunc == "Lowest false ID rate") {
                            ##### If truncating at lowest false ID rate ----
                            roc_test = roc.test(
                                roc_cond1,
                                roc_cond2,
                                reuse.auc = FALSE,
                                partial.auc = c(1, 1 - min(
                                    cond1_partial, cond2_partial
                                )),
                                partial.auc.focus = "sp",
                                method = "bootstrap",
                                paired = FALSE,
                                boot.n = input$nboot_iter,
                                progress = "none"
                            )
                        } else if (input$roc_trunc == "Highest false ID rate") {
                            ##### If truncating at highest false ID rate ----
                            roc_test = roc.test(
                                roc_cond1,
                                roc_cond2,
                                reuse.auc = FALSE,
                                partial.auc = c(1, 1 - max(
                                    cond1_partial, cond2_partial
                                )),
                                partial.auc.focus = "sp",
                                method = "bootstrap",
                                paired = FALSE,
                                boot.n = input$nboot_iter,
                                progress = "none"
                            ) 
                        } else {
                            ##### If truncating at a custom false ID rate ----
                            roc_test = roc.test(
                                roc_cond1,
                                roc_cond2,
                                reuse.auc = FALSE,
                                partial.auc = c(1, other_vars$custom_trunc),
                                partial.auc.focus = "sp",
                                method = "bootstrap",
                                paired = FALSE,
                                boot.n = input$nboot_iter,
                                progress = "none"
                            ) 
                        }
                    } else {
                        if (input$roc_trunc == "Lowest false ID rate") {
                            ##### If truncating at lowest false ID rate ----
                            roc_test = roc.test(
                                roc_cond1,
                                roc_cond2,
                                reuse.auc = FALSE,
                                partial.auc = c(1, 1 - min(
                                    cond1_partial, cond2_partial
                                )),
                                partial.auc.focus = "sp",
                                method = "bootstrap",
                                paired = FALSE,
                                boot.n = input$nboot_iter,
                                progress = "none"
                            )
                        } else if (input$roc_trunc == "Highest false ID rate") {
                            ##### If truncating at highest false ID rate ----
                            roc_test = roc.test(
                                roc_cond1,
                                roc_cond2,
                                reuse.auc = FALSE,
                                partial.auc = c(1, 1 - max(
                                    cond1_partial, cond2_partial
                                )),
                                partial.auc.focus = "sp",
                                method = "bootstrap",
                                paired = FALSE,
                                boot.n = input$nboot_iter,
                                progress = "none"
                            ) 
                        } else {
                            ##### If truncating at a custom false ID rate ----
                            roc_test = roc.test(
                                roc_cond1,
                                roc_cond2,
                                reuse.auc = FALSE,
                                partial.auc = c(1, other_vars$custom_trunc),
                                partial.auc.focus = "sp",
                                method = "bootstrap",
                                paired = FALSE,
                                boot.n = input$nboot_iter,
                                progress = "none"
                            ) 
                        }
                    }
                        
                    message("Completed ROC test")
                        
                        sim_store$auc_diff[i] = as.numeric(roc_test$estimate[1]) - as.numeric(roc_test$estimate[2])
                        sim_store$auc_1[i] = as.numeric(roc_test$estimate[1])
                        sim_store$auc_2[i] = as.numeric(roc_test$estimate[2])
                        sim_store$auc_p[i] = as.numeric(roc_test$p.value)
                        
                        if (input$test_tails == "2_tail") {
                            sim_store$sig[i] = ifelse(sim_store$auc_p[i] < input$alpha_level, 1, 0)
                        } else if (input$test_tails == sprintf("%s > %s",
                                                               parameters$cond1,
                                                               parameters$cond2)) {
                            sim_store$sig[i] = ifelse(sim_store$auc_p[i] < input$alpha_level * 2 &
                                                          sim_store$auc_diff[i] > 0, 1, 0)
                        } else {
                            sim_store$sig[i] = ifelse(sim_store$auc_p[i] < input$alpha_level * 2 &
                                                          sim_store$auc_diff[i] < 0, 1, 0)
                        }
                    }
                    
                    #### Generate data for DPP function ----
                    if (input$measure != "pAUC") {
                        TA_dataframe_cond1 = data.frame(conf = TA_data_cond1,
                                                        tpORta = 0,
                                                        cond = levels(data$cond)[1])
                        
                        TP_dataframe_cond1 = data.frame(conf = TP_data_cond1,
                                                        tpORta = 1,
                                                        cond = levels(data$cond)[1])
                        
                        TA_dataframe_cond2 = data.frame(conf = TA_data_cond2,
                                                        tpORta = 0,
                                                        cond = levels(data$cond)[2])
                        
                        TP_dataframe_cond2 = data.frame(conf = TP_data_cond2,
                                                        tpORta = 1,
                                                        cond = levels(data$cond)[2])
                        
                        data_DPP = rbind(TA_dataframe_cond1,
                                         TP_dataframe_cond1,
                                         TA_dataframe_cond2,
                                         TP_dataframe_cond2) %>% 
                            mutate(cond = as.factor(cond))
                        
                        ##### Bootstrap the DPP values and the difference ----
                        DPP_results = boot(data = data_DPP, 
                                           statistic = DPP_difference,
                                           R = input$nboot_iter)
                        
                        sim_store$dpp_diff[i] = DPP_results$t0[3]
                        sim_store$dpp_1[i] = DPP_results$t0[1]
                        sim_store$dpp_2[i] = DPP_results$t0[2]
                        
                        ###### Adjusting the difference confidence interval based on alpha level/test ----
                        if (input$test_tails == "2_tail") {
                            #generate 95% CIs Bias Corrected and Accelerated
                            confidence_interval_diff = boot.ci(DPP_results, index=3, conf=(1-input$alpha_level), type='bca')
                            ci_diff=confidence_interval_diff$bca[,c(4,5)]
                            
                            
                            sim_store$sig_dpp[i] = ifelse(ci_diff[1] < 0 & ci_diff[2] < 0, 1, 
                                                          ifelse(ci_diff[1] > 0 & ci_diff[2] > 0, 1, 
                                                                 ifelse(is.na(ci_diff[1]) | is.na(ci_diff[2]), NA, 0)))
                            
                        } else if (input$test_tails == sprintf("%s > %s",
                                                               parameters$cond1,
                                                               parameters$cond2)) {
                            
                            confidence_interval_diff = boot.ci(results, index=3, conf=(1-(input$alpha_level*2)), type='bca')
                            ci_diff=confidence_interval_diff$bca[,c(4,5)]
                            
                            sim_store$sig_dpp[i] = ifelse(ci_diff[2] < 0, 1, 
                                                          ifelse(is.na(ci_diff[1]) | is.na(ci_diff[2]), NA, 0))
                        } else {
                            confidence_interval_diff = boot.ci(results, index=3, conf=(1-(input$alpha_level*2)), type='bca')
                            ci_diff=confidence_interval_diff$bca[,c(4,5)]
                            
                            sim_store$sig_dpp[i] = ifelse(ci_diff[1] > 0, 1, 
                                                          ifelse(is.na(ci_diff[1]) | is.na(ci_diff[2]), NA, 0))
                        }
                    }
                    
                    #### Insert NAs for power estimates if only one measure is chosen ----
                    if (input$measure == "DPP") {
                        sim_store$sig[i] = NA
                    }
                    
                    if (input$measure == "pAUC") {
                        sim_store$sig_dpp[i] = NA
                    }
                    
                    ##### TESTING: Save condition data to look at later ----
                    TA_data_cond1_store = data.frame(
                        conf_level = TA_data_cond1,
                        cond = "A",
                        culprit_present = "TA",
                        sim = i,
                        n = curr_n,
                        eff = eff,
                        auc1 = ifelse(input$measure == "DPP", NA,
                                      as.numeric(roc_test$estimate[1])),
                        auc2 = ifelse(input$measure == "DPP", NA,
                                      as.numeric(roc_test$estimate[2])),
                        D_stat = ifelse(input$measure == "DPP", NA,
                                        as.numeric(roc_test$statistic)),
                        sd_bootstrap = NA
                    )
                    
                    TP_data_cond1_store = data.frame(
                        conf_level = TP_data_cond1,
                        cond = "A",
                        culprit_present = "TP",
                        sim = i,
                        n = curr_n,
                        eff = eff,
                        auc1 = ifelse(input$measure == "DPP", NA,
                                      as.numeric(roc_test$estimate[1])),
                        auc2 = ifelse(input$measure == "DPP", NA,
                                      as.numeric(roc_test$estimate[2])),
                        D_stat = ifelse(input$measure == "DPP", NA,
                                        as.numeric(roc_test$statistic)),
                        sd_bootstrap = NA
                    )
                    
                    TA_data_cond2_store = data.frame(
                        conf_level = TA_data_cond2,
                        cond = "B",
                        culprit_present = "TA",
                        sim = i,
                        n = curr_n,
                        eff = eff,
                        auc1 = ifelse(input$measure == "DPP", NA,
                                      as.numeric(roc_test$estimate[1])),
                        auc2 = ifelse(input$measure == "DPP", NA,
                                      as.numeric(roc_test$estimate[2])),
                        D_stat = ifelse(input$measure == "DPP", NA,
                                        as.numeric(roc_test$statistic)),
                        sd_bootstrap = NA
                    )
                    
                    TP_data_cond2_store = data.frame(
                        conf_level = TP_data_cond2,
                        cond = "B",
                        culprit_present = "TP",
                        sim = i,
                        n = curr_n,
                        eff = eff,
                        auc1 = ifelse(input$measure == "DPP", NA,
                                      as.numeric(roc_test$estimate[1])),
                        auc2 = ifelse(input$measure == "DPP", NA,
                                      as.numeric(roc_test$estimate[2])),
                        D_stat = ifelse(input$measure == "DPP", NA,
                                        as.numeric(roc_test$statistic)),
                        sd_bootstrap = NA
                    )
                    
                    #data_files$raw_data_store = rbind(data_files$raw_data_store,
                    #                                  TA_data_cond1_store,
                    #                                  TP_data_cond1_store,
                    #                                  TA_data_cond2_store,
                    #                                  TP_data_cond2_store)
                    
                    sim_counter = sim_counter + 1
                    
                    other_vars$sim_counter = other_vars$sim_counter + 1
                    
                    showNotification(
                        sprintf(
                            "%s / %s simulations complete",
                            sim_counter,
                            other_vars$sim_total
                        )
                    )
                }
                # Store power estimates
                pwr_store[h, g] = mean(sim_store$sig, na.rm = TRUE)
                
                pwr_store_dpp[h, g] = mean(sim_store$sig_dpp, na.rm = TRUE)
                
                # Store AUC difference estimates
                auc_store[h, g] = mean(sim_store$auc_diff, na.rm = TRUE)
                
                ## 95% Quantile on the AUC difference estimate
                auc_store_ci_upr[h, g] = quantile(sim_store$auc_diff,
                                                  probs = c(.025, .975),
                                                  na.rm = TRUE)[2]
                auc_store_ci_lwr[h, g] = quantile(sim_store$auc_diff,
                                                  probs = c(.025, .975),
                                                  na.rm = TRUE)[1]
                
                # Store Condition 1 AUC
                auc_1_store[h, g] = mean(sim_store$auc_1, na.rm = TRUE)
                
                ## 95% Quantile on the AUC estimate
                auc_1_store_ci_upr[h, g] = quantile(sim_store$auc_1,
                                                  probs = c(.025, .975),
                                                  na.rm = TRUE)[2]
                auc_1_store_ci_lwr[h, g] = quantile(sim_store$auc_1,
                                                  probs = c(.025, .975),
                                                  na.rm = TRUE)[1]
                
                # Store Condition 2 AUC
                auc_2_store[h, g] = mean(sim_store$auc_2, na.rm = TRUE)
                
                ## 95% Quantile on the AUC estimate
                auc_2_store_ci_upr[h, g] = quantile(sim_store$auc_2,
                                                    probs = c(.025, .975),
                                                    na.rm = TRUE)[2]
                auc_2_store_ci_lwr[h, g] = quantile(sim_store$auc_2,
                                                    probs = c(.025, .975),
                                                    na.rm = TRUE)[1]
                
                # Store DPP difference estimates
                dpp_store[h, g] = mean(sim_store$dpp_diff, na.rm = TRUE)
                
                ## 95% Quantile on the DPP difference estimate
                dpp_store_ci_upr[h, g] = quantile(sim_store$dpp_diff,
                                                  probs = c(.025, .975),
                                                  na.rm = TRUE)[2]
                dpp_store_ci_lwr[h, g] = quantile(sim_store$dpp_diff,
                                                  probs = c(.025, .975),
                                                  na.rm = TRUE)[1]
                
                # Store Condition 1 DPP
                dpp_1_store[h, g] = mean(sim_store$dpp_1, na.rm = TRUE)
                
                ## 95% Quantile on the DPP estimate
                dpp_1_store_ci_upr[h, g] = quantile(sim_store$dpp_1,
                                                    probs = c(.025, .975),
                                                    na.rm = TRUE)[2]
                dpp_1_store_ci_lwr[h, g] = quantile(sim_store$dpp_1,
                                                    probs = c(.025, .975),
                                                    na.rm = TRUE)[1]
                
                # Store Condition 2 DPP
                dpp_2_store[h, g] = mean(sim_store$dpp_2, na.rm = TRUE)
                
                ## 95% Quantile on the DPP estimate
                dpp_2_store_ci_upr[h, g] = quantile(sim_store$dpp_2,
                                                    probs = c(.025, .975),
                                                    na.rm = TRUE)[2]
                dpp_2_store_ci_lwr[h, g] = quantile(sim_store$dpp_2,
                                                    probs = c(.025, .975),
                                                    na.rm = TRUE)[1]
            }
        }
        } else {
        ## If simulating from SDT parameters ----
            ### Set parameter values ----
            parameters$sim_seq_a = input$sim_seq_a
            parameters$lineup_sizes_a = input$lineup_sizes_a
            parameters$mu_t_a = input$mu_t_a
            parameters$sigma_t_a = input$sigma_t_a
            parameters$cs_a = extract(input$cs_a)
            parameters$cond1 = "A"
            
            parameters$sim_seq_b = input$sim_seq_b
            parameters$lineup_sizes_b = input$lineup_sizes_b
            parameters$mu_t_b = input$mu_t_b
            parameters$sigma_t_b = input$sigma_t_b
            parameters$cs_b = extract(input$cs_b)
            parameters$cond2 = "B"
            
            parameters$effs = 0
            
            #### Set the probability of target presence (based on # of TP and total lineups) ----
            p = input$n_TP_lineups / input$n_total_lineups
            
            #### Put parameter values into vectors ----
            params_a = c(p, parameters$mu_t_a, parameters$sigma_t_a, parameters$cs_a)
            params_b = c(p, parameters$mu_t_b, parameters$sigma_t_b, parameters$cs_b)
            
            message("Set parameter values for main simulation")
            
            ### loop over effect sizes ----
            for (g in 1:length(parameters$effs)) {
                eff = parameters$effs[g]
        
                ### loop over Ns ####
                for (h in 1:length(parameters$ns)) {
                    
                    sim_store = data.frame(auc_diff = rep(NA, times = input$nsims),
                                           auc_1 = rep(NA, times = input$nsims),
                                           auc_2 = rep(NA, times = input$nsims),
                                           auc_p = rep(NA, times = input$nsims),
                                           sig = rep(NA, times = input$nsims),
                                           dpp_diff = rep(NA, times = input$nsims),
                                           dpp_1 = rep(NA, times = input$nsims),
                                           dpp_2 = rep(NA, times = input$nsims),
                                           sig_dpp = rep(NA, times = input$nsims))
                    
                    #curr_n = parameters$ns[h]
                    
                    if (input$between_within == "Between-subjects") {
                        curr_n = parameters$ns[h]
                    } else {
                        curr_n = parameters$ns[h] * 2
                    }
                    
                    curr_trials = round((curr_n * input$n_total_lineups) / 2)
                    
                    message(curr_trials)
                    message("Set number of trials")
                    
                    #### Loop over sims ----
                    for (i in 1:input$nsims) {
                        ##### For each sim, generate ROC data from simulated models ----
                        if (input$sim_seq_a == "Sequential") {
                            req(input$pos_prop_a)
                            
                            parameters$pos_prop_a = extract(input$pos_prop_a)
                            
                            ###### Check if the probability vector is same length as lineup size ----
                            if (length(parameters$pos_prop_a) != parameters$lineup_sizes_a) {
                                showModal(modalDialog(
                                    title = "Warning",
                                    "# of entered sequential probabilities does not match lineup size."
                                ))
                                
                                parameters$pos_prop_a = c(rep(1/parameters$lineup_sizes_a), times = lineup_sizes_a)
                                
                                hide("sim_start")
                            } else {
                                simmed_data_a = as.data.frame(t(as.data.frame(sdtlu_seq_sim(params_a, parameters$lineup_sizes_a, curr_trials, 1, pos_prop = parameters$pos_prop_a))))
                                simmed_data_a$id_type = rep(c(rep("suspect", length(parameters$cs_a)),
                                                              rep("filler", length(parameters$cs_a)),
                                                              "reject"), times = 2)
                                simmed_data_a$conf_level_rev = rep(c(1:length(parameters$cs_a), 1:length(parameters$cs_a), NA),
                                                                   times = 2)
                                simmed_data_a$conf_level = rep(c(length(parameters$cs_a):1, length(parameters$cs_a):1, NA),
                                                               times = 2)
                                simmed_data_a$culprit_present = c(rep("present", times = length(parameters$cs_a)*2+1),
                                                                  rep("absent", times = length(parameters$cs_a)*2+1))
                                
                                simmed_data_a_TP_rej = data.frame()
                                
                                for (m in 1:length(parameters$cs_a)) {
                                    simmed_data_a_TP_rej = rbind(simmed_data_a_TP_rej,
                                                                 filter(simmed_data_a, id_type == "reject" & culprit_present == "present"))
                                }
                                
                                simmed_data_a_TP_rej$conf_level_rev = 1:length(parameters$cs_a)
                                simmed_data_a_TP_rej$conf_level = length(parameters$cs_a):1
                                simmed_data_a_TP_rej$V1 = round(simmed_data_a_TP_rej$V1/length(parameters$cs_a))
                                
                                simmed_data_a_TA_rej = data.frame()
                                
                                for (n in 1:length(parameters$cs_a)) {
                                    simmed_data_a_TA_rej = rbind(simmed_data_a_TA_rej,
                                                                 filter(simmed_data_a, id_type == "reject" & culprit_present == "absent"))
                                }
                                
                                simmed_data_a_TA_rej$conf_level_rev = 1:length(parameters$cs_a)
                                simmed_data_a_TA_rej$conf_level = length(parameters$cs_a):1
                                simmed_data_a_TA_rej$V1 = round(simmed_data_a_TA_rej$V1/length(parameters$cs_a))
                                
                                simmed_data_a_final = filter(rbind(simmed_data_a,
                                                                   simmed_data_a_TP_rej,
                                                                   simmed_data_a_TA_rej),
                                                             !is.na(conf_level))
                                simmed_data_a_final$cond = "A"
                            }
                        } else {
                            simmed_data_a = as.data.frame(t(as.data.frame(sdtlu_sim_sim(params_a, parameters$lineup_sizes_a, curr_trials, 1))))
                            simmed_data_a$id_type = rep(c(rep("suspect", length(parameters$cs_a)),
                                                          rep("filler", length(parameters$cs_a)),
                                                          "reject"), times = 2)
                            
                            message("Breakpoint 1")
                            
                            simmed_data_a$conf_level_rev = rep(c(1:length(parameters$cs_a), 1:length(parameters$cs_a), NA),
                                                               times = 2)
                            simmed_data_a$conf_level = rep(c(length(parameters$cs_a):1, length(parameters$cs_a):1, NA),
                                                           times = 2)
                            simmed_data_a$culprit_present = c(rep("present", times = length(parameters$cs_a)*2+1),
                                                              rep("absent", times = length(parameters$cs_a)*2+1))
                            
                            simmed_data_a_TP_rej = data.frame()
                            
                            for (m in 1:length(parameters$cs_a)) {
                                simmed_data_a_TP_rej = rbind(simmed_data_a_TP_rej,
                                                             filter(simmed_data_a, id_type == "reject" & culprit_present == "present"))
                            }
                            
                            simmed_data_a_TP_rej$conf_level_rev = 1:length(parameters$cs_a)
                            simmed_data_a_TP_rej$conf_level = length(parameters$cs_a):1
                            simmed_data_a_TP_rej$V1 = round(simmed_data_a_TP_rej$V1/length(parameters$cs_a))
                            
                            simmed_data_a_TA_rej = data.frame()
                            
                            for (n in 1:length(parameters$cs_a)) {
                                simmed_data_a_TA_rej = rbind(simmed_data_a_TA_rej,
                                                             filter(simmed_data_a, id_type == "reject" & culprit_present == "absent"))
                            }
                            
                            simmed_data_a_TA_rej$conf_level_rev = 1:length(parameters$cs_a)
                            simmed_data_a_TA_rej$conf_level = length(parameters$cs_a):1
                            simmed_data_a_TA_rej$V1 = round(simmed_data_a_TA_rej$V1/length(parameters$cs_a))
                            
                            simmed_data_a_final = filter(rbind(simmed_data_a,
                                                               simmed_data_a_TP_rej,
                                                               simmed_data_a_TA_rej),
                                                         !is.na(conf_level))
                            simmed_data_a_final$cond = "A"
                        }
                        
                        message("Generated Condition A data for main simulation")
                        
                        ##### Condition B ----
                        if (input$sim_seq_b == "Sequential") {
                            req(input$pos_prop_b)
                            
                            parameters$pos_prop_b = extract(input$pos_prop_b)
                            
                            ###### Check if the probability vector is same length as lineup size ----
                            if (length(parameters$pos_prop_b) != parameters$lineup_sizes_b) {
                                showModal(modalDialog(
                                    title = "Warning",
                                    "# of entered sequential probabilities does not match lineup size."
                                ))
                                
                                parameters$pos_prop_b = c(rep(1/parameters$lineup_sizes_b), times = lineup_sizes_b)
                                
                                hide("sim_start")
                            } else {
                                simmed_data_b = as.data.frame(t(as.data.frame(sdtlu_seq_sim(params_b, parameters$lineup_sizes_b, curr_trials, 1, pos_prop = parameters$pos_prop_b))))
                                simmed_data_b$id_type = rep(c(rep("suspect", length(parameters$cs_b)),
                                                              rep("filler", length(parameters$cs_b)),
                                                              "reject"), times = 2)
                                simmed_data_b$conf_level_rev = rep(c(1:length(parameters$cs_b), 1:length(parameters$cs_b), NA),
                                                                   times = 2)
                                simmed_data_b$conf_level = rep(c(length(parameters$cs_b):1, length(parameters$cs_b):1, NA),
                                                               times = 2)
                                simmed_data_b$culprit_present = c(rep("present", times = length(parameters$cs_b)*2+1),
                                                                  rep("absent", times = length(parameters$cs_b)*2+1))
                                
                                simmed_data_b_TP_rej = data.frame()
                                
                                for (o in 1:length(parameters$cs_b)) {
                                    simmed_data_b_TP_rej = rbind(simmed_data_b_TP_rej,
                                                                 filter(simmed_data_b, id_type == "reject" & culprit_present == "present"))
                                }
                                
                                simmed_data_b_TP_rej$conf_level_rev = 1:length(parameters$cs_b)
                                simmed_data_b_TP_rej$conf_level = length(parameters$cs_b):1
                                simmed_data_b_TP_rej$V1 = round(simmed_data_b_TP_rej$V1/length(parameters$cs_b))
                                
                                simmed_data_b_TA_rej = data.frame()
                                
                                for (p in 1:length(parameters$cs_b)) {
                                    simmed_data_b_TA_rej = rbind(simmed_data_b_TA_rej,
                                                                 filter(simmed_data_b, id_type == "reject" & culprit_present == "absent"))
                                }
                                
                                simmed_data_b_TA_rej$conf_level_rev = 1:length(parameters$cs_b)
                                simmed_data_b_TA_rej$conf_level = length(parameters$cs_b):1
                                simmed_data_b_TA_rej$V1 = round(simmed_data_b_TA_rej$V1/length(parameters$cs_b))
                                
                                simmed_data_b_final = filter(rbind(simmed_data_b,
                                                                   simmed_data_b_TP_rej,
                                                                   simmed_data_b_TA_rej),
                                                             !is.na(conf_level))
                                simmed_data_b_final$cond = "B"
                            }
                        } else {
                            simmed_data_b = as.data.frame(t(as.data.frame(sdtlu_sim_sim(params_b, parameters$lineup_sizes_b, curr_trials, 1))))
                            simmed_data_b$id_type = rep(c(rep("suspect", length(parameters$cs_b)),
                                                          rep("filler", length(parameters$cs_b)),
                                                          "reject"), times = 2)
                            simmed_data_b$conf_level_rev = rep(c(1:length(parameters$cs_b), 1:length(parameters$cs_b), NA),
                                                               times = 2)
                            simmed_data_b$conf_level = rep(c(length(parameters$cs_b):1, length(parameters$cs_b):1, NA),
                                                           times = 2)
                            simmed_data_b$culprit_present = c(rep("present", times = length(parameters$cs_b)*2+1),
                                                              rep("absent", times = length(parameters$cs_b)*2+1))
                            
                            simmed_data_b_TP_rej = data.frame()
                            
                            for (o in 1:length(parameters$cs_b)) {
                                simmed_data_b_TP_rej = rbind(simmed_data_b_TP_rej,
                                                             filter(simmed_data_b, id_type == "reject" & culprit_present == "present"))
                            }
                            
                            simmed_data_b_TP_rej$conf_level_rev = 1:length(parameters$cs_b)
                            simmed_data_b_TP_rej$conf_level = length(parameters$cs_b):1
                            simmed_data_b_TP_rej$V1 = round(simmed_data_b_TP_rej$V1/length(parameters$cs_b))
                            
                            simmed_data_b_TA_rej = data.frame()
                            
                            for (p in 1:length(parameters$cs_b)) {
                                simmed_data_b_TA_rej = rbind(simmed_data_b_TA_rej,
                                                             filter(simmed_data_b, id_type == "reject" & culprit_present == "absent"))
                            }
                            
                            simmed_data_b_TA_rej$conf_level_rev = 1:length(parameters$cs_b)
                            simmed_data_b_TA_rej$conf_level = length(parameters$cs_b):1
                            simmed_data_b_TA_rej$V1 = round(simmed_data_b_TA_rej$V1/length(parameters$cs_b))
                            
                            simmed_data_b_final = filter(rbind(simmed_data_b,
                                                               simmed_data_b_TP_rej,
                                                               simmed_data_b_TA_rej),
                                                         !is.na(conf_level))
                            simmed_data_b_final$cond = "B"
                        }
                        
                        #### Combine Condition A & B data ----
                        simmed_data = rbind(simmed_data_a_final,
                                            simmed_data_b_final)
                        
                        message("Generated simmed data in main simulation loop")
                        
                        #### Generate trial-level data ----
                        simmed_data_trial = data.frame()
                        
                        for (q in 1:nrow(simmed_data)) {
                            simmed_data_slice = simmed_data[q,]
                            simmed_data_append = do.call("rbind", replicate(simmed_data_slice$V1, simmed_data_slice, simplify = FALSE))
                            simmed_data_trial = rbind(simmed_data_trial,
                                                      simmed_data_append)    
                        }
                        
                        simmed_data_trial$cond = factor(simmed_data_trial$cond,
                                                        levels = c("A", "B"))
                        
                        ### Generate ROC data ----
                        ###### For Condition 1 ----
                        ####### TA ----
                        TA_data_cond1 = select(filter(simmed_data_trial, cond == "A" & culprit_present == "absent"),
                                               conf_level, id_type)
                        TA_data_cond1$conf_level = ifelse(TA_data_cond1$id_type == "suspect", 
                                                          TA_data_cond1$conf_level, 0)
                        
                        TA_data_cond1 = unlist(as.list(dplyr::select(TA_data_cond1, conf_level)))
                        
                        TA_data_cond1 = TA_data_cond1[!is.na(TA_data_cond1)]
                        
                        cond1_partial = length(TA_data_cond1[TA_data_cond1 > 0]) / length(TA_data_cond1)
                        
                        ####### TP ----
                        TP_data_cond1 = select(filter(simmed_data_trial, cond == "A" & culprit_present == "present"),
                                               conf_level, id_type)
                        TP_data_cond1$conf_level = ifelse(TP_data_cond1$id_type == "suspect", 
                                                          TP_data_cond1$conf_level, 0)
                        
                        TP_data_cond1 = unlist(as.list(dplyr::select(TP_data_cond1, conf_level)))
                        
                        TP_data_cond1 = TP_data_cond1[!is.na(TP_data_cond1)]
                        
                        ###### For Condition 2 ----
                        ####### TA ----
                        TA_data_cond2 = select(filter(simmed_data_trial, cond == "B" & culprit_present == "absent"),
                                               conf_level, id_type)
                        TA_data_cond2$conf_level = ifelse(TA_data_cond2$id_type == "suspect", 
                                                          TA_data_cond2$conf_level, 0)
                        
                        TA_data_cond2 = unlist(as.list(dplyr::select(TA_data_cond2, conf_level)))
                        
                        TA_data_cond2 = TA_data_cond2[!is.na(TA_data_cond2)]
                        
                        cond2_partial = length(TA_data_cond2[TA_data_cond2 > 0]) / length(TA_data_cond2)
                        
                        ####### TP ----
                        TP_data_cond2 = select(filter(simmed_data_trial, cond == "B" & culprit_present == "present"),
                                               conf_level, id_type)
                        TP_data_cond2$conf_level = ifelse(TP_data_cond2$id_type == "suspect", 
                                                          TP_data_cond2$conf_level, 0)
                        
                        TP_data_cond2 = unlist(as.list(dplyr::select(TP_data_cond2, conf_level)))
                        
                        TP_data_cond2 = TP_data_cond2[!is.na(TP_data_cond2)]
                        
                        ##### Generate the ROCs ----
                        
                        if (input$measure != "DPP") {
                            
                            ###### Condition 1 ----
                            
                            
                            #if (length(TP_data_cond1) > length(TA_data_cond1)) {
                            #    TA_data_cond1 = append(TA_data_cond1,
                            #                           rep(
                            #                               0,
                            #                               length(TP_data_cond1) - length(TA_data_cond1)
                            #                           ))
                            #} else if (length(TA_data_cond1) > length(TP_data_cond1)) {
                            #    TP_data_cond1 = append(TP_data_cond1,
                            #                           rep(
                            #                               0,
                            #                               length(TA_data_cond1) - length(TP_data_cond1)
                            #                           ))
                            #} else {
                            #    TA_data_cond1 = TA_data_cond1
                            #    TP_data_cond1 = TP_data_cond1
                            #}
                            
                            roc_cond1 = roc(
                                controls = TA_data_cond1,
                                cases = TP_data_cond1[!is.na(TP_data_cond1)],
                                direction = "<",
                                ci = F
                                #partial.auc = c(1, 1 - min(
                                #    cond1_partial, cond2_partial
                            )
                            
                            ###### Condition 2 ----
                            #if (length(TP_data_cond2) > length(TA_data_cond2)) {
                            #    TA_data_cond2 = append(TA_data_cond2,
                            #                           rep(
                            #                               0,
                            #                               length(TP_data_cond2) - length(TA_data_cond2)
                            #                           ))
                            #} else if (length(TA_data_cond2) > length(TP_data_cond2)) {
                            #    TP_data_cond2 = append(TP_data_cond2,
                            #                           rep(
                            #                               0,
                            #                               length(TA_data_cond2) - length(TP_data_cond2)
                            #                           ))
                            #} else {
                            #    TA_data_cond2 = TA_data_cond2
                            #    TP_data_cond2 = TP_data_cond2
                            #}
                            
                            roc_cond2 = roc(
                                controls = TA_data_cond2,
                                cases = TP_data_cond2[!is.na(TP_data_cond2)],
                                direction = "<",
                                ci = F
                                #partial.auc = c(1, 1 - min(
                                #    cond1_partial, cond2_partial
                            )
                            
                            ##### ROC test ----
                            if (input$between_within == "Between-subjects") {
                                if (input$roc_trunc == "Lowest false ID rate") {
                                    ##### If truncating at lowest false ID rate ----
                                    roc_test = roc.test(
                                        roc_cond1,
                                        roc_cond2,
                                        reuse.auc = FALSE,
                                        partial.auc = c(1, 1 - min(
                                            cond1_partial, cond2_partial
                                        )),
                                        partial.auc.focus = "sp",
                                        method = "bootstrap",
                                        paired = FALSE,
                                        boot.n = input$nboot_iter,
                                        progress = "none"
                                    )
                                } else if (input$roc_trunc == "Highest false ID rate") {
                                    ##### If truncating at highest false ID rate ----
                                    roc_test = roc.test(
                                        roc_cond1,
                                        roc_cond2,
                                        reuse.auc = FALSE,
                                        partial.auc = c(1, 1 - max(
                                            cond1_partial, cond2_partial
                                        )),
                                        partial.auc.focus = "sp",
                                        method = "bootstrap",
                                        paired = FALSE,
                                        boot.n = input$nboot_iter,
                                        progress = "none"
                                    ) 
                                } else {
                                    ##### If truncating at a custom false ID rate ----
                                    roc_test = roc.test(
                                        roc_cond1,
                                        roc_cond2,
                                        reuse.auc = FALSE,
                                        partial.auc = c(1, other_vars$custom_trunc),
                                        partial.auc.focus = "sp",
                                        method = "bootstrap",
                                        paired = FALSE,
                                        boot.n = input$nboot_iter,
                                        progress = "none"
                                    ) 
                                }
                            } else {
                                if (input$roc_trunc == "Lowest false ID rate") {
                                    ##### If truncating at lowest false ID rate ----
                                    roc_test = roc.test(
                                        roc_cond1,
                                        roc_cond2,
                                        reuse.auc = FALSE,
                                        partial.auc = c(1, 1 - min(
                                            cond1_partial, cond2_partial
                                        )),
                                        partial.auc.focus = "sp",
                                        method = "bootstrap",
                                        paired = FALSE,
                                        boot.n = input$nboot_iter,
                                        progress = "none"
                                    )
                                } else if (input$roc_trunc == "Highest false ID rate") {
                                    ##### If truncating at highest false ID rate ----
                                    roc_test = roc.test(
                                        roc_cond1,
                                        roc_cond2,
                                        reuse.auc = FALSE,
                                        partial.auc = c(1, 1 - max(
                                            cond1_partial, cond2_partial
                                        )),
                                        partial.auc.focus = "sp",
                                        method = "bootstrap",
                                        paired = FALSE,
                                        boot.n = input$nboot_iter,
                                        progress = "none"
                                    ) 
                                } else {
                                    ##### If truncating at a custom false ID rate ----
                                    roc_test = roc.test(
                                        roc_cond1,
                                        roc_cond2,
                                        reuse.auc = FALSE,
                                        partial.auc = c(1, other_vars$custom_trunc),
                                        partial.auc.focus = "sp",
                                        method = "bootstrap",
                                        paired = FALSE,
                                        boot.n = input$nboot_iter,
                                        progress = "none"
                                    ) 
                                }
                            }
                            
                            message(roc_test$estimate[1],
                                    roc_test$estimate[2],
                                    roc_test$p.value)
                            
                            message("Completed ROC test")
                            message(i)
                            
                            sim_store$auc_diff[i] = as.numeric(roc_test$estimate[1]) - as.numeric(roc_test$estimate[2])
                            message("Breakpoint 2")
                            sim_store$auc_1[i] = as.numeric(roc_test$estimate[1])
                            message("Breakpoint 3")
                            sim_store$auc_2[i] = as.numeric(roc_test$estimate[2])
                            message("Breakpoint 4")
                            sim_store$auc_p[i] = as.numeric(roc_test$p.value)
                            
                            message("Added AUC values to sim_store")
                            
                            if (input$test_tails == "2_tail") {
                                sim_store$sig[i] = ifelse(sim_store$auc_p[i] < input$alpha_level, 1, 0)
                            } else if (input$test_tails == sprintf("%s > %s",
                                                                   parameters$cond1,
                                                                   parameters$cond2)) {
                                sim_store$sig[i] = ifelse(sim_store$auc_p[i] < input$alpha_level * 2 &
                                                              sim_store$auc_diff[i] > 0, 1, 0)
                            } else {
                                sim_store$sig[i] = ifelse(sim_store$auc_p[i] < input$alpha_level * 2 &
                                                              sim_store$auc_diff[i] < 0, 1, 0)
                            }
                        }
                        
                        message("Recorded test significance in sim_store")
                        
                        #### Generate data for DPP function ----
                        if (input$measure != "pAUC") {
                            TA_dataframe_cond1 = data.frame(conf = TA_data_cond1,
                                                            tpORta = 0,
                                                            cond = "A")
                            
                            TP_dataframe_cond1 = data.frame(conf = TP_data_cond1,
                                                            tpORta = 1,
                                                            cond = "A")
                            
                            TA_dataframe_cond2 = data.frame(conf = TA_data_cond2,
                                                            tpORta = 0,
                                                            cond = "B")
                            
                            TP_dataframe_cond2 = data.frame(conf = TP_data_cond2,
                                                            tpORta = 1,
                                                            cond = "B")
                            
                            data_DPP = rbind(TA_dataframe_cond1,
                                             TP_dataframe_cond1,
                                             TA_dataframe_cond2,
                                             TP_dataframe_cond2) %>% 
                                mutate(cond = as.factor(cond))
                            
                            ##### Bootstrap the DPP values and the difference ----
                            DPP_results = boot(data = data_DPP, 
                                               statistic = DPP_difference,
                                               R = input$nboot_iter)
                            
                            sim_store$dpp_diff[i] = DPP_results$t0[3]
                            sim_store$dpp_1[i] = DPP_results$t0[1]
                            sim_store$dpp_2[i] = DPP_results$t0[2]
                            
                            ###### Adjusting the difference confidence interval based on alpha level/test ----
                            if (input$test_tails == "2_tail") {
                                #generate 95% CIs Bias Corrected and Accelerated
                                confidence_interval_diff = boot.ci(DPP_results, index=3, conf=(1-input$alpha_level), type='bca')
                                ci_diff=confidence_interval_diff$bca[,c(4,5)]
                                
                                
                                sim_store$sig_dpp[i] = ifelse(ci_diff[1] < 0 & ci_diff[2] < 0, 1, 
                                                              ifelse(ci_diff[1] > 0 & ci_diff[2] > 0, 1, 
                                                                     ifelse(is.na(ci_diff[1]) | is.na(ci_diff[2]), NA, 0)))
                                
                            } else if (input$test_tails == sprintf("%s > %s",
                                                                   parameters$cond1,
                                                                   parameters$cond2)) {
                                
                                confidence_interval_diff = boot.ci(results, index=3, conf=(1-(input$alpha_level*2)), type='bca')
                                ci_diff=confidence_interval_diff$bca[,c(4,5)]
                                
                                sim_store$sig_dpp[i] = ifelse(ci_diff[2] < 0, 1, 
                                                              ifelse(is.na(ci_diff[1]) | is.na(ci_diff[2]), NA, 0))
                            } else {
                                confidence_interval_diff = boot.ci(results, index=3, conf=(1-(input$alpha_level*2)), type='bca')
                                ci_diff=confidence_interval_diff$bca[,c(4,5)]
                                
                                sim_store$sig_dpp[i] = ifelse(ci_diff[1] > 0, 1, 
                                                              ifelse(is.na(ci_diff[1]) | is.na(ci_diff[2]), NA, 0))
                            }
                        }
                        
                        #### Insert NAs for power estimates if only one measure is chosen ----
                        if (input$measure == "DPP") {
                            sim_store$sig[i] = NA
                        }
                        
                        if (input$measure == "pAUC") {
                            sim_store$sig_dpp[i] = NA
                        }
                        
                        ##### TESTING: Save condition data to look at later ----
                        TA_data_cond1_store = data.frame(
                            conf_level = TA_data_cond1,
                            cond = "A",
                            culprit_present = "TA",
                            sim = i,
                            n = curr_n,
                            eff = NA,
                            auc1 = ifelse(input$measure == "DPP", NA,
                                          as.numeric(roc_test$estimate[1])),
                            auc2 = ifelse(input$measure == "DPP", NA,
                                          as.numeric(roc_test$estimate[2])),
                            D_stat = ifelse(input$measure == "DPP", NA,
                                            as.numeric(roc_test$statistic)),
                            sd_bootstrap = NA
                        )
                        
                        TP_data_cond1_store = data.frame(
                            conf_level = TP_data_cond1,
                            cond = "A",
                            culprit_present = "TP",
                            sim = i,
                            n = curr_n,
                            eff = NA,
                            auc1 = ifelse(input$measure == "DPP", NA,
                                          as.numeric(roc_test$estimate[1])),
                            auc2 = ifelse(input$measure == "DPP", NA,
                                          as.numeric(roc_test$estimate[2])),
                            D_stat = ifelse(input$measure == "DPP", NA,
                                            as.numeric(roc_test$statistic)),
                            sd_bootstrap = NA
                        )
                        
                        TA_data_cond2_store = data.frame(
                            conf_level = TA_data_cond2,
                            cond = "B",
                            culprit_present = "TA",
                            sim = i,
                            n = curr_n,
                            eff = NA,
                            auc1 = ifelse(input$measure == "DPP", NA,
                                          as.numeric(roc_test$estimate[1])),
                            auc2 = ifelse(input$measure == "DPP", NA,
                                          as.numeric(roc_test$estimate[2])),
                            D_stat = ifelse(input$measure == "DPP", NA,
                                            as.numeric(roc_test$statistic)),
                            sd_bootstrap = NA
                        )
                        
                        TP_data_cond2_store = data.frame(
                            conf_level = TP_data_cond2,
                            cond = "B",
                            culprit_present = "TP",
                            sim = i,
                            n = curr_n,
                            eff = NA,
                            auc1 = ifelse(input$measure == "DPP", NA,
                                          as.numeric(roc_test$estimate[1])),
                            auc2 = ifelse(input$measure == "DPP", NA,
                                          as.numeric(roc_test$estimate[2])),
                            D_stat = ifelse(input$measure == "DPP", NA,
                                            as.numeric(roc_test$statistic)),
                            sd_bootstrap = NA
                        )
                        
                        #data_files$raw_data_store = rbind(data_files$raw_data_store,
                        #                                  TA_data_cond1_store,
                        #                                  TP_data_cond1_store,
                        #                                  TA_data_cond2_store,
                        #                                  TP_data_cond2_store)
                        
                        sim_counter = sim_counter + 1
                        
                        other_vars$sim_counter = other_vars$sim_counter + 1
                        
                        showNotification(
                            sprintf(
                                "%s / %s simulations complete",
                                sim_counter,
                                other_vars$sim_total
                            )
                        )
                    }
                    # Store power estimates
                    pwr_store[h, g] = mean(sim_store$sig, na.rm = TRUE)
                    
                    pwr_store_dpp[h, g] = mean(sim_store$sig_dpp, na.rm = TRUE)
                    
                    # Store AUC difference estimates
                    auc_store[h, g] = mean(sim_store$auc_diff, na.rm = TRUE)
                    
                    ## 95% Quantile on the AUC difference estimate
                    auc_store_ci_upr[h, g] = quantile(sim_store$auc_diff,
                                                      probs = c(.025, .975),
                                                      na.rm = TRUE)[2]
                    auc_store_ci_lwr[h, g] = quantile(sim_store$auc_diff,
                                                      probs = c(.025, .975),
                                                      na.rm = TRUE)[1]
                    
                    # Store Condition 1 AUC
                    auc_1_store[h, g] = mean(sim_store$auc_1, na.rm = TRUE)
                    
                    ## 95% Quantile on the AUC estimate
                    auc_1_store_ci_upr[h, g] = quantile(sim_store$auc_1,
                                                        probs = c(.025, .975),
                                                        na.rm = TRUE)[2]
                    auc_1_store_ci_lwr[h, g] = quantile(sim_store$auc_1,
                                                        probs = c(.025, .975),
                                                        na.rm = TRUE)[1]
                    
                    # Store Condition 2 AUC
                    auc_2_store[h, g] = mean(sim_store$auc_2, na.rm = TRUE)
                    
                    ## 95% Quantile on the AUC estimate
                    auc_2_store_ci_upr[h, g] = quantile(sim_store$auc_2,
                                                        probs = c(.025, .975),
                                                        na.rm = TRUE)[2]
                    auc_2_store_ci_lwr[h, g] = quantile(sim_store$auc_2,
                                                        probs = c(.025, .975),
                                                        na.rm = TRUE)[1]
                    
                    # Store DPP difference estimates
                    dpp_store[h, g] = mean(sim_store$dpp_diff, na.rm = TRUE)
                    
                    ## 95% Quantile on the DPP difference estimate
                    dpp_store_ci_upr[h, g] = quantile(sim_store$dpp_diff,
                                                      probs = c(.025, .975),
                                                      na.rm = TRUE)[2]
                    dpp_store_ci_lwr[h, g] = quantile(sim_store$dpp_diff,
                                                      probs = c(.025, .975),
                                                      na.rm = TRUE)[1]
                    
                    # Store Condition 1 DPP
                    dpp_1_store[h, g] = mean(sim_store$dpp_1, na.rm = TRUE)
                    
                    ## 95% Quantile on the DPP estimate
                    dpp_1_store_ci_upr[h, g] = quantile(sim_store$dpp_1,
                                                        probs = c(.025, .975),
                                                        na.rm = TRUE)[2]
                    dpp_1_store_ci_lwr[h, g] = quantile(sim_store$dpp_1,
                                                        probs = c(.025, .975),
                                                        na.rm = TRUE)[1]
                    
                    # Store Condition 2 DPP
                    dpp_2_store[h, g] = mean(sim_store$dpp_2, na.rm = TRUE)
                    
                    ## 95% Quantile on the DPP estimate
                    dpp_2_store_ci_upr[h, g] = quantile(sim_store$dpp_2,
                                                        probs = c(.025, .975),
                                                        na.rm = TRUE)[2]
                    dpp_2_store_ci_lwr[h, g] = quantile(sim_store$dpp_2,
                                                        probs = c(.025, .975),
                                                        na.rm = TRUE)[1]
                }
            }
        }
        ### generate results dataframes ----
        #### AUC difference ----
        auc_store = auc_store %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = "Avg. AUC difference",
                   -N)
        
        auc_store_ci_upr = auc_store_ci_upr %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = "AUC difference 95% CI upper",
                   -N)
        
        auc_store_ci_lwr = auc_store_ci_lwr %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = "AUC difference 95% CI lower",
                   -N)
        
        #### AUC in Cond 1 ----
        auc_1_store = auc_1_store %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("Avg. AUC in", parameters$cond1, sep = " "),
                   -N)
        
        auc_1_store_ci_upr = auc_1_store_ci_upr %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("AUC", parameters$cond1, "95% CI upper", sep = " "),
                   -N)
        
        auc_1_store_ci_lwr = auc_1_store_ci_lwr %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("AUC", parameters$cond1, "95% CI lower", sep = " "),
                   -N)
        
        #### AUC in Cond 2 ----
        auc_2_store = auc_2_store %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("Avg. AUC in", parameters$cond2, sep = " "),
                   -N)
        
        auc_2_store_ci_upr = auc_2_store_ci_upr %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("AUC", parameters$cond2, "95% CI upper", sep = " "),
                   -N)
        
        auc_2_store_ci_lwr = auc_2_store_ci_lwr %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("AUC", parameters$cond2, "95% CI lower", sep = " "),
                   -N)
        
        #### DPP difference ----
        dpp_store = dpp_store %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = "Avg. DPP difference",
                   -N)
        
        dpp_store_ci_upr = dpp_store_ci_upr %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = "DPP difference 95% CI upper",
                   -N)
        
        dpp_store_ci_lwr = dpp_store_ci_lwr %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = "DPP difference 95% CI lower",
                   -N)
        
        #### DPP in Cond 1 ----
        dpp_1_store = dpp_1_store %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("Avg. DPP in", parameters$cond1, sep = " "),
                   -N)
        
        dpp_1_store_ci_upr = dpp_1_store_ci_upr %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("DPP", parameters$cond1, "95% CI upper", sep = " "),
                   -N)
        
        dpp_1_store_ci_lwr = dpp_1_store_ci_lwr %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("DPP", parameters$cond1, "95% CI lower", sep = " "),
                   -N)
        
        #### DPP in Cond 2 ----
        dpp_2_store = dpp_2_store %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("Avg. DPP in", parameters$cond2, sep = " "),
                   -N)
        
        dpp_2_store_ci_upr = dpp_2_store_ci_upr %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("DPP", parameters$cond2, "95% CI upper", sep = " "),
                   -N)
        
        dpp_2_store_ci_lwr = dpp_2_store_ci_lwr %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("DPP", parameters$cond2, "95% CI lower", sep = " "),
                   -N)
        
        #### Combine the dataframes ----
        pwr_store_dpp = as.data.frame(pwr_store_dpp) %>%
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = "Power (DPP)",
                   -N) %>% 
            select(N, `Effect size`, `Power (DPP)`)
        
        data_files$pwr_store = as.data.frame(pwr_store) %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = "Power (pAUC)",
                   -N) %>% 
            select(N, `Effect size`, `Power (pAUC)`) %>% 
            left_join(pwr_store_dpp) %>% 
            left_join(auc_1_store) %>% 
            left_join(auc_1_store_ci_lwr) %>% 
            left_join(auc_1_store_ci_upr) %>% 
            left_join(auc_2_store) %>% 
            left_join(auc_2_store_ci_lwr) %>% 
            left_join(auc_2_store_ci_upr) %>% 
            left_join(auc_store) %>% 
            left_join(auc_store_ci_lwr) %>% 
            left_join(auc_store_ci_upr) %>% 
            left_join(dpp_1_store) %>% 
            left_join(dpp_1_store_ci_lwr) %>% 
            left_join(dpp_1_store_ci_upr) %>% 
            left_join(dpp_2_store) %>% 
            left_join(dpp_2_store_ci_lwr) %>% 
            left_join(dpp_2_store_ci_upr) %>% 
            left_join(dpp_store) %>% 
            left_join(dpp_store_ci_lwr) %>% 
            left_join(dpp_store_ci_upr)
            
        end_time = Sys.time()
        other_vars$end_time = Sys.time()
        
        duration = round(as.numeric(difftime(end_time, start_time,
                            units = "mins")), 2)

        other_vars$time_taken = 
            paste("Time taken: ", duration, " minutes", sep = "")
        
        output$time_taken = renderText({
            other_vars$time_taken
        })
        
        other_vars$sims_complete = 1
        
        #### TESTING: Save the raw simulation results ----
        #data_files$raw_data_store = filter(data_files$raw_data_store,
        #                                   !is.na(conf_level)) %>% 
        #    rowwise() %>% 
        #    mutate(sd_bootstrap = (auc1 - auc2) / D_stat,
        #           eff = as.character(eff)) %>% 
        #    rename("N" = n,
        #           "Effect size" = eff) %>% 
        #    left_join(data_files$pwr_store)
        #
        #write.csv(data_files$raw_data_store,
        #          "raw_sim_results.csv",
        #          row.names = FALSE,
        #          na = "")
        
        
        #other_vars$time_taken = 
        #    sprintf("Time taken: %s minutes",
        #            round((end_time - start_time)/60, 2))
        
        #message(start_time)
        #message(end_time)
    })
    
    ## render final simulation results ----
    output$pwr_store = renderDataTable({
        data_files$pwr_store
    })
    
    ## render final simulation power plot ----
    output$pwr_plot = renderPlot({
        plots$pwr_plot
    })
    
    ## hide "Simulation Results" tab until simulations are done ----
    
    observeEvent(data_files$pwr_store, {
        if (other_vars$sim_counter == other_vars$sim_total) {
            
            output$results_render = renderMenu({
                menuItem("Simulation Results", tabName = "results_tab", icon = icon("poll"))
            })
            
            if (length(parameters$ns) == 1) {
                plots$pwr_plot = "Only 1 sample size tested, no curves generated"
                    #data_files$pwr_store %>% 
                    #ggplot(aes(x = N,
                    #           y = Power,
                    #           #linetype = `Effect size`,
                    #           color = `Effect size`))+
                    #geom_point(size = 3)+
                    #scale_x_continuous(breaks = parameters$ns)+
                    #apatheme+
                    #labs(x = "\nN",
                    #     y = "Power to detect effect\n")+
                    #theme(text = element_text(size = 20))
                
                shinyjs::hide("power_curves_box")
                
            } else {
                plots$pwr_plot = 
                    data_files$pwr_store %>% 
                    dplyr::select(N, `Power (pAUC)`, `Power (DPP)`, `Effect size`) %>% 
                    gather(key = "Measure",
                           value = "Power",
                           -c(N, `Effect size`)) %>% 
                    ggplot(aes(x = N,
                               y = Power,
                               linetype = Measure,
                               color = `Effect size`))+
                    geom_line()+
                    scale_x_continuous(breaks = parameters$ns)+
                    apatheme+
                    labs(x = "\nN",
                         y = "Power to detect effect\n")+
                    theme(text = element_text(size = 20))
                
                shinyjs::show("power_curves_box")
            }
            
            updateTabItems(session, "tabs", "results_tab")
        } else {
            output$results_render = NULL
        }
    })
    
    observeEvent(other_vars$sims_complete, {
        if (other_vars$sims_complete == 0) {
            shinyjs::show("no_results")
            shinyjs::hide("results_box")
            shinyjs::hide("power_curves_box")
        } else {
            shinyjs::hide("no_results")
            shinyjs::show("results_box")
        }
    })
    
    #observeEvent(other_vars$sim_counter == other_vars$sim_total, {
    #    output$results_render = renderMenu({
    #        menuItem("Simulation Results",
    #                 tabName = "results_tab",
    #                 icon = icon("poll"))
    #    })
    #})
    
    ## generate summary report for download ----
    output$report_dl = downloadHandler(
        filename = function() {
            paste0("roc-sim-report_", Sys.Date(), ".html")
        },
        content = function(file) {
            rmarkdown::render("reports/report.Rmd",
                              output_file = file, 
                              params = list(
                                  sim_params = data_files$sim_params,
                                  hypothetical_plot = plots$hypothetical_plot,
                                  sim_results = data_files$pwr_store,
                                  pwr_plot = plots$pwr_plot,
                                  time_taken = other_vars$time_taken
                              ),
                              envir = new.env(),
                              intermediates_dir = tempdir())
        }
    )
    
    ## upload simulation results to google sheet ----
    observeEvent(input$upload_results, {
        cond_1_greater = paste0(parameters$cond1, " >")
        cond_2_greater = paste0(parameters$cond2, " >")
        
        google_sheet = as_tibble(read_sheet(google_sheet_id, "Power results"))
        
        data_files$upload_data = data_files$pwr_store %>% 
            as_tibble() %>% 
            mutate(sim_id = Sys.time(),
                   `Effect size type` = ifelse(input$eff_type == "constant",
                                               "Constant across confidence levels",
                                               "Different across confience levels"),
                   `Differing effect sizes` = other_vars$effs_different_report,
                   `Between- or within-subjects` = parameters$between_within,
                   `TA lineups/subj per condition` = input$n_TA_lineups,
                   `TP lineups/subj per condition` = input$n_TP_lineups,
                   `Simulated samples` = input$nsims,
                   `AUC/DPP bootstraps` = input$nboot_iter,
                   `Time taken (m)` = parse_number(other_vars$time_taken),
                   `Estimated time taken (m)` = NA,
                   `Test tails` = input$test_tails,
                   `ROC truncation` = input$roc_trunc,
                   `Type I error rate` = input$alpha_level) %>% 
            rename(`Avg. AUC in Cond A` = !!paste("Avg. AUC in", parameters$cond1, sep = " "),
                   `Cond A AUC 95% CI Upper` = !!paste("AUC", parameters$cond1, "95% CI upper", sep = " "),
                   `Cond A AUC 95% CI Lower` = !!paste("AUC", parameters$cond1, "95% CI lower", sep = " "),
                   `Avg. AUC in Cond B` = !!paste("Avg. AUC in", parameters$cond2, sep = " "),
                   `Cond B AUC 95% CI Upper` = !!paste("AUC", parameters$cond2, "95% CI upper", sep = " "),
                   `Cond B AUC 95% CI Lower` = !!paste("AUC", parameters$cond2, "95% CI lower", sep = " "),
                   `AUC difference 95% CI Lower` = "AUC difference 95% CI lower",
                   `AUC difference 95% CI Upper` = "AUC difference 95% CI upper",
                   `Power (pAUC)` = "Power (pAUC)",
                   `Avg. DPP in Cond A` = !!paste("Avg. DPP in", parameters$cond1, sep = " "),
                   `Cond A DPP 95% CI Upper` = !!paste("DPP", parameters$cond1, "95% CI upper", sep = " "),
                   `Cond A DPP 95% CI Lower` = !!paste("DPP", parameters$cond1, "95% CI lower", sep = " "),
                   `Avg. DPP in Cond B` = !!paste("Avg. DPP in", parameters$cond2, sep = " "),
                   `Cond B DPP 95% CI Upper` = !!paste("DPP", parameters$cond2, "95% CI upper", sep = " "),
                   `Cond B DPP 95% CI Lower` = !!paste("DPP", parameters$cond2, "95% CI lower", sep = " "),
                   `DPP difference 95% CI Lower` = "DPP difference 95% CI lower",
                   `DPP difference 95% CI Upper` = "DPP difference 95% CI upper",
                   `Power (DPP)` = "Power (DPP)") %>% 
            mutate(`Test tails` = ifelse(grepl("2_tail", `Test tails`), "Two-tailed",
                                         ifelse(grepl(cond_1_greater, `Test tails`), "A > B", "B > A"))) %>% 
            select(sim_id, 
                   N,
                   `Effect size`,
                   `Effect size type`,
                   `Differing effect sizes`,
                   `Between- or within-subjects`,
                   `Avg. AUC in Cond A`,
                   `Cond A AUC 95% CI Lower`,
                   `Cond A AUC 95% CI Upper`,
                   `Avg. AUC in Cond B`,
                   `Cond B AUC 95% CI Lower`,
                   `Cond B AUC 95% CI Upper`,
                   `Avg. AUC difference`,
                   `AUC difference 95% CI Lower`,
                   `AUC difference 95% CI Upper`,
                   `Power (pAUC)`,
                   `Avg. DPP in Cond A`,
                   `Cond A DPP 95% CI Lower`,
                   `Cond A DPP 95% CI Upper`,
                   `Avg. DPP in Cond B`,
                   `Cond B DPP 95% CI Lower`,
                   `Cond B DPP 95% CI Upper`,
                   `Avg. DPP difference`,
                   `DPP difference 95% CI Lower`,
                   `DPP difference 95% CI Upper`,
                   `Power (DPP)`,
                   `Type I error rate`, 
                   `Test tails`,
                   `ROC truncation`,
                   `TA lineups/subj per condition`,
                   `TP lineups/subj per condition`,
                   `Simulated samples`,
                   `AUC/DPP bootstraps`,
                   `Time taken (m)`,
                   `Estimated time taken (m)`)
        
        data_files$upload_data = rbind(data_files$upload_data, google_sheet) %>% 
            arrange(desc(sim_id)) %>%
            sheet_write(google_sheet_id, "Power results")
        
        #sheet_append(google_sheet_id, data_files$upload_data, "Power results")
        
        showModal(modalDialog(
            title = "Upload complete",
            "Your results have been uploaded. Thank you!"
            ,
            fade = TRUE,
            easyClose = TRUE,
            size = "l"))
    })
    
    ## render results compendium ----
    output$power_results = renderDataTable({
        input$upload_results
        as_tibble(read_sheet(google_sheet_id, "Power results")) %>% 
            arrange(desc(sim_id))
    })
    
    ## recovering results from compendium ----
    ### plotting power curves ----
    observeEvent(input$show_previous_sim, {
        
        data_files$compendium_data = as_tibble(read_sheet(google_sheet_id, "Power results")) %>% 
            mutate(sim_id = as.character(sim_id),
                   `Effect size` = as.factor(`Effect size`))
        
        message(class(data_files$compendium_data$sim_id))
        
        if (length(input$previous_sim_id) == 0) {
            
            showModal(modalDialog(
                title = "Warning",
                "No sim_id entered"))
        } else if (length(input$previous_sim_id) > 0 & nrow(data_files$compendium_data  %>% 
                        filter(sim_id == input$previous_sim_id)) == 0) {
            
            showModal(modalDialog(
                title = "Warning",
                "Entered sim_id not in database. 
                Make sure it has been copied correctly"))
        } else {
            shinyjs::show("previous_sim_box")
            
            showModal(modalDialog(
                title = "Success",
                "Scroll down to the bottom of the page to view the selected results,
                with the option to transfer selected parameter values to your simulation"))
            
            data_files$compendium_data = data_files$compendium_data %>% 
                filter(sim_id == input$previous_sim_id)
            
            length_previous_ns = data_files$compendium_data %>% 
                dplyr::select(N) %>% 
                distinct() %>% 
                nrow() %>% 
                as.numeric()
            
            previous_ns = data_files$compendium_data %>%
                dplyr::select(N) %>%
                distinct() %>% 
                unlist() %>% 
                paste(collapse = ",") %>% 
                as.character()
            
            message(previous_ns)
            
            previous_effs = data_files$compendium_data %>% 
                dplyr::select(`Effect size`) %>% 
                distinct() %>% 
                unlist() %>% 
                paste(collapse = ",") %>% 
                as.character()
            
            message(previous_effs)
            
            if (length_previous_ns == 1) {
                plots$previous_sim_plot = NA
            } else {
                plots$previous_sim_plot = data_files$compendium_data %>% 
                    ggplot(aes(x = N,
                               y = Power,
                               #linetype = `Effect size`,
                               color = `Effect size`))+
                    geom_line()+
                    scale_x_continuous(breaks = data_files$compendium_data$N)+
                    apatheme+
                    labs(x = "\nN",
                         y = "Power to detect effect\n")+
                    theme(text = element_text(size = 20))   
            }
            
            message("Created previous sim plot")
            
            ### Creating dataframes with effects and parameter values ----
            data_files$previous_sim_effs = data_files$compendium_data %>% 
                dplyr::select(N, `Effect size`, `Avg. AUC difference`) %>% 
                dplyr::group_by(`Effect size`) %>% 
                dplyr::summarize("Effect size (AUC difference)" = mean(`Avg. AUC difference`))
            
            message("Created previous sim effects dataframe")
            message(data_files$previous_sim_effs)
            
            data_files$previous_sim_params = data_files$previous_sim_params %>% 
                mutate(Parameter = c(
                    "Ns",
                    "Effects",
                    "# of lineups/subject",
                    "# TA lineups/subject",
                    "# TP lineups/subject",
                    "# of simulated samples per effect size/N",
                    "# of bootstraps per AUC/DPP test",
                    "Partial AUC truncation"),
                    Value = c(previous_ns,
                              previous_effs,
                              data_files$compendium_data$`TA lineups/subj per condition`[1] + data_files$compendium_data$`TP lineups/subj per condition`[1],
                              data_files$compendium_data$`TA lineups/subj per condition`[1],
                              data_files$compendium_data$`TP lineups/subj per condition`[1],
                              data_files$compendium_data$`Simulated samples`[1],
                              data_files$compendium_data$`AUC bootstraps`[1],
                              data_files$compendium_data$`ROC truncation`[1]))
        
            message("Created previous sim parameters dataframe")
            
            output$previous_sim_plot = renderPlot({
                plots$previous_sim_plot
            })
            
            message("Rendered previous sim plot")
            
            ### rendering the dataframes ----
            output$previous_sim_effs = renderDataTable({
                data_files$previous_sim_effs
            })
            
            message("Rendered previous effs data")
            
            output$previous_sim_params = renderDataTable({
                data_files$previous_sim_params
            })
            
            message("Rendered previous params data")
            
            shinyjs::show("previous_sim_box")
            show("transfer_params")
            }
    })
    
    ### Transferring parameter values to main simulation ----
    observeEvent(input$transfer_params, {
        updateNumericInput(session,
                           "n_total_lineups",
                           value = as.numeric(data_files$previous_sim_params$Value[
                               data_files$previous_sim_params$Parameter == "# of lineups/subject"
                           ]))
        
        updateNumericInput(session,
                           "n_TA_lineups",
                           value = as.numeric(data_files$previous_sim_params$Value[
                               data_files$previous_sim_params$Parameter == "# TA lineups/subject"
                           ]))
        
        updateNumericInput(session,
                           "n_TP_lineups",
                           value = as.numeric(data_files$previous_sim_params$Value[
                               data_files$previous_sim_params$Parameter == "# TP lineups/subject"
                           ]))
        
        updateNumericInput(session,
                           "nsims",
                           value = as.numeric(data_files$previous_sim_params$Value[
                               data_files$previous_sim_params$Parameter == "# of simulated samples per effect size/N"
                           ]))
        
        updateNumericInput(session,
                           "nboot_iter",
                           value = as.numeric(data_files$previous_sim_params$Value[
                               data_files$previous_sim_params$Parameter == "# of bootstraps per AUC/DPP test"
                           ]))
        
        updateTextInput(session,
                        "effs",
                        value = data_files$previous_sim_params$Value[
                            data_files$previous_sim_params$Parameter == "Effects"
                        ])
        
        updateTextInput(session,
                        "ns",
                        value = data_files$previous_sim_params$Value[
                            data_files$previous_sim_params$Parameter == "Ns"
                        ])
        
        updateRadioButtons(session,
                           "roc_trunc",
                           selected = data_files$previous_sim_params$Value[
                               data_files$previous_sim_params$Parameter == "Partial AUC truncation"
                           ])
    })
    
    ### rendering the plot ----
    #output$previous_sim_plot = renderPlot({
    #    plots$previous_sim_plot
    #})
    #
    #### rendering the dataframes ----
    #output$previous_sim_effs = renderDataTable({
    #    data_files$previous_sim_effs
    #})
    #
    #output$previous_sim_params = renderDataTable({
    #    data_files$previous_sim_params
    #})
    
    #output$test = renderText({
    #    "Testing this"
    #})
    
    ## Google sheets authorization
    #observeEvent(input$tab, {
    #    gs4_auth(cache = ".secrets", email = "eric7mah@gmail.com")
    #})
}

shinyApp(ui, server)
