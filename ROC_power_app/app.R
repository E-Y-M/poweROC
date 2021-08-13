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
})

## functions ----
source("scripts/func.R") # helper functions
source("scripts/gs4.R") # google sheets functions

## Google Sheets setup ----
#setwd("./ROC_power_app")
#gs4_deauth()
gs4_auth(cache = ".secrets", email = "eric7mah@gmail.com")
google_sheet_id = "12hwA8QHK7D_kgo-OJjGv9xoasgZ2ksChkPtW875aOQQ"

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
                                        prob = c(.5, .5)))

# user interface ----
shinyjs::useShinyjs()

## tabs ----
### introduction ----
intro_tab <- tabItem(
    tabName = "intro_tab",
    box(width = 12,
        collapsible = TRUE,
        title = "What is this?",
        tags$p('This R Shiny app allows users to simulate power for ROC curve analyses of eyewitness lineup data*. This app was heavily inspired by both Boogert et al.`s (2021) ', a(href = 'https://lmickes.github.io/pyWitness/index.html', 'pyWitness', .noWS = "outside"), ' program and Cohen et al.`s (2021) ', a(href = 'https://link.springer.com/article/10.3758%2Fs13428-020-01402-7', 'sdtlu', .noWS = "outside"), ' R package. Both allow for in-depth simulation and analysis of various SDT models from eyewitness lineup data, but simulation for power is not their primary focus. The goal of this app is to provide a simple user-friendly interface for the kinds of ROC analyses commonly conducted in lineup experiments. This app takes as input lineup data with either one condition or two between-subjects conditions, and allows users to visualize various hypothetical ROC curves, simulate datasets by repeatedly sampling from the data under different conditions/effect sizes/sample sizes to provide power estimates, download summary reports of power simulations, upload simulation results for other users, and view the results of previous simulations uploaded by other users (see "Previous simulation results" tab).', .noWS = c("after-begin", "before-end")),
        tags$br(),
        tags$p("*And certain recognition memory designs; see the note in the 'Data Upload' tab")
    ),
    box(width = 12,
        collapsible = TRUE,
        title = "How does it work?",
        tags$p('This app requires an uploaded data file containing lineup data (see the “Data Upload” tab for instructions). This file can contain a single condition (e.g., pilot data) or data from two conditions (e.g., data from another experiment similar to the one being powered for). If the former, the app will automatically duplicate data from the single provided condition to use as a basis for effect size adjustment and comparison. Before simulating data, various parameters will need to be specified (e.g., effect/sample sizes to test, number of simulation samples, one- or two-tailed testing protocol, etc.). The simulations themselves operate like so:'),
        tags$ol(
            tags$li("For each specified effect size:"),
            tags$li("   Apply that effect size to the # of correct IDs for the 2nd condition in the data file", style="white-space: pre-wrap"),
            tags$li("	Calculate the new proportion of correct IDs at each confidence level", style="white-space: pre-wrap"),
            tags$li("	For each specified sample size:", style="white-space: pre-wrap"),
            tags$li("	  For each simulation sample:", style="white-space: pre-wrap"),
            tags$li("		 Sample lineup outcomes/confidence according to the newly defined proportions", style="white-space: pre-wrap"),
            tags$li("		 Compute ROC curves for each condition and compare via pROC partial AUC analysis, as per Gronlund et al.`s (2014) ", a(href = 'http://mickeslab.com/handy/roc-tutorial/', 'ROC tutorial', .noWS = "outside"), style="white-space: pre-wrap", .noWS = c("after-begin", "before-end")),
            tags$li("		 Record test significance", style="white-space: pre-wrap"),
            tags$li("Record proportion of significant tests at each effect size/N", style="white-space: pre-wrap")),
        tags$br(),
        tags$p(strong('NOTE:'), ' Due to the computationally intensive bootstrap resampling involved in ROC analyses, simulations can potentially take a long time (e.g., several hours). Thus, users may want to download a local copy of the app to run in R/RStudio (see link below) to avoid simulation disruption with dropped internet connections. Whether running the web or a local version, it is also recommended that hibernation settings be temporarily disabled.'),
        tags$p('Complete source code for this app can be downloaded from GitHub at ', a(href = 'https://github.com/E-Y-M/poweROC', 'https://github.com/E-Y-M/poweROC', .noWS = "outside"), ', and any issues can be reported at ', a(href = 'https://github.com/E-Y-M/poweROC/issues', 'https://github.com/E-Y-M/poweROC/issues', .noWS = "outside"), '. This app is very much in the beta stage, so feedback/suggestions/bug reports are very much appreciated!', .noWS = c("after-begin", "before-end"))
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
            'Upload your data (.csv format) here. Data files uploaded are NOT saved to the server and are only used for a given session. Data must be formatted so that each row represents a single lineup decision by a single participant. Data files must contain the following (case-sensitive) columns:'
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
                ': The between-subjects condition for that participant/lineup. Only necessary to include if you have data with two pre-existing conditions (which is recommended), otherwise the variable will be created and populated automatically. Note that the condition that comes 2nd alphabetically will be the one that effect sizes are applied to.'
            )
        ),
        tags$br(),
        tags$p(strong('NOTE:'), ' While this app is primarily designed for lineup data, it can also be used for general recognition memory experiments where participants study items then are tested for recognition of old/new items. In this case, each row should be a recognition trial with hits and false alarms recoded to "suspect" for id_type (rejections recoded to "reject"), and culprit_present denoting old/new status.'),
        tags$p('See the example data file below for proper formatting'),
        fileInput(
            "user_data",
            "Upload your data",
            multiple = FALSE,
            accept = ".csv"
        ),
        hidden(radioButtons(
            "designated_suspect",
            "Does your data contain a designated innocent suspect?",
            choices = c("Yes",
                        "No"),
            selected = "Yes"
        )),
        bsTooltip("designated_suspect",
                  "If your data does not contain a designated innocent suspect, the app will automatically convert culprit-absent filler IDs to suspect IDs with a probability of 1/lineup size",
                  placement = "left",
                  trigger = "hover"),
        htmlOutput("cond1_lineup_text"),
        hidden(numericInput(
            "lineup_size_1",
            "",
            value = 6,
            min = 1
        )),
        htmlOutput("cond2_lineup_text"),
        hidden(numericInput(
            "lineup_size_2",
            "",
            value = 6,
            min = 1
        ))
    ),
    box(width = 6,
        collapsible = TRUE,
        title = "Example data",
        dataTableOutput("example_data")),
    box(width = 6,
        collapsible = TRUE,
        title = "Your data",
        div(style = 'overflow-x: scroll',dataTableOutput("processed_data")))
)

### simulation parameters tab ----
parameters_tab = tabItem(tabName = "parameters_tab",
                         fluidRow(
                             column(
                                 3,
                                 textInput(
                                     "effs",
                                     "Effect sizes to test",
                                     value = "",
                                     placeholder = "0.5, 1.5"
                                 ),
                                 bsTooltip("effs",
                                           "Specify effect sizes to test in a comma-separated list. In this app, effect sizes are operationalized as a multiplier to apply to the # of correct IDs at each confidence level for the 2nd condition in your data file. E.g., an effect size of 2 means that the 2nd condition results in 2x more correct IDs at each confidence level. If you have data for two conditions and only wish to test the effect size in the data, leave this as 1",
                                           placement = "bottom",
                                           trigger = "hover"),
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
                                 3,
                                 numericInput("n_total_lineups",
                                              "# of lineups/subject",
                                              value = 2,
                                              min = 1,
                                              step = 1),
                                 bsTooltip("n_total_lineups",
                                           "Specify the total # of lineups each subject will complete",
                                           placement = "bottom",
                                           trigger = "hover"),
                                 hidden(numericInput(
                                     "n_TA_lineups",
                                     "# of TA lineups/subject",
                                     value = .5,
                                     min = 0
                                 )),
                                 bsTooltip("n_TA_lineups",
                                           "Specify the # of target-absent lineups each subject will complete. If you have an odd # of total lineups (e.g., each subject completes either 2 TA and 1 TP lineup or 1 TA and 2 TP lineups), then divide the total # of lineups by 2 and the program will adjust for this in sampling.",
                                           placement = "bottom",
                                           trigger = "hover"),
                                 hidden(numericInput(
                                     "n_TP_lineups",
                                     "# of TP lineups/subject",
                                     value = .5,
                                     min = 0
                                 )),
                                 bsTooltip("n_TP_lineups",
                                           "Specify the # of target-present lineups each subject will complete. If you have an odd # of total lineups (e.g., each subject completes either 2 TA and 1 TP lineup or 1 TA and 2 TP lineups), then divide the total # of lineups by 2 and the program will adjust for this in sampling.",
                                           placement = "bottom",
                                           trigger = "hover")
                             ),
                             column(
                                 3,
                                 numericInput(
                                     "nsims",
                                     "# of simulated samples per effect size/N",
                                     value = 100,
                                     min = 1
                                 ),
                                 bsTooltip(
                                     "nsims",
                                     "Specify the # of samples to simulate for each effect size/N combination. 100 provides relatively stable estimates, but if time is not a concern recommend a larger # (e.g., 500-1000)",
                                     placement = "bottom",
                                     trigger = "hover"
                                 ),
                                 numericInput(
                                     "nboot_iter",
                                     "# of bootstraps per AUC test",
                                     value = 1000,
                                     min = 1
                                 ),
                                 bsTooltip(
                                     "nboot_iter",
                                     "Specify the # of bootstrap iterations per pROC AUC test. 1000 provides relatively stable estimates, but if time is not a concern or if the AUC difference in question is small, recommend upping to the pROC default of 2000",
                                     #"Specify the # of bootstrap iterations per pROC AUC test. 1000 provides relatively stable estimates, but if time is not a concern recommend upping to pROC's default of 2000",
                                     placement = "bottom",
                                     trigger = "hover"
                                 )
                             ), 
                             column(
                                 3,
                                 #radioButtons(
                                 #    "roc_paired",
                                 #    "Paired ROCs (i.e., within-subjects comparison)?",
                                 #    choices = c("No", "Yes"),
                                 #    selected = "No"
                                 #),
                                 radioButtons(
                                     "roc_trunc",
                                     "Partial AUC truncation",
                                     choices = c("Highest false ID rate",
                                                 "Lowest false ID rate"),
                                     selected = "Lowest false ID rate"
                                 ),
                                 bsTooltip("roc_trunc",
                                           "Specify whether the AUC comparison will occur at the highest or lowest false ID rate between your conditions",
                                           placement = "bottom",
                                           trigger = "hover"),
                                 uiOutput("test_tails"),
                                 bsTooltip("test_tails",
                                           "If the AUC comparison will be one-sided, select the test direction",
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
                                           trigger = "hover"),
                                 hidden(actionButton(
                                     "sim_start",
                                     "Simulate"
                                 )),
                                 textOutput("start_time")
                             )
                         ),
                         fluidRow(
                             tags$p(strong("Plot of hypothetical ROCs to test")),
                             plotOutput("ROC_data_plot")
                         ))

### simulation results tab ----
results_tab = tabItem(
    tabName = "results_tab",
    box(
        width = 12,
        title = "Results",
        collapsible = TRUE,
        textOutput("time_taken"),
        tags$br(),
        dataTableOutput("pwr_store"),
        tags$br(),
        tags$p(strong("You can download a summary report of the power analysis by clicking the button below. You can also (anonymously) upload your power analysis results to our compendium--doing so helps other users and provides information I can use to improve this app.")),
        downloadButton("report_dl",
                       "Download summary report"),
        actionButton("upload_results",
                     "Upload analysis results",
                     icon = icon("upload"))
    ),
    bsTooltip(
        "upload_results",
        "Anonymously upload your analysis results for others to use. Note that this does not upload your data, just the power simulation results",
        placement = "bottom",
        trigger = "hover"
    ),
    box(
        width = 12,
        title = "Power curves",
        collapsible = TRUE,
        plotOutput("pwr_plot")
    )
)

### results compendium tab ----
previous_tab = tabItem(tabName = "previous_tab",
                       box(width = 12,
                           title = "Previous simulation results",
                           tags$p("This page shows power analysis results uploaded by other users. Results are anonymously uploaded to a Google Sheet linked to this app."),
                           div(style = 'overflow-x: scroll', dataTableOutput("power_results"))))

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
            menuItem("Introduction", tabName = "intro_tab", icon = icon("info-circle")),
            menuItem("Data Upload", tabName = "data_tab", icon = icon("table")),
            menuItem("Simulation Parameters", tabName = "parameters_tab", icon = icon("gear")),
            #menuItem("Simulation Results", tabName = "results_tab", icon = icon("poll"))
            sidebarMenuOutput("results_render"),
            menuItem("Previous simulation results", tabName = "previous_tab", icon = icon("history"))
            #sidebarMenuOutput("trench_toggle"),
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
            data_tab,
            parameters_tab,
            results_tab,
            previous_tab
        )
    )
)

# server ----
server <- function(input, output, session) {
    ## data files ----
    data_files = reactiveValues(user_data = NULL,
                                processed_data = NULL,
                                saved_data = NULL,
                                upload_data = NULL,
                                pwr_store = NULL,
                                sim_params = data.frame(Parameter = rep(NA, times = 10)))
    
    output$example_data = renderDataTable({
        example_data
    })
    
    observeEvent(input$sim_start, {
        data_files$sim_params = data_files$sim_params %>% 
            mutate(Parameter = c(
                "Ns",
                "Effects",
                "# of lineups/subject",
                "# TA lineups/subject",
                "# TP lineups/subject",
                "# of simulated samples per effect size/N",
                "# of bootstraps per AUC test",
                "Partial AUC truncation",
                "Two-tailed or one-tailed?",
                "Type I error rate"),
                Value = c(input$ns,
                            input$effs,
                            input$n_total_lineups,
                            input$n_TA_lineups,
                            input$n_TP_lineups,
                            input$nsims,
                            input$nboot_iter,
                            input$roc_trunc,
                            input$test_tails,
                            input$alpha_level))
    })
    
    observeEvent(input$user_data, {
        data_files$user_data = read_csv(input$user_data$datapath)
    })
    
    observeEvent(input$designated_suspect, {
        req(data_files$processed_data)
        if (input$designated_suspect == "Yes") {
            
            output$cond1_lineup_text = NULL
            output$cond2_lineup_text = NULL

            
            hide("lineup_size_1")
            hide("lineup_size_2")
        } else {
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
        }
    })

    observeEvent(input$user_data, {
        if (!c("id_type", "conf_level", "culprit_present") %in% colnames(data_files$user_data)) {
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
            if (length(unique(data_files$user_data$cond)) > 1) {
                minimum_conf = min(data_files$user_data$conf_level)
                
                if (minimum_conf == 0) {
                    data_files$processed_data = data_files$user_data %>% 
                        mutate(id_type = tolower(id_type),
                               culprit_present = tolower(culprit_present),
                               cond = as.character(cond),
                               conf_level = conf_level + 1,
                               conf_level_rev = max(conf_level)+1 - conf_level) %>% 
                        arrange(cond)
                } else {
                    data_files$processed_data = data_files$user_data %>% 
                        mutate(id_type = tolower(id_type),
                               culprit_present = tolower(culprit_present),
                               cond = as.character(cond),
                               conf_level = conf_level,
                               conf_level_rev = max(conf_level)+1 - conf_level) %>% 
                        arrange(cond)
                }
            } else {
                minimum_conf = min(data_files$user_data$conf_level)
                
                if (minimum_conf < 1) {
                    data_files$processed_data = data_files$user_data %>% 
                        rbind(data_files$user_data) %>% 
                        mutate(cond = rep(c("A", "B"), each = nrow(data_files$user_data)),
                               id_type = tolower(id_type),
                               culprit_present = tolower(culprit_present),
                               conf_level = conf_level + 1,
                               conf_level_rev = max(conf_level)+1 - conf_level) %>% 
                        arrange(cond)
                } else {
                    data_files$processed_data = data_files$user_data %>% 
                        rbind(data_files$user_data) %>% 
                        mutate(cond = rep(c("A", "B"), each = nrow(data_files$user_data)),
                               id_type = tolower(id_type),
                               culprit_present = tolower(culprit_present),
                               conf_level = conf_level,
                               conf_level_rev = max(conf_level)+1 - conf_level) %>% 
                        arrange(cond)
                }

            }
            data_files$saved_data = data_files$processed_data
            show("designated_suspect")
            message("Created processed data")
        }
    })
    
    output$user_data = renderDataTable({
        data_files$user_data
    })
    
    output$processed_data = renderDataTable({
        data_files$processed_data
    })
    
    
    ## deal with datasets without designated innocent suspects ----
    observeEvent(c(input$designated_suspect, input$lineup_size_1, input$lineup_size_2), {
        if (input$designated_suspect == "Yes") {
            data_files$processed_data = data_files$saved_data
        } else {
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
    })
    
    ## simulation parameters ----
    parameters = reactiveValues(effs = vector(),
                                ns = vector(),
                                n_TA_lineups = NA,
                                n_TP_lineups = NA,
                                cond1 = NA,
                                cond2 = NA)
                                #roc_paired = NA)
    
    observeEvent(data_files$processed_data, {
        #parameters$cond1 = data_files$processed_data %>% 
        #    as.data.frame() %>% 
        #    select(cond) %>% 
        #    unique() %>% 
        #    slice(1) %>% 
        #    as.character()
        
        parameters$cond1 = as.character(unique(data_files$processed_data$cond)[1])
        
        #parameters$cond2 = data_files$processed_data %>% 
        #    as.data.frame() %>% 
        #    select(cond) %>% 
        #    unique() %>% 
        #    slice(2) %>% 
        #    as.character()
        
        parameters$cond2 = as.character(unique(data_files$processed_data$cond)[2])
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
                           pwr_plot = NULL)
    
    ## other reactive variables ----
    other_vars = reactiveValues(sim_counter = 0,
                                sim_total = NA,
                                time_taken = NA,
                                start_time = NA,
                                end_time = NA,
                                avg_n = NA,
                                end_time_est = NA,
                                duration_est = NA)
    
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
    
    ### one- or two-tailed test ----
    output$test_tails = renderUI({
        opt = c("Two-tailed" = "2_tail",
                sprintf("%s > %s",
                        parameters$cond1,
                        parameters$cond2),
                sprintf("%s > %s",
                        parameters$cond2,
                        parameters$cond1))
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
    
    ### generate hypothetical ROCs before simulation ----
    observeEvent(c(input$effs, input$ns, input$roc_trunc), {
        req(data_files$processed_data)
        req(parameters$effs)
        req(parameters$ns)
        
        #### getting proportion data from each condition ----
        data_props = data_files$processed_data %>%
            group_by(id_type, culprit_present, cond) %>% 
            count() %>% 
            ungroup() %>% 
            group_by(culprit_present, cond) %>% 
            mutate(total = sum(n),
                   prop = n/total) %>% 
            ungroup()
        
        message("Processed proportion data")
        
        ##### Getting TA & TP suspect proportions for Condition 1 ----
        cond1_TA_susp_prop = data_props %>% 
            filter(cond == unique(data_props$cond)[1] &
                       culprit_present == "absent" & 
                       id_type == "suspect") %>% 
            dplyr::select(prop) %>% 
            as.numeric()
        
        cond1_TP_susp_prop = data_props %>% 
            filter(cond == unique(data_props$cond)[1] &
                       culprit_present == "present" & 
                       id_type == "suspect") %>% 
            dplyr::select(prop) %>% 
            as.numeric()
        
        ##### Getting TA & TP suspect proportions for Condition 2 ----
        cond2_TA_susp_prop = data_props %>% 
            filter(cond == unique(data_props$cond)[2] &
                       culprit_present == "absent" & 
                       id_type == "suspect") %>% 
            dplyr::select(prop) %>% 
            as.numeric()
        
        cond2_TP_susp_prop = data_props %>% 
            filter(cond == unique(data_props$cond)[2] &
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
            data = data_original
            eff = parameters$effs[g]
            for (h in 1:nrow(data)) {
                if (data$culprit_present[h] == "present" & data$cond[h] == unique(data$cond)[2]) {
                    data$n[h] = round(data$n[h]*eff)
                } else {
                    data$n[h] = data$n[h]
                }
            }
            
            data$prop = data$n / data$total
            
            for (i in 1:length(unique(data$cond))) {
                curr_cond = unique(data$cond)[i]
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
            partial_threshold = ROC_data_wide %>% 
                filter(criteria == max_criteria) %>% 
                select(absent) %>% 
                min() 
        } else {
            partial_threshold = ROC_data_wide %>% 
                filter(criteria == max_criteria) %>% 
                select(absent) %>% 
                max()
        }

        message("Created data for plotting")
        message(ROC_data_wide)
        
        
        ROC_data_plot = ROC_data_wide %>% 
            ggplot(aes(x = absent, y = present, color = cond, linetype = as.factor(eff)))+
            geom_point(alpha = .5)+
            geom_line()+
            geom_vline(xintercept = partial_threshold)+
            apatheme+
            labs(x = "\nFalse ID rate",
                 y = "Correct ID rate\n",
                 linetype = "Effect",
                 color = "Condition")+
            theme(text = element_text(size = 20))
        
        plots$hypothetical_plot = ROC_data_plot
        
        output$ROC_data_plot = renderPlot({
            ROC_data_plot
        })
        
    })
    
    ## main simulation loop ----
    observeEvent(input$sim_start, {
        other_vars$start_time = Sys.time()
        start_time = Sys.time()
        
        other_vars$sim_total = input$nsims * length(parameters$ns) * length(parameters$effs)
        duration = (other_vars$avg_n/1000 * input$nboot_iter/1000) * other_vars$sim_total
        other_vars$duration_est = duration/60
        other_vars$end_time_est = start_time + duration
        
        message(other_vars$end_time_est)
        
        showModal(modalDialog(HTML(sprintf("Start time: %s <br/>Estimated completion time is %s<br/>Do not close this tab/window until you see the 'Simulation Results' tab appear on the left",
                            other_vars$start_time,
                            other_vars$end_time_est)),
                    fade = FALSE,
                    easyClose = FALSE,
                    size = "l"))
        
        sim_store = data.frame(auc_p = rep(NA, times = input$nsims))
        pwr_store = matrix(nrow = length(parameters$ns),
                           ncol = length(parameters$effs))
        auc_store = matrix(nrow = length(parameters$ns),
                           ncol = length(parameters$effs))
        auc_1_store = matrix(nrow = length(parameters$ns),
                             ncol = length(parameters$effs))
        auc_2_store = matrix(nrow = length(parameters$ns),
                             ncol = length(parameters$effs))
        
        show("sim_progress")
        sim_counter = 0
        other_vars$sim_counter = 0
        
        
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
            
            data = data_original
            
            ### Create the root data file to sample from across the Ns and sims ----
            for (z in 1:nrow(data)) {
                if (data$culprit_present[z] == "present" &
                    data$cond[z] == unique(data$cond)[2]) {
                    data$n[z] = round(data$n[z] * eff)
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
                                            cond == unique(data$cond)[1])
            
            # Empty the root vector
            #TA_ROC_cond1_root = vector()
            #
            #for (y in 1:nrow(TA_data_cond1)) {
            #  TA_ROC_cond1_root = c(TA_ROC_cond1_root, rep(TA_data_cond1$conf_level_rev[y], times = TA_data_cond1$n[y]))
            #}
            
            ##### TP ----
            TP_data_cond1_root = filter(data,
                                        culprit_present == "present" &
                                            cond == unique(data$cond)[1])
            
            #### For Condition 2 ----
            ##### TA ----
            TA_data_cond2_root = filter(data,
                                        culprit_present == "absent" &
                                            cond == unique(data$cond)[2])
            
            ##### TP ----
            TP_data_cond2_root = filter(data,
                                        culprit_present == "present" &
                                            cond == unique(data$cond)[2])
            
            ### loop over Ns ####
            for (h in 1:length(parameters$ns)) {
                curr_n = parameters$ns[h]
                #if (parameters$roc_paired == FALSE) {
                #    curr_n = parameters$ns[h]
                #} else {
                #    curr_n = parameters$ns[h] * 2
                #}
                
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
                    
                    
                    ###### For Condition 1 ----
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
                    
                    ### ROC test ----
                    
                    if (input$roc_trunc == "Lowest false ID rate") {
                        #### If truncating at lowest false ID rate ----
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
                    } else {
                        #### If truncating at highest false ID rate ----
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
                    }
                    
                    sim_store$auc_diff[i] = roc_test$estimate[1] - roc_test$estimate[2]
                    sim_store$auc_1[i] = roc_test$estimate[1]
                    sim_store$auc_2[i] = roc_test$estimate[2]
                    sim_store$auc_p[i] = roc_test$p.value
                    
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
                pwr_store[h, g] = mean(sim_store$sig)
                auc_store[h, g] = mean(sim_store$auc_diff)
                auc_1_store[h, g] = mean(sim_store$auc_1)
                auc_2_store[h, g] = mean(sim_store$auc_2)
            }
        }
        ### generate resuts dataframes ----
        auc_store = auc_store %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = "Avg. AUC difference",
                   -N)
        
        auc_1_store = auc_1_store %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("Avg. AUC in", parameters$cond1, sep = " "),
                   -N)
        
        auc_2_store = auc_2_store %>% 
            as.data.frame() %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = !!paste("Avg. AUC in", parameters$cond2, sep = " "),
                   -N)
        
        data_files$pwr_store = as.data.frame(pwr_store) %>% 
            `colnames<-`(parameters$effs) %>% 
            mutate(N = parameters$ns) %>% 
            gather(key = "Effect size",
                   value = "Power",
                   -N) %>% 
            select(N, `Effect size`, `Power`) %>% 
            left_join(auc_1_store) %>% 
            left_join(auc_2_store) %>% 
            left_join(auc_store)
            
        end_time = Sys.time()
        other_vars$end_time = Sys.time()

        other_vars$time_taken = 
            paste("Time taken: ", (end_time - start_time)/60, " minutes", sep = "")
        
        output$time_taken = renderText({
            other_vars$time_taken
        })
            
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
                plots$pwr_plot = 
                    data_files$pwr_store %>% 
                    ggplot(aes(x = N,
                               y = Power,
                               #linetype = `Effect size`,
                               color = `Effect size`))+
                    geom_point(size = 3)+
                    scale_x_continuous(breaks = parameters$ns)+
                    apatheme+
                    labs(x = "\nN",
                         y = "Power to detect effect\n")+
                    theme(text = element_text(size = 20))
            } else {
                plots$pwr_plot = 
                    data_files$pwr_store %>% 
                    ggplot(aes(x = N,
                               y = Power,
                               linetype = `Effect size`,
                               color = `Effect size`))+
                    geom_line()+
                    scale_x_continuous(breaks = parameters$ns)+
                    apatheme+
                    labs(x = "\nN",
                         y = "Power to detect effect\n")+
                    theme(text = element_text(size = 20))
            }
        } else {
            output$results_render = NULL
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
                   `TA lineups/subj` = input$n_TA_lineups,
                   `TP lineups/subj` = input$n_TP_lineups,
                   `Simulated samples` = input$nsims,
                   `AUC bootstraps` = input$nboot_iter,
                   `Time taken (m)` = parse_number(other_vars$time_taken),
                   `Estimated time taken (m)` = other_vars$duration_est,
                   `Test tails` = input$test_tails,
                   `ROC truncation` = input$roc_trunc,
                   `Type I error rate` = input$alpha_level) %>% 
            rename(`Avg. AUC in Cond A` = !!paste("Avg. AUC in", parameters$cond1, sep = " "),
                   `Avg. AUC in Cond B` = !!paste("Avg. AUC in", parameters$cond2, sep = " ")) %>% 
            mutate(`Test tails` = ifelse(grepl("2_tail", `Test tails`), "Two-tailed",
                                         ifelse(grepl(cond_1_greater, `Test tails`), "A > B", "B > A"))) %>% 
            select(sim_id, 
                   N,
                   `Effect size`,
                   `Avg. AUC in Cond A`,
                   `Avg. AUC in Cond B`,
                   `Avg. AUC difference`,
                   Power,
                   `Type I error rate`, 
                   `Test tails`,
                   `ROC truncation`,
                   `TA lineups/subj`,
                   `TP lineups/subj`,
                   `Simulated samples`,
                   `AUC bootstraps`,
                   `Time taken (m)`,
                   `Estimated time taken (m)`)
        
        data_files$upload_data = rbind(data_files$upload_data, google_sheet) %>% 
            arrange(sim_id) %>% 
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
        as_tibble(read_sheet(google_sheet_id, "Power results"))
    })
    
}

shinyApp(ui, server)
