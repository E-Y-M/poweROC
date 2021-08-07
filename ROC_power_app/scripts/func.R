# display debugging messages in R (if local) 
# and in the console log (if running in shiny)
debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) message(txt)
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}

debug_sprintf <- function(fmt, ...) {
  debug_msg(sprintf(fmt, ...))
}

`%nin%` = Negate(`%in%`)

most_repeated_character <- function(x) {
  tab <- table(strsplit(x, '')[[1]])
  names(tab)[tab == max(tab)]
}

# APA theme for plots
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

# function to extract numbers from textInput ----
extract <- function(text) {
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  as.numeric(split)
}
