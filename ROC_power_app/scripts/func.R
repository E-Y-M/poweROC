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

# modified function to get sdtlu ROC predictions ----
sdtlu_roc_model = function (params, lineup_size, params_quants = NULL, model_type = "sim", 
                           pos_prop = NULL) 
{
    n_params <- length(params)
    p <- params[1]
    mu_t <- params[2]
    sigma_t <- params[3]
    c <- params[4:n_params]
    n_crits <- length(c)
    if (is.null(pos_prop)) {
        pos_prop <- rep(1/lineup_size, lineup_size)
    }
    mu_f <- 0
    sigma_f <- 1
    c_min <- -5
    c_max <- mu_t + 5 * sigma_t
    c_step <- 0.01
    c_seq <- seq(c_min, c_max, c_step)
    c_n_steps <- length(c_seq)
    graph_quants <- FALSE
    if (!is.null(params_quants)) {
        graph_quants <- TRUE
    }
    if (model_type == "sim") {
        results_sim <- sdtlu_graph_model_roc_sim(mu_f, sigma_f, 
                                                 mu_t, sigma_t, lineup_size, c_seq)
        tp_s <- results_sim[[1]]
        ta_s <- results_sim[[2]]
    }
    else if (model_type == "seq") {
        results_seq <- sdtlu_graph_model_roc_seq(mu_f, sigma_f, 
                                                 mu_t, sigma_t, lineup_size, pos_prop, c_seq)
        tp_s <- results_seq[[1]]
        ta_s <- results_seq[[2]]
    }
    if (graph_quants) {
        n_quants <- ncol(params_quants)
        p_ci_lh <- c(params_quants[1, 2], params_quants[1, n_quants - 
                                                            1])
        mu_t_lh <- c(params_quants[2, 2], params_quants[2, n_quants - 
                                                            1])
        sigma_t_lh <- c(params_quants[3, 2], params_quants[3, 
                                                           n_quants - 1])
        c_lh <- matrix(NA, nrow = n_crits, ncol = 2)
        for (i in 1:n_crits) {
            c_lh[i, ] <- c(params_quants[3 + i, 2], params_quants[3 + 
                                                                      i, n_quants - 1])
        }
        if (model_type == "sim") {
            results_sim <- sdtlu_graph_model_roc_sim(mu_f, sigma_f, 
                                                     mu_t_lh[1], sigma_t_lh[1], lineup_size, c_seq)
            tp_s_lo <- results_sim[[1]]
            ta_s_lo <- results_sim[[2]]
            results_sim <- sdtlu_graph_model_roc_sim(mu_f, sigma_f, 
                                                     mu_t_lh[2], sigma_t_lh[2], lineup_size, c_seq)
            tp_s_hi <- results_sim[[1]]
            ta_s_hi <- results_sim[[2]]
        }
        else if (model_type == "seq") {
            results_seq <- sdtlu_graph_model_roc_seq(mu_f, sigma_f, 
                                                     mu_t_lh[1], sigma_t_lh[1], lineup_size, pos_prop, 
                                                     c_seq)
            tp_s_lo <- results_seq[[1]]
            ta_s_lo <- results_seq[[2]]
            results_seq <- sdtlu_graph_model_roc_seq(mu_f, sigma_f, 
                                                     mu_t_lh[2], sigma_t_lh[2], lineup_size, pos_prop, 
                                                     c_seq)
            tp_s_hi <- results_seq[[1]]
            ta_s_hi <- results_seq[[2]]
        }
    }
    old_pty <- par()$pty
    par(pty = "s")
    if (graph_quants) {
        y_max <- max(c(tp_s_hi, ta_s_hi))
    }
    else {
        y_max <- max(c(tp_s, ta_s))
    }
    #title <- paste0("model ROC (size=", lineup_size, ")")
    #plot(NA, ty = "n", xlim = c(0, y_max), ylim = c(0, y_max), 
    #     xlab = "p(suspect | target absent)", ylab = "p(suspect | target present)", 
    #     main = title, cex.lab = 2, cex.main = 2, xaxt = "n", 
    #     yaxt = "n")
    #axis(side = 1, at = c(0, y_max/2, y_max), labels = round(c(0, 
    #                                                           y_max/2, y_max), 2), cex.axis = 2)
    #axis(side = 2, at = c(0, y_max/2, y_max), labels = round(c(0, 
    #                                                           y_max/2, y_max), 2), cex.axis = 2)
    #points(ta_s, tp_s, ty = "l", lwd = 2, col = "red")
    #abline(a = 0, b = 1, col = "gray", lty = 2)
    
    x = list()
    y = list()
    
    for (i in 1:n_crits) {
        pos <- which(abs(c_seq - c[i]) == min(abs(c_seq - c[i])))
        x[i] <- ta_s[pos]
        y[i] <- tp_s[pos]
        #points(x, y, pch = 16, cex = 2.5, col = "red")
    }
    #if (graph_quants) {
    #    points(ta_s_lo, tp_s_lo, ty = "l", lwd = 1, col = "red")
    #    points(ta_s_hi, tp_s_hi, ty = "l", lwd = 1, col = "red")
    #}
    #par(pty = old_pty)
    #monotonic <- all(ta_s[1:(c_n_steps - 1)] - ta_s[2:c_n_steps] > 
    #                     -1e-07)
    
    returned_data = list(x, y, ta_s, tp_s)
    
    return(returned_data)
}

# modified function to get sdtlu data ROC ----
sdtlu_roc_data = function (resp_data, y_max = NULL) 
{
    resp_data <- colSums(resp_data)
    n_resp_cats <- length(resp_data)
    n_conf_levels <- (n_resp_cats - 2)/4
    resp_data_tp_s_props <- resp_data[1:(2 * n_conf_levels + 
                                             1)]
    resp_data_tp_s_props <- resp_data_tp_s_props/sum(resp_data_tp_s_props)
    resp_data_tp_s_props <- resp_data_tp_s_props[1:n_conf_levels]
    resp_data_ta_s_props <- resp_data[(2 * n_conf_levels + 1 + 
                                           1):n_resp_cats]
    resp_data_ta_s_props <- resp_data_ta_s_props/sum(resp_data_ta_s_props)
    resp_data_ta_s_props <- resp_data_ta_s_props[1:n_conf_levels]
    tp_s <- cumsum(resp_data_tp_s_props)
    ta_s <- cumsum(resp_data_ta_s_props)
    old_pty <- par()$pty
    par(pty = "s")
    if (is.null(y_max)) {
        y_max <- max(c(tp_s, ta_s))
    }
    #plot(NA, ty = "n", xlim = c(0, y_max), ylim = c(0, y_max), 
    #     xlab = "p(suspect | target absent)", ylab = "p(suspect | target present)", 
    #     main = "data ROC", cex.lab = 2, cex.main = 2, xaxt = "n", 
    #     yaxt = "n")
    #axis(side = 1, at = c(0, y_max/2, y_max), labels = round(c(0, 
    #                                                           y_max/2, y_max), 2), cex.axis = 2)
    #axis(side = 2, at = c(0, y_max/2, y_max), labels = round(c(0, 
    #                                                           y_max/2, y_max), 2), cex.axis = 2)
    #points(ta_s, tp_s, ty = "b", lwd = 2, col = "blue", pch = 16, 
    #       cex = 2.5)
    #abline(a = 0, b = 1, col = "gray", lty = 2)
    #par(pty = old_pty)
    
    returned_data = list(tp_s,
                         ta_s)
    
    return(returned_data)
}
