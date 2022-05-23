#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("R/dgps.R")
source("R/estimators.R")
source("R/k_fold_cross_validation.R")
source("R/plotting.R")
source("R/color_cycles.R")

set.seed(42)

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  
  
  # Reactive values
  ## Data generating process'
  # TODO: Rewrite so it is based on a history instead?
  dgps <- reactiveValues(dnorm_mix = NULL,
                         rnorm_mix = NULL)
  
  ## Data
  # TODO: Rewrite so it is based on a history instead.
  data <- reactiveValues(x = NULL,
                         x_linspace = NULL,
                         eps = NULL,
                         ddistances = NULL,
                         p_x = NULL,
                         p_x_tilde = NULL,
                         dtilde = NULL,
                         dpath = NULL,
                         dmix_string = NULL)
  
  ## Estimators
  # TODO: Rewrite so it is based on a history instead.
  estimators <- reactiveValues(mean = NULL, avg_den = NULL,  ate = NULL)
  
  ## Influence functions
  # TODO: Rewrite so it is based on a history instead.
  ifs <- reactiveValues(mean = NULL, avg_den = NULL, ate = NULL, k_fold = NULL)
  
  # Functions
  dmix_string_generator <- function() {
    req(data$x)
    mu_a <- input$sliderMeanDistA
    std_a <- input$sliderStdDistA
    mu_b <- input$sliderMeanDistB
    std_b <- input$sliderStdDistB
    m <- input$sliderDistributionMix
        
    str <- paste0("P = ", m, " * N(", mu_a, ", ", std_a, ") + (1 - ", m, ") * N(", mu_b, 
                   ", ", std_b, ")")
    
    return(str)
  }
  
  # Observes
  ## Set DGPS
  ### TODO: Move to the dgps.R file
  observe({
    mean_a <- input$sliderMeanDistA
    std_a <- input$sliderStdDistA
    mean_b <- input$sliderMeanDistB
    std_b <- input$sliderStdDistB
    d_mix <- input$sliderDistributionMix
    
    dgps$dnorm_mix <-  function(x) {
      return(d_mix * dnorm(x, mean_a, std_a) + (1 - d_mix) * 
               dnorm(x, mean_b, std_b))
    }
    dgps$rnorm_mix <-  function(n) {
      p <- rbinom(n, 1, d_mix)
      return(p * rnorm(n, mean_a, std_a) + (1 - p) * rnorm(n, mean_b, std_b))
    }
  })
  
  
  ## Generate data, calculate estimators and influence functions
  observeEvent(input$generateButton, {
    req(dgps$dnorm_mix, dgps$rnorm_mix)
    
    showModal(modalDialog("Generating data and calculating estimators and  influence functions...", 
                          footer=NULL))
    
    # Setting epsilon mesh
    data$eps <- seq(0, 1, by = input$sliderEpsMesh)
    
    # Generating data and distributions
    # TODO: make a list in a list to get the function return instead of using tmp.
    tmp <- dgp(input$sampleSize, dgps$dnorm_mix, dgps$rnorm_mix)
    
    # Storing in reactive variable
    data$x <- tmp$x
    data$x_linspace <- tmp$x_linspace
    data$p_x <- tmp$p_x
    data$p_x_tilde <- tmp$p_x_tilde
    data$dtilde <- tmp$dtilde
    data$dpath <- tmp$dpath
    data$dmix_string <- dmix_string_generator()
    
    # Calculating estimators and influence functions
    tmp <- calculate_estimators_and_ifs(data$x, data$eps, 
                                        dgps$dnorm_mix, data$dtilde, data$dpath)
    
    # Storing in reactive variables
    data$ddistances <- tmp$ddistances
    estimators$mean <- tmp$estimators$mean
    ifs$mean <- tmp$ifs$mean
    estimators$avg_den <- tmp$estimators$avg_den
    ifs$avg_den <- tmp$ifs$avg_den
    
    # k-fold cross validation
    ifs$k_fold <- k_fold_cross_fitting(data$x)
    
    # Set plot ranges
    path_plot_ranges$ranges <- set_initial_plot_ranges()
    
    removeModal()
  })
  
  # Plotting
  ## Data histogram
  output$dataHistPlot <- renderPlot({
    req(data$x)
    hist(data$x, main = "", xlab = "")
  })
  
  ## Path plot
  output$pathPlot <- renderPlot({
    req(data$x, data$p_x, data$p_x_tilde)
    
    plot_path(data$x_linspace, data$p_x, data$p_x_tilde, path_plot_ranges$ranges)
  })
  
  ## Numerical derivative
  output$ndPlot <- renderPlot({
    req(data$eps, data$p_x, data$p_x_tilde)
    
    if (input$checkboxDistDist) {
      x <- data$ddistances
      xlbl <- "Distribution distance"
    }
    else {
      x <- data$eps
      xlbl <- "Path along epsilon/t"
    }
    
    # Calculating numerical derivative
    t <- estimators[[input$estimator]]
    dydx <- (tail(t, 2)[1] - tail(t, 1)) / 
             (tail(data$eps, 2)[1] - tail(data$eps, 1))
    one_step <- tail(t, 1) - dydx
    
    plot_curve(x, 
               t, 
               one_step, 
               legend_pos = input$ndLegendPos, 
               xlbl = xlbl,
               ylbl = input$estimator)
  })
  
  
  ## Influence function
  # TODO: Add history so to show all generated samples for the same DGP. Reset 
  # when DGP is changed.
  output$ifPlot <- renderPlot({
    req(data$eps, data$p_x, data$p_x_tilde)
    
    if (input$checkboxDistDist) {
      x <- data$ddistances
      xlbl <- "Distribution distance"
    }
    else {
      x <- data$eps
      xlbl <- "Path along epsilon/t"
    }
    
    # Calculating one step
    if_val <- ifs[[input$estimator]]
    if_val_k_fold <- ifs$k_fold[[input$estimator]]
    t_one_step <- tail(estimators[[input$estimator]], 1) + ifs[[input$estimator]]
    t_one_step_k_fold <- tail(estimators[[input$estimator]], 1) + ifs$k_fold[[input$estimator]]
    
    plot_curve(x = x,
               y = estimators[[input$estimator]], 
               one_step = t_one_step,
               k_fold = t_one_step_k_fold, 
               legend_pos = input$ifLegendPos,
               xlbl = xlbl,
               ylbl = input$estimator)
  })
  
  # Brush handlers
  ## Path plot ranges
  path_plot_ranges <- reactiveValues(ranges = list(x = NULL, y = NULL))
  
  set_initial_plot_ranges <- reactive({
    req(data$x, data$p_x)
    x <- c(min(data$x), max(data$x))
    y <- c(min(min(data$p_x), min(data$p_x_tilde)),
           max(max(data$p_x), max(data$p_x_tilde)))
    return(list(x = x, y = y))
  })
  
  observeEvent(input$plot1_dblclick, {
    req(data$x, data$p_x, data$p_x_tilde)
    
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      path_plot_ranges$ranges$x <- c(brush$xmin, brush$xmax)
      path_plot_ranges$ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      path_plot_ranges$ranges <- set_initial_plot_ranges()
    }
  })
  
  # Text output
  ## Current DGP
  output$currentDgpText <- renderUI({
    str1 <- "Current settings:"
    str2 <- dmix_string_generator()
    HTML(paste(str1, str2, sep = "<br/>"))
    
  })
  
  ## Data DGP
  output$dataDgpText <- renderUI({
    req(data$dmix_string)
    str1 <- "Data generated by:"
    str2 <- data$dmix_string
    HTML(paste(str1, str2, sep = "<br/>"))
  })
  
  ## Estimator and IF
  output$estimatorAndIfText <- renderUI({
    x_bar <- mean(data$x)
    mean_fun <- function(x) { x * dgps$dnorm_mix(x) }
    mean_true <- integrate(mean_fun, -1000, 1000)$value
    avg_den_fun <- function(x) { dgps$dnorm_mix(x)^2 }
    avg_den_true <- integrate(avg_den_fun, -1000, 1000)$value
    t_est <- tail(estimators[[input$estimator]], 1)
    t_true <- head(estimators[[input$estimator]], 1)
    if_val <- ifs[[input$estimator]]
    if_val_k_fold <- ifs$k_fold[[input$estimator]]
    t_one_step <- tail(estimators[[input$estimator]], 1) + ifs[[input$estimator]]
    t_one_step_k_fold <- tail(estimators[[input$estimator]], 1) + ifs$k_fold[[input$estimator]]
    diff_true_est <- abs(t_est - t_true)
    diff_true_one_step <- abs(t_one_step - t_true)
        
    str <- ""
    if (input$estimator == "mean") {
      str <- append(str, sprintf("Sample mean =  %.6f<br/>True mean = %.6f", x_bar, mean_true))
    }
    else if (input$estimator == "avg_den") {
      str <- append(str, sprintf("True avg density = %.6f", avg_den_true))
    }
    str <- append(str, paste0("T(P) (sample) = ", sprintf("%.6f", t_true)))
    str <- append(str, paste0("T(P_tilde) = ", sprintf("%.6f", t_est)))
    str <- append(str, paste0("T_one-step = ", sprintf("%.6f", t_one_step)))
    str <- append(str, paste0("T_one-step^k-fold = ", sprintf("%.6f", t_one_step_k_fold)))
    str <- append(str, paste0("IF = ", sprintf("%.6f", if_val)))
    str <- append(str, paste0("IF_k-fold = ", sprintf("%.6f", if_val_k_fold)))
    str <- append(str, paste0("|T(P) - T(P_tilde)| = ", sprintf("%.3e", 
                                                        diff_true_est)))
    str <- append(str, paste0("|T(P) - T_one-step| = ", sprintf("%.3e", 
                                                         diff_true_one_step)))
    
    HTML(paste(str, collapse = "<br/>"))
  })
  

})

