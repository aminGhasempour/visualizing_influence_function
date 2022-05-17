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
source("R/estimators_and_ifs.R")
source("R/plotting_functions.R")
source("R/color_cycles.R")

set.seed(42)

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  
  
  # Reactive values
  ## Data generating process'
  dgps <- reactiveValues(dnorm_mix = NULL,
                         rnorm_mix = NULL)
  
  ## Data
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
  estimators <- reactiveValues(mean = NULL, avg_den = NULL,  ate = NULL)
  
  ## Influence functions
  ifs <- reactiveValues(mean = NULL, avg_den = NULL, ate = NULL)
  
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
  observe({
    mean_a <- input$sliderMeanDistA
    std_a <- input$sliderStdDistA
    mean_b <- input$sliderMeanDistB
    std_b <- input$sliderStdDistB
    d_mix <- input$sliderDistributionMix
    
    dgps$dnorm_mix <-  function(x) {
      d_mix * dnorm(x, mean_a, std_a) + (1 - d_mix) * dnorm(x, mean_b, std_b)
    }
    dgps$rnorm_mix <-  function(n) {
      d_mix * rnorm(n, mean_a, std_a) + (1 - d_mix) * rnorm(n, mean_b, std_b)
    }
  })
  
  
  ## Generate data, calculate estimators and influence functions
  observeEvent(input$generateButton, {
    req(dgps$dnorm_mix, dgps$rnorm_mix)
    
    showModal(modalDialog("Generating data and calculating influence functions...", 
                          footer=NULL))
    
    # Setting epsilon mesh
    data$eps <- seq(0, 1, by = input$sliderEpsMesh)
    
    # Generating data and distributions
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
    
    plot_path(data$x_linspace, data$p_x, data$p_x_tilde, path_plot_ranges)
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
      xlbl <- "Epsilon"
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
  output$ifPlot <- renderPlot({
    req(data$eps, data$p_x, data$p_x_tilde)
    
    if (input$checkboxDistDist) {
      x <- data$ddistances
      xlbl <- "Distribution distance"
    }
    else {
      x <- data$eps
      xlbl <- "Epsilon"
    }
    
    # Calculating one step
    t <- estimators[[input$estimator]]
    if_val <- ifs[[input$estimator]]
    one_step <- tail(t, 1) + if_val
    
    plot_curve(x,
               estimators[[input$estimator]], 
               one_step, 
               legend_pos = input$ifLegendPos,
               xlbl = xlbl,
               ylbl = input$estimator)
  })
  
  # Brush handlers
  ## Path plot ranges
  path_plot_ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot1_dblclick, {
    req(data$x, data$p_x, data$p_x_tilde)
    
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      path_plot_ranges$x <- c(brush$xmin, brush$xmax)
      path_plot_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      path_plot_ranges$x <- c(min(data$x), max(data$x))
      path_plot_ranges$y <- c(min(min(data$p_x), min(data$p_x_tilde)),
                            max(max(data$p_x), max(data$p_x_tilde)))
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
    t_est <- tail(estimators[[input$estimator]], 1)
    t_true <- head(estimators[[input$estimator]], 1)
    if_val <- ifs[[input$estimator]]
    t_one_step <- tail(estimators[[input$estimator]], 1) + ifs[[input$estimator]]
    diff_true_est <- abs(t_est - t_true)
    diff_true_one_step <- abs(t_one_step - t_true)
        
    str1 <- paste0("Estimator value: ", sprintf("%.6f", t_est))
    str2 <- paste0("True estimator value: ", sprintf("%.6f", t_true))
    str3 <- paste0("IF value: ", sprintf("%.6f", if_val))
    str4 <- paste0("One-step value: ", sprintf("%.6f", t_one_step))
    str5 <- paste0("Difference true/estimate: ", sprintf("%.3e", 
                                                         diff_true_est))
    str6 <- paste0("Difference true/one-step: ", sprintf("%.3e", 
                                                         diff_true_one_step))
    
    HTML(paste(str1, str2, str3, str4, str5, sep = "<br/>"))
  })
  

})

