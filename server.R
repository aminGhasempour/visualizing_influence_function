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
                         dpath = NULL)
  
  ## Estimators
  estimators <- reactiveValues(mean = NULL, avg_den = NULL,  ate = NULL)
  
  ## Influence functions
  ifs <- reactiveValues(mean = NULL, avg_den = NULL, ate = NULL)
  
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
    
    # Calculating estimators and influence functions
    tmp <- calculate_estimators_and_ifs(data$x_linspace, data$eps, 
                                        dgps$dnorm_mix, data$dtilde, data$dpath)
    print(tmp)
    
    # Storing in reactive variables
    data$ddistances <- tmp$ddistances
    estimators$mean <- tmp$estimators$mean
    ifs$mean <- tmp$ifs$mean
    estimators$avg_den <- tmp$estimators$avg_den
    ifs$avg_den <- tmp$ifs$avg_den
    #print(data$ddistances)
    #print(estimators$mean)
    #print(ifs$mean)
    
    removeModal()
  })
  
  # Plotting
  ## Data histogram
  output$dataHistPlot <- renderPlot({
    req(data$x)
    hist(data$x)
  })
  
  ## Path plot
  output$pathPlot <- renderPlot({
    req(data$x, data$p_x, data$p_x_tilde)
    
    plot_path(data$x_linspace, data$p_x, data$p_x_tilde, path_plot_ranges)
  })
  
  ## Numerical derivative
  output$ndPlot <- renderPlot({
    req(data$eps, data$p_x, data$p_x_tilde)
    
    eps <- data$ddistances
    t <- estimators[[input$estimator]]
    
    # Numerical derivative
    dydx <-
      (tail(t, 2)[1] - tail(t, 1)) / abs(tail(eps, 1) - tail(eps, 2)[1])
    plot_curve(eps, 
               t, 
               dydx, 
               legend_pos = input$ndLegendPos, 
               xlbl = "Distribution distance",
               ylbl = input$estimator)
  })
  
  
  ## Influence function
  output$ifPlot <- renderPlot({
    req(data$eps, data$p_x, data$p_x_tilde)
    
    plot_curve(data$ddistances, 
               estimators[[input$estimator]], 
               ifs[[input$estimator]], 
               legend_pos = input$ifLegendPos,
               xlbl = "Distribution distance",
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
  

})

