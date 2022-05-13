#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("data_generation.R")
source("plotting_functions.R")
source("color_cycles.R")

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  
  # Reactive values
  ## Data generating process'
  dgps <- reactiveValues(dnorm_mix = NULL,
                         rnorm_mix = NULL)
  
  ## Data
  data <- reactiveValues(x = NULL,
                         eps = NULL,
                         distribution_distances = NULL,
                         p_x = NULL,
                         p_x_tilde = NULL,
                         p_eps = NULL,
                         t_values = NULL,
                         if_values = NULL)
  
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
    
    data$eps <- input$sliderEpsMesh
    
    data_tmp <- dgp(dgps$dnorm_mix, dgps$rnorm_mix, input$sampleSize)
    
    data$x <- data_tmp$x
    data$x_linspace <- data_tmp$x_linspace
    data$p_x <- data_tmp$p_x
    data$p_x_tilde <- data_tmp$p_x_tilde
    data$dapprox <- data_tmp$dapprox
    data$deps <- data_tmp$deps
    
    ifs_tmp <- calculate_estimators_and_ifs(data$x_linspace, data$eps, 
    
    removeModal()
  })
  
  # Plotting
  ## Data histogram
  output$dataHistPlot <- renderPlot({
    req(data$x)
    hist(data$x)
  })
  
  ## Path plot
  ### Plotting ranges
  pathPlotRanges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot1_dblclick, {
    req(data$x, data$p_x, data$p_x_tilde)
    
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      pathPlotRanges$x <- c(brush$xmin, brush$xmax)
      pathPlotRanges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      pathPlotRanges$x <- c(min(x), max(x))
      pathPlotRanges$y <- c(min(min(p_x), min(p_x_tilde)),
                            max(max(p_x), max(p_x_tilde)))
    }
  })
  
  output$pathPlot <- renderPlot({
    req(data$x, data$p_x, data$p_x_tilde)
    
    plot_path(data$x, data$p_x, data$p_x_tilde, pathPlotRanges, input$eps)    
  })
  
  ## Numerical derivative
  output$ndPlot <- renderPlot({
    req(data$x, data$p_x, data$p_x_tilde)
    
    eps <- data$eps
    t <- data$t_values[[input$estimator]]
    
    # Numerical derivative
    dydx <-
      (tail(t, 2)[1] - tail(t, 1)) / abs(tail(eps, 1) - tail(eps, 2)[1])
    plot_curve(eps, 
               t, 
               dydx, 
               legend_pos = input$ndLegendPos, 
               title = "Numerical derivative based 1-step")
  })
  
  
  ## Influence function
  output$ifPlot <- renderPlot({
    req(data$x, data$p_x, data$p_x_tilde)
    
    plot_curve(data$eps, 
               data$t_values[[input$estimator]], 
               data$if_values[[input$estimator]], 
               legend_pos = input$ifLegendPos,
               title = "IF based 1-step")
    
  })

})
