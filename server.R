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
  dgps <- reactiveValues(dgp = NULL,
                         dgp_string = NULL,
                         history = list(),
                         string_history = list())
  
  ## Data
  # TODO: Rewrite so it is based on a history instead.
  data <- reactiveValues(data = NULL,
                         history = list())
  
  ## Estimators
  # TODO: Rewrite so it is based on a history instead.
  estimators <- reactiveValues(mean = NULL, avg_den = NULL, ate = NULL)
  
  ## Influence functions
  # TODO: Rewrite so it is based on a history instead.
  ifs <- reactiveValues(mean = NULL, avg_den = NULL, ate = NULL, k_fold = NULL)
  
  # Functions
  # Observes
  ## Observe changes in the DGP
  observe({
    # Create DGP
    dgps$dgp <- generate_dgp(input$sliderMeanDistA, input$sliderStdDistA, 
                             input$sliderMeanDistB, input$sliderStdDistB,
                             input$sliderDistributionMix)
    # Create string describing DGP
    dgps$dgp_string <- generate_dmix_string(input$sliderMeanDistA, 
                                            input$sliderStdDistA, 
                                            input$sliderMeanDistB, 
                                            input$sliderStdDistB, 
                                            input$sliderDistributionMix)
  })
  
  
  ## Generate data, calculate estimators and influence functions
  observeEvent(input$generateButton, {
    req(dgps$dgp$dmix, dgps$dgp$rmix)
    
    showModal(modalDialog("Generating data and calculating estimators and  influence functions...", 
                          footer=NULL))
    
    # Generating data
    data$data <- generate_data(input$sampleSize, dgps$dgp$dmix, dgps$dgp$rmix)
    # Setting epsilon mesh
    # TODO: Remove this slider
    data$data$eps <- seq(0, 1, by = input$sliderEpsMesh)
    
    # Storing the distribution that generated the data and string describing it
    dgps$history <- append(dgps$history, dgps$dgp)
    dgps$string_history <- append(dgps$string_history, dgps$dgp_string)
    
    # Calculating estimators and influence functions
    tmp <- calculate_estimators_and_ifs(data$data$x, data$data$eps, 
                                        dgps$dgp$dmix, data$data$dtilde, 
                                        data$data$dpath)
    
    # Storing in reactive variables
    data$data$ddistances <- tmp$ddistances
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
    req(data$data$x)
    hist(data$data$x, main = "", xlab = "")
  })
  
  ## Path plot
  output$pathPlot <- renderPlot({
    req(data$data$x, data$data$p_x, data$data$p_x_tilde)
    
    plot_path(data$data$x_linspace, data$data$p_x, data$data$p_x_tilde, 
              path_plot_ranges$ranges)
  })
  
  ## Numerical derivative
  output$ndPlot <- renderPlot({
    #req(data$data$eps, data$data$p_x, data$data$p_x_tilde)
    
    if (input$checkboxDistDist) {
      x <- data$data$ddistances
      xlbl <- "Distribution distance"
    }
    else {
      x <- data$data$eps
      xlbl <- "Path along epsilon/t"
    }
    
    # Calculating numerical derivative
    t <- estimators[[input$estimator]]
    dydx <- (tail(t, 2)[1] - tail(t, 1)) / 
             (tail(data$data$eps, 2)[1] - tail(data$data$eps, 1))
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
    #req(data$data$eps, data$data$p_x, data$data$p_x_tilde)
    
    if (input$checkboxDistDist) {
      x <- data$data$ddistances
      xlbl <- "Distribution distance"
    }
    else {
      x <- data$data$eps
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
    req(data$data$x, data$data$p_x)
    return(find_ranges(data$data$x, data$data$p_x, data$data$p_x_tilde))
  })
  
  observeEvent(input$plot1_dblclick, {
    req(data$data$x, data$data$p_x, data$data$p_x_tilde)
    
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
    req(dgps$dgp_string)
    str <- "Current settings:"
    str <- append(str, dgps$dgp_string)
    HTML(paste(str, collapse = "<br/>"))
    
  })
  
  ## Data DGP
  output$dataDgpText <- renderUI({
    req(length(dgps$string_history) > 0)
    str <- "Data generated by:"
    str <- append(str, dgps$dgp_string)
    #str <- append(str, dgps$string_history(length(dgps$string_history)))
    HTML(paste(str, collapse = "<br/>"))
  })
  
  ## Estimator and IF
  output$estimatorAndIfText <- renderUI({
    x_bar <- mean(data$data$x)
    mean_fun <- function(x) { x * dgps$dgp$dmix(x) }
    mean_true <- integrate(mean_fun, -1000, 1000)$value
    avg_den_fun <- function(x) { dgps$dgp$dmix(x)^2 }
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

