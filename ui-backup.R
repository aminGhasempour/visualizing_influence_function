#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Visualizing Influence Function"),
    
    # Distribution settings
    fluidRow(
      
      column(4,
          sliderInput(
            "sliderMeanDistA",
            "Mean of distribution A",
            min = -3,
            max = 3,
            value = 0,
            step = 0.1
          ),
          
          sliderInput(
            "sliderStdDistA",
            "Std of distribution A",
            min = 0.1,
            max = 3,
            value = 0,
            step = 0.1
          ),
      ),
      
      column(4, 
          sliderInput(
            "sliderMeanDistB",
            "Mean of distribution B",
            min = -3,
            max = 3,
            value = 0,
            step = 0.1
          ),
          
          sliderInput(
            "sliderStdDistB",
            "Std of distribution B",
            min = 0.1,
            max = 3,
            value = 0,
            step = 0.1
          ),
        ),
      
      column(4,
        sliderInput(
          "sliderDistributionMix",
          "Mixing coefficient for data generation",
          min = 0,
          max = 1,
          value = 0,
          step = 0.1
        ),
        
        sliderInput(
          "sampleSize",
          "Sample size",
          min = 100,
          max = 100000,
          value = 100
        ),
        
        actionButton(inputId = "generateButton",
                     label = "Generate data"),
      ),
    ),

    # Path plot
    sidebarLayout(
      sidebarPanel(
        
        sliderInput(
          "epsilon",
          "Value of epsilon",
          min = 0,
          max = 1,
          value = 0,
          step = 0.1
        ),
        
        selectInput(
          inputId = "estimator",
          label = "Select estimator",
          choices = list("Mean" = "mean", "Average density" = "avg_den")
        ),
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput(
          "pathPlot",
          dblclick = "plot1_dblclick",
          brush = brushOpts(id = "plot1_brush",
                            resetOnNew = TRUE)
        )
      )
    ), 
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "ndLegendPos",
          label = "Legend position:",
          choices = c(
            "topright",
            "right",
            "bottomright",
            "topleft",
            "left",
            "bottomleft",
            "top",
            "bottom"
          ),
          selected = "topleft"
        )
      
      # Show a plot of the generated distribution
      ),
    
      mainPanel(plotOutput("ndPlot"))
    ), 
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "ifLegendPos",
          label = "Legend position:",
          choices = c(
            "topright",
            "right",
            "bottomright",
            "topleft",
            "left",
            "bottomleft",
            "top",
            "bottom"
          ),
          selected = "topleft"
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("ifPlot")
      )
    )
))
