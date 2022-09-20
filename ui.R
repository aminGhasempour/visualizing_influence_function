#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "Visualizing the Influence Function")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data generation", tabName = "tabDataGeneration", icon = icon("th")),
    menuItem("Influence functions", tabName = "tabInfluenceFunctions", 
             icon = icon("th")),
    actionButton(inputId = "generateButton", label = "Generate data")
  )
)

body <- dashboardBody(
  
  # Main row, always showing
  fluidRow(
    box(title = "Histogram over data", status = "primary", 
      plotOutput("dataHistPlot")
    ),
    
    box(title = "Distribution path", status = "primary", 
      plotOutput(
        "pathPlot",
        dblclick = "plot1_dblclick",
        brush = brushOpts(id = "plot1_brush",
                          resetOnNew = TRUE)
      ),
      
      htmlOutput("dataDgpText"),
    ),
  ),
  
  # Tabs
  # TODO: Add the approximated distribution to the histogram.
  tabItems(
    tabItem("tabDataGeneration",
      fluidRow(
        box(title = "Distribution A", status = "warning", width = 4,
          sliderInput("sliderMeanDistA", "Mean of distribution A", min = -3, 
                      max = 3, value = 0, step = 0.1),
          
          sliderInput("sliderStdDistA", "Std of distribution A", min = 0.1, 
                      max = 3, value = 1, step = 0.1)
        ),
        
        box(title = "Distribution B", status = "warning", width = 4,
          sliderInput("sliderMeanDistB", "Mean of distribution B", min = -3, 
                      max = 3, value = 0, step = 0.1),
          
          sliderInput("sliderStdDistB", "Std of distribution B", min = 0.1, 
                      max = 3, value = 1, step = 0.1)
        ),
        
        box(title = "Mixing", status = "warning", width = 4,
          sliderInput("sliderDistributionMix", 
                      "Mixing coefficient for data generation", min = 0, 
                      max = 1, value = 0.5, step = 0.1),
          
          sliderInput("sampleSize", "Sample size", min = 10, max = 1000, 
                      value = 40, step = 10),
          
          htmlOutput("currentDgpText")
          
        )
      )
    ), 
    
    tabItem("tabInfluenceFunctions",
      fluidRow(
        box(title = "Select parameter to estimate", status = "warning", width = 12,
          selectInput(
            inputId = "estimator",
            label = "Select parameter",
            choices = list("Mean" = "mean", "Average density" = "avg_den")
          ), 
          
          checkboxInput("checkboxDistDist", "Distribution distance", 
                        value = TRUE)
          
        )
      ),
      
      fluidRow(
        box(title = "Values", status = "primary", width = 3,
          htmlOutput("estimatorAndIfText")
        ),
        
        box(title = "One-step Estimator", status = "primary", width = 9,
            radioButtons(
              'Onestep',
              'Estimator:',
              c("IF based" = "ifPlot",
                "Numerical derivative based" = "ndPlot"
                
              )
            ),plotOutput("EstPlot")
        )
      )
    )
  )
)

dashboardPage(header, sidebar, body)
