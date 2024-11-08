library(shiny)
library(shinythemes)
library(ggplot2)

ui = fluidPage(
  titlePanel("Linear Regression Analysis"),
  theme = shinytheme("superhero"),
  tags$style(HTML("
       #calculate_btn {
         background-color: gray;
         color: white;
         border: none;
         padding: 10px 30px;
         text-align: center;
         font-size: 16px;
         cursor: pointer;
         border-radius: 10px; 
       } 
     ")),
  sidebarLayout(
    sidebarPanel(
      textInput("x", "Enter X values (comma-separated):", value = "21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2"),
      textInput("y", "Enter Y values (comma-separated):", value = "2.62, 2.875, 2.32, 3.215, 3.44, 3.46, 3.57, 3.19, 3.15, 3.44"),
      numericInput("alpha", "Confidence level (alpha):", value = 0.05, min = 0.001, max = 1, step = 0.001),
      checkboxInput("calc_ci", "Calculate Confidence Interval", FALSE),
      
      conditionalPanel(
        condition = "input.calc_ci",
        selectInput("CItype", "Select confidence interval type:",
                    choices = list("Confidence interval for mean response" = "mean",
                                   "Prediction interval for y0" = "prediction",
                                   "Confidence interval for y0 bar with level (1-alpha)" = "Mprediction"),
                    selected = "mean")
      ),
      
      # Panel only for input fields based on CI type
      conditionalPanel(
        condition = "input.CItype == 'mean' && input.calc_ci",
        h3("Mean Response"),
        numericInput("x0", "Enter x0 value for prediction:", value = 14.3),
        numericInput("alpha", "Enter confidence level (alpha):", value = 0.05, min = 0.001, max = 1, step = 0.001)
      ),
      
      conditionalPanel(
        condition = "input.CItype == 'prediction' && input.calc_ci",
        h3("Prediction Interval (y0hat)"),
        numericInput("x0", "Enter x0 value for prediction:", value = 14.3),
        numericInput("alpha", "Enter confidence level (alpha):", value = 0.05, min = 0.001, max = 1, step = 0.001)
      ),
      
      conditionalPanel(
        condition = "input.CItype == 'Mprediction' && input.calc_ci",
        h3("Mprediction Interval (y0bar)"),
        numericInput("x0", "Enter x0 value for prediction:", value = 14.3),
        numericInput("m", "Enter m:", value = 5, min = 0, step = 1),
        numericInput("alpha", "Enter confidence level (alpha):", value = 0.05, min = 0.001, max = 1, step = 0.001)
      ),
      checkboxInput("remove_intercept", "If intercept is non-significant, fit model without intercept?", value = TRUE),
      actionButton("calculate_btn", "Calculate")
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.calculate_btn > 0",
        h4("Scatter Plot"),
        plotOutput("scatterPlot"),
        h4("Regression Summary"),
        verbatimTextOutput("modelSummary"),
        verbatimTextOutput("regressionTests"),
        h4("MSE "),
        verbatimTextOutput("mse"),
        
        conditionalPanel(
          condition = "input.calc_ci",
          h4("Confidence Interval"),
          verbatimTextOutput("predictions"),
          verbatimTextOutput("confidenceIntervalOutput")
        )
      )
    )
  )
)
