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
      textAreaInput("x", "Enter X values (comma-separated):", value = "21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2"),
      textAreaInput("y", "Enter Y values (comma-separated):", value = "2.62, 2.875, 2.32, 3.215, 3.44, 3.46, 3.57, 3.19, 3.15, 3.44"),
      numericInput("alpha", "Confidence level (alpha):", value = 0.05, min = 0.001, max = 1, step = 0.001),
      checkboxInput("calc_ci", "Calculate Confidence Interval", value = FALSE),
      
      conditionalPanel(
        condition = "input.calc_ci",
        selectInput("CItype", "Select confidence interval type:",
                    choices = list("Confidence interval for mean response" = "mean",
                                   "Prediction interval for y0" = "prediction",
                                   "Confidence interval for y0 bar with level (1-alpha)" = "Mprediction"),
                    selected = "mean")
      ),
      
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

server = function(input, output, session) {
  
  observeEvent(input$calculate_btn, {
    x = as.numeric(unlist(strsplit(input$x, ",")))
    y = as.numeric(unlist(strsplit(input$y, ",")))
    x0 = input$x0
    reg_model = lm(y ~ x)
    model_summary = summary(reg_model)
    alpha = input$alpha
    p_value_intercept = model_summary$coefficients[1, 4]
    slope_test = coef(summary(reg_model))[2, 4]
    intercept_test = coef(summary(reg_model))[1, 4]
    model_significance = anova(reg_model)$"Pr(>F)"[1]
    check_bool <- 0
    
    if (input$remove_intercept && p_value_intercept > alpha) {
      reg_model <- lm(y ~ 0 + x)
      b0 <- 0
      b1 <- coef(reg_model)[1]
      MSE <- sum(residuals(reg_model)^2) / (length(x) - 1)
      check_bool <- 1
    } else {
      b0 <- coef(reg_model)[1]
      b1 <- coef(reg_model)[2]
      MSE <- sum(residuals(reg_model)^2) / (length(x) - 2)
    }
    
    output$modelSummary <- renderPrint({
      cat("Regression Model Summary:\n")
      print(summary(reg_model))
    })
    
    output$regressionTests <- renderPrint({
      cat("Test Results:\n")
      cat("Slope test p-value:", slope_test, "\n")
      cat("Intercept test p-value:", intercept_test, "\n")
      cat("Overall model significance test p-value:", model_significance, "\n")
    })
    
    output$mse = renderPrint({
      cat("MSE = ", MSE, "\n")
    })
    
    calc_ci <- function(z) {
      df <- length(x) - ifelse(check_bool, 1, 2)
      t_value <- qt(1 - alpha / 2, df = df)
      len <- ifelse(check_bool, 0, 1/length(x))
      se <- sqrt(MSE * (if (input$CItype == "mean") {
        len + (if (check_bool) z^2 else (z - mean(x))^2) / sum((x - mean(x))^2)
      } else if (input$CItype == "prediction") {
        1 + len + (if (check_bool) z^2 else (z - mean(x))^2) / sum((x - mean(x))^2)
      } else if (input$CItype == "Mprediction") {
        1 / input$m + len + (if (check_bool) z^2 else (z - mean(x))^2) / sum((x - mean(x))^2)
      }))
      
      lower <- (b0 + b1 * z) - t_value * se
      upper <- (b0 + b1 * z) + t_value * se
      
      list(lower = lower, upper = upper)
    }
    
    if (input$calc_ci) {
      ci <- calc_ci(x0)
      output$confidenceIntervalOutput <- renderPrint({
        cat("Confidence Interval:\n")
        cat("Lower bound =", ci$lower, "\n")
        cat("Upper bound =", ci$upper, "\n")
      })
    } 
    bounds <- data.frame(x = x, y = y)
    bounds$lower <- sapply(bounds$x, function(z) calc_ci(z)$lower)
    bounds$upper <- sapply(bounds$x, function(z) calc_ci(z)$upper)
    
    output$scatterPlot <- renderPlot({
      ci_title <- switch(input$CItype,
                         "mean" = "Mean Confidence Interval",
                         "prediction" = "Prediction Interval",
                         "Mprediction" = "Modified Prediction Interval")
      
      p <- ggplot(bounds, aes(x = x, y = y)) +
        geom_point() +
        xlab("X") +
        ylab("Y")
      
      if (!check_bool) {
        p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "dark red")
        if (input$calc_ci) {
          p <- p + geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#FF8000", alpha = 0.15)
        }
        p <- p + ggtitle(paste("Linear Regression With Intercept +", ci_title))
      } else {
        p <- p + geom_smooth(method = "lm", formula = y ~ 0 + x, se = FALSE, color = "dark red")
        if (input$calc_ci) {
          p <- p + geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#CC00CC", alpha = 0.15)
        }
        p <- p + ggtitle(paste("Linear Regression Without Intercept +", ci_title))
      }
      
      print(p)
    })
  })
}

shinyApp(ui = ui, server = server)