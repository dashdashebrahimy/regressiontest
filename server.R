
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
    if (input$remove_intercept && p_value_intercept < alpha) {
      reg_model <- lm(y ~ 0 + x)
      b0 <- 0
      b1 <- coef(reg_model)[1]
      MSE <- sum(residuals(reg_model)^2) / (length(x) - 1)
      sxx <- sum(x^2)
      yhat = b0 + b1 * x
      yhat0 = b0 + b1 * x0
      MSE = sum(residuals(reg_model)^2) / (length(x) - 2)
      check_bool <- 1
    } 
    else {
      b0 <- coef(reg_model)[1]
      b1 <- coef(reg_model)[2]
      MSE <- sum(residuals(reg_model)^2) / (length(x) - 2)
      sxx <- sum((x - mean(x))^2)
      yhat = b0 + b1 * x
      yhat0 = b0 + b1 * x0
      MSE = sum(residuals(reg_model)^2) / (length(x) - 2)
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
    
    calc_ci <- function(z) {
      df <- length(x) - ifelse(input$remove_intercept, 1, 2)
      t_value <- qt(1 - alpha / 2, df = df)
      len <- ifelse(input$remove_intercept, 0, 1/length(x))
      se <- sqrt(MSE * (if (input$CItype == "mean") {
        len + (if (input$remove_intercept) z^2 else (z - mean(x))^2) / sxx
      } else if (input$CItype == "prediction") {
        1 + len + (if (input$remove_intercept) z^2 else (z - mean(x))^2) / sxx
      } else if (input$CItype == "Mprediction") {
        1 / input$m + len + (if (input$remove_intercept) z^2 else (z - mean(x))^2) / sxx
      }))
      
      lower <- (b0 + b1 * z) - t_value * se
      upper <- (b0 + b1 * z) + t_value * se
      
      list(lower = lower, upper = upper)
    }
    output$predictions = renderPrint({
      cat("Predicted value for x0 = ", x0, ": yhat0 = ", yhat0, "\n")
    })
    output$mse = renderPrint({
      cat("MSE = ", MSE, "\n")
    })
    
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
    if(check_bool){
      output$scatterPlot = renderPlot({
        ggplot(bounds, aes(x = x, y = y)) +
          geom_point() +
          geom_smooth(method = "lm", formula = y~0+x, se = FALSE, color = "dark red") +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#FF8000", alpha = 0.15) +
          ggtitle("Linear Regression With No_Intercept Ray+ Confidence Intervals") +
          xlab("X") +
          ylab("Y")
      })
    }
    else{
      output$scatterPlot = renderPlot({
        ggplot(bounds, aes(x = x, y = y)) +
          geom_point() +
          geom_smooth(method = "lm", formula = y~x, se = FALSE, color = "dark red") +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#CC00CC", alpha = 0.15) +
          ggtitle("Linear Regression + Confidence Intervals") +
          xlab("X") +
          ylab("Y")
      })
    }
  })
}
