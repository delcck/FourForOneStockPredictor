# Set up the server session for our regression model and data visualization shiny app

sever <- function(input, output, session) {

  #--Define helpful functions
  CONVERT_TIME = function(input_time) {
    diff_time = diff(input_time)
    temp_time = c(0, diff_time)
    output_time = c()
    for (i in seq_along(temp_time)) {
      if (i == 1) {
        output_time[i] = temp_time[i]
      } else {
        output_time[i] = temp_time[i] + output_time[i-1]
      }
    }
    return(output_time)
  }
  SHIFT = function(input_list, shift=0) {
    output_list = c()
    for (i in seq_along(input_list)) {
      if (i <= shift) {
        output_list[i] = NA
      } else {
        output_list[i] = input_list[i - shift]
      }
    }
    return(output_list)
  }
  DOWNLOAD_DATA = function(label_stock, feature_stocks) {
    label_data <- alphavantager::av_get(label_stock,
                                        av_fun = "TIME_SERIES_DAILY_ADJUSTED",
                                        outputsize = "full",
                                        show_col_types = FALSE)

    time_col = CONVERT_TIME(label_data$timestamp)
    label_col = label_data$close
    df = data.frame("TIME" = time_col,
                    "LABEL" = label_col)
    processed = 0
    for (feature_stock in feature_stocks) {
      feature_data <- alphavantager::av_get(feature_stock,
                                            av_fun = "TIME_SERIES_DAILY_ADJUSTED",
                                            outputsize = "full",
                                            show_col_types = FALSE)
      processed = processed + 1
      col_name = paste("O", as.character(processed), sep = "")
      df[col_name] = feature_data$close
    }
    return(df)
  }

  TRANSFORM_DATA = function(input_df, shifts) {
    num_feature_stock = ncol(input_df) - 2
    output_df = data.frame(
      "TIME" = input_df$TIME,
      "LABEL" = input_df$LABEL
    )
    #--APPLY SHIFT TO LABEL
    processed = 0
    in_col_name = "LABEL"
    for (shift in shifts) {
      processed = processed + 1
      temp_col = SHIFT(input_df$LABEL, shift = shift)
      col_name = paste("C", as.character(processed), sep = "")
      output_df[col_name] = temp_col
    }
    #-APPLY SHIFT TO FEATURES
    for (i in 1:num_feature_stock) {
      in_col_name = paste("O", as.character(i), sep = "")
      feature_data = input_df[[in_col_name]]
      for (shift in shifts) {
        processed = processed + 1
        temp_col = SHIFT(feature_data, shift = shift)
        col_name = paste("C", as.character(processed), sep = "")
        output_df[col_name] = temp_col
      }
    }
    return(drop_na(output_df))
  }

  GENERATE_INPUT = function(input_df, shifts, forward_time = 1) {
    #-CONSTRUCT THE TIMESTAMP FOR PREDICTION
    time_out = tail(input_df$TIME, 1) + forward_time

    output_for_lm = data.frame("TIME" = time_out)

    #-CONSTRUCT INPUTS FOR SELECTED STOCKS
    num_feature_stock = ncol(input_df) - 2
    processed = 0
    for (shift in shifts) {
      processed = processed + 1
      col_name = paste("C", as.character(processed), sep = "")
      output_for_lm[col_name] = tail(input_df$LABEL, shift)[1]
    }
    for (i in 1:num_feature_stock) {
      in_col_name = paste("O", as.character(i), sep = "")
      feature_data = input_df[[in_col_name]]
      for (shift in shifts) {
        processed = processed + 1
        col_name = paste("C", as.character(processed), sep = "")
        output_for_lm[col_name] = tail(feature_data, shift)[1]
      }
    }
    return(output_for_lm)
  }
  FORMULA_CONSTRUCT = function(label_stock, feature_stocks, shifts) {
    linear_terms = c("TIME")
    used_stocks = c(label_stock, feature_stocks)
    processed = 1
    for (i in seq_along(used_stocks)) {
      for (shift in shifts) {
        col_name = paste("C", as.character(processed), sep = "")
        linear_terms[processed+1] = col_name
        processed = processed + 1
      }
    }
    non_linear_terms = c()
    for (i in 1:length(linear_terms)) {
      if (i == 1) {
        non_linear_terms[i] = paste("(", linear_terms[i], sep = "")
      } else if (i == length(linear_terms)) {
        non_linear_terms[i] = paste(linear_terms[i], ")^2", sep = "")
      } else {
        non_linear_terms[i] = linear_terms[i]
      }
    }
    return(
      reformulate(
        termlabels = c(linear_terms, non_linear_terms),
        response = "LABEL"
      )
    )
  }
  REGRESSION_MODEL = function(input_df, label_stock, feature_stocks, shifts) {
    return(
      lm(FORMULA_CONSTRUCT(label_stock = label_stock,
                           feature_stocks = feature_stocks,
                           shifts = shifts),
         data = input_df)
    )
  }
  my_visualization = function(input_df, prediction, forward_time = 1) {
    num_row = nrow(input_df)
    input_time = input_df$TIME
    input_label = input_df$LABEL
    add_time = tail(input_time, 1)[1] + forward_time
    input_time[num_row + 1] = add_time
    input_label[num_row + 1] = prediction
    color_label = c()
    for (i in 1:num_row) {
      color_label[i] = "Actual"
    }
    color_label[num_row + 1] = "Prediction"
    plot_df = data.frame(
      "TIME" = input_time,
      "STOCK_PRICE" = input_label,
      "PREDICTION_OR_ACTUAL" = color_label
    )
    ggplot(plot_df,
           aes(x = TIME, y = STOCK_PRICE, color=PREDICTION_OR_ACTUAL)
    ) + geom_point() + labs(y = "Stock price (USD)", x = "Time (Number of days from our referecne date)", color = "Actual data/Prediction")
  }
  my_correlation_vis = function(input_df, select_feature, label_stock, feature_stocks) {
    n_feature = ncol(input_df) - 2
    select_col = paste("O", as.character(select_feature), sep = "")
    plot_df = data.frame(
      "LABEL" = input_df$LABEL,
      "FEATURE" = input_df[[select_col]]
    )
    ggplot(plot_df,
           aes(x = FEATURE, y = LABEL)
    ) + geom_point() + labs(y = paste(label_stock, "(USD)"),
                            x = paste(feature_stocks[select_feature], "(USD)")
    )
  }
  my_all_in_one = function(input_df, label_stock, feature_stocks) {
    #--Rename our dataframe
    colnames(input_df)[2] = label_stock
    colnames(input_df)[3] = feature_stocks[1]
    colnames(input_df)[4] = feature_stocks[2]
    colnames(input_df)[5] = feature_stocks[3]
    colnames(input_df)[6] = feature_stocks[4]
    #--Convert from wide to long format
    plot_df = gather(
      input_df,
      StockKey,
      StockPrice,
      2:6,
      factor_key=TRUE
    )
    ggplot(plot_df, aes(x = TIME, y = StockPrice, color = StockKey)) +
      geom_line() +
      labs(
        y = "Stock price (USD)",
        x = "Time (Number of days from our referecne date)",
        color = "Stock key")
  }

  feature_stocks <- eventReactive(
    input$action,
    {
      c(input$predictor1,
        input$predictor2,
        input$predictor3,
        input$predictor4)
    }
  )
  label_stock <- eventReactive(
    input$action,
    {
      input$response
    }
  )
  shifts <- eventReactive(
    input$action,
    {
      1:input$shift
    }
  )
  download_data <- eventReactive(
    input$action,
    {
      alphavantager::av_api_key(input$key)
      DOWNLOAD_DATA(label_stock(), feature_stocks())
    }
  )

  transformed_data <- eventReactive(
    input$action,
    {
      TRANSFORM_DATA(download_data(), shifts())
    }
  )
  inputs_for_prediction <- eventReactive(
    input$action,
    {
      GENERATE_INPUT(download_data(), shifts())
    }
  )
  my_formula <- eventReactive(
    input$action,
    {
      FORMULA_CONSTRUCT(label_stock(), feature_stocks(), shifts())
    }
  )
  our_model <- eventReactive(
    input$action,
    {
      REGRESSION_MODEL(transformed_data(), label_stock(), feature_stocks(), shifts()
      )
    }
  )
  our_predictions <- eventReactive(
    input$action,
    {
      predict(our_model(), inputs_for_prediction())
    }
  )
  model_sum <- eventReactive(
    input$action,
    {
      if (input$detail == "Yes") {
        summary(our_model())
      } else {
        paste("The R^2 score for the model based on this set of stocks is: ", summary(our_model())[9])
      }
    }
  )

  plot0 <- eventReactive(
    input$action,
    {
      my_visualization(transformed_data(), our_predictions())
    }
  )
  plot1 <- eventReactive(
    input$action,
    {
      my_correlation_vis(download_data(), 1, label_stock(), feature_stocks())
    }
  )
  plot2 <- eventReactive(
    input$action,
    {
      my_correlation_vis(download_data(), 2, label_stock(), feature_stocks())
    }
  )
  plot3 <- eventReactive(
    input$action,
    {
      my_correlation_vis(download_data(), 3, label_stock(), feature_stocks())
    }
  )
  plot4 <- eventReactive(
    input$action,
    {
      my_correlation_vis(download_data(), 4, label_stock(), feature_stocks())
    }
  )
  plot5 <- eventReactive(
    input$action,
    {
      my_all_in_one(download_data(), label_stock(), feature_stocks())
    }
  )
  output$plot0 <- renderPlot(plot0())
  output$summary <- renderPrint(model_sum())
  output$plot1 <- renderPlot(plot1())
  output$plot2 <- renderPlot(plot2())
  output$plot3 <- renderPlot(plot3())
  output$plot4 <- renderPlot(plot4())
  output$plot5 <- renderPlot(plot5())


  session$onSessionEnded(
    function() {
      stopApp()
    }
  )
}
