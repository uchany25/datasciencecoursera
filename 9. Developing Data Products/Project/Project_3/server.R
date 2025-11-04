shinyServer(function(input, output) {
  
  new_data <- reactive({
    current_sl <- input$sepal_length
    current_sw <- input$sepal_width
    current_pl <- input$petal_length
    current_pw <- input$petal_width
    
    changed_inputs_count <- sum(
      current_sl != initial_value_sl,
      current_sw != initial_value_sw,
      current_pl != initial_value_pl,
      current_pw != initial_value_pw
    )
    
    if (changed_inputs_count < 2) {
      return(list(error = "ERROR: Please input data for at least 2 variables (change 2 sliders)."))
    }
    
    if (current_sl == initial_value_sl) {
      impute_data <- data.frame(Sepal.Width = current_sw, Petal.Length = current_pl, Petal.Width = current_pw)
      current_sl <- predict(rf_reg_sl, impute_data)
    }
    
    if (current_sw == initial_value_sw) {
      impute_data <- data.frame(Sepal.Length = current_sl, Petal.Length = current_pl, Petal.Width = current_pw)
      current_sw <- predict(rf_reg_sw, impute_data)
    }
    
    if (current_pl == initial_value_pl) {
      impute_data <- data.frame(Sepal.Length = current_sl, Sepal.Width = current_sw, Petal.Width = current_pw)
      current_pl <- predict(rf_reg_pl, impute_data)
    }
    
    if (current_pw == initial_value_pw) {
      impute_data <- data.frame(Sepal.Length = current_sl, Sepal.Width = current_sw, Petal.Length = current_pl)
      current_pw <- predict(rf_reg_pw, impute_data)
    }
    
    data.frame(
      Sepal.Length = current_sl, Sepal.Width  = current_sw,
      Petal.Length = current_pl, Petal.Width  = current_pw
    )
  })
  
  
# 1. Model
  
model_svm <- reactive({
  req(input$kernel_choice) # Check if a kernel is selected before training
  svm(Species ~., data = train_iris, kernel = input$kernel_choice)
})

# 2. Predictions (Using the new_data() output)
rf_prediction <- reactive({
  data <- new_data()
  # Check for the error message from new_data()
  if (is.list(data) && !is.null(data$error)) {
    return(data$error) 
  }
 as.character(predict(model_rf, data))
})
  
svm_prediction <- reactive({
  data <- new_data()
  if (is.list(data) && !is.null(data$error)) {
    return(data$error)
  }
  predict(model_svm(), data) # Call reactive model_svm()
})
  
knn_prediction <- reactive({
  data <- new_data()
  if (is.list(data) && !is.null(data$error)) {
    return(data$error)
  }
  # KNN directly uses the test data frame
 as.character(knn(train = knn_train_data, test = data, cl = knn_train_labels, k = 5))
})
  
# 3. Outputs
output$rf_result <- renderText({
  rf_prediction()
})

output$svm_result <- renderText({
  paste("Kernel (", input$kernel_choice, ") Prediction:", svm_prediction())
})

output$knn_result <- renderText({
  knn_prediction()
})
})
