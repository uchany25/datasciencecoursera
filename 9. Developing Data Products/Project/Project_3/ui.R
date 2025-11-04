shinyUI(fluidPage(
  
  titlePanel("Iris Species Prediction Simulator (RF, SVM, KNN)"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Input Measurements:"),
      
      # Sliders using global.R min/max/initial values
      sliderInput("sepal_length", "Sepal Length:", min = min_sl, max = max_sl, value = initial_value_sl, step = 0.1),
      sliderInput("sepal_width", "Sepal Width:", min = min_sw, max = max_sw, value = initial_value_sw, step = 0.1),
      sliderInput("petal_length", "Petal Length:", min = min_pl, max = max_pl, value = initial_value_pl, step = 0.1),
      sliderInput("petal_width", "Petal Width:", min = min_pw, max = max_pw, value = initial_value_pw, step = 0.1),
      
      hr(),
      
      # SVM Kernel Choice
      h4("SVM Configuration:"),
      radioButtons(
        inputId = "kernel_choice",
        label = "Select Kernel Type:",
        choices = c("Linear" = "linear", "Radial" = "radial"),
        selected = "radial"
      )
    ),
    
    mainPanel(
      h3("Prediction Results Comparison"),
      tabsetPanel(
        type = "tabs",
        # Output IDs matched to server.R (rf_result, svm_result, knn_result)
        tabPanel("Random Forest", br(), h4("Predicted Species:"), textOutput("rf_result")),
        tabPanel("Support Vector Machine", br(), h4("Predicted Species:"), textOutput("svm_result")),
        tabPanel("K-Nearest Neighbors", br(), h4("Predicted Species:"), textOutput("knn_result"))
      )
    )
  )
))