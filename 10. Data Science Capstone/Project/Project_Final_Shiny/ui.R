library(shiny)
library(shinyjs)

fluidPage(
  useShinyjs(),
  titlePanel("N-gram Word Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Model Configuration"),
      
# Data Source Choice
      checkboxGroupInput("data_sources", "1. Select Data Sources:",
                         choices = c("Blogs" = "blogs", "News" = "news", "Twitter" = "twitter"),
                         selected = c("blogs", "news", "twitter")),
      
# Setting Sampling Percentage
      sliderInput("sample_percent", "2. Sampling Percentage (%):",
                  min = 1, max = 100, value = 30, step = 1, post = "%"),
      
# Stop word setting
      radioButtons("stop_word_setting", "3. Stop Word Removal:",
                   choices = c("None (Highest Accuracy)" = "none",
                               "Remove Bad Words Only" = "bad_only",
                               "Remove All (Stop + Bad)" = "all"),
                   selected = "bad_only"),
      
# Setting Smoothing / Back-off Methods
      selectInput("smoothing_method", "4. Smoothing / Back-off:",
                  choices = c("Back-off (Frequency-based)" = "backoff",
                              "Kneser-Ney (Smoothed)" = "kneserney"),
                  selected = "backoff"),
      
# N-gram Max number (6-gram fixed)
      numericInput("max_n", "5. Maximum N-gram Used: (6-gram)", value = 6, min = 2, max = 6, step = 1, width = '100%'),
      
# Generate Train Data Button
      actionButton("train_model", "Generate N-grams (Train Model)", icon = icon("cogs"), class = "btn-success"),
      hr(),
      p(em("Hint: Click 'Generate N-grams' after setting the configuration."))
    ),
    
# Main Panel (Input and Output)
    mainPanel(
      h3("📝 Prediction Input"),
      
# text input
      textAreaInput("fragment_input", "Enter Sentence Fragment:",
                    placeholder = "e.g., Talking to your mom has the same effect as a hug and helps reduce your",
                    rows = 3),
      
# candidates input
      textInput("candidates_input", "Candidate Words (comma separated, optional):",
                placeholder = "e.g., stress, hunger, sleepiness"),
      
# predict action
      actionButton("predict_word", "Predict Next Word", icon = icon("magic"), class = "btn-primary"),
      
      hr(),
      
# model status
      h3("Model Status:"),
      textOutput("model_status"),
      
      hr(),

# print dynamic prediction_title      
      uiOutput("prediction_title"), 
      
# outcome table
      tableOutput("prediction_output")
    )
  )
)