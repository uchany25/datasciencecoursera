shinyServer(function(input, output, session) {
  shinyjs::disable("predict_word")
  ngram_data <- reactiveVal(NULL)
  
# Model trained status 
  model_trained <- reactiveVal(FALSE) 
  
  observe({
    input$data_sources 
    input$sample_percent 
    input$stop_word_setting
    input$smoothing_method 
    input$max_n 
    
    model_trained(FALSE)
  })
  
  observeEvent(input$train_model, {
    gc(verbose = FALSE) # memory cleaner 
    
    shinyjs::disable("train_model")
    shinyjs::disable("predict_word")
    
    shiny::showNotification("Generating N-grams... This may take a while depending on sampling. Please do not close the app.",
                            type = "message", duration = NULL, id = "train_notif")
    
# 1. Data Source choice
    req(input$data_sources)
    
    data_list <- list(
      blogs = en_US_blogs,
      news = en_US_news,
      twitter = en_US_twitter
    )
    selected_data <- unlist(data_list[input$data_sources])
    
# 2. Data Preprocessing
    set.seed(42)
    sample_ratio <- input$sample_percent / 100
    sample_size <- length(selected_data) * sample_ratio
    sample_data <- sample(selected_data, size = sample_size)
    
# text preprocessing
    sample_data_clean <- tibble(text = sample_data) %>%
      mutate(text = iconv(text, "latin1", "ASCII", sub="")) %>%
      filter(text != "") %>%
      mutate(text = str_replace_all(text, "http\\S+|www\\.\\S+", "")) %>%
      mutate(text = str_replace_all(text, "[^[:alnum:][:space:]'\\.]", " ")) %>%
      mutate(text = str_squish(text)) %>%
      pull(text)
    
    corpus_data <- corpus(sample_data_clean)
    
# 3. stop_word
    words_to_remove <- switch(input$stop_word_setting,
                              "none" = character(0),
                              "bad_only" = bad_words,
                              "all" = unique(c(bad_words, tidytext::stop_words$word))
    )
    
# 4. N-gram create and store
    n_grams <- list()
    model_creator_func <- if (input$smoothing_method == "backoff") {
      create_ngram_dt
    } else {
      create_ngram_kn_dt
    }
    
# N-gram create (1-gram -> max_n-gram)
    for (n in 1:input$max_n) {
      n_grams[[paste0(n, "g")]] <- model_creator_func(corpus_data, n, words_to_remove)
    }
    
# 5. UI active after train
    ngram_data(list(
      grams = n_grams,
      words_to_remove_pred = words_to_remove,
      smoothing_method = input$smoothing_method
    ))
    
# Turn 'TRUE' if train done
    model_trained(TRUE)
    
    shiny::removeNotification(id = "train_notif")
    shinyjs::enable("train_model")
    shinyjs::enable("predict_word")
    shiny::showNotification("N-gram Model Generation Complete! Ready to predict.", type = "message")
  })
  
  observeEvent(input$predict_word, {
    req(ngram_data())
    fragment <- input$fragment_input
    candidates_str <- input$candidates_input
    has_candidates <- nchar(candidates_str) > 0
    
# 1. candidate word
    if (has_candidates) {
      candidates <- unlist(strsplit(candidates_str, "[,\\s]+"))
      candidates <- tolower(candidates[candidates != ""])
    } else {
# No candidate words -> use top 1000 unigram word
      candidates <- ngram_data()$grams[["1g"]][1:min(1000, nrow(ngram_data()$grams[["1g"]])), word1]
    }
    
# 2. Predict func setting
    prediction_func <- if (ngram_data()$smoothing_method == "backoff") {
      predict_word_dt
    } else {
      predict_word_kn
    }
    
# Predict
    prediction_result <- prediction_func(
      fragment,
      candidates,
      ngram_data()$grams,
      ngram_data()$words_to_remove_pred
    )
    
# 3. Output
    output$prediction_output <- renderTable({
      
      result_df <- prediction_result$result
      
      if (prediction_result$level == "No Match") {
        data.frame(Prediction = "No matching N-gram found.", Level = "N/A")
      } else {
        top_result <- result_df[1,]
        data.frame(
          Prediction = top_result$word,
          Level = top_result$level,
          Context_Words = top_result$context,
          Score = top_result$count
        )
      }
      
    }, striped = TRUE, bordered = TRUE, align = 'l')
  })
  
# Model Status
  output$model_status <- renderText({
    if (model_trained()) {
      paste0("✅ Model TRAINED! (Method: ", input$smoothing_method, ") | Ready for Prediction!")
    } else if (is.null(ngram_data())) {
      return("❌ Model NOT TRAINED. Click 'Generate N-grams' to start training.")
    } else {
      return("⚠️ Model Configuration Changed. Click 'Generate N-grams' to retrain the model.")
    }
  })
  
# Prediction Title dynamic print
  output$prediction_title <- renderUI({
    method_name <- switch(input$smoothing_method,
                          "backoff" = "Back-off (Frequency-based)",
                          "kneserney" = "Kneser-Ney (Smoothed)",
                          "N/A")
    
    HTML(paste0("<h4>", "📊 Prediction Result (", method_name, ")", "</h4>"))
  })
  
})