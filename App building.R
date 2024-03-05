# Load necessary libraries
library(shiny)
library(quanteda)

# Define the UI
ui <- fluidPage(
  titlePanel("FINAL PROJECT - DATA SCIENCE CAPSTONE - USING NLP TO PREDICT WORDS"),
  sidebarLayout(
    sidebarPanel(
      textInput("input_phrase", "Enter a phrase:"),
      actionButton("predict_button", "Predict"),
      hr(),
      h5("Prediction:"),
      verbatimTextOutput("prediction_output")
    ),
    mainPanel()
  )
)

# Define the server logic
server <- function(input, output) {
  # Load data only if the file exists
  file_path <- "C:/Users/bfahs/OneDrive/Documents/datasciencecoursera/DF-Capstone/en_US/sampledfile.txt"
  if (file.exists(file_path)) {
    mytf3 <- readLines(file_path, encoding = "UTF-8")
    myCorpus <- corpus(mytf3)
  } else {
    # Display a message or take alternative action if the file does not exist
    print("Error: File 'sampledfile.txt' not found.")
  }
  
  
  # Load the corpus and define the processing functions
  getProfanities <- function() {
    profanityFile <- "profanities.txt"
    if (!file.exists(profanityFile)) {
      download.file('http://www.cs.cmu.edu/~biglou/resources/bad-words.txt',
                    profanityFile)
    }
    profanities <- read.csv("profanities.txt", header = FALSE, stringsAsFactors = FALSE)
    profanities$V1
  }
  
  process_sentences <- function(sentences, n = 1L) {
    # Tokenize sentences
    tokens <- tokens(sentences, remove_punct = TRUE)
    
    # Remove profanities
    tokens <- tokens_remove(tokens, getProfanities())
    
    # Convert to lowercase
    tokens <- tokens_tolower(tokens)
    
    # Create n-grams
    ngrams <- tokens_ngrams(tokens, n = n)
    
    return(ngrams)
  }
  
  # Define the predict function
  predict_next_word <- function(phrase) {
    # Process the input phrase
    phrase_tokens <- tokens(phrase, remove_punct = TRUE)
    phrase_tokens <- tokens_remove(phrase_tokens, getProfanities())
    phrase_tokens <- tokens_tolower(phrase_tokens)
    
    # Get the last token in the phrase
    last_token <- tail(phrase_tokens, 1)
    
    # Find the most frequent next word
    next_word <- ""
    if (length(last_token) > 0) {
      next_word_df <- textstat_frequency(tokens_select(myCorpus, pattern = last_token))
      next_word <- rownames(next_word_df)[1]
    }
    
    return(next_word)
  }
  
  # Define the reactive output for prediction
  output$prediction_output <- renderPrint({
    if (input$predict_button > 0) {
      phrase <- input$input_phrase
      prediction <- predict_next_word(phrase)
      prediction
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)

