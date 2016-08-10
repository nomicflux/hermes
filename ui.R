library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Hermes Word Prediction Algorithm"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("phrase",
                label="Enter a phrase:",
                placeholder="Hey"),
      p("To use: simply enter a phrase.  The prediction algorithm will take a moment to ponder, and will get back to you with its choice.  It will try to complete the word you're on, or find the next word after you hit space.")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(condition="!output.setup_done", 
                       h1("Setting up...."),
                       p("(This should only take about 7 seconds)")),
      conditionalPanel(condition="output.setup_done",
                       h1(textOutput("setup_done")),
                       h3(textOutput("next_word_1")),
                       h2("Or try:"),
                       textOutput("next_word_2"),
                       textOutput("next_word_3"))
    )
  )
))