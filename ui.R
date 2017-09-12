

library(shiny)

shinyUI(fluidPage(
  
  shinyUI(fluidPage(
    titlePanel("Next word prediction"),
    sidebarLayout(
      sidebarPanel(
        
        textInput("entry",
                  "Type in your partial phrase below:",value = ""),
                  submitButton("Predict")
        
      
       
        
      ),
      mainPanel(
        tabsetPanel(
          
        tabPanel("Result", 
                   
        h5("Input a word or text and press <ENTER> or click <Predict> to see the next word(s) suggestions:"),
        h4("Input Sentence: "),
        h5(textOutput('text1') ),
        h3(("Predicted Next word:")),
        verbatimTextOutput("predictedText"),
        
        textOutput('text2')    
        ))
      )
    )
  ))
  
  
))
