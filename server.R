# Copyright 2016, Ian Tan
# Data Science Institute
# Multimedia University



library(httr)
library(jsonlite)
library(tm)
library(stringr)
library(shiny)
# Load the data here.  I pre-processed them using the code in
# session 3 to have them in data.frame format.
#tf <- findFreqTerms(fourgram, lowfreq = 3)
#tf <- sort(rowSums(as.matrix(fourgram[tf,])), decreasing = TRUE)
#tf <- data.frame(fourgram=names(tf), frequency=tf)
setwd('C://Users//jieya//OneDrive//Documents//R//Module 10 Capstone//capstone shiny')


unigram <- get(load("fDF1.RData"))
bigram <- get(load("fDF2.RData"))
trigram <- get(load("fDF3.RData"))
fourgram <- get(load("fDF4.RData"))



clean_input <- function(entry) {
  if(entry == "" | is.na(entry))
    return("")
  entry <- unlist(strsplit(entry, c(" ", "\n", "-")))
  entry <- tolower(entry)
  
  #entry <- gsub("[0-9](?:st|nd|rd|th)", "", entry, ignore.case=F, perl=T) #remove ordinal numbers
  entry <- gsub("[.\\-!]", " ", entry, ignore.case=F, perl=T) #remove punctuation
  entry <- gsub("[[:punct:]]", "", entry)
  entry <- gsub("[^\\p{L}'\\s]+", "", entry, ignore.case=F, perl=T) #remove punctuation, leaving '
  entry <- gsub("^\\s+|\\s+$", "", entry) #trim leading and trailing whitespace
  entry <- stripWhitespace(entry)
  if(entry == "" | is.na(entry))
    return("")
  entry <- unlist(strsplit(entry, " "))
  
  return(entry)
}


predictWords <- function(inStr) {
  
  assign("mesg", "in PredNextTerm", envir = .GlobalEnv)
  
  # Clean up the input string and extract only the words with no leading and trailing white spaces
  inStr <- clean_input(inStr);
  
  # Split the input string across white spaces and then extract the length
  inStr <- unlist(strsplit(inStr, split=" "));
  inStrLen <- length(inStr);
  
  nxtTermFound <- FALSE;
  predNxtTerm <- as.character(NULL);
  #mesg <<- as.character(NULL);
  
  
  # 1. First test the Four Gram using the four gram data frame
  if (inStrLen >= 3 & !nxtTermFound)
  {
    # Assemble the terms of the input string separated by one white space each
    inStr1 <- paste(inStr[(inStrLen-2):inStrLen], collapse=" ");
    
    # Subset the Four Gram data frame 
    searchStr <- paste("^",inStr1, sep = "");
    fourgramTemp <- fourgram[grep (searchStr, fourgram$terms), ];
    
    # Check to see if any matching record returned
    if ( length(fourgramTemp[, 1]) > 1 )
    {
      predNxtTerm <- fourgramTemp[1,1];
      nxtTermFound <- TRUE;
      mesg <<- "Next word is predicted using 4-gram."
    }
    fourgramTemp <- NULL;
  }
  
  # 2. Next test the Three Gram using the three gram data frame
  if (inStrLen >= 2 & !nxtTermFound)
  {
    # Assemble the terms of the input string separated by one white space each
    inStr1 <- paste(inStr[(inStrLen-1):inStrLen], collapse=" ");
    
    # Subset the Three Gram data frame 
    searchStr <- paste("^",inStr1, sep = "");
    trigramTemp <- trigram[grep (searchStr, trigram$terms), ];
    
    # Check to see if any matching record returned
    if ( length(trigramTemp[, 1]) > 1 )
    {
      predNxtTerm <-trigramTemp[1,1];
      nxtTermFound <- TRUE;
      mesg <<- "Next word is predicted using 3-gram."
    }
    trigramTemp <- NULL;
  }
  
  # 3. Next test the Two Gram using the three gram data frame
  if (inStrLen >= 1 & !nxtTermFound)
  {
    # Assemble the terms of the input string separated by one white space each
    inStr1 <- inStr[inStrLen];
    
    # Subset the Two Gram data frame 
    searchStr <- paste("^",inStr1, sep = "");
    bigramTemp <- bigram[grep (searchStr, bigram$terms), ];
    
    # Check to see if any matching record returned
    if ( length(bigramTemp[, 1]) > 1 )
    {
      
      
      predNxtTerm <- bigramTemp[1,1];
      nxtTermFound <- TRUE;
      mesg <<- "Next word is predicted using 2-gram.";
    }
    bigramTemp <- NULL;
  }
  
  # 4. If no next term found in Four, Three and Two Grams return the most
  #    frequently used term from the One Gram using the one gram data frame
  if (!nxtTermFound & inStrLen > 0)
  {
    predNxtTerm <- unigram$terms[1];
    mesg <- "No next word found, the most frequent word is selected as next word."
  }
  
  nextTerm <- word(predNxtTerm, -1);
  
  if (inStrLen > 0){
    dfTemp1 <- data.frame(nextTerm, mesg);
    return(dfTemp1);
  } else {
    nextTerm <- "";
    mesg <-"";
    dfTemp1 <- data.frame(nextTerm, mesg);
    return(dfTemp1);
  }   
}


msg <- ""
shinyServer(function(input, output) {
  
  output$predictedText <- renderPrint({
    str2 <- clean_input(input$entry)
   
    strDF <- predictWords(str2)
    
    input$action;
    
    msg <<- as.character(strDF[1,2]);
    cat("", as.character(strDF[1,1]))
    cat("\n\t");
    cat("\n\t");
    cat("Note: ", as.character(strDF[1,2]));
    
  });
  
  output$text1 <- renderText({
    paste( input$entry)});
  
  output$text2 <- renderText({
    input$action;
  })
  
})

  
