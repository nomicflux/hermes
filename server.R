library(shiny)

## load("docs.RData")

library(dplyr)
library(quanteda)

load("twittergrams.RData")
load("counts.RData")
load("semantic.RData")

most.freq.word <- names(topfeatures(ngrams[[1]], 1))[[1]]
single.word.dict <- list(features(ngrams[[1]]))

profanity <- readLines("en_profanity.txt")

lambdas <- c(.1,.1,.2,.3,.1,.1,.1)

getLastWord <- function(phrase) {
  tmp <- strsplit(phrase, "_")
  tail(tmp[[1]], n=1)
}

predictPhrase <- function(phrase) {
  words <- length(phrase)
  maxWords <- length(ngrams)
  if(words > maxWords - 1) {
    words <- maxWords - 1
    phrase <- phrase[words - (maxWords - 2):words]
  }
  ngramFeatures <- features(ngrams[[words]])
  ngramPlusFeatures <- features(ngrams[[words + 1]])
  pastedPhrase <- paste(phrase,collapse="_")
  pastedPhrasePlus <- paste("^", pastedPhrase, "_", sep="")
  phraseCountRow <- ngramFeatures == pastedPhrase
  if(sum(phraseCountRow) > 0) {
    totalCount <- totals[[words]] + minima[[words]]
    phraseCount <- docfreq(ngrams[[words]][,phraseCountRow]) + minima[[words]]
    phraseProb <- phraseCount / totalCount
    
    possiblePhrasesRows <- grep(pastedPhrasePlus, ngramPlusFeatures)
    if(sum(possiblePhrasesRows) > 0) {
      freqs <- docfreq(ngrams[[words+1]][,possiblePhrasesRows])
      totalFreq <- totals[[words+1]] + minima[[words + 1]]
      possibleProb <- (freqs + minima[[words + 1]]) / totalFreq
      possibleProb ## * phraseProb
    } else {
      NULL
    }
  } else {
    NULL
  }
}

getStems <- function(phrase) {
  res <- phrase %>%
    toLower %>%
    tokenize(removePunct=TRUE, removeTwitter=TRUE, removeNumbers=TRUE) %>%
    removeFeatures(stopwords("SMART")) %>%
    wordstem(language="porter") %>%
    head(n=1)
  res[[1]]
}

cleanPhrase <- function(phrase) {
  res <- phrase %>%
    toLower %>%
    tokenize(removePunct=TRUE, removeTwitter=TRUE, removeNumbers=TRUE) %>%
    removeFeatures(features=profanity) %>%
    selectFeatures(single.word.dict) %>%
    head(n=1)
  res[[1]]
}

findSemanticMatch <- function(phrase) {
  stemmed <- getStems(phrase)
  size <- length(stemmed)
  if(size > 4) {
    size <- 4
    stemmed <- stemmed[size-3:size]
  }
  features <- features(twitter.semantic)
  stemmedFound <- new.env()
  for(i in 1:size) {
    togrep <- paste("^", stemmed[i],"_",sep="")
    found <- grep(togrep, features)
    if(length(found) > 0) {
      freqs <- docfreq(twitter.semantic[,found])
      totalCt <- sum(freqs)
      if(length(freqs) > 0) {
        words <- sapply(names(freqs), getLastWord)
        for(w in 1:length(words)) {
          word <- words[w]
          freq <- freqs[w] / totalCt
          if(is.null(stemmedFound[[word]])) {
            stemmedFound[[word]] = 0
          }
          stemmedFound[[word]] = stemmedFound[[word]] + freq
        }
      }
    }
  }
  stemmedFound
}

backoff <- function(orig.phrase, start="") {
  phrase <- cleanPhrase(orig.phrase)
  sizeStart <- nchar(start)
  size <- length(phrase)
  if(size > length(ngrams) - 1) {
    size <- length(ngrams) - 1
    phrase <- phrase[length(phrase)-size+1:length(phrase)]
  } else if(size == 0) {
    return(most.freq.word)
  }
  words <- new.env()
  for(i in 1:size) {
    tryPhrase <- phrase[i:size]
    res <- predictPhrase(tryPhrase)
    ## print(res)
    if(!is.null(res)) {
      allOpts <- sapply(names(res), getLastWord)
      for(j in 1:length(allOpts)) {
        w <- allOpts[j]
        f <- res[j]
        if(!is.null(w) && w != "" && (sizeStart == 0 || substring(w, 1, sizeStart) == start)) {
          if(is.null(words[[w]])) {
            words[[w]] = 0
          }
          words[[w]] = words[[w]] + lambdas[[size-i + 1]]*f
        }
      }
    }
  }
  return(words)
}

cleanWord <- function(word) {
  if(word == "i") {
    "I"
  } else {
    strsplit(word, "[.]") %>% unlist %>% paste(collapse="'")
  }
}

predictNextWord <- function(phrase, start="", toReturn=3, semantic.mult=1/25) {
  if(phrase != "") {
    backoffWords <- backoff(phrase, start)
    semanticWords <- findSemanticMatch(phrase)
    for(word in names(backoffWords)) {
      stemmed <- wordstem(word, language="porter")
      if(nchar(stemmed) > 0 && !is.null(semanticWords[[stemmed]])) {
        backoffWords[[word]] = backoffWords[[word]] + semanticWords[[stemmed]] * semantic.mult
      }
    }
    res <- as.data.frame(as.list(backoffWords))
    sorted <- names(sort(res, decreasing=TRUE))
  } else {
    sorted = c()
  }
  if(length(sorted) < toReturn) {
    df <- names(topfeatures(ngrams[[1]], toReturn - length(sorted)))
    rbind(sorted, df)
  } else {
    sorted[1:toReturn]
  }
}

predictFromSentence <- function(sentence, toReturn=3) {
  sizeSentence <- nchar(sentence)
  if(sentence == "") {
    predictNextWord(sentence, toReturn=toReturn)
  } else {
    lastChar <- substr(sentence, sizeSentence, sizeSentence)
    if(lastChar == " ") { 
      predictNextWord(sentence, toReturn=toReturn)
    } else {
      words <- strsplit(sentence, " ")[[1]]
      firstPart <- paste(words[1:(length(words) - 1)], sep=" ", collapse=" ")
      lastWord <- words[length(words)]
      if(nchar(lastWord) > 0) {
        res <- predictNextWord(firstPart, start=lastWord, toReturn=toReturn)
        if(res[1] == toLower(lastWord)) {
          predictNextWord(sentence, toReturn=toReturn)          
        } else {
          res
        }
      } else {
        predictNextWord(sentence, toReturn=toReturn)
      }
    }
  }
}

shinyServer(function(input, output) {
  predicted <- reactive({
    predictFromSentence(input$phrase)
  })
  output$next_word_1 <- renderText({
      predicted()[1] %>% cleanWord
  })
  output$next_word_2 <- renderText({
      predicted()[2] %>% cleanWord
  })
  output$next_word_3 <- renderText({
      predicted()[3] %>% cleanWord
  })
  output$setup_done <- renderText({"Next word:"})
})