## @knitr function-plot_word_cloud
plot_word_cloud <- function(data, title_text = "", size, plot_word_cloud = TRUE, title_offset = -5) {

    ## Replace real NA with Nothing string
    ## The reason is some of the text mining packages replace NA value automagically.
  
    # data$d1[has_nothing_response(data$d1)] <- "Nothing"
    # data$d2[has_nothing_response(data$d2)] <- "Nothing"
    # data$d3[has_nothing_response(data$d3)] <- "Nothing"
    # data$d4[has_nothing_response(data$d4)] <- "Nothing"
    # data$d5[has_nothing_response(data$d5)] <- "Nothing"

    words <- data %>%
    unite("row_words", d1, d2, d3, d4, d5, sep = "@", remove = FALSE) %>%
    dplyr::select(row_words)
  all_words = paste(words$row_words, collapse = "@")

  d <- cleanse_words(all_words, FUN = preprocess_words_to_docs)

  if (length(d$word) > 1) {
    if (plot_word_cloud == TRUE) {
      set.seed(1234)
      plot_word_cloud = wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35, 
                colors=brewer.pal(3, "Dark2"), scale = c(size, 0.5))
      title(title_text, line = title_offset)
    }
    
    dt <- data.table(word = d$word, frequency = d$freq)

    names(dt) <- c("word", "freq")
    #dt$freq <- (dt$freq)^(2/3)
    #print(wordcloud2(dt, size = 0.25))

    return(dt)
  } else  if (length(d$word) == 1) {
    if (plot_word_cloud == TRUE) {
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, d$word[1], 
           cex = 1.6, col = "black")
      title(title_text, line = title_offset)
    }
    
    dt <- data.table(word = d$word, frequency = d$freq)
    return(dt)
  } else {
    return(NULL)
  }
}

## @knitr function-is_consider_na
## @knitr warning=FALSE
has_nothing_response <- function(word) {
  return(grepl("^na$", word, ignore.case = TRUE))
}

## @knitr function-cleanse_words
## @knitr warning=FALSE
cleanse_words <- function(words, FUN = preprocess_words_to_docs_minimal) {
  docs <- FUN(words)
  #inspect(docs)
  
  dtm <- TermDocumentMatrix(docs, control=list(wordLengths=c(1,Inf)))
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  return(d)
}

## @knitr function-stem_words
## @knitr warning=FALSE
stem_words <- function(words, FUN = stem_words_to_doc) {
  docs <- FUN(words)
  #inspect(docs)
  
  dtm <- TermDocumentMatrix(docs, control=list(wordLengths=c(1,Inf)))
  m <- as.matrix(dtm)
  
  d <- data.frame()
  
  orig_words <- words
  stem_words <- c()
  
  stem_terms <- row.names(m)
  
  for (col in 1:ncol(m)) {
    stem_word = toString(stem_terms[which(m[, col] == 1)])
    stem_words <- c(stem_words, stem_word)
  }
  
  d <- data.frame(word = orig_words, stem_word = stem_words)
  return(d)
}

library("hunspell")

## @knitr function-preprocess_words_to_docs_full
## @knitr warning=FALSE
preprocess_words_to_docs_full <- function(words) {
  
  docs <- Corpus(VectorSource(words))
  docs.copy <- docs
  
  toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  # docs <- tm_map(docs, toSpace, "-")
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("nada", "nothing"))
  # Remove punctuations
  # docs <- tm_map(docs, removePunctuation)
  
  # after_cleanse_content <- docs[[1]]$content
  # 
  # Correct spelling and attempt to take the suggestion
  # spell_check_words <- after_cleanse_content
  # incorrect_words <- hunspell(spell_check_words)[[1]]
  # 
  # browser()
  # for (word_index in 1:length(incorrect_words)) {
  #   incorrect_word <- incorrect_words[word_index]
  #   print(paste0("Found incorrect word: ", incorrect_word))
  # 
  #   suggestions <- hunspell_suggest(incorrect_word)
  #   print(suggestions)
  # }

  #docs.temp <- tm_map(docs, stemDocument, language = "english")
  #docs.final <- tm_map(docs.temp, stemCompletion, dictionary = docs.copy) 
  return(docs)
}

## @knitr function-preprocess_words_to_docs_minimal
## @knitr warning=FALSE
preprocess_words_to_docs_minimal <- function(words) {
  
  ## MINIMAL - Will not remove the following:
  ## - Stop words
  ## - "Nothing", "NA", "NADA"

  docs <- Corpus(VectorSource(words))
  docs.copy <- docs
  
  toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  # docs <- tm_map(docs, toSpace, "-")
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove punctuations
  # docs <- tm_map(docs, removePunctuation)
  
  return(docs)
}

## @knitr function-preprocess_words_to_docs
## @knitr warning=FALSE
preprocess_words_to_docs <- function(words) {
  
  ## MINIMAL - Will not remove the following:
  ## - Stop words

  docs <- Corpus(VectorSource(words))
  docs.copy <- docs
  
  toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  # docs <- tm_map(docs, toSpace, "-")
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove punctuations
  # docs <- tm_map(docs, removePunctuation)
  
  # Remove words (na)
  #docs <- tm_map(docs, removeWords, c("nada", "nothing", "na", "none"))
  
  return(docs)
}

## @knitr function-stem_words_to_doc
## @knitr warning=FALSE
stem_words_to_doc <- function(words) {
  
  ## Only stems words, assuming it has been preprocessed minimally
  
  docs <- Corpus(VectorSource(words))
  docs.copy <- docs
  
  # docs <- tm_map(docs, removePunctuation)
  docs.temp <- tm_map(docs, lemmatize_strings, language = "english")
  #docs.final <- tm_map(docs.temp, stemCompletion, dictionary = docs.copy) 
  
  return(docs.temp)
}
