## @knitr function-plot_word_cloud
plot_word_cloud <- function(data, title_text = "", size, plot_word_cloud = TRUE, title_offset = -5) {

    ## Replace real NA with Nothing string
    ## The reason is some of the text mining packages replace NA value automagically.
  
    # data$d1[has_nothing_response(data$d1)] <- "Nothing"
    # data$d2[has_nothing_response(data$d2)] <- "Nothing"
    # data$d3[has_nothing_response(data$d3)] <- "Nothing"
    # data$d4[has_nothing_response(data$d4)] <- "Nothing"
    # data$d5[has_nothing_response(data$d5)] <- "Nothing"

    words <- data
    
    # Ignore Don't Know
    words[words$rb1 == "Don't Know", d1 := "NA"]
    words[words$rb2 == "Don't Know", d2 := "NA"]
    words[words$rb3 == "Don't Know", d3 := "NA"]
    words[words$rb4 == "Don't Know", d4 := "NA"]
    words[words$rb5 == "Don't Know", d5 := "NA"]
    
    # Convert confidence ratings to numbers
    words[words$rb1 == "Don't Know",]$rb1 <- "0"
    words[words$rb2 == "Don't Know",]$rb2 <- "0"
    words[words$rb3 == "Don't Know",]$rb3 <- "0"
    words[words$rb4 == "Don't Know",]$rb4 <- "0"
    words[words$rb5 == "Don't Know",]$rb5 <- "0"

    words[words$rb1 == "Guess",]$rb1 <- "1"
    words[words$rb2 == "Guess",]$rb2 <- "1"
    words[words$rb3 == "Guess",]$rb3 <- "1"
    words[words$rb4 == "Guess",]$rb4 <- "1"
    words[words$rb5 == "Guess",]$rb5 <- "1"
    
    words[words$rb1 == "Maybe",]$rb1 <- "2"
    words[words$rb2 == "Maybe",]$rb2 <- "2"
    words[words$rb3 == "Maybe",]$rb3 <- "2"
    words[words$rb4 == "Maybe",]$rb4 <- "2"
    words[words$rb5 == "Maybe",]$rb5 <- "2"

    words[words$rb1 == "Confident",]$rb1 <- "3"
    words[words$rb2 == "Confident",]$rb2 <- "3"
    words[words$rb3 == "Confident",]$rb3 <- "3"
    words[words$rb4 == "Confident",]$rb4 <- "3"
    words[words$rb5 == "Confident",]$rb5 <- "3"
    
    words[words$rb1 == "Very Confident",]$rb1 <- "4"
    words[words$rb2 == "Very Confident",]$rb2 <- "4"
    words[words$rb3 == "Very Confident",]$rb3 <- "4"
    words[words$rb4 == "Very Confident",]$rb4 <- "4"
    words[words$rb5 == "Very Confident",]$rb5 <- "4"

    confidence_words <- words %>%
      melt(measure.vars = c("d1","d2","d3","d4","d5","rb1","rb2","rb3","rb4","rb5"))
    confidence_words <- confidence_words[value != "NA" & value != "0", ]
    confidence_words$value <- tolower(confidence_words$value)

    confidence_words_w <- data.table(confidence_words)[variable %in% c("d1", "d2", "d3", "d4", "d5")]
    confidence_words_c <- data.table(confidence_words)[variable %in% c("rb1", "rb2", "rb3", "rb4", "rb5")]
    
    confidence_words_c$variable <- sub(pattern = "rb([1-5])", replacement = "d\\1", x = confidence_words_c$variable)
    confidence_words_merge <- merge(confidence_words_w, confidence_words_c, 
                                    by = c("subject", "blocknum", "trialnum", "values.soa", "values.img_file", "variable"),
                                    suffixes = c(".word", ".confidence"),
                                    all=FALSE)

    words <- words %>%
    unite("row_words", d1, d2, d3, d4, d5, sep = "@", remove = FALSE) %>%
    dplyr::select(row_words)
    all_words = paste(words$row_words, collapse = "@")

    d <- cleanse_words(all_words, FUN = preprocess_words_to_docs)

  if (length(d$word) > 1) {
    if (plot_word_cloud == TRUE) {
      set.seed(1234)
      plot_word_cloud = wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                max.words=300, random.order=FALSE, rot.per=0.35, 
                colors=brewer.pal(3, "Dark2"), scale = c(size, 1.5))
      #title(title_text, line = title_offset)
    }
    
    dt <- data.table(word = d$word, frequency = d$freq)

    names(dt) <- c("word", "freq")
    #dt$freq <- (dt$freq)^(2/3)
    dt <- dt[word != "na"]

    cur_colour_list <- rev(colorRampPalette(brewer.pal(name="Blues", n = 8))(20))
    default_color_list <- rep("#bfbfbf", nrow(dt))

    total_colors <- length(cur_colour_list)
    word_size <- min(length(cur_colour_list), nrow(dt[freq > 1]))
    top_x_words <- dt$word[1:word_size]
    #View(top_x_words)
    
    accum_word_conf <- confidence_words_merge[
      value.word %in% top_x_words, 
      .(avg_confidence = mean(as.integer(value.confidence))), 
      by = value.word][order(-avg_confidence)]

    #View(accum_word_conf)
    
    final_colour_list <- rev(colorRampPalette(brewer.pal(name="Blues", n = 8))(20))[1:word_size]
    all_colour_list <- c(final_colour_list[match(top_x_words, accum_word_conf$value.word)], default_color_list)
    
    wc <- wordcloud2(dt, size = 0.8, color = all_colour_list, minSize = 5)
    print(wc + WCtheme(1))
    #print(wc)

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
