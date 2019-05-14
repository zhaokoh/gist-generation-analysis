## @knitr function-plot_word_cloud
plot_word_cloud <- function(data, title_text = "", size, plot_word_cloud = TRUE, title_offset = -5) {
  words <- data %>%
    unite("row_words", d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, sep = "@", remove = FALSE) %>%
    dplyr::select(row_words)
  all_words = paste(words$row_words, collapse = "@")
  
  docs <- Corpus(VectorSource(all_words))
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  docs <- tm_map(docs, toSpace, "-")
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Convert the 
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("nada", "nothing"))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  
  
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  #inspect(docs)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  #head(d, 10);
  
  if (length(d$word) > 1) {
    if (plot_word_cloud == TRUE) {
      set.seed(1234)
      plot_word_cloud = wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35, 
                colors=brewer.pal(3, "Dark2"), scale = c(size, 0.5))
      title(title_text, line = title_offset)
    }
    
    dt <- data.table(word = d$word, frequency = d$freq)
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