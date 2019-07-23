## @knitr function-aggregate_word_column
aggregate_word_column <- function(df, aggregate_cols) {
  new_df <- copy(df)
  
  aggregate_cols <- c(aggregate_cols, "stem_word")
  stem_agg_word <- new_df[, .(agg_word = toString(unique(word))), by = aggregate_cols]
  
  new_df <- cbind(new_df, agg_word = stem_agg_word[match(unlist(new_df$stem_word), stem_agg_word$stem_word)]$agg_word)
  return(new_df)
}
