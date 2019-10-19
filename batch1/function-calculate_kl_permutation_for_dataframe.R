## @knitr function-calculate_kl_permutation_for_dataframe
calculate_kl_permutation_for_dataframe <- function(df, normalised) {
  kl_df <- copy(df)
  
  # Sanity check - no subject should have come up with the same word more than once
  test_more_than_one_word_per_subject_df <- kl_df[confidence > 0][, .(soa_subject_single_word_freq = sum(frequency)), by = c("img_id", "soa", "word", 'subject')][soa_subject_single_word_freq > 1]
  
  if (nrow(test_more_than_one_word_per_subject_df) > 0) {
    browser()
    stopifnot(nrow(test_more_than_one_word_per_subject_df) == 0)
  }
  
  kl_df <- aggregate_word_column(kl_df, c("img_id", "soa"))
  
  # Create a data frame that calculate how many unique subjects per img+soa (used for merging later and the denominator for the proportion)
  kl_img_soa_sbj_df <- copy(kl_df)
  kl_img_soa_sbj_df <- kl_img_soa_sbj_df[, .(soa_total_sbj_img_tested = length(unique(subject))), by = c("img_id", "soa")]
  
  ## Calculate word_soa_proportion based on "word count"/total(word count)
  # kl_df <- kl_df[, .(soa_single_stem_word_freq = sum(frequency)), by = c("img_id", "soa", "agg_word")]
  # kl_df <- kl_df[, soa_total_stem_words := sum(soa_single_stem_word_freq), by = c("img_id", "soa")]
  # kl_df <- kl_df[, .(word_soa_proportion = soa_single_stem_word_freq/soa_total_stem_words), by = c("img_id", "soa", "agg_word")]
  
  # Merge the total subjects, and calculate the proportion
  kl_img_soa_word_df <- kl_df[, .(soa_sbj_word_freq = length(unique(subject)), soa_single_word_confidence = mean(confidence)), by = c("img_id", "soa", "agg_word")]
  kl_img_soa_word_df <- merge(kl_img_soa_word_df, kl_img_soa_sbj_df, all.x = TRUE, by = c("img_id", "soa"))
  kl_df <- kl_img_soa_word_df[, .(word_soa_proportion = soa_sbj_word_freq/soa_total_sbj_img_tested), by = c("img_id", "soa", "agg_word")]
  
  if (normalised) {
    # Normalise the columns before we calculate KL divergence (need to sum up to 1)
    kl_total_word_soa_df <- kl_df[, .(total_word_soa_proportion = sum(word_soa_proportion)), by = c("img_id", "soa")]
    kl_df = merge(kl_df, kl_total_word_soa_df, all.x = TRUE, by = c("img_id", "soa"))
    kl_df <- kl_df[, .(word_soa_proportion_norm = word_soa_proportion/total_word_soa_proportion), by = c("img_id", "soa", "agg_word")]
    
    kl_df <- compute_pairwise_kl(kl_df, "agg_word", "word_soa_proportion_norm")
  }
  
  return (kl_df)
}
