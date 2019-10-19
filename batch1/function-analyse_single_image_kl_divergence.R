## @knitr function-analyse_single_image_kl_divergence
analyse_single_image_kl_divergence <- function(rdf, imgid) {
  mrdf <- rdf[img_id == imgid]
  kl_df <- copy(mrdf)
  kl_df <- kl_df[, .(soa_single_word_freq = sum(frequency)), by = c("img_id", "soa", "word")]
  kl_df <- kl_df[, soa_total_words := sum(soa_single_word_freq), by = c("img_id", "soa")]
  kl_df <- kl_df[, .(word_soa_proportion = soa_single_word_freq/soa_total_words), by = c("img_id", "soa", "word")]
  
  kl_proportion_stats_img_dt <- compute_pairwise_kl(kl_df, prob_colname = "word_soa_proportion")
  melt_kl_proportion_stats_img_dt <- melt(kl_proportion_stats_img_dt, id.vars = "img_id", variable.name = "kl_type", value.name = "kl_value")
  
  word_prop_conf_df_img <- copy(mrdf)
  word_prop_conf_df_img[is.na(confidence)]$confidence <- 1
  word_prop_conf_df_img <- word_prop_conf_df_img[, .(soa_single_word_freq = sum(frequency), 
                                                     soa_single_word_confidence = mean(confidence)
  ), 
  by = c("img_id", "soa", "word")]
  word_prop_conf_df_img <- word_prop_conf_df_img[, soa_total_words := sum(soa_single_word_freq), by = c("img_id", "soa")]
  word_prop_conf_df_img <- word_prop_conf_df_img[, soa_weighted_proportion := (soa_single_word_freq/soa_total_words)*soa_single_word_confidence, by = c("img_id", "soa", "word")]
  word_prop_conf_df_img <- word_prop_conf_df_img[, soa_total_weighted_proportion := sum(soa_weighted_proportion), by = c("img_id", "soa")]
  word_prop_conf_df_img <- word_prop_conf_df_img[, .(norm_soa_weighted_proportion = (soa_weighted_proportion/soa_total_weighted_proportion)), by = c("img_id", "soa", "word")]
  
  kl_weighted_pror_stats_img_dt <- compute_pairwise_kl(word_prop_conf_df_img, prob_colname = "norm_soa_weighted_proportion")
  melt_kl_weighted_pror_stats_img_dt <- melt(kl_weighted_pror_stats_img_dt, id.vars = "img_id", variable.name = "kl_type", value.name = "kl_value")
  
  plot_kl_divergence(melt_kl_proportion_stats_img_dt, paste0("[", batch_number, "] KL Divergence (Proportion)"), paste0("proportion-", imgid))
  plot_kl_divergence(melt_kl_weighted_pror_stats_img_dt, paste0("[", batch_number, "] KL Divergence (Weighted Proportion)"), paste0("weighted-", imgid))
}
