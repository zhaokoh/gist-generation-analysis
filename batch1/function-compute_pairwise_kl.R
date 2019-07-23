## @knitr function-compute_pairwise_kl
compute_pairwise_kl <- function(df, word_colname = "word", prob_colname) {
  kl_stats_dt <- df[, .(kl_67_133 = compute_kl(.BY, .SD, c("67", "133") , word_colname, prob_colname),
                        kl_67_267 = compute_kl(.BY, .SD, c("67", "267"), word_colname, prob_colname),
                        kl_67_Unlimited = compute_kl(.BY, .SD, c("67", "Unlimited"), word_colname, prob_colname),
                        kl_133_67 = compute_kl(.BY, .SD, c("133", "67"), word_colname, prob_colname),
                        kl_133_267 = compute_kl(.BY, .SD, c("133", "267"), word_colname, prob_colname),
                        kl_133_Unlimited = compute_kl(.BY, .SD, c("133", "Unlimited"), word_colname, prob_colname),
                        kl_267_67 = compute_kl(.BY, .SD, c("267", "67"), word_colname, prob_colname),
                        kl_267_133 = compute_kl(.BY, .SD, c("267", "133"), word_colname, prob_colname),
                        kl_267_Unlimited = compute_kl(.BY, .SD, c("267", "Unlimited"), word_colname, prob_colname),
                        kl_Unlimited_67 = compute_kl(.BY, .SD, c("Unlimited", "67"), word_colname, prob_colname),
                        kl_Unlimited_133 = compute_kl(.BY, .SD, c("Unlimited", "133"), word_colname, prob_colname),
                        kl_Unlimited_267 = compute_kl(.BY, .SD, c("Unlimited", "267"), word_colname, prob_colname)
  ), by = c("img_id")]
  return(kl_stats_dt)
}
