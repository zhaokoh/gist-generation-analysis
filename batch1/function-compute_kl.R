## @knitr function-compute_kl
compute_kl <- function(keys, img_dt, soa_vector, word_colname, probability_colname) {
  word_order <- unique(reorder(img_dt[[word_colname]], img_dt[[probability_colname]], sum))

  img_dt[[word_colname]] <- as.factor(img_dt[[word_colname]])
  
  soa1 = soa_vector[1]
  soa2 = soa_vector[2]
  
  soa1_order_table <- data.table(cbind(soa1, as.character(word_order)))
  colnames(soa1_order_table) <- c("soa", word_colname)
  soa1_order_table[[word_colname]] <- as.factor(soa1_order_table[[word_colname]])

  soa2_order_table <- data.table(cbind(soa2, as.character(word_order)))
  colnames(soa2_order_table) <- c("soa", word_colname)
  soa2_order_table[[word_colname]] <- as.factor(soa2_order_table[[word_colname]])
  
  soa1_list <- merge(img_dt[soa == soa1], soa1_order_table, by=c("soa", word_colname), all=TRUE)[order(match(img_dt[[word_colname]], word_order))]
  soa2_list <- merge(img_dt[soa == soa2], soa2_order_table, by=c("soa", word_colname), all=TRUE)[order(match(img_dt[[word_colname]], word_order))]
  
  soa1_list[which(is.na(soa1_list[[probability_colname]])), probability_colname] <- 0
  soa2_list[which(is.na(soa2_list[[probability_colname]])), probability_colname] <- 0
  
  kl_1_2 <- round(KL(rbind(
    soa1_list[[probability_colname]], 
    soa2_list[[probability_colname]]), 
    test.na = FALSE, unit="log2"),
    4)
  return(kl_1_2)
}
