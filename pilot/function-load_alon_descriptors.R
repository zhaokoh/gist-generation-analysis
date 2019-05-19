## @knitr function-load_alon_descriptors
load_alon_descriptors <- function(alon_present_words_filename, alon_subject_descriptions_filename, alon_remove_words_filename) {
  alon_img_present_dt = fread(alon_present_words_filename)
  alon_img_descriptions_dt = fread(alon_subject_descriptions_filename)
  
  setnames(alon_img_descriptions_dt, c("img_id", "desc1", "desc2", "desc3", "desc4", "desc5"))
  
  alon_filtered_img_present_dt = alon_img_present_dt[image_id %in% unique(all_df$img_id)]
  alon_img_words <- separate_rows(alon_filtered_img_present_dt, present_words, convert = TRUE) %>%
    setnames(c("img_id", "word"))
  
  alon_img_desc = data.table();
  
  for (row_id in 1:nrow(alon_img_words)) {
    row <- alon_img_words[row_id, ]
    
    desc_row <- alon_img_descriptions_dt[img_id == row$img_id, ]
    expr <- paste("/ ", row$word, " | ",row$word, "|", row$word, " |", "^", row$word, "|", row$word, "$", "/", sep = "")
    sum <- 0
    
    for (desc in c(desc_row$desc1, desc_row$desc2, desc_row$desc3, desc_row$desc4, desc_row$desc5)) {
      pos <- gregexpr(expr, desc, ignore.case = TRUE)
      if (pos[[1]][1] != -1) {
        sum <- sum + 1
      }
    }
    
    r <- data.table(row$img_id, row$word, sum)
    alon_img_desc <- rbind(alon_img_desc, r)
  }
  
  setnames(alon_img_desc, c("img_id", "word", "frequency"))
  alon_img_desc <- alon_img_desc[, probability := frequency/sum(frequency), by = img_id]

  # Sanity check (both lines below should equal)
  # length(unique(alon_img_desc[, img_id]))
  # sum(alon_img_desc[, sum(probability), by=img_id]$V1)

  alon_remove_words_dt = fread(alon_remove_words_filename)
  alon_remove_words_list = unique(gsub("'", "", alon_remove_words_dt[,1]$V1))
  
  not_removed_words_zero_freq <- alon_img_desc[frequency == 0][!word %in% alon_remove_words_list,]
  print(not_removed_words_zero_freq)
  
  alon_img_desc <- alon_img_desc[frequency != 0]
  return (alon_img_desc)
}
