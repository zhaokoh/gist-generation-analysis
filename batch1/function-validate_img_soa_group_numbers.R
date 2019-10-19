## @knitr function-validate_img_soa_group_numbers
validate_img_soa_group_numbers <- function(summary_file, details_file, expected_group_number, expected_image_number, enforce_validation = true) {
  ## This is sanity check to ensure all groups have go through all images and soa permutation
  
  summary_df = fread(summary_file, na.strings = "NULLX")
  summary_df <- subset(summary_df, values.completed == 1) # Only select completed participants
  
  details_df = fread(details_file, na.strings = "NULLX")
  details_df = subset(details_df, subject %in% summary_df$script.subjectid)

  subject_group <- summary_df[, .(subject = as.character(script.subjectid), group = script.groupid)]
    
  exp_block_df <- details_df[blockcode != 'practice_block' & response != 0,];
  exp.data <- data.table(exp_block_df %>%
                           spread(trialcode, response) %>%
                           .[, .(subject, blocknum, trialnum, values.soa, values.img_file, 
                                 d1, rb1, d2, rb2, d3, rb3, d4, rb4, d5, rb5)] %>%
                           arrange(subject, blocknum, trialnum))
  exp_subject_img_soa <- exp.data[, .(subject = as.character(subject), soa = values.soa, img = values.img_file)]
  exp_subject_img_soa <- merge(exp_subject_img_soa, subject_group, by = "subject", all = FALSE)
  exp_aggregate_group <- exp_subject_img_soa[, .(total_image = length(unique(.SD$img))), by = c("group")]
  exp_aggregate_soa <- exp_subject_img_soa[, .(total_image = length(unique(.SD$img)), total_group = length(unique(.SD$group))), by = c("soa")]
  exp_aggregate_img_soa <- exp_subject_img_soa[, .(total_subject = length(unique(.SD$subject)), total_subject = length(unique(.SD$group))), by = c("img", "soa")]
  
  # Assertions
  if (enforce_validation) {
    stopifnot(length(unique(subject_group$group)) == expected_group_number) # Group should 30 (unique)
    stopifnot(length(unique(exp_aggregate_group$group)) == expected_group_number)
    stopifnot(length(unique(exp_aggregate_group$total_image)) == 1)
    stopifnot(unique(exp_aggregate_group$total_image) == expected_image_number) # Total number of unique images per group
    stopifnot(unique(exp_aggregate_soa$total_image) == expected_image_number) # Total number of unique images per soa
    stopifnot(unique(exp_aggregate_soa$total_group) == expected_group_number) # Total number of unique groups per soa
    stopifnot(unique(exp_aggregate_img_soa$total_group) == expected_group_number/3) # Total number of unique groups per img per soa (should be 10)
    stopifnot(unique(exp_aggregate_img_soa$total_subject) >= expected_group_number/3) # Total number of unique subject per img per soa (should be greater or equals to group).
    
    # All the details subject should be unique - this is to check if one subject completed the experiment twice (with different timestamps)
    more_than_obce_subjects_df = details_df[, .N, by = c("time", "subject")][, .N, by = c("subject")][N > 1]
    if (nrow(more_than_obce_subjects_df) > 0) {
      browser()
    }
    stopifnot(nrow(more_than_obce_subjects_df) == 0)
  }
  
  return(list("summary" = summary_df, "details" = details_df))
}


