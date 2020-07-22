source('embed_import_libraries.R')
source('embed_configuration.R')

# Functions
source('function-image_utilities.R')

library("scales")
library("grid")
library("gridExtra")

## Data Summary

load(file = "psy4100_master_data.RData")

print(paste0("Total participants (includes Shinji's): ", total_subjects))
print(paste0("Total participants (only MTurk): ", total_mturk_subjects))
print(paste0("Total images (including 3 practice images):", length(included_img_ids)))

master_df <- master_raw_stem_df[soa != "Unlimited", ]

# This is to replace the stem words with hyphen with the original.
master_df[stem_word %like% "-,", stem_word := word ] 
master_df[stem_word %like% ", -", stem_word := word ] 
master_df[stem_word %like% "-, ", stem_word := word ] 
master_df$confidence <- master_df$confidence + 1
#master_df <- master_df[!img_id %in% c(3, 9, 14), ]

source('functions-type1auc_intersubjectivity.R')

# Load the duplicate image ids.
load('duplicate_similar_images.RData')

# Calculate the image ids to exclude in the baseline (other sets) - only keep the main image (key)
img_ids_to_exclude_in_baseline <- c()
for (i in 1:nrow(duplicate_img_ids)) {
  a_row <- duplicate_img_ids[i, ]
  img_ids_to_exclude_in_baseline = c(img_ids_to_exclude_in_baseline, (setdiff(unlist(a_row$duplicate_set), list(a_row$img_id))))
}

print(sprintf("Image ids (%d) to be excluded: %s", length(img_ids_to_exclude_in_baseline), paste0(img_ids_to_exclude_in_baseline, collapse = ", ")))

# Main execution function

# Build a giant list for all the images based on image ids.
all_image_ids <- unique(master_df$img_id)
all_image_dfs <- NULL
all_image_dfs <- vector(mode = "list", length = length(all_image_ids))
for (img_index in 1:length(all_image_ids)) {
  img_id <- all_image_ids[img_index]
  img_df <- construct_subject_image_df(master_df, img_id, target_soa = 0)
  all_image_dfs[[img_index]] <- img_df
}

library(foreach)
library(doParallel)

cores <- detectCores()
print(paste0("Number of cores: ", cores))

registerDoParallel(cores)

all_img_type1auc <- data.table()

result <- foreach (target_img_id_idx=1:length(all_image_ids), .combine=rbind) %dopar% {
#for (target_img_id in all_image_ids) {

  target_img_id = all_image_ids[target_img_id_idx]
  original_img_df <- all_image_dfs[[which(all_image_ids == target_img_id)]]
  
  # Get the other image dfs and also filter duplicate/similar images with this target_img_id
  # duplicate_img_ids$img_id == target_img_id
  # duplicate_for_this_image <- duplicate_img_ids[mapply(`%in%`, target_img_id, duplicate_set)]$duplicate_set # could be empty

  all_other_image_dfs <- all_image_dfs[which(!all_image_ids %in% c(target_img_id, img_ids_to_exclude_in_baseline))]
  print(sprintf("Baseline size = %d", length(all_other_image_dfs)))

  img_df <- original_img_df
  other_image_dfs <- vector(mode = "list", length = length(all_other_image_dfs))
  for (other_image_index in 1:length(all_other_image_dfs)) {
    other_image_df <- all_other_image_dfs[[other_image_index]]
    #other_image_df_filter_soa <- other_image_df[soa == target_soa, ]
    other_image_dfs[[other_image_index]] <- other_image_df
  }
  
  type1auc_df <- img_df[, .(type1auc = calculate_auc_img_sbj_word(img_df, other_image_dfs, target_img_id, as.String(stem_word), sbj_idx, soa, output_plot = FALSE, include_confidence = FALSE), 
                            type1auc_weighted = calculate_auc_img_sbj_word(img_df, other_image_dfs, target_img_id, as.String(stem_word), sbj_idx, soa, output_plot = FALSE, include_confidence = TRUE)), 
                        by=c("sbj_idx", "stem_word", "soa")]

  #type1auc_df_avg <- type1auc_df[, .(avg_type1auc = mean(type1auc), avg_type1auc_weighted = mean(type1auc_weighted)), by=c("stem_word", "soa")][order(-avg_type1auc)]
  #type1auc_df_avg <- type1auc_df_avg[avg_type1auc > 0.5, ]
  
  type1auc_df$img_id <- target_img_id
  all_img_type1auc <- rbind(all_img_type1auc, type1auc_df)
  
  print(sprintf("Finished image %s.", target_img_id))
  return (type1auc_df)
}

save(result, file = "all_img_type1auc_with_practice_exclude_duplicates.RData")





