construct_subject_image_df <- function(master_df, img_id_value, target_soa, show_progress = FALSE) {
  if (target_soa != 0) {
    master_df <- master_df[soa == target_soa, ]
  }
  soa_subject_df <- unique(master_df[img_id == img_id_value, subject, by=c("soa")])
  
  if (nrow(soa_subject_df) == 0) {
    warning(sprintf("No data found for image %d", img_id_value))
    return(NULL)
  }
  
  soa_subject_count_df <- soa_subject_df[, .(subject_count = .N), by=c("soa")]
  #max_subject_per_soa <- min(soa_subject_count_df$subject_count);
  max_subject_per_soa <- 10
  
  ## Pre-process data set (label each subject index)
  img_df <- master_df[img_id == img_id_value, ]
  
  img_df$sbj_idx <- 0
  img_df$total_agreements <- 0
  img_df$no_of_agreements <- 0
  img_df$no_of_false_alarm <- 0
  
  for (a_soa in unique(img_df$soa)) { 
    
    # Trim subjects (This need to come before allocating subjects)
    soa_df <- img_df[soa == a_soa, ]
    subject_ids <- unique(soa_df$subject)
    unique_subject_count <- length(subject_ids)
    
    if (unique_subject_count > max_subject_per_soa) {
      all_sbj_ids <- unique(soa_df$subject)
      exclude_sbj_ids <- all_sbj_ids[max_subject_per_soa+1:length(all_sbj_ids)]
      
      img_df <- img_df[!(subject %in% exclude_sbj_ids), ]
      
      if (show_progress) {
        print(sprintf("[Dropping subjects] SOA %s has %d subjects, max subject is %d.", a_soa, unique_subject_count, max_subject_per_soa))
      }
    } else if (unique_subject_count != max_subject_per_soa) {
      throw(sprintf("This image %s only contains %d subjects.", img_id_value, unique_subject_count))
    }
    
    # Allocate subject index
    subject_ids <- unique(soa_df$subject)
    for (sbj_indices in 1:length(subject_ids)) {
      img_df[subject == subject_ids[sbj_indices], ]$sbj_idx <- sbj_indices
    }
  }
  
  return(img_df)
}

# We construct all the images DFs

calc_cumsum_percentage <- function(arr) {
  arr[is.na(arr)] <- 0
  if (sum(arr) > 0) {
    return (cumsum(arr)/sum(arr)*100)
  } else {
    return (arr)
  }
}

calculate_auc_img_sbj_word <- function(img_df, other_image_dfs, target_img_id, target_word, target_subject_index, target_soa, output_plot = FALSE, include_confidence = FALSE) {
  
  all_plots <- list()
  all_agg_data <- data.table()
  fix_no_other_participants <- 27
  
  hit_count_df <- img_df[stem_word == target_word & sbj_idx != target_subject_index, .(subject, sbj_idx, word, confidence, soa, stem_word, frequency)]

  hit_count <- replicate(fix_no_other_participants, 0) 
  hit_confidence <- replicate(fix_no_other_participants, 0)

  # Setting 1 below is because we consider the current image with 27 participants is one trial.
  # If we perform the same experiment multiple times, then the hit count will increase and equal to the number of trials.
  hit_count[nrow(hit_count_df)] <- 1 
  hit_confidence[nrow(hit_count_df)] <- mean(hit_count_df$confidence)

  other_df <- img_df[sbj_idx != target_subject_index, .(dummy = 0), by = c("subject", "sbj_idx", "soa")]
  
  merge_df <- merge(hit_count_df, other_df, all=TRUE, by=c("subject", "sbj_idx", "soa"))
  merge_df <- merge_df[order(soa, sbj_idx)]
  merge_df$frequency <- replace_na(merge_df$frequency, 0)
  
  fa_count <- replicate(fix_no_other_participants, 0)
  fa_confidence_count <- replicate(fix_no_other_participants, 0)
  fa_sum_confidence <- replicate(fix_no_other_participants, 0)
  
  for (other_img_index in 1:length(other_image_dfs)) {
    #other_img_id <- all_other_image_ids[other_img_index]
    other_img_all_df <- other_image_dfs[[other_img_index]]
    other_img_df <- other_img_all_df[sbj_idx != target_subject_index, ]
    
    a <-as.array(other_img_df[stem_word == target_word, .(sbj_idx),]$sbj_idx)
    
    if (length(a) > 0) {
      fa_count[length(a)] <- fa_count[length(a)] + 1
      fa_confidence_count[length(a)] <- fa_confidence_count[length(a)] + 
        sum(as.array(other_img_df[stem_word == target_word, .(count = .N),]$count))
      fa_sum_confidence[length(a)] <- fa_sum_confidence[length(a)] + 
        sum(as.array(other_img_df[stem_word == target_word, .(confidence),]$confidence))
    }
  }

  fa_confidence <- fa_sum_confidence/fa_confidence_count
  fa_confidence[is.na(fa_confidence)] <- 0

  hit_count[is.na(hit_count)] <- 0
  fa_count[is.na(fa_count)] <- 0
  
  # Append the zeroes element
  # FROM HERE NOW ON, the index is index + 1 (cater for zero-based index)
  if (sum(hit_count) == 0) {
    hit_count <- c(fix_no_other_participants, hit_count)
  } else {
    hit_count <- c(0, hit_count)
  }

  if (sum(fa_count) == 0) {
    fa_count <- c(length(other_image_dfs), fa_count)
  } else {
    fa_count <- c(length(other_image_dfs) - sum(fa_count), fa_count)
  }

  # Create a discrete version of hit and fa count (for plot)
  discrete_cell_count <- 4
  cell_interval <- (fix_no_other_participants + 1)/discrete_cell_count

  hit_ui_count <- replicate(discrete_cell_count, 0)
  fa_ui_count <- replicate(discrete_cell_count, 0)
  hit_ui_confidence <- replicate(discrete_cell_count, 0)
  fa_ui_confidence <- replicate(discrete_cell_count, 0)
  
  hit_confidence <- c(0, hit_confidence) 
  fa_confidence <- c(0, fa_confidence) 
  
  for (i in 1:discrete_cell_count) {
    hit_ui_count[i] <- sum(hit_count[(((i - 1) * cell_interval) + 1): (i * cell_interval)])
    fa_ui_count[i] <- sum(fa_count[(((i - 1) * cell_interval) + 1): (i * cell_interval)])

    if (hit_ui_count[i] > 0) {
      subset_hit_confidence <- hit_confidence[(((i - 1) * cell_interval) + 1): (i * cell_interval)]
      hit_ui_confidence[i] <- mean(subset_hit_confidence[which(subset_hit_confidence > 0)])
    }

    if (fa_ui_count[i] > 0) {
        subset_fa_confidence <- fa_confidence[(((i - 1) * cell_interval) + 1): (i * cell_interval)]
        fa_ui_confidence[i] <- mean(subset_fa_confidence[which(subset_fa_confidence > 0)])
    }
  }

  # Reverse direction: 100% - 0%
  hit_count <- rev(hit_count)
  hit_confidence <- rev(hit_confidence)
  fa_count <- rev(fa_count)
  fa_confidence <- rev(fa_confidence)
  
  hit_ui_count <- rev(hit_ui_count)
  hit_ui_confidence <- rev(hit_ui_confidence)
  fa_ui_count <- rev(fa_ui_count)
  fa_ui_confidence <- rev(fa_ui_confidence)
  
  hit_ui_count[is.na(hit_ui_count)] <- 0
  fa_ui_count[is.na(fa_ui_count)] <- 0
  hit_ui_confidence[is.na(hit_ui_confidence)] <- 0
  fa_ui_confidence[is.na(fa_ui_confidence)] <- 0
  
  hit_ui_percentage <- replicate(discrete_cell_count, 0)
  if (sum(hit_count[1:fix_no_other_participants]) == 0) {
    hit_percentage <- c(hit_count[1:fix_no_other_participants], 1)
    hit_ui_percentage[discrete_cell_count] <- 1
  } else {
    hit_percentage <- c(hit_count[1:fix_no_other_participants]/sum(hit_count[1:fix_no_other_participants]), 0)
    hit_ui_percentage <- hit_ui_count/sum(hit_ui_count)
  }
  
  # if (sum(fa_count[1:fix_no_other_participants]) == 0) {
  #   fa_percentage <- c(fa_count[1:fix_no_other_participants], 1)
  # } else {
  #   fa_percentage <- c(fa_count[1:fix_no_other_participants]/sum(fa_count[1:fix_no_other_participants]), 0)
  # }
  fa_percentage <- fa_count/sum(fa_count)
  fa_ui_percentage <- fa_ui_count/sum(fa_ui_count)
  
  hit_percentage_orig <- hit_percentage
  fa_percentage_orig <- fa_percentage
  hit_ui_percentage_orig <- hit_ui_percentage
  fa_ui_percentage_orig <- fa_ui_percentage
  
  if (include_confidence) {
    hit_percentage[1:fix_no_other_participants] <- hit_percentage[1:fix_no_other_participants] * hit_confidence[1:fix_no_other_participants]
    fa_percentage[1:fix_no_other_participants] <- fa_percentage[1:fix_no_other_participants] * fa_confidence[1:fix_no_other_participants]

    if (hit_ui_confidence[discrete_cell_count] == 0) {
      hit_ui_percentage[1:discrete_cell_count-1] <- hit_ui_percentage[1:discrete_cell_count-1] * hit_ui_confidence[1:discrete_cell_count-1]
    } else {
      hit_ui_percentage <- hit_ui_percentage * hit_ui_confidence
    }
    
    if (fa_ui_confidence[discrete_cell_count] == 0) {
      fa_ui_percentage[1:discrete_cell_count-1] <- fa_ui_percentage[1:discrete_cell_count-1] * fa_ui_confidence[1:discrete_cell_count-1]
    } else {
      fa_ui_percentage <- fa_ui_percentage * fa_ui_confidence
    }
  }
  
  hit_cum_percentage <- round(c(calc_cumsum_percentage(hit_percentage), 100), digits = 3)
  fa_cum_percentage <- round(c(calc_cumsum_percentage(fa_percentage), 100), digits = 3)
  
  hit_ui_cum_percentage <- round(c(calc_cumsum_percentage(hit_ui_percentage), 100), digits = 3)
  fa_ui_cum_percentage <- round(c(calc_cumsum_percentage(fa_ui_percentage), 100), digits = 3)
  
  criterion=rev(c(0:fix_no_other_participants))
  sig_df <- data.table(criterion=criterion, criterion_index=round(criterion/27, digits=3),
                       hit_count=hit_count, hit_percentage=hit_percentage, 
                       hit_cum_percentage=head(hit_cum_percentage,-1), fa_count=fa_count, fa_percentage=fa_percentage,
                       fa_cum_percentage=head(fa_cum_percentage, -1), hit_confidence=hit_confidence, fa_confidence=fa_confidence,
                       hit_percentage_orig=hit_percentage_orig, fa_percentage_orig=fa_percentage_orig)
  
  summary_criterion=c(1:discrete_cell_count)
  sig_summary_df <- data.table(criterion=summary_criterion,
                       hit_count=hit_ui_count, hit_percentage=hit_ui_percentage, 
                       hit_cum_percentage=head(hit_ui_cum_percentage,-1), fa_count=fa_ui_count, fa_percentage=fa_ui_percentage,
                       fa_cum_percentage=head(fa_ui_cum_percentage, -1), hit_confidence=hit_ui_confidence, fa_confidence=fa_ui_confidence,
                       hit_percentage_orig=hit_ui_percentage_orig, fa_percentage_orig=fa_ui_percentage_orig)

  roc_points <- sig_df[, .(tpr = hit_cum_percentage, fpr = fa_cum_percentage)]
  roc_points <- roc_points/100
  roc <- data.table()
  roc <- rbind(roc, roc_points[order(fpr)])
  
  library("pROC")
  
  roc_df <- transform(roc, 
                      dFPR = c(diff(fpr), 0),
                      dTPR = c(diff(tpr), 0))
  
  auc=sum((roc_df$tpr*roc_df$dFPR)) + sum(sum(roc_df$dTPR*roc_df$dFPR)/2)
  #print(sprintf('word = %s, sbj_idx = %d, auc = %.4f', target_word, target_subject_index, auc))
  
  if (output_plot) {
    sig_plt <- ggplot(sig_df) +
      geom_path(aes(x=criterion_index, y=hit_cum_percentage), colour="green", size=2) +
      geom_point(aes(x=criterion_index, y=hit_cum_percentage), colour="green", size=5) +
      geom_path(aes(x=criterion_index, y=fa_cum_percentage), colour="red", size=2) +
      geom_point(aes(x=criterion_index, y=fa_cum_percentage), colour="red", size=5) +
      scale_x_reverse(labels = percent) + 
      xlab("Minimum % of people required") +
      ylab("Cumulative Percentage") +
      theme(axis.text=element_text(size=16))
    
    roc_plt <- ggplot(roc, aes(x=fpr, y=tpr)) +
      geom_point(size=5) +
      geom_path(size=2) +
      geom_abline(intercept = 0, slope = 1, color="red",
                  linetype="dashed", size=1.5)+
      scale_x_continuous(limits=c(0, 1)) +
      scale_y_continuous(limits=c(0, 1)) +
      ylab("True positive rate (TPR)") +
      xlab("False positive rate (FPR)") + 
      ggtitle(sprintf("ROC Curve (subject index=%d, word=%s, type1auc = %.4f)", 1, target_word, auc)) +
      theme(axis.text=element_text(size=16))
    
    if (include_confidence) {
      table_cells <- rbind(sig_df$hit_count, sig_df$fa_count,
                           round(sig_df$hit_percentage_orig*100, digits=3), 
                           round(sig_df$fa_percentage_orig*100, digits=3),
                           round(sig_df$hit_confidence, digits=3), 
                           round(sig_df$fa_confidence, digits=3),
                           round(sig_df$hit_percentage*100, digits=3), 
                           round(sig_df$fa_percentage*100, digits=3),
                           round(sig_df$hit_cum_percentage, digits=3), 
                           round(sig_df$fa_cum_percentage, digits=3))
      row_names <- c("A Count", 
        "B Count", 
        "A%", 
        "B%", 
        "A Mean Confidence",
        "B Mean Confidence",
        "Confidence-weighted A%", 
        "Confidence-weighted B%", 
        "Cum A%","Cum B%")
      
      table_cells_short <- rbind(sig_summary_df$hit_count, 
                                 sig_summary_df$fa_count,
                                 round(sig_summary_df$hit_percentage_orig*100, digits=3), 
                                 round(sig_summary_df$fa_percentage_orig*100, digits=3),
                                 round(sig_summary_df$hit_confidence, digits=3), 
                           round(sig_summary_df$fa_confidence, digits=3),
                           round(sig_summary_df$hit_percentage*100, digits=3), 
                           round(sig_summary_df$fa_percentage*100, digits=3),
                           round(sig_summary_df$hit_cum_percentage, digits=3), 
                           round(sig_summary_df$fa_cum_percentage, digits=3))
      row_names <- c("A Count", 
                     "B Count", 
                     "A%", 
                     "B%", 
                     "A Mean Confidence",
                     "B Mean Confidence",
                     "Confidence-weighted A%", 
                     "Confidence-weighted B%", 
                     "Cum A%","Cum B%")      
    } else {
      table_cells <- rbind(sig_df$hit_count, sig_df$fa_count,
                           round(sig_df$hit_percentage*100, digits=3), 
                           round(sig_df$fa_percentage*100, digits=3),
                           round(sig_df$hit_cum_percentage, digits=3), 
                           round(sig_df$fa_cum_percentage, digits=3))
      
      table_cells_short <- rbind(sig_summary_df$hit_count, sig_summary_df$fa_count,
                                 round(sig_summary_df$hit_percentage*100, digits=3), 
                                 round(sig_summary_df$fa_percentage*100, digits=3),
                                 round(sig_summary_df$hit_cum_percentage, digits=3), 
                                 round(sig_summary_df$fa_cum_percentage, digits=3))
      row_names <- c("A", 
                     "B", 
                     "A%", 
                     "B%", 
                     "Cum A%","Cum B%")
    }
    
    tt_sml <- ttheme_default(
      core=list(fg_params=list(fontsize=16)),
      colhead=list(fg_params=list(fontsize=16)),
      rowhead=list(fg_params=list(fontsize=16)))    

    tt <- ttheme_default(
      core=list(fg_params=list(fontsize=21)),
      colhead=list(fg_params=list(fontsize=21)),
      rowhead=list(fg_params=list(fontsize=21)))    
    
    gg <- grid.arrange(
      grid.arrange(
        ggplotGrob(sig_plt),
        ggplotGrob(roc_plt),
        ncol = 3, widths=c(0.3, 0.3, 0.4), padding = 0),
      tableGrob(table_cells, cols = c(paste0(rev(round(c(0:fix_no_other_participants)*100/27, digits = 2)), "%")), 
                rows = row_names, theme = tt_sml),
      tableGrob(table_cells_short, cols = c("[100%, 75%]", "(75%, 50%]", "(50%, 25%]", "(25%, 0%]"), 
                rows = row_names, theme = tt),
      ncol = 1, heights=c(0.3, 0.4, 0.4), padding = 0)
    
    h <- 18
    ar <- 1.8
  
    if (include_confidence) {
      ggsave(gg, height=h, width= h*ar, filename = paste0("img_conf_weighted_", target_img_id, "_", target_soa,  "_", target_subject_index, "_word_", target_word, ".png"))
    } else {
      ggsave(gg, height=h, width= h*ar, filename = paste0("img_", target_img_id, "_", target_soa, "_", target_subject_index, "_word_", target_word, ".png"))
    }
  }
  
  return(auc)
}