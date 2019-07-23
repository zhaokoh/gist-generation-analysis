## @knitr function-analyse_plot_single_image
## Function - Perform single image analysis and generate plots
analyse_plot_single_image <- function(master_df, imgid, ignore_count_threshold = 0) {
  dfi <- copy(master_df[img_id == imgid])
  
  ## Filter any unnecessary words
  dfi <- dfi[!word %in% c("na", "none", "nothing", "nada")]
  
  soa_labeller <- function(variable, value) {
    soa_labels <- c()
    for (single_soa in value) {
      total_subjects <- img_soa_total_subjects[img_id == imgid & soa == single_soa]$total_subjects[1]
      total_words <- img_soa_total_non_unique_words[img_id == imgid & soa == single_soa]$total_words[1]
      soa_labels <- rbind(soa_labels, paste0(single_soa, " (N=", total_subjects, ",W=", total_words, ")"))
    }
    return(soa_labels)
  }
  
  p <- ggplot(melt_merge_img_soa_total_words[img_id == imgid], aes(x=as.numeric(factor(soa)), y=value, col=word_type)) +
    geom_point() +
    geom_line(stat = "identity") +
    ggtitleimg("Word count per SOA", imgid) +
    scale_x_discrete(name="SOA", labels=c("67", "133", "267", "Unlimited")) +
    scale_y_continuous(name = "Word count") +
    theme(legend.title = element_blank())
  
  x_range <- ggplot_build(p)$layout$panel_params[[1]]$x.range
  y_range <- ggplot_build(p)$layout$panel_params[[1]]$y.range
  
  image <- image_scale(get_image_by_id(nishimoto_images_folder, imgid), "200")
  
  p <- p + annotation_custom(rasterGrob(image, interpolate = FALSE), 
                        xmin = x_range[2], xmax = x_range[2] + 2, ymin = y_range[2] - 10, ymax = y_range[2])  +
    coord_cartesian(clip = 'off') +
    theme(plot.margin = unit(c(1,3,1,1), "lines"))
  ggsave(paste0("word_count_per_soa_img_", imgid, '.png'), p)
  
  ## Plot: Word frequency by SOA (Individual Image)
  
  word_freq_factor <- reorder(dfi$word, dfi$frequency, sum)
  word_freq_factor_df <- data.table(word = as.vector(names(attr(word_freq_factor, "scores"))), frequency = as.vector(unname(attr(word_freq_factor, "scores"))))
  word_freq_factor_df <- word_freq_factor_df[frequency > ignore_count_threshold]
  words_above_threshold <- word_freq_factor_df$word
  
  word_freq_df <- copy(dfi)
  word_freq_df <- word_freq_df[word %in% words_above_threshold]

  p1 <- ggplot(word_freq_df) + 
    geom_histogram(stat = "identity") +
    aes(x=reorder(word_freq_df$word, word_freq_df$frequency, sum), y=1, col=soa, fill = soa) +
    coord_flip() +
    ggtitleimg("Word frequency distribution by SOA", imgid) +
    xlab("Word") + 
    ylab("Count") +
    theme(axis.text = element_text(size = 6)) +
    facet_grid(. ~ soa, labeller = soa_labeller) 

  max_count <- max(word_freq_df[, sum(frequency), by = c("soa", "word")]$V1)
  p1 <- p1 + scale_y_continuous(breaks = seq(0, max_count, by=2))
  gt <- add_image_to_plot(p1, image)
  ggsave(paste0("ind_word_freq_dist_per_soa_img_", imgid, '.png'), gt)

  ## Plot: Word proportion by SOA (Individual Image)
  word_prop_df <- copy(dfi)
  word_prop_df <- word_prop_df[word %in% words_above_threshold]
  word_prop_df <- word_prop_df[, .(soa_single_word_freq = sum(frequency)), by = c("img_id", "soa", "word")]
  word_prop_df <- word_prop_df[, soa_total_words := sum(soa_single_word_freq), by = c("img_id", "soa")]
  word_prop_df <- word_prop_df[, .(word_soa_proportion = soa_single_word_freq/soa_total_words), by = c("img_id", "soa", "word")]
  
  p2 <- ggplot(word_prop_df) + 
    geom_histogram(stat = "identity") +
    aes(x=reorder(word_prop_df$word, word_prop_df$word_soa_proportion, sum), y=word_soa_proportion, col=soa, fill = soa) +
    coord_flip() +
    ggtitleimg("Word proportion distribution by SOA", imgid) +
    xlab("Word") + 
    ylab("Proportion") +
    theme(axis.text = element_text(size = 6)) +
    facet_grid(. ~ soa, labeller = soa_labeller) 
  
  max_count <- max(word_prop_df[, word_soa_proportion, by = c("soa", "word")]$word_soa_proportion)
  p2 <- p2 + scale_y_continuous(breaks = seq(0, max_count, by=0.05)) +
      theme(axis.text.x = element_text(size = 5))
  gt <- add_image_to_plot(p2, image)
  ggsave(paste0("ind_word_proportion_dist_per_soa_img_", imgid, '.png'), gt)
  
  ## Plot: Word proportion weighted with mean confidence by SOA (Individual Image)
  word_prop_conf_df <- copy(dfi)
  word_prop_conf_df <- word_prop_conf_df[word %in% words_above_threshold]
  
  ### Unlimited SOA has no confidence [ASSUMPTION]
  word_prop_conf_df[is.na(confidence)]$confidence <- 1
  word_prop_conf_df <- word_prop_conf_df[, .(soa_single_word_freq = sum(frequency), 
                                             soa_single_word_confidence = mean(confidence)
  ), 
  by = c("img_id", "soa", "word")]
  word_prop_conf_df <- word_prop_conf_df[, soa_total_words := sum(soa_single_word_freq), by = c("img_id", "soa")]
  word_prop_conf_df <- word_prop_conf_df[, soa_weighted_proportion := (soa_single_word_freq/soa_total_words)*soa_single_word_confidence, by = c("img_id", "soa", "word")]
  word_prop_conf_df <- word_prop_conf_df[, soa_total_weighted_proportion := sum(soa_weighted_proportion), by = c("img_id", "soa")]
  word_prop_conf_df <- word_prop_conf_df[, .(norm_soa_weighted_proportion = (soa_weighted_proportion/soa_total_weighted_proportion)), by = c("img_id", "soa", "word")]
  
  p3 <- ggplot(word_prop_conf_df) + 
    geom_histogram(stat = "identity") +
    aes(x=reorder(word_prop_conf_df$word, word_prop_conf_df$norm_soa_weighted_proportion, sum), y=norm_soa_weighted_proportion, col=soa, fill = soa) +
    coord_flip() +
    ggtitleimg("Word confidence-weighted proportion distribution by SOA", imgid) +
    xlab("Word") + 
    ylab("Proportion") +
    theme(axis.text = element_text(size = 6)) +
    facet_grid(. ~ soa, labeller = soa_labeller) 
  
  max_count <- max(word_prop_conf_df[, norm_soa_weighted_proportion, by = c("soa", "word")]$norm_soa_weighted_proportion)
  p3 <- p3 + scale_y_continuous(breaks = seq(0, max_count, by=0.05)) +
      theme(axis.text.x = element_text(size = 5))
  gt <- add_image_to_plot(p3, image)
  ggsave(paste0("ind_word_weighted_proportion_dist_per_soa_img_", imgid, '.png'), gt)
}


add_image_to_plot <- function(plt, image) {
  q1 <- ggplot() + annotation_custom(rasterGrob(image, interpolate = FALSE))
  
  p2 <- ggplotGrob(plt)
  q2 <- ggplotGrob(q1)
  
  gt <- gtable(widths = unit(c(1, 1, .4), "null"), heights = unit(c(.8, 1, 1), "null"))
  gt <- gtable_add_grob(gt, p2, t = 1, b = 3, l = 1, r = 3)
  gt <- gtable_add_grob(gt, q2, t = 1, l = 3)
  
  return(gt)
}