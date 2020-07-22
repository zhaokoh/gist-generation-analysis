## @knitr function-analyse_plot_single_image
## Function - Perform single image analysis and generate plots
analyse_plot_single_image <- function(master_df, imgid, ignore_count_threshold = 0) {

  img_soa_total_subjects <- unique(master_df[, .(img_id, soa, subject)])[, .(total_subjects = .N), by = c("img_id", "soa")]
  img_soa_total_groups <- unique(master_df[, .(img_id, soa, group)])[, .(total_groups = .N), by = c("img_id", "soa")]
  
  img_soa_total_non_unique_words <- master_df[, .(total_words = .N), by = c("img_id", "soa")]
  img_soa_total_unique_words <- unique(master_df[, .(unique_words = .N), by = c("img_id", "soa", "word")])[, .(total_unique_words = .N), by = c("img_id", "soa")]

  merge_img_soa_total_words <- merge(img_soa_total_non_unique_words, img_soa_total_unique_words, by = c("img_id", "soa"))
  melt_merge_img_soa_total_words <- melt(merge_img_soa_total_words, id.vars = c("img_id", "soa"), variable.name = "word_type")
  
  merge_img_soa_unique_words_subjects <- merge(img_soa_total_subjects, img_soa_total_unique_words, by = c("img_id", "soa"))
  merge_img_soa_variability <- merge_img_soa_unique_words_subjects[, .(variability = total_unique_words/total_subjects), by = c("img_id", "soa")]
  
  dfi <- copy(master_df[img_id == imgid])
  
  ## Filter any unnecessary words
  dfi <- dfi[!word %in% c("na", "none", "nothing", "nada")]
  
  soa_labeller <- function(variable, value) { 
    soa_labels <- c()
    for (single_soa in value) {
      total_subjects <- img_soa_total_subjects[img_id == imgid & soa == single_soa]$total_subjects[1]
      total_words <- img_soa_total_non_unique_words[img_id == imgid & soa == single_soa]$total_words[1]
      total_unique_words <- img_soa_total_unique_words[img_id == imgid & soa == single_soa]$total_unique_words[1]
      #soa_labels <- rbind(soa_labels, paste0(single_soa, " (N=", total_subjects, ",W=", total_unique_words, "/", total_words, ")"))
      soa_labels <- rbind(soa_labels, paste0(single_soa, " (N=", total_subjects, ")"))
    }
    return(soa_labels)
  }
  
  # p <- ggplot(melt_merge_img_soa_total_words[img_id == imgid], aes(x=as.numeric(factor(soa)), y=value, col=word_type)) +
  #   geom_point() +
  #   geom_line(stat = "identity") +
  #   ggtitleimg("Word count per SOA", imgid) +
  #   scale_x_discrete(name="SOA", labels=c("67", "133", "267", "Unlimited")) +
  #   scale_y_continuous(name = "Word count") +
  #   theme(legend.title = element_blank(), legend.position = "none")
  # 
  # x_range <- ggplot_build(p)$layout$panel_params[[1]]$x.range
  # y_range <- ggplot_build(p)$layout$panel_params[[1]]$y.range
  # 
  # image <- image_scale(get_image_by_id(nishimoto_images_folder, imgid), "200")
  
  # p <- p + annotation_custom(rasterGrob(image, interpolate = FALSE), 
  #                       xmin = x_range[2], xmax = x_range[2] + 2, ymin = y_range[2] - 10, ymax = y_range[2])  +
  #   coord_cartesian(clip = 'off') +
  #   theme(plot.margin = unit(c(1,3,1,1), "lines"))
  # ggsave(paste0("word_count_per_soa_img_", imgid, '.png'), p)
  
  
  ## Word variability 
  
  p <- ggplot(merge_img_soa_variability[img_id == imgid], aes(x=soa, y=variability, group=1)) +
    geom_point() +
    stat_summary(geom = 'line') +
    ggtitleimg("Word variability per SOA", imgid) +
    scale_x_discrete(name="Presentation Duration", labels=c("67", "133", "267")) +
    scale_y_continuous(name = "Variability of Reported Words") +
    theme(legend.title = element_blank(), legend.position = "none", axis.title = element_text(size=22), axis.text = element_text(size=20))

  x_range <- ggplot_build(p)$layout$panel_params[[1]]$x.range
  y_range <- ggplot_build(p)$layout$panel_params[[1]]$y.range

  image <- image_scale(get_image_by_id(nishimoto_images_folder, imgid), "200")

  p <- p + annotation_custom(rasterGrob(image, interpolate = FALSE),
                        xmin = x_range[2], xmax = x_range[2] + 2, ymin = y_range[2] - 10, ymax = y_range[2])  +
    coord_cartesian(clip = 'off') +
    theme(plot.margin = unit(c(1,3,1,1), "lines"))
  ggsave(paste0("word_variability_per_soa_img_", imgid, '.png'), p)
  
  
  ## Word variability all
  
  t <- lmer(variability ~ 1 + soa + (1 | img_id), data = merge_img_soa_variability)
  print(summary(t))
  
  d.residuals <- data.table(
    Yhat = fitted(t),
    Residuals = resid(t))
  
  print(ggplot(d.residuals, aes(Residuals)) +
          geom_histogram())
  
  print(ggplot(d.residuals, aes(sample = Residuals)) +
          stat_qq() + stat_qq_line())
  
  print(ggplot(d.residuals, aes(Yhat, Residuals)) +
          geom_point(alpha = .2))
  
  print(confint(t, method = "profile"))
  
  library(lmerTest)
  print(anova(t))
  
  posthoc <- glht(t, linfct = mcp(soa = "Tukey"))
  print(summary(posthoc))
  
  dodge <- position_dodge(width = 0.8)
  
  anno <- data.frame(x1 = c(1.0, 1.0, 2.0), x2 = c(2, 3, 3), 
                     y1 = c(1.65, 4.6, 1.5), y2 = c(1.65, 4.6, 1.5), 
                     xstar = c(1.5, 2, 2.5), ystar = c(1.70, 4.65, 1.6),
                     lab = c("***", "***", "**"))
  
  p_word_var <- ggplot(merge_img_soa_variability, aes(x = soa, y = variability)) +
    geom_violin(position = dodge) +
    geom_boxplot(width=0.3, position = dodge, show.legend = FALSE) +
    # stat_summary(fun.y = mean, geom="point", shape=23, size=2) +
    # stat_summary(fun.y = median, geom="point", size=2, color="red") +
    #geom_jitter(width = 0.1, size = 1) + 
    #geom_point(position = position_jitter(width = 0.1)) +
    labs(y=paste("Variability of Reported Words")) +
    #ggtitle(paste0(title)) +
    ggtitle(paste0("Total Number of Images=", length(unique(merge_img_soa_variability$img_id)))) +
    scale_x_discrete(name="Presentation Duration (ms)", labels=c("67", "133", "267")) +
    theme(legend.title = element_blank(), legend.position = "none") +
    geom_text(data = anno, aes(x = xstar,  y = ystar, label = lab)) +
    geom_segment(data = anno, aes(x = x1, xend = x1,
                                  y = y1, yend = y2),
                 colour = "black") +
    geom_segment(data = anno, aes(x = x2, xend = x2,
                                  y = y1, yend = y2),
                 colour = "black") +
    geom_segment(data = anno, aes(x = x1, xend = x2,
                                  y = y2, yend = y2),
                 colour = "black") +
    theme_cowplot() + 
    theme(legend.text = element_text(size=16), axis.text = element_text(size = 22),
          axis.title=element_text(size=22,face="bold"), plot.title = element_text(size = 20))

    h <- 7
    ar <- 1.5
    ggsave(p_word_var, height=h, width= h*ar, filename = "word_variability_all_images.png")

    
    ## TEMP START
    
    # p_temp <- ggplot(merge_img_soa_variability, aes(x = soa, y = variability)) +
    #   stat_summary(aes(y=variability, group = 1), fun.y=mean, colour="red", geom="line", size = 2) +
    #   stat_summary(fun.y=mean, colour="black", geom="point") +
    #   #geom_boxplot(width=0.3, position = dodge, show.legend = FALSE) +
    #   # stat_summary(fun.y = mean, geom="point", shape=23, size=2) +
    #   # stat_summary(fun.y = median, geom="point", size=2, color="red") +
    #   #geom_jitter(width = 0.1, size = 1) + 
    #   #geom_point(position = position_jitter(width = 0.1)) +
    #   labs(y=paste("Variability of Reported Words")) +
    #   #ggtitle(paste0(title)) +
    #   ggtitle(paste0("N=", length(unique(merge_img_soa_variability$img_id)))) +
    #   scale_x_discrete(name="Presentation Duration (ms)", labels=c("67", "133", "267")) +
    #   theme(legend.title = element_blank(), legend.position = "none")
    #   # geom_text(data = anno, aes(x = xstar,  y = ystar, label = lab)) +
    #   # geom_segment(data = anno, aes(x = x1, xend = x1,
    #   #                               y = y1, yend = y2),
    #   #              colour = "black") +
    #   # geom_segment(data = anno, aes(x = x2, xend = x2,
    #   #                               y = y1, yend = y2),
    #   #              colour = "black") +
    #   # geom_segment(data = anno, aes(x = x1, xend = x2,
    #   #                               y = y2, yend = y2),
    #   #              colour = "black")
    # # theme(theme_cowplot())
    
    ## TEMP END
    
  ## Plot: Word frequency by SOA (Individual Image)
  
  word_freq_factor <- reorder(dfi$word, dfi$frequency, sum)
  word_freq_factor_df <- data.table(word = as.vector(names(attr(word_freq_factor, "scores"))), frequency = as.vector(unname(attr(word_freq_factor, "scores"))))
  word_freq_factor_df <- word_freq_factor_df[frequency > ignore_count_threshold]
  words_above_threshold <- word_freq_factor_df$word

  word_freq_df <- copy(dfi)
  word_freq_df <- word_freq_df[word %in% words_above_threshold]

  # p1 <- ggplot(word_freq_df) + 
  #   geom_histogram(stat = "identity") +
  #   aes(x=reorder(word_freq_df$word, word_freq_df$frequency, sum), y=1, col=soa, fill = soa) +
  #   coord_flip() +
  #   ggtitleimg("Word frequency distribution by SOA", imgid) +
  #   xlab("Word") + 
  #   ylab("Count") +
  #   theme(axis.text = element_text(size = 6)) +
  #   facet_grid(. ~ soa, labeller = soa_labeller) 
  # 
  # max_count <- max(word_freq_df[, sum(frequency), by = c("soa", "word")]$V1)
  # p1 <- p1 + scale_y_continuous(breaks = seq(0, max_count, by=2))
  # gt <- add_image_to_plot(p1, image)
  # ggsave(paste0("ind_word_freq_dist_per_soa_img_", imgid, '.png'), gt)

  ## Plot: Word proportion by SOA (Individual Image)
  # word_prop_df <- copy(dfi)
  # word_prop_df <- word_prop_df[word %in% words_above_threshold]
  # word_prop_df <- word_prop_df[, .(soa_single_word_freq = sum(frequency)), by = c("img_id", "soa", "word")]
  # word_prop_df <- word_prop_df[, soa_total_words := sum(soa_single_word_freq), by = c("img_id", "soa")]
  # word_prop_df <- word_prop_df[, .(word_soa_proportion = soa_single_word_freq/soa_total_words), by = c("img_id", "soa", "word")]
  # 
  # p2 <- ggplot(word_prop_df) + 
  #   geom_histogram(stat = "identity") +
  #   aes(x=reorder(word_prop_df$word, word_prop_df$word_soa_proportion, sum), y=word_soa_proportion, col=soa, fill = soa) +
  #   coord_flip() +
  #   ggtitleimg("Word proportion distribution by Presentation Duration", imgid) +
  #   xlab("Word") + 
  #   ylab("Proportion") +
  #   theme(axis.text = element_text(size = 6)) +
  #   facet_grid(. ~ soa, labeller = soa_labeller) 
  # 
  # max_count <- max(word_prop_df[, word_soa_proportion, by = c("soa", "word")]$word_soa_proportion)
  # p2 <- p2 + scale_y_continuous(breaks = seq(0, max_count, by=0.05)) +
  #     theme(axis.text.x = element_text(size = 5))
  # gt <- add_image_to_plot(p2, image)
  # ggsave(paste0("ind_word_proportion_dist_per_soa_img_", imgid, '.png'), gt)
  
  ## Plot: Word proportion weighted with mean confidence by SOA (Individual Image)
  # word_prop_conf_df <- copy(dfi)
  # word_prop_conf_df <- word_prop_conf_df[word %in% words_above_threshold]
  # 
  # ### Unlimited SOA has no confidence [ASSUMPTION]
  # word_prop_conf_df[is.na(confidence)]$confidence <- 1
  # word_prop_conf_df <- word_prop_conf_df[, .(soa_single_word_freq = sum(frequency), 
  #                                            soa_single_word_confidence = mean(confidence)
  # ), 
  # by = c("img_id", "soa", "word")]
  # word_prop_conf_df <- word_prop_conf_df[, soa_total_words := sum(soa_single_word_freq), by = c("img_id", "soa")]
  # word_prop_conf_df <- word_prop_conf_df[, soa_weighted_proportion := (soa_single_word_freq/soa_total_words)*soa_single_word_confidence, by = c("img_id", "soa", "word")]
  # word_prop_conf_df <- word_prop_conf_df[, soa_total_weighted_proportion := sum(soa_weighted_proportion), by = c("img_id", "soa")]
  # word_prop_conf_df <- word_prop_conf_df[, .(norm_soa_weighted_proportion = (soa_weighted_proportion/soa_total_weighted_proportion)), by = c("img_id", "soa", "word")]
  # 
  # p3 <- ggplot(word_prop_conf_df) + 
  #   geom_histogram(stat = "identity") +
  #   aes(x=reorder(word_prop_conf_df$word, word_prop_conf_df$norm_soa_weighted_proportion, sum), y=norm_soa_weighted_proportion, col=soa, fill = soa) +
  #   coord_flip() +
  #   ggtitleimg("Word confidence-weighted proportion distribution by Presentation Duration", imgid) +
  #   xlab("Word") + 
  #   ylab("Proportion") +
  #   theme(axis.text = element_text(size = 6)) +
  #   facet_grid(. ~ soa, labeller = soa_labeller) 
  # 
  # max_count <- max(word_prop_conf_df[, norm_soa_weighted_proportion, by = c("soa", "word")]$norm_soa_weighted_proportion)
  # p3 <- p3 + scale_y_continuous(breaks = seq(0, max_count, by=0.05)) +
  #     theme(axis.text.x = element_text(size = 5))
  # gt <- add_image_to_plot(p3, image)
  # ggsave(paste0("ind_word_weighted_proportion_dist_per_soa_img_", imgid, '.png'), gt)
  
  
  ## Plot: Reportable proportion (by subjects) by SOA (Individual Image)
  subject_prop_df <- copy(dfi)
  subject_prop_img_soa_df <- subject_prop_df[, .(soa_total_sbj_img_tested = length(unique(subject))), by = c("img_id", "soa")]

  subject_prop_df <- subject_prop_df[word %in% words_above_threshold]

  # Assumption is each participant can only report the same word once.
  # Validation check below to ensure each participant only report a word once per image per SOA.
  test_more_than_one_word_per_subject_df <- subject_prop_df[, .(soa_subject_single_word_freq = sum(frequency)), by = c("img_id", "soa", "word", 'subject')][soa_subject_single_word_freq > 1]
  if (nrow(test_more_than_one_word_per_subject_df) > 0) {
    browser()
    #stopifnot(nrow(test_more_than_one_word_per_subject_df) == 0)
  }

  # Validation test above ensure that one word can only be specified by one subject once.
  # Safe to assume the grouping below is counting the subject.
  subject_prop_img_soa_word_df <- subject_prop_df[, .(soa_sbj_word_freq = length(unique(subject)), soa_single_word_confidence = mean(confidence)), by = c("img_id", "soa", "word")]
  subject_prop_img_soa_word_df <- merge(subject_prop_img_soa_word_df, subject_prop_img_soa_df, all.x = TRUE, by = c("img_id", "soa"))
  subject_prop_df <- subject_prop_img_soa_word_df[, .(word_soa_proportion = soa_sbj_word_freq/soa_total_sbj_img_tested), by = c("img_id", "soa", "word")]
  subject_prop_confidence_df <- merge(subject_prop_df, subject_prop_img_soa_word_df, by=c("img_id", "soa", "word"), all.x = TRUE)
  
  p4 <- ggplot(subject_prop_df) +
  geom_histogram(stat = "identity") +
  aes(x=reorder(subject_prop_df$word, subject_prop_df$word_soa_proportion, sum), y=word_soa_proportion, col=soa, fill = soa) +
  coord_flip() +
  #ggtitleimg("Proportion of report by Presentation Duration", imgid) +
  #ggtitle(paste0("Image ", imgid)) +
  xlab("Word") +
  ylab("Proportion of participants who reported the word") +
  theme(axis.text = element_text(size = 6)) +
  theme(legend.title = element_blank(), legend.position = "none") +
  facet_grid(. ~ soa, labeller = soa_labeller)
  #facet_grid(. ~ soa)

  max_count <- max(subject_prop_df[, word_soa_proportion, by = c("soa", "word")]$word_soa_proportion)
  p4 <- p4 +
    #scale_y_continuous(breaks = seq(0, max_count, by=10)) +
    theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 12))
  gt <- add_image_to_plot(p4, image)
  ggsave(paste0("ind_sbj_proportion_dist_per_soa_img_", imgid, '.png'), gt)
  

  p4_1 <- ggplot(subject_prop_confidence_df) +
    geom_histogram(stat = "identity") +
    aes(x=reorder(subject_prop_confidence_df$word, subject_prop_confidence_df$word_soa_proportion, sum), y=soa_single_word_confidence, col=soa, fill = soa) +
    coord_flip() +
    #ggtitleimg("Proportion of report by Presentation Duration", imgid) +
    #ggtitle(paste0("Image ", imgid)) +
    xlab("Word") +
    ylab("Mean confidence ratings") +
    theme(axis.text = element_text(size = 6)) +
    theme(legend.title = element_blank(), legend.position = "none") +
    facet_grid(. ~ soa, labeller = soa_labeller)
  #facet_grid(. ~ soa)
  
  max_count <- max(subject_prop_confidence_df[, word_soa_proportion, by = c("soa", "word")]$word_soa_proportion)
  p4_1 <- p4_1 +
    #scale_y_continuous(breaks = seq(0, max_count, by=10)) +
    theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 12))
  gt <- add_image_to_plot(p4_1, image)
  ggsave(paste0("ind_sbj_confidence_dist_per_soa_img_", imgid, '.png'), gt)
  
  
  # p4_1 <- ggplot(subject_prop_img_soa_word_df) +
  #   geom_histogram(stat = "identity") +
  #   aes(x=reorder(subject_prop_img_soa_word_df$word, subject_prop_img_soa_word_df$word_soa_proportion, sum), y=soa_single_word_confidence, col=soa, fill = soa) +
  #   coord_flip() +
  #   #ggtitleimg("Proportion of report by Presentation Duration", imgid) +
  #   #ggtitle(paste0("Image ", imgid)) +
  #   xlab("Word") +
  #   ylab("Mean confidence ratings") +
  #   theme(axis.text = element_text(size = 6)) +
  #   theme(legend.title = element_blank(), legend.position = "none") +
  #   facet_grid(. ~ soa, labeller = soa_labeller)
  # #facet_grid(. ~ soa)
  # 
  # max_count <- max(subject_prop_img_soa_word_df[, word_soa_proportion, by = c("soa", "word")]$word_soa_proportion)
  # p4 <- p4 +
  #   #scale_y_continuous(breaks = seq(0, max_count, by=10)) +
  #   theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 12))
  # gt <- add_image_to_plot(p4, image)
  # ggsave(paste0("ind_sbj_mean_confidence_dist_per_soa_img_", imgid, '.png'), gt)
  
  
  ## Plot confidence-weighted subject proportion
  # subject_prop_df <- subject_prop_img_soa_word_df[, .(word_soa_proportion = (soa_sbj_word_freq/soa_total_sbj_img_tested) * (soa_single_word_confidence/4)), by = c("img_id", "soa", "word")]
  # 
  # p5 <- ggplot(subject_prop_df) + 
  #   geom_histogram(stat = "identity") +
  #   aes(x=reorder(subject_prop_df$word, subject_prop_df$word_soa_proportion, sum), y=word_soa_proportion, col=soa, fill = soa) +
  #   coord_flip() +
  #   ggtitleimg("Proportion of report (confidence-weighted) by Presentation Duration", imgid) +
  #   xlab("Word") + 
  #   ylab("Proportion of participants who report the word (confidence-weighted)") +
  #   theme(axis.text = element_text(size = 6)) +
  #   facet_grid(. ~ soa, labeller = soa_labeller) 
  # 
  # max_count <- max(subject_prop_df[, word_soa_proportion, by = c("soa", "word")]$word_soa_proportion)
  # p5 <- p5 + 
  #   #scale_y_continuous(breaks = seq(0, max_count, by=10)) +
  #   theme(axis.text.x = element_text(size = 5))
  # gt <- add_image_to_plot(p5, image)
  # ggsave(paste0("ind_sbj_proportion_weighted_dist_per_soa_img_", imgid, '.png'), gt)
  
  ## Calculate the KL divergence (with all words (not filtered with threshold))
  # subject_prop_df <- copy(dfi)
  # subject_prop_img_soa_df <- subject_prop_df[, .(soa_total_sbj_img_tested = length(unique(subject))), by = c("img_id", "soa")]
  # 
  # subject_prop_img_soa_word_df <- subject_prop_df[, .(soa_sbj_word_freq = length(unique(subject)), soa_single_word_confidence = mean(confidence)), by = c("img_id", "soa", "word")]
  # subject_prop_img_soa_word_df <- merge(subject_prop_img_soa_word_df, subject_prop_img_soa_df, all.x = TRUE, by = c("img_id", "soa"))
  # subject_prop_df <- subject_prop_img_soa_word_df[, .(word_soa_proportion = soa_sbj_word_freq/soa_total_sbj_img_tested), by = c("img_id", "soa", "word")]
  # 
  # word_prop_df <- copy(dfi)
  # word_prop_df <- word_prop_df[, .(soa_single_word_freq = sum(frequency)), by = c("img_id", "soa", "word")]
  # word_prop_df <- word_prop_df[, soa_total_words := sum(soa_single_word_freq), by = c("img_id", "soa")]
  # word_prop_df <- word_prop_df[, .(word_soa_proportion = soa_single_word_freq/soa_total_words), by = c("img_id", "soa", "word")]
  # 
  # kl_67_133 <- compute_kl(NULL, word_prop_df, c("67", "133"), "word", "word_soa_proportion", print_plot = TRUE, plot_filename = "67-133.png")
  # kl_133_267 <- compute_kl(NULL, word_prop_df, c("133", "267"), "word", "word_soa_proportion",  print_plot = TRUE, plot_filename = "133-267.png")
  # kl_67_267 <- compute_kl(NULL, word_prop_df, c("67", "267"), "word", "word_soa_proportion", print_plot = TRUE, plot_filename = "67-267.png")
  # 
  # print(kl_67_133)
  # print(kl_133_267)
  # print(kl_67_267)
}


add_image_to_plot <- function(plt, img) {
  q1 <- ggplot() + annotation_custom(rasterGrob(img, interpolate = FALSE))
  
  p2 <- ggplotGrob(plt)
  q2 <- ggplotGrob(q1)
  
  gt <- gtable(widths = unit(c(1, 1, .4), "null"), heights = unit(c(.8, 1, 1), "null"))
  gt <- gtable_add_grob(gt, p2, t = 1, b = 3, l = 1, r = 3)
  gt <- gtable_add_grob(gt, q2, t = 3, l = 3)

  return(gt)
}