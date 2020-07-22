## @knitr function-plot_kl_divergence
## Plot KL divergence (all images)
plot_kl_divergence <- function(in_dt_raw, title, suffix = "", batch_no = "") {
  
  # group_order <- c("kl_67_133", 
  #                  "kl_67_267", 
  #                  "kl_133_267", 
  #                  "kl_67_Unlimited", 
  #                  "kl_133_Unlimited",
  #                  "kl_267_Unlimited"
  # )
  # 
  # group_label <- c("67-133", "67-267", 
  #                  "133-267", "67-Unlimited", 
  #                  "133-Unlimited", "267-Unlimited")
  
  group_order <- c("kl_67_133", "kl_133_67",
                   "kl_67_267", "kl_267_67",
                   "kl_67_Unlimited", "kl_Unlimited_67",
                   "kl_133_267", "kl_267_133",
                   "kl_133_Unlimited", "kl_Unlimited_133",
                   "kl_267_Unlimited", "kl_Unlimited_267"
  )

  group_label <- c("67-133", "133-67", "67-267", "267-67",
                   "67-Unlimited", "Unlimited-67", "133-267", "267-133",
                   "133-Unlimited", "Unlimited-133", "267-Unlimited", "Unlimited-267")

  in_dt_raw <- in_dt_raw[, .(odd_even_group = (which(group_order == lapply(kl_type, as.character))) %% 2 == 0), by = c("img_id", "kl_type", "kl_value")]
  in_dt_raw <- in_dt_raw[, .(from_soa = gsub("kl_(.*)+_{1}.*", "\\1", kl_type),
                     to_soa = gsub("kl_(.*)_{1}(.*)", "\\2", kl_type)), 
                 by = c("img_id", "kl_type", "kl_value", "odd_even_group")]
  in_dt_raw <- in_dt_raw[, .(unlimited = grepl("Unlimited", kl_type, fixed=TRUE)), by = c("img_id", "kl_type", "kl_value", "odd_even_group", "from_soa", "to_soa")]
  
  in_dt_raw$kl_type <- factor(in_dt_raw$kl_type, levels=group_order)
  in_dt_raw$from_soa <- factor(in_dt_raw$from_soa, levels=c("67", "133", "267", "Unlimited"))
  in_dt_raw$to_soa <- factor(in_dt_raw$to_soa, levels=c("67", "133", "267", "Unlimited"))
  in_dt_raw$unlimited <- factor(in_dt_raw$unlimited)
  
  ## in_dt excludes certain images because shinji's set did not have it.
  ## This should contains 418 images
  in_dt <- in_dt_raw
  in_dt <- in_dt[kl_value > 0]
  in_dt <- in_dt[img_id < 9999000]
  
  shinji_imgs <- in_dt[unlimited == TRUE]$img_id
  all_imgs <- in_dt[unlimited != TRUE]$img_id
  
  setdiff(shinji_imgs, all_imgs)
  
  ## This should contains 423 images  
  in_dt_no_unlimited <- in_dt_raw[!grepl("Unlimited", kl_type), ]
  
  
  ## Plot the limited and unlimited SOAs distribution
  
  # print(ks.test(in_dt[unlimited == TRUE]$kl_value, in_dt[unlimited == FALSE]$kl_value))
  
  # print(bartlett.test(kl_value ~ unlimited, data = in_dt))
  # ttest <- t.test(kl_value ~ unlimited, data = in_dt, conf.level = 0.95)
  # print(ttest)
  
  # p <- ggplot(data.table(in_dt[unlimited==FALSE]), aes(x=kl_value)) + geom_histogram()
  # print(p)
  # print(ggplot(in_dt, aes(x=kl_value, colour = unlimited)) + 
  #         geom_density(size = 2) + 
  #         ylab("Density") +
  #         xlab(""))
  # print(qqPlot(in_dt[unlimited == TRUE]$kl_value, xlab="Quantiles", ylab="Unlimited Viewing"))
  # print(qqPlot(in_dt[unlimited == FALSE]$kl_value, xlab="Quantiles", ylab="67/133/267ms"))

  t <- lmer(kl_value ~ 1 + unlimited + (1 | img_id), data = in_dt)
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

  posthoc <- glht(t, linfct = mcp(unlimited = "Tukey"))
  print(summary(posthoc))

  dodge <- position_dodge(width = 0.8)
  library(ggsignif)
  
  anno <- data.frame(x1 = c(1.0), x2 = c(2), 
                     y1 = c(1.0), y2 = c(1.0), 
                     xstar = c(1.5), ystar = c(1.02),
                     lab = c("***"))

  p <- ggplot(in_dt, aes(x = unlimited, y = kl_value)) +
    geom_violin(position = dodge) +
    geom_boxplot(width=0.3, position = dodge, show.legend = FALSE) +
    # stat_summary(fun.y = mean, geom="point", shape=23, size=2) +
    # stat_summary(fun.y = median, geom="point", size=2, color="red") +
    #geom_jitter(width = 0.1, size = 1) + 
    #geom_point(position = position_jitter(width = 0.1)) +
    labs(y=expression(paste("Jensen-Shannon Distance"))) +
    xlab("Presentation Time") + 
    #ggtitle(paste0(title)) +
    ggtitle(paste0("N=", length(unique(in_dt$img_id)))) +
    scale_x_discrete(name="", labels=c("Limited", "Unlimited Viewing")) +
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
                 colour = "black")

  print(p)
  
  ggsave(paste0(batch_number, "-analysis-viewing-time-kl-divergence-", suffix, ".png"))
  
  dodge <- position_dodge(width = 0.8)
  library(ggsignif)
  
  p <- ggplot(in_dt, aes(x = kl_type, y = kl_value)) +
    geom_violin(position = dodge) +
    geom_boxplot(width=0.3, position = dodge, show.legend = FALSE) +
    stat_summary(fun.y = mean, geom="point", shape=23, size=2) +
    stat_summary(fun.y = median, geom="point", size=2, color="red") +
    #geom_jitter(width = 0.1, size = 1) + 
    #geom_point(position = position_jitter(width = 0.1)) +
    labs(y=expression(paste("KL Divergence (", log[2], ")"))) +
    xlab("Approximate SOA") + 
    ggtitle(paste0(title)) +
    scale_x_discrete(name="SOA Pair", labels=group_label) +
    theme(axis.text.x = element_text(angle = 90), legend.title = element_blank(), legend.position = "none")
  print(p)
  
  ggsave(paste0(batch_number, "-analysis-pilot-pairwise-kl-divergence-", suffix, ".png"))
  
  anno <- data.frame(x1 = c(0.9, 1.9, 0.9, 0.9, 1.9, 0.9, 0.9, 1.9, 0.9, 0.9, 1.9), 
                     x2 = c(3.1, 3.1, 2.1, 3.1, 3.1, 2.1, 3.1, 3.1, 2.1, 3.1, 3.1), 
                     y1 = c(12.8, 0.5, 11.2, 13, 0.5, 11.4, 13, 0.5, 11.6, 12.5, 5), 
                     y2 = c(12.8, 0.5, 11.2, 13, 0.5, 11.4, 13, 0.5, 11.6, 12.5, 5), 
                     xstar = c(2, 2.5, 1.5, 2, 2.5, 1.5, 2, 2.5, 1.5, 2, 2.5),
                     ystar = c(13, 0.1, 11.4, 13.2, 0.1, 11.6, 13.2, 0.1, 11.8, 12.7, 4.6),
                     lab = c("***", "***", "***", "***", "***", "***", "***", "***", "***", "***", "**"),
                     from_soa = c("67", "67", "133", "133", "133", "267", "267", "267", "Unlimited", "Unlimited", "Unlimited"))
  
  ## Compare against Unlimited
  p <- ggplot(data = in_dt, aes(x = to_soa, y = kl_value)) +
    geom_violin(position = dodge, aes(col = img_id)) +
    geom_boxplot(width=0.3, position = dodge, show.legend = FALSE, aes(col = img_id)) +
    stat_summary(fun.y = mean, geom="point", shape=23, size=2) +
    stat_summary(fun.y = median, geom="point", size=2, color="red") +
    labs(y=expression(paste("KL Divergence (", log[2], ")"))) +
    xlab("Approximate SOA") + 
    ggtitle(title) +
    scale_fill_brewer(palette="Spectral") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    # geom_text(data = anno, aes(x = xstar,  y = ystar, label = lab)) +
    # geom_segment(data = anno, aes(x = x1, xend = x1,
    #                               y = y1, yend = y2),
    #              colour = "black") +
    # geom_segment(data = anno, aes(x = x2, xend = x2,
    #                               y = y1, yend = y2),
    #              colour = "black") +
    # geom_segment(data = anno, aes(x = x1, xend = x2,
    #                               y = y2, yend = y2),
    #              colour = "black") +
    facet_grid(. ~ from_soa, scales = "free_x")
    
  print(p)
  
  ggsave(paste0(batch_number, "-analysis-pilot-approx-kl-divergence-", suffix, ".png"))
  
  
  anno <- data.frame(x1 = c(0.9, 0.9), 
                     x2 = c(2.1, 2.1), 
                     y1 = c(11.2, 11.4), 
                     y2 = c(11.2, 11.4), 
                     xstar = c(1.5, 1.5),
                     ystar = c(11.4, 11.6),
                     lab = c("***", "***"),
                     from_soa = c("133", "267"))
  
  ## Compare without unlimited
  p <- ggplot(data = in_dt_no_unlimited, aes(x = to_soa, y = kl_value)) +
    geom_violin(position = dodge, aes(col = img_id)) +
    stat_summary(fun.y = mean, geom="point", shape=23, size=2) +
    stat_summary(fun.y = median, geom="point", size=2, color="red") +
    geom_boxplot(width=0.3, position = dodge, show.legend = FALSE, aes(col = img_id)) +
    labs(y=expression(paste("KL Divergence (", log[2], ")"))) +
    xlab("Approximate Presentation Time") + 
    ggtitle(paste0("Images = ", length(unique(in_dt_no_unlimited$img_id)))) +
    scale_fill_brewer(palette="Spectral") +
    #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    # geom_text(data = anno, aes(x = xstar,  y = ystar, label = lab)) +
    # geom_segment(data = anno, aes(x = x1, xend = x1,
    #                               y = y1, yend = y2),
    #              colour = "black") +
    # geom_segment(data = anno, aes(x = x2, xend = x2,
    #                               y = y1, yend = y2),
    #              colour = "black") +
    # geom_segment(data = anno, aes(x = x1, xend = x2,
    #                               y = y2, yend = y2),
    #              colour = "black") +
    facet_grid(. ~ from_soa, scales = "free_x")
  
  print(p)
  
  ggsave(paste0(batch_number, "-analysis-pilot-approx-no-unlimited-kl-divergence-", suffix, ".png"))

  ## Compare without unlimited (only 67, 133, 267)

  anno <- data.frame(x1 = c(1.0, 1.0, 2.0), 
                     x2 = c(3, 2, 3), 
                     y1 = c(1, 0.48, 0.43), 
                     y2 = c(1, 0.48, 0.43), 
                     xstar = c(2, 1.5, 2.5),
                     ystar = c(1.02, 0.46, 0.41),
                     lab = c("***", "**", "***"),
                     kl_type = c("67", "133", "267"))

  in_dt_subset <- in_dt_no_unlimited[kl_type %in% c("kl_67_133", "kl_133_267", "kl_67_267")]
  p <- ggplot(data = in_dt_subset, aes(x = kl_type, y = kl_value)) +
    geom_violin(position = dodge, aes(col = img_id)) +
    geom_boxplot(width=0.3, position = dodge, show.legend = FALSE, aes(col = img_id)) +
    # stat_summary(fun.y = mean, geom="point", shape=23, size=2) +
    # stat_summary(fun.y = median, geom="point", size=2, color="red") +
    labs(y="Jensen-Shannon Distance", x="Presentation Duration Pair") +
    scale_x_discrete(labels = c("67ms & 133ms", "67ms & 267ms", "133ms & 267ms")) +
    #coord_cartesian(ylim=c(0.7, 0.8)) +
    ggtitle(paste0("Total Number of Images = ", length(unique(in_dt_no_unlimited$img_id)))) +
    scale_fill_brewer(palette="Spectral") +
    theme(axis.title = element_text(size = 16, face="bold"), text=element_text(size=16, face="bold"), plot.title = element_text(size = 14,face="bold")) +
    geom_text(data = anno, aes(x = xstar,  y = ystar, label = lab)) +
    geom_segment(data = anno, aes(x = x1, xend = x1,
                                  y = y1, yend = y2),
                 colour = "black") +
    geom_segment(data = anno, aes(x = x2, xend = x2,
                                  y = y1, yend = y2),
                 colour = "black") +
    geom_segment(data = anno, aes(x = x1, xend = x2,
                                  y = y2, yend = y2),
                 colour = "black")
  #facet_grid(. ~ from_soa, scales = "free_x")
  
  print(p)
  
  ggsave(paste0(batch_number, "-analysis-pilot-approx-no-unlimited-subset-kl-divergence-", suffix, ".png"))
  
  t <- lmer(kl_value ~ 1 + kl_type + (1 | img_id), data = in_dt_subset)
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

  posthoc <- glht(t, linfct = mcp(kl_type = "Tukey"))
  print(summary(posthoc))
}


