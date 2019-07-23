## @knitr function-plot_kl_divergence
## Plot KL divergence (all images)
plot_kl_divergence <- function(in_dt, title, suffix = "", batch_no = "") {
  
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
  
  in_dt <- in_dt[, .(odd_even_group = (which(group_order == lapply(kl_type, as.character))) %% 2 == 0), by = c("img_id", "kl_type", "kl_value")]
  in_dt <- in_dt[, .(from_soa = gsub("kl_(.*)+_{1}.*", "\\1", kl_type),
                     to_soa = gsub("kl_(.*)_{1}(.*)", "\\2", kl_type)), 
                 by = c("img_id", "kl_type", "kl_value", "odd_even_group")]
  
  in_dt$kl_type <- factor(in_dt$kl_type, levels=group_order)
  in_dt$from_soa <- factor(in_dt$from_soa, levels=c("67", "133", "267", "Unlimited"))
  in_dt$to_soa <- factor(in_dt$to_soa, levels=c("67", "133", "267", "Unlimited"))
  
  p <- ggplot(in_dt, aes(x = kl_type, y = kl_value, col = odd_even_group)) +
    geom_boxplot(outlier.size = 0) +
    #geom_jitter(width = 0.1, size = 1) + 
    geom_point(position = position_jitter(width = 0.1)) +
    ylab("KL Divergence (log2)") +
    ggtitle(paste0(title, " - Pairwise")) +
    scale_x_discrete(name="Group", labels=group_label) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90), legend.title = element_blank(), legend.position = "none")
  print(p)
  
  ggsave(paste0(batch_number, "-analysis-pilot-pairwise-kl-divergence-", suffix, ".png"))
  
  ## Compare against Unlimited
  p <- ggplot(in_dt, aes(x = to_soa, y = kl_value, col = img_id)) +
    geom_boxplot() +
    ylab("KL Divergence (log2)") +
    ggtitle(title) +
    facet_grid(. ~ from_soa, scales = "free_x") + 
    theme_light() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  print(p)
  
  ggsave(paste0(batch_number, "-analysis-pilot-approx-kl-divergence-", suffix, ".png"))
}

