## @knitr function-compute_kl
library(philentropy)


compute_divergence <- function(x, algorithm) {
  kl_value <- NaN
  if (algorithm == "KL") {
    kl_value = round(KL(x, unit = "log2"), 4)
  } else if (algorithm == "JSD") {
    kl_value = round(JSD(x, unit = "log2"), 4)
  } else if (algorithm == "JS-DISTANCE") {
    kl_value = round(sqrt(JSD(x, unit = "log2")), 4)
  }
  
  return(kl_value)
}

compute_kl_by_dataset <- function(keys, img_dt, dataset_vector, word_colname, probability_colname, algorithm = "JS-DISTANCE", print_plot = TRUE) {
  # Assuming the same SOA (by caller)
  word_order <- unique(reorder(img_dt[[word_colname]], img_dt[[probability_colname]], sum))
  
  img_dt[[word_colname]] <- as.factor(img_dt[[word_colname]])
  
  d1 = dataset_vector[1]
  d2 = dataset_vector[2]
  
  d1_order_table <- data.table(cbind(d1, as.character(word_order)))
  colnames(d1_order_table) <- c("dataset", word_colname)
  d1_order_table[[word_colname]] <- as.factor(d1_order_table[[word_colname]])
  
  d2_order_table <- data.table(cbind(d2, as.character(word_order)))
  colnames(d2_order_table) <- c("dataset", word_colname)
  d2_order_table[[word_colname]] <- as.factor(d2_order_table[[word_colname]])
  
  d1_img_dt <- img_dt[dataset == d1]
  d2_img_dt <- img_dt[dataset == d2]
  word_cols <- img_dt[[word_colname]]
  
  # print(nrow(d1_img_dt))
  # print(nrow(d2_img_dt))
  # print(nrow(d1_order_table))
  # print(nrow(d2_order_table))
  # print("-------------")
  
  d1_list <- merge(d1_img_dt, d1_order_table, by=c("dataset", word_colname), all=TRUE)[order(match(word_cols, word_order))][!is.na(dataset),]
  d2_list <- merge(d2_img_dt, d2_order_table, by=c("dataset", word_colname), all=TRUE)[order(match(word_cols, word_order))][!is.na(dataset),]
  
  d1_list[which(is.na(d1_list[[probability_colname]])), probability_colname] <- 0
  d2_list[which(is.na(d2_list[[probability_colname]])), probability_colname] <- 0

  kl_1_2 <- compute_divergence(rbind(
    d1_list[[probability_colname]], 
    d2_list[[probability_colname]]), algorithm)
  
  if (print_plot) {
    t <- data.table(rbind(
      d1_list[[probability_colname]], 
      d2_list[[probability_colname]]))
    
    t <- cbind(t, data.table(title = c("1", "2")))
    colnames(t) <-  c(as.character(d1_list$agg_word), "title")
    plot_dt <- melt(t, id.vars = c("title"))
    plot_dt$variable <- as.character.factor(plot_dt$variable)

    s1<-ggplot(plot_dt[title == "1"]) +
      geom_histogram(aes(x = variable, y = value), stat = "identity", alpha=0.3, fill = "red") +
      #stat_smooth(aes(x = variable, y = value, group = title, color = title), se = FALSE) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(plot_dt$value))) + 
      xlab("") +
      ylab("") +
      ggtitle(paste0("KL: ", kl_1_2)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
    
    s2<-ggplot(plot_dt[title == "2"]) +
      geom_histogram(aes(x = variable, y = value), stat = "identity", alpha=0.3, fill = "green") +
      #stat_smooth(aes(x = variable, y = value, group = title, color = title), se = FALSE) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(plot_dt$value))) + 
      xlab("") +
      ylab("") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
    
    p_grid <- plot_grid(s1, s2, nrow = 2)
    
    h <- 7
    ar <- 1.5
    ggsave(p_grid, height=h, width= h*ar, filename = "test.png")
  }  
  
  return(kl_1_2)
}

compute_kl <- function(keys, img_dt, soa_vector, word_colname, probability_colname, algorithm = "JS-DISTANCE", print_plot = FALSE, plot_filename = "test.png") {
  # Assuming the same dataset (by caller)
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
  
  soa1_list <- merge(img_dt[soa == soa1], soa1_order_table, by=c("soa", word_colname), all=TRUE)[order(match(img_dt[[word_colname]], word_order))][!is.na(soa),]
  soa2_list <- merge(img_dt[soa == soa2], soa2_order_table, by=c("soa", word_colname), all=TRUE)[order(match(img_dt[[word_colname]], word_order))][!is.na(soa),]
  
  soa1_list[which(is.na(soa1_list[[probability_colname]])), probability_colname] <- 0
  soa2_list[which(is.na(soa2_list[[probability_colname]])), probability_colname] <- 0
  
  kl_1_2 <- compute_divergence(rbind(
    soa1_list[[probability_colname]], 
    soa2_list[[probability_colname]]), algorithm)
  
  if (print_plot) {
    t <- data.table(rbind(
      soa1_list[[probability_colname]], 
      soa2_list[[probability_colname]]))

    t <- cbind(t, data.table(title = c("1", "2")))
    colnames(t) <-  c(as.character(soa1_list[[word_colname]]), "title")
    
    plot_dt <- melt(t, id.vars = c("title"))
  
    color1 <- "red"
    color2 <- "green"

    if (soa1 == "67") {
      color1 <- "#F8766D"
    } else if (soa1 == "133") {
      color1 <- "#00BA37"
    } else if (soa1 == "267") {
      color1 <- "#619CFF"
    }
        
    if (soa2 == "67") {
      color2 <- "#F8766D"
    } else if (soa2 == "133") {
      color2 <- "#00BA37"
    } else if (soa2 == "267") {
      color2 <- "#619CFF"
    }
    
    s1<-ggplot(plot_dt[title == "1"]) + #F8766D #00BA37 #619CFF
      geom_histogram(aes(x = variable, y = value), stat = "identity", alpha=0.8, fill = color1) +
      #stat_smooth(aes(x = variable, y = value, group = title, color = title), se = FALSE) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(plot_dt$value))) + 
      xlab("") +
      ylab("") +
      ggtitle(paste0("KL: ", kl_1_2)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
      
    s2<-ggplot(plot_dt[title == "2"]) +
      geom_histogram(aes(x = variable, y = value), stat = "identity", alpha=0.8, fill = color2) +
      #stat_smooth(aes(x = variable, y = value, group = title, color = title), se = FALSE) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(plot_dt$value))) + 
      xlab("") +
      ylab("") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

    p_grid <- plot_grid(s1, s2, nrow = 2, labels = c(soa1, soa2))

    h <- 7
    ar <- 1.5
    ggsave(p_grid, height=h, width= h*ar, filename = plot_filename)
  }
  
  return(kl_1_2)
}
