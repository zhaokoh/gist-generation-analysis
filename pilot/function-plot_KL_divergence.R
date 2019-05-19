## @knitr function-plot_KL_divergence
plot_KL_divergence <- function(comb_table, img) {
  comb_table$word <- iconv(comb_table$word, from = "UTF-8", to = "ASCII", sub = "")
  prob <- rbind(comb_table$nwp_67, comb_table$nwp_133, comb_table$nwp_267, comb_table$prob)
  prob[is.na(prob)] <- 0
  
  KLMatrix = matrix(, dim(prob)[1], dim(prob)[1], dimnames = list(c("67ms", "133ms", "267ms", "Unlimited"), c("67ms", "133ms", "267ms", "Unlimited")))
  
  for (row in c(1:nrow(KLMatrix))) {
    for (col in c(1:ncol(KLMatrix))) {
      KLMatrix[row, col] <- KL(rbind(prob[row,], prob[col,]), test.na = FALSE, unit="log2") # measure in bits
    }
  }
  
  # Swap row and col so the confusion matrix shows y as rows
  KLMatrix <- t(KLMatrix)
  
  KL_df <- as.data.table(melt(KLMatrix))
  p <- ggplot(KL_df, aes(x = Var1, y = Var2)) +
    ggtitle("KL Divergence Matrix for Image " + as.String(img)) +
    theme(axis.title = element_blank(), line = element_blank(), axis.text = element_text(face = "bold")) +
    geom_tile(aes(fill = value), colour = "steelblue") +
    scale_fill_gradient(low = "white", high="steelblue")
  
  im_fullpath = paste(nishimoto_images_folder, paste0("im", sprintf("%07d", img), ".jpg"), sep="");
  image_raw <- readJPEG(im_fullpath)
  image <- image_read(image_raw)
  
  img_p <- qplot(1:8, 1:8, geom="blank") +
    theme(axis.title = element_blank(), line = element_blank(),
          axis.ticks = element_blank(), axis.line = element_blank(),
          axis.text = element_blank()) +
    annotation_custom(rasterGrob(image_scale(image, "200"), interpolate = TRUE))
  
  figure <- ggarrange(p, img_p, ncol =1, nrow = 2, heights = c(0.7, 0.3))
  print(figure)
  
  return(KL_df)
}