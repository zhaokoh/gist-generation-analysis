## @knitr function-plot_image_result
## @knitr fig.show = 'asis'
plot_image_result <- function(summary_table, img) {
  comb_table <- summary_table[img_id == img]
  
  KL_Matrix <- plot_KL_divergence(comb_table, img)
  
  print_common_words(comb_table, img)
  
  return (KL_Matrix)
}

## @knitr function-plot_image_result_no_return
## @knitr fig.show = 'asis'
plot_image_result_no_return <- function(summary_table, img) {
  comb_table <- summary_table[img_id == img]
  
  plot_KL_divergence(comb_table, img)
  print_common_words(comb_table, img)
}