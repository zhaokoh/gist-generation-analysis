## @knitr function-save_plot_to_file

# A4 size
A4_HEIGHT = 210
A4_WIDTH = 297

save_plot_to_file <- function(plot_obj, filenm, h = A4_HEIGHT, w = A4_WIDTH) {
  ggsave(plot_obj, height=h, width= w, units="mm", filename = filenm)
}

get_custom_theme <- function() {
  my_theme <- theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line()) +
    theme(text = element_text(size=12), axis.text = element_text(size = 10)) +
    theme(plot.margin=grid::unit(c(5, 5, 5, 5), "mm"))
}