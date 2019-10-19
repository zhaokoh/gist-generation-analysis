setwd("~/Dropbox/Education/Honours/PSY4100/project/analysis/batch1")

source('embed_import_libraries.R')
source('function-plot_neighbors_custom.R')
library("LSAfun")
library("misc3d")

load('lsa.RData')

doc_67 <-as.textmatrix(lsa_67$dk)
doc_133 <-as.textmatrix(lsa_133$dk)
doc_267 <-as.textmatrix(lsa_267$dk)

doc_id <- "1023"
output_width <- 1400
output_height <- 800

png(paste0("~/Downloads/img", doc_id, "_67.png"), width = output_width, height = output_height)
plot_neighbors_custom(doc_id, n=20, tvectors=doc_67, method="MDS", dims=2, replace_with_image = TRUE)
dev.off()

png(paste0("~/Downloads/img", doc_id, "_133.png"), width = output_width, height = output_height)
plot_neighbors_custom(doc_id, n=20, tvectors=doc_133, method="MDS", dims=2, replace_with_image = TRUE)
dev.off()

png(paste0("~/Downloads/img", doc_id, "_267.png"), width = output_width, height = output_height)
plot_neighbors_custom(doc_id, n=20, tvectors=doc_267, method="MDS", dims=2, replace_with_image = TRUE)
dev.off()

## Plot words within img

png(paste0("~/Downloads/img", doc_id, "_67_words.png"), width = output_width, height = output_height)
plot_wordlist(unique(df_67[img_id == doc_id]$stem_word), tvectors=lsm_67, cex = 1.4, dims = 2)
dev.off()

png(paste0("~/Downloads/img", doc_id, "_133_words.png"), width = output_width, height = output_height)
plot_wordlist(unique(df_133[img_id == doc_id]$stem_word), tvectors=lsm_133, cex = 1.4, dims = 2)
dev.off()

png(paste0("~/Downloads/img", doc_id, "_267_words.png"), width = output_width, height = output_height)
plot_wordlist(unique(df_267[img_id == doc_id]$stem_word), tvectors=lsm_627, cex = 1.4, dims = 2)
dev.off()

# top_5_words <- tail(df_67[img_id == doc_id, .N, by = "stem_word"][order(N)], 5)

# png(paste0("~/Downloads/img", doc_id, "_67_top_5_words.png"), width = output_width, height = output_height)
# plot_wordlist(paste(top_5_words$stem_word), tvectors=lsm_67, n=20, method = "MDS", dims=3, cex = 1.2, )
# dev.off()

closest_documents <- names(neighbors(doc_id, n=20, tvectors=doc_67))
all_closest_words <- unique(df_67[img_id %in% closest_documents, ]$stem_word)
#png(paste0("~/Downloads/img", doc_id, "_67_all_words.png"), width = output_width, height = output_height)
plot_wordlist(all_closest_words, tvectors=lsm_67, n=20, method = "MDS", dims=3, cex = 1.2)

# neighbors("human", n=20, tvectors=lsm)
# 
# distance("man", "girl", tvectors=lsm, breakdown=FALSE)
# 
# plot_wordlist(c("bowl"), tvectors=lsm, method="MDS", dims=2)
# 
# choose.target("man", lower=.8, upper=.9, n=20, tvectors=lsm)
# 
# genericSummary("cookies", k=1)
# 
# multidocs(x=c("3"), y=c("9"), tvectors=lsm)
