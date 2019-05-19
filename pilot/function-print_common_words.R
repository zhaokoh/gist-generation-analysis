## @knitr function-print_common_words
print_common_words <- function(comb_table, img) {
  common_words_all_soas = comb_table[!is.na(p_67) & !is.na(p_133) & !is.na(p_267) & prob > 0]
  common_words_zhao_all_soas = comb_table[!is.na(p_67) & !is.na(p_133) & !is.na(p_267)]
  common_words_67_alon = comb_table[!is.na(p_67) & prob > 0]
  common_words_133_alon = comb_table[!is.na(p_133) & prob > 0]
  common_words_267_alon = comb_table[!is.na(p_267) & prob > 0]
  
  max_rows = max(
    dim(common_words_all_soas)[1], 
    dim(common_words_zhao_all_soas)[1], 
    dim(common_words_67_alon)[1], 
    dim(common_words_133_alon)[1], 
    dim(common_words_267_alon)[1]
  )
  
  words_table <- data.table(
    array(common_words_all_soas$word) %>% sort %>% append(array(NA, max_rows - length(common_words_all_soas$word))),
    array(common_words_zhao_all_soas$word)%>% sort %>% append(array(NA, max_rows - length(common_words_zhao_all_soas$word))),
    array(common_words_67_alon$word) %>% sort %>% append(array(NA, max_rows - length(common_words_67_alon$word))),
    array(common_words_133_alon$word) %>% sort %>% append(array(NA, max_rows - length(common_words_133_alon$word))),
    array(common_words_267_alon$word) %>% sort %>% append(array(NA, max_rows - length(common_words_267_alon$word)))
  )
  
  words_table[is.na(words_table)] <- "-"
  
  #setnames(words_table, c("all_soas_alon", "all_soas", "67_alon", "133_alon", "267_alon"))
  
  # words_table %>%
  #   kable(format = "latex", booktabs = T) %>%
  #   kable_styling(bootstrap_options = c("striped"), position = "left")
  
  setnames(words_table, c("all_soas_alon", "all_soas", "67_alon", "133_alon", "267_alon"))
  
  words_table %>%  
    kable(format = "latex", booktabs = T) %>%
    kable_styling(bootstrap_options = c("striped"))
  
}