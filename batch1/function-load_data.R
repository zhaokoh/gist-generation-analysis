## @knitr function-load_data
load_data <- function(summary_file, details_file, by_subject = FALSE) {
  summary_df = fread(summary_file, na.strings = "NULLX")
  summary_df <- subset(summary_df, values.completed == 1) # Only select completed participants
  
  details_df = fread(details_file, na.strings = "NULLX")
  details_df = subset(details_df, subject %in% summary_df$script.subjectid)

  all_df <- data.table();

  # Practice data
  practice_block_df <- details_df[blockcode=='practice_block' & response != 0,]
  if (nrow(practice_block_df) > 0) {
    practice.data <- practice_block_df %>%
      spread(trialcode, response) %>%
      .[, .(subject, blocknum, trialnum, latency, values.soa, values.img_file, 
            d1, rb1, d2, rb2, d3, rb3, d4, rb4, d5, rb5)] %>%
      arrange(subject, blocknum, trialnum, latency);
    practice.data <- data.table(practice.data)
    
    all_df <- construct_table_by_exp_data(practice.data, by_subject)
  }
  
  # Real data
  actual_block_df <- details_df[blockcode != 'practice_block' & grepl('^block[0-9]', blockcode) & response != 0,];
  
  exp.data <- actual_block_df %>%
    spread(trialcode, response) %>%
    .[, .(subject, blocknum, trialnum, latency, values.soa, values.img_file, 
          d1, rb1, d2, rb2, d3, rb3, d4, rb4, d5, rb5)] %>%
    arrange(subject, blocknum, trialnum, latency);
  
  exp.data <- data.table(exp.data)
  all_df <- rbind(all_df, construct_table_by_exp_data(exp.data, by_subject))

  
  # Here we merged in the group id that the subject belongs to (only if we retrieve the subject)
  if (by_subject) {
    subject_groupid <- data.table(subject = as.character(summary_df$script.subjectid), group = summary_df$script.groupid)
    all_df <- merge(all_df, subject_groupid, by = "subject", all = FALSE)
  
    subject_duplicate_words_df <- all_df[confidence != 0, .(.N), by=c("subject", "img_id", "soa", "word")][N > 1]
    if (nrow(subject_duplicate_words_df) > 0) {
      subject_duplicate_words_df
      browser()
      stopifnot(nrow(subject_duplicate_words_df) == 0)
    }
    
  }
  
  return(all_df)
}

## @knitr function-plot_word_cloud_for_imaged
plot_word_cloud_for_image <- function(details_df, img_id, soa) {
  
  # Real data
  actual_block_df <- details_df[
    (blockcode=='practice_block' & response != 0) | 
      (blockcode != 'practice_block' & grepl('^block[0-9]', blockcode) & response != 0),];
  
  exp.data <- actual_block_df %>%
    spread(trialcode, response) %>%
    .[, .(subject, blocknum, trialnum, values.soa, values.img_file, 
          d1, rb1, d2, rb2, d3, rb3, d4, rb4, d5, rb5)] %>%
    arrange(subject, blocknum, trialnum);
  
  exp.data <- data.table(exp.data)
  img_soa.data <- exp.data[values.img_file == img_id & values.soa == soa,]
  block_trial = unique(img_soa.data[,c('blocknum','trialnum')])
  number_of_data = length(unique(img_soa.data$subject))
  #word_list = plot_word_cloud(img_soa.data, paste("SOA = ", soa, "ms (N=", number_of_data, ")"), FALSE)
}

construct_table_by_exp_data <- function(experiment.data, by_subject = FALSE) {
  all_df = data.table()
  
  for (img in unique(experiment.data$values.img_file)) {
    
    # Uncomment the following line - useful to debug for particular image.
    # if (img != "im0000014.jpg") {
    #   next
    # }
    
    img.data <- experiment.data[values.img_file == img,];
    total_image_data <- nrow(unique(experiment.data[values.img_file == img,]))
    
    soa_list <- c();
    confidence_list <- c();
    entropy_list <- c();
    
    # im_fullpath = paste(nishimoto_images_folder, img, sep="");
    # image_raw <- readJPEG(im_fullpath)
    
    for (soa in sort(unique(experiment.data$values.soa))) {
      
      # browser(expr=soa == 267)
       
      confidence_soa_img <- c()
      img_soa.data <- experiment.data[values.img_file == img & values.soa == soa,]
      block_trial = unique(img_soa.data[,c('blocknum','trialnum')])
      number_of_data = nrow(block_trial);
      word_list = plot_word_cloud(img_soa.data, paste("SOA = ", soa, "ms (N=", number_of_data, ")"), small_word_cound, plot_word_cloud = FALSE, na_dont_know_words = FALSE)
      
      if (is.null(word_list) | number_of_data == 0) {
        next
      }
      
      # img_soa.data.long <- melt(img_soa.data, id.vars = c("subject", "blocknum","trialnum", "values.soa", "values.est_soa"),   
      #                           measure.vars = c("d1","d2","d3","d4","d5",
      #                                            "rb1","rb2","rb3","rb4","rb5"))
      img_soa.data.long <- melt(img_soa.data, id.vars = c("subject", "blocknum","trialnum", "latency", "values.soa"),   
                                measure.vars = c("d1","d2","d3","d4","d5",
                                                 "rb1","rb2","rb3","rb4","rb5"))
      
      
      # Code the confidence ratings (convert text to numbers)
      img_soa.data.long[which(startsWith(as.character(img_soa.data.long$variable), "rb") & img_soa.data.long$value == "Don't Know"),]$value <- "0"
      img_soa.data.long[which(startsWith(as.character(img_soa.data.long$variable), "rb") & img_soa.data.long$value == "Guess"),]$value <- "1"
      img_soa.data.long[which(startsWith(as.character(img_soa.data.long$variable), "rb") & img_soa.data.long$value == "Maybe"),]$value <- "2"
      img_soa.data.long[which(startsWith(as.character(img_soa.data.long$variable), "rb") & img_soa.data.long$value == "Confident"),]$value <- "3"
      img_soa.data.long[which(startsWith(as.character(img_soa.data.long$variable), "rb") & img_soa.data.long$value == "Very Confident"),]$value <- "4"
      
      response_values <- img_soa.data.long[grepl("^d[0-9]+$", variable)]$value
      # response_values[which(tolower(response_values) == "na")] <- "Nothing"
      
      dtmm <- create_document_term_matrix(response_values)
      
      # The length of the response values are not the same as TM-processed values. Check the values."      
      if (length(unique(tolower(response_values))) != length(colnames(dtmm))) {
        print(setdiff(unique(tolower(response_values)), colnames(dtmm)))
        browser()
        
        # Opportunity to debug
        create_document_term_matrix(response_values)
        stopifnot(length(unique(tolower(response_values))) == length(colnames(dtmm)))
      }

      ## Sanity check again - If response values size != dtmm size 
      ## This suggest the TM package is removing some words, need to investigate which word.

      for (a_word in word_list$word) {
        match_words_indices = as.vector(which(dtmm[, which(colnames(dtmm) == a_word)] > 0))
        word_conf <- img_soa.data.long[match_words_indices,.(subject, blocknum, trialnum, variable = gsub("d","rb", variable))]
        
        if (by_subject) {
          # One subject per row
          subject_confidence <- merge(img_soa.data.long, word_conf, by = c("subject", "blocknum", "trialnum", "variable"), all = FALSE)
          subject_confidence[, c("confidence") := as.numeric(value)]
          
          for (sbj in unique(subject_confidence$subject)) {
            one_row <- data.table(img, soa, subject = as.character(sbj), word = a_word, 
                                  frequency = 1, 
                                  confidence = subject_confidence[subject == sbj]$confidence,
                                  trialnum = subject_confidence[subject == sbj]$trialnum,
                                  latency = subject_confidence[subject == sbj]$latency)
            all_df <- rbind(all_df, one_row)
          }
        } else {
          # Combine participants/subjects
          mean_words_confidence_ratings <- mean(as.numeric(
            merge(img_soa.data.long, word_conf, by = c("subject", "blocknum", "trialnum", "variable"), all = FALSE)$value))
          
          if (is.nan(mean_words_confidence_ratings)) {
            mean_words_confidence_ratings = 0
          }
          
          one_row <- data.table(img, soa, word = a_word, frequency = word_list[word == a_word, c("freq")]$freq, confidence = mean_words_confidence_ratings)
          all_df <- rbind(all_df, one_row)
        }
      }
    }

    
  }
  

  if (length(all_df) != 0) {
    #all_df <- all_df[, img_id := as.integer(gsub("im[0]*([1-9][0-9]*).jpg", "\\1", img))]
    ## REMOVE ME
    all_df <- all_df[, img_id := img]
  }
  

  return(all_df)
}

create_document_term_matrix <- function (word_list) {
  docs <- preprocess_words_to_docs_minimal(word_list)
  dtm <- DocumentTermMatrix(docs, control=list(wordLengths=c(1,Inf)))
  return(as.matrix(dtm))
}
