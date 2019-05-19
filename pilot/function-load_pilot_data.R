## @knitr function-load_pilot_data
load_pilot_data <- function(summary_file, details_file, included_participants = default_included_participants, by_subject = FALSE) {
  summary_df = fread(summary_file)
  summary_df = subset(summary_df, script.subjectid %in% included_participants)

  details_df = fread(details_file)
  details_df = subset(details_df, subject %in% included_participants)

  all_df <- data.table();

  # Practice data
  practice_block_df <- details_df[blockcode=='practice_block' & response != 0,];
  practice.data <- practice_block_df %>%
    spread(trialcode, response) %>%
    .[, .(subject, blocknum, trialnum, values.soa, values.img_file, 
          d1, rb1, d2, rb2, d3, rb3, d4, rb4, d5, rb5,
          d6, rb6, d7, rb7, d8, rb8, d9, rb9, d10, rb10)] %>%
    arrange(subject, blocknum, trialnum);
  practice.data <- data.table(practice.data)
  
  all_df <- construct_table_by_exp_data(practice.data, by_subject)
  
  # Real data
  actual_block_df <- details_df[blockcode != 'practice_block' & response != 0,];
  
  exp.data <- actual_block_df %>%
    spread(trialcode, response) %>%
    .[, .(subject, blocknum, trialnum, values.soa, values.img_file, 
          d1, rb1, d2, rb2, d3, rb3, d4, rb4, d5, rb5,
          d6, rb6, d7, rb7, d8, rb8, d9, rb9, d10, rb10)] %>%
    arrange(subject, blocknum, trialnum);
  exp.data <- data.table(exp.data)
  all_df <- rbind(all_df, construct_table_by_exp_data(exp.data, by_subject))
  
  return(all_df)
}

construct_table_by_exp_data <- function(experiment.data, by_subject = FALSE) {
  all_df = data.table()
  for (img in unique(experiment.data$values.img_file)) {
    
    # Uncomment the following line - useful to debug for particular image.
    # if (img != "im0001445.jpg") {
    #   next
    # }

    img.data <- experiment.data[values.img_file == img,];
    total_image_data <- nrow(unique(experiment.data[values.img_file == img,]))
    
    # This is to filter any row which has descriptor (after d6) but no confidence
    if (!is_empty(which(experiment.data$d6 != "" & experiment.data$rb6 == ""))) {
      experiment.data <-experiment.data[-(which(experiment.data$d6 != "" & experiment.data$rb6 == ""))]
    }
    
    if (!is_empty(which(experiment.data$d7 != "" & experiment.data$rb7 == ""))) {
      experiment.data <-experiment.data[-(which(experiment.data$d7 != "" & experiment.data$rb7 == ""))]
    }
    
    if (!is_empty(which(experiment.data$d8 != "" & experiment.data$rb8 == ""))) {
      experiment.data <-experiment.data[-(which(experiment.data$d8 != "" & experiment.data$rb8 == ""))]
    }
    
    if (!is_empty(which(experiment.data$d9 != "" & experiment.data$rb9 == ""))) {
      experiment.data <-experiment.data[-(which(experiment.data$d9 != "" & experiment.data$rb9 == ""))]
    }
    
    if (!is_empty(which(experiment.data$d10 != "" & experiment.data$rb10 == ""))) {
      experiment.data <-experiment.data[-(which(experiment.data$d10 != "" & experiment.data$rb10 == ""))]
    }
    
    soa_list <- c();
    confidence_list <- c();
    entropy_list <- c();
    
    # im_fullpath = paste(nishimoto_images_folder, img, sep="");
    # image_raw <- readJPEG(im_fullpath)
    
    for (soa in sort(unique(experiment.data$values.soa))) {
      confidence_soa_img <- c()
      img_soa.data <- experiment.data[values.img_file == img & values.soa == soa,];
      block_trial = unique(img_soa.data[,c('blocknum','trialnum')])
      number_of_data = nrow(block_trial);
      word_list = plot_word_cloud(img_soa.data, paste("SOA = ", soa, "ms (N=", number_of_data, ")"), small_word_cound, FALSE)
      
      if (is.null(word_list) | number_of_data == 0) {
        next
      }
      
      img_soa.data.long <- melt(img_soa.data, id.vars = c("subject", "blocknum","trialnum", "values.soa"),   
                                measure.vars = c("d1","d2","d3","d4","d5","d6","d7","d8","d9","d10",
                                                 "rb1","rb2","rb3","rb4","rb5","rb6","rb7","rb8","rb9","rb10"))
      
      # Code the confidence ratings (convert text to numbers)
      img_soa.data.long[which(startsWith(as.character(img_soa.data.long$variable), "rb") & img_soa.data.long$value == 'Guess'),]$value <- "1"
      img_soa.data.long[which(startsWith(as.character(img_soa.data.long$variable), "rb") & img_soa.data.long$value == 'Maybe'),]$value <- "2"
      img_soa.data.long[which(startsWith(as.character(img_soa.data.long$variable), "rb") & img_soa.data.long$value == 'Confident'),]$value <- "3"
      img_soa.data.long[which(startsWith(as.character(img_soa.data.long$variable), "rb") & img_soa.data.long$value == 'Very Confident'),]$value <- "4"
      
      response_values = img_soa.data.long$value
      dtmm <- create_document_term_matrix(response_values)
      
      for (a_word in word_list$word) {
        match_words_indices = as.vector(which(dtmm[, which(colnames(dtmm) == a_word)] > 0))
        word_conf <- img_soa.data.long[match_words_indices,.(subject, blocknum, trialnum, variable = gsub("d","rb", variable))]
        
        if (by_subject) {
          # One subject per row
          subject_confidence <- merge(img_soa.data.long, word_conf, by = c("subject", "blocknum", "trialnum", "variable"), all = FALSE)
          subject_confidence[, c("confidence") := as.numeric(value)]
          
          for (sbj in unique(subject_confidence$subject)) {
            one_row <- data.table(img, soa, subject = sbj, word = a_word, 
                                  frequency = 1, 
                                  confidence = subject_confidence[subject == sbj]$confidence)
            all_df <- rbind(all_df, one_row)
          }
        } else {
          # Combine participants/subjects
          mean_words_confidence_ratings <- mean(as.numeric(
            merge(img_soa.data.long, word_conf, by = c("subject", "blocknum", "trialnum", "variable"), all = FALSE)$value))
          
          one_row <- data.table(img, soa, word = a_word, frequency = word_list[word == a_word, c("frequency")]$frequency, confidence = mean_words_confidence_ratings)
          all_df <- rbind(all_df, one_row)
        }
      }
    }
  }
  
  if (length(all_df) != 0) {
    all_df <- all_df[, img_id := as.integer(gsub("im[0]+([1-9][0-9]*).jpg", "\\1", img))]
  }
  
  return(all_df)
}

create_document_term_matrix <- function (word_list) {
  docs <- preprocess_words_to_docs(word_list)
  dtm <- DocumentTermMatrix(docs)
  return(as.matrix(dtm))
}