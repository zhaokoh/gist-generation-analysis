## @knitr function-load_subject_data
load_subject_data <- function(summary_file, details_file) {
  summary_df = fread(summary_file)
  summary_df <- subset(summary_df, values.completed == 1) # Only select completed participants
  
  details_df = fread(details_file)
  details_df = subset(details_df, subject %in% summary_df$script.subjectid)

  all_df <- data.table();

  all_subjects <- unique(summary_df$script.subjectid)
  for (i in all_subjects) {
    sbj_df <- details_df %>% filter(subject == i)
    one_row <- data.table(i, sex = get_sbj_attribute(sbj_df, "sex"), 
                          age = get_sbj_attribute(sbj_df, "age"), 
                          nationality = get_sbj_attribute(sbj_df, "nationality"),
                          first_language = get_sbj_attribute(sbj_df, "first_language"),
                          second_language = get_sbj_attribute(sbj_df, "second_language"),
                          number_of_years_english_speaking = get_sbj_attribute(sbj_df, "number_of_years_english_speaking"),
                          mturk_completion_code = get_sbj_attribute(sbj_df, "mturk_completion_code", "values.completion_code"))
    all_df <- rbind(all_df, one_row)
  }

  return(all_df)
}

get_sbj_attribute <- function(df, attribute_name, output_column_name = "response") {
  response <- df[df$trialcode == attribute_name, output_column_name]
  if (length(response) == 0) {
    response = NA
  } else if (response == "Prefer Not to Answer" | response == "" ) {
    response = NA
  }

  return(response)
}
