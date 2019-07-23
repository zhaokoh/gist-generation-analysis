source('embed_import_libraries.R')

df = fread("/Users/Zhao/Dropbox/Education/Honours/PSY4100/project/src/gist-generation/inquisit/batch_V1_imgset_1_2_lb_raw_all_2019-06-18-00.csv")

first_trial_df = df[startsWith(trialcode, "pic_")]

# Remove outlier

soa_diff =  data.table(as.numeric(first_trial_df$values.est_soa) - as.numeric(first_trial_df$values.soa))

summary(soa_diff)




ggplot(soa_diff, aes(x = V1)) + 
  geom_histogram() + 
  xlab("Difference in milliseconds")

ggsave()

  

