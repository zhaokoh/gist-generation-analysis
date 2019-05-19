# All Data Files
summary_file = '../../data/pilot/v0_all/gist_imgset1_summary_all.csv'
details_file = '../../data/pilot/v0_all/gist_imgset1_raw_all.csv'

alon_present_words = '../analysis-data/alon_image_present_words.csv'
alon_remove_words = '../analysis-data/removeWordsEXP5.csv'
alon_subject_descriptions = '../analysis-data/translatedEXP5.csv'


# This is the first lab participant experiments (4 blocks, 20 trials in each block)
included_v0_lab_participants = c(
  10, 13, 16
)

# This is the first mturk experiment (4 blocks, 20 trials in each block)
included_v0_mturk_participants = c(
  681418, 819055, 359390, 225362, 304875, 597984, 995980
)

# This version of MTurk experiment uses only 20 images so that we can get sufficient descriptors to analyse
# (1 block, 20 trials in each block)
included_v1_mturk_participants = c(
  450941,
  179958,
  688449,
  371597,
  738211,
  805538,
  886746,
  543099,
  613746,
  748872,
  #385008, # Foreign language
  109075,
  722648,
  651021,
  856836,
  674775,
  831725,
  449656,
  886090,
  178610,
  137135)

# This combines all the pilot participants
included_all_participants = c(included_v0_lab_participants, included_v0_mturk_participants, included_v1_mturk_participants)

# *** THIS decides the participants to be included in the analysis (in function-load_pilot_data)
default_included_participants <- c(included_v0_lab_participants, included_v1_mturk_participants)


nishimoto_images_folder = "/Volumes/Spaceship/Alon_Gist/nishimoto-images/"

large_word_cloud = 3
small_word_cound = 2

