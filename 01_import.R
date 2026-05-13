#---------------------------------------------------------------------------#
# Imports and cleans experiment and questionnaire data for further analysis #
#---------------------------------------------------------------------------#

library(data.table)
library(tidyr)
library(here)
library(tidyverse)

##################################
# IMPORT & CLEAN EXPERIMENT DATA #
##################################

# --- NOTE: Data is assumed to exist in folder named "data" in the working directory. --- 
paths <- here("data", c(
  "data_exp_194853-v8_task-8ee3.csv",
  "data_exp_194853-v8_task-ebzj.csv",
  "data_exp_194853-v8_task-ervk.csv",
  "data_exp_194853-v8_task-fxyl.csv",
  "data_exp_194853-v8_task-wvcs.csv"
))

# Participant.Public.ID = participant id
# Response = Participant's answer on a scale from 1-9
# Tag = Measured emotion
datalist <- list()
for (file_path in paths) {
  data <- read.csv(file_path, header = TRUE)
  
  if ('Spreadsheet..video1' %in% names(data)) {
    names(data)[names(data) == 'Spreadsheet..video1'] <- 'videoset1'
  }
  if ('Spreadsheet..video2' %in% names(data)) {
    names(data)[names(data) == 'Spreadsheet..video2'] <- 'videoset2'
  }
  
  data <- data[, c('Participant.Public.ID', 'Object.Name' , 'Response', 'videoset1', 'videoset2')]
  datalist[[length(datalist) + 1]] <- data
}

# Bind all as one dataset
data <- do.call(rbind, datalist)
data <- as.data.table(data)

# Clean data by removing empty strings, NA values, strings 'BEGIN' and 'END'
idx <- data$Response == '' | is.na(data$Response) | data$Response == 'BEGIN' | data$Response == 'END'
data <- data[!idx, ]

# Match rating scales to correspond to measured emotion scales
data <- data %>%
  mutate(Object.Name = case_when(
    Object.Name == 'Rating Scale1' ~ 'Unpleasant-Pleasant',
    Object.Name == 'Rating Scale2' ~ 'Aroused-Calm',
    Object.Name == 'Rating Scale3' ~ 'Relaxed-Tense',
    Object.Name == 'Rating Scale4' ~ 'Constricted Spacious',
    Object.Name == 'Rating Scale5' ~ 'Unsafe-Safe',
    Object.Name == 'Rating Scale6' ~ 'Leave-Stay',
    Object.Name == 'Rating Scale7' ~ 'Liking',
    Object.Name == 'Rating Scale8' ~ 'Excitement',
    Object.Name == 'Rating Scale9' ~ 'Joy',
    Object.Name == 'Rating Scale10' ~ 'Anxiety',
    Object.Name == 'Rating Scale11' ~ 'Fear',
    Object.Name == 'Rating Scale12' ~ 'Awe',
    TRUE ~ Object.Name # Keeps any other values in the column unchanged
  ))

# Add new column to separate the video selection participant made and the emotion ratings 
data <- data %>%
  # Group by participant
  group_by(Participant.Public.ID) %>%
  
  # Create column 'Choice', if object name is 'Response' take the value from the column Response, otherwise NA
  mutate(Choice = ifelse(Object.Name == "Response", Response, NA)) %>%
  
  # Fill columns with missing (NA) values in column Choice with the name of the video participant has selected 
  fill(Choice, .direction = "down") %>%
  ungroup() %>%
  
  # Filter out rows where objects name is 'Response' so Response column will have only numeric values 
  filter(Object.Name != "Response")

data <- data %>%
  mutate(Response = as.numeric(Response))

###


######################
# TRIAL SUMMARY DATA #
######################

# TO-DO: tähän ei voi kyllä laittaa keskiarvoja tunteista, koska 1 rivi ei koske tiettyä videota
# mihin tarvitaan: halutaan tietää mikä on "voittajakategoria" (valittiin eniten) ja lasketaan jokaiselle videolle suosioindeksi,
# eli luku, joka kertoo kuinka monta kertaa kyseinen video on valittu. Selvitetään, ennustaako tunnevalinnat suosioindeksiä 

trial_summary <- data %>%
  group_by(videoset1, videoset2) %>%
  summarise(
    n_total = n(),
    
    # Percentage picking Video 1
    pct_picked_v1 = (sum(Choice == videoset1, na.rm = TRUE) / n()) * 100,
    
    # Percentage picking Video 2
    pct_picked_v2 = (sum(Choice == videoset2, na.rm = TRUE) / n()) * 100,
    
    .groups = 'drop'
  ) %>%
  left_join(
    data %>%
      group_by(videoset1, videoset2, Object.Name) %>%
      summarise(avg_rating = mean(Response, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = Object.Name, values_from = avg_rating),
    by = c("videoset1", "videoset2")
  )
                                   
print(trial_summary)
library(dplyr)
library(stringr)

trial_summary <- data %>%
  # 1. First, identify the category for the two videos being compared
  mutate(
    v1_category = case_when(
      str_detect(videoset1, "natpos") ~ "natpos",
      str_detect(videoset1, "natneg") ~ "natneg",
      str_detect(videoset1, "citpos") ~ "citpos",
      str_detect(videoset1, "citneg") ~ "citneg",
      TRUE ~ "other"
    ),
    v2_category = case_when(
      str_detect(videoset2, "natpos") ~ "natpos",
      str_detect(videoset2, "natneg") ~ "natneg",
      str_detect(videoset2, "citpos") ~ "citpos",
      str_detect(videoset2, "citneg") ~ "citneg",
      TRUE ~ "other"
    )
  ) %>%
  # 2. Group by the unique video pairs and their categories
  group_by(videoset1, v1_category, videoset2, v2_category) %>%
  summarise(
    n_total = n(),
    
    # Amount of people who picked each
    n_picked_v1 = sum(Choice == videoset1, na.rm = TRUE),
    n_picked_v2 = sum(Choice == videoset2, na.rm = TRUE),
    
    # Percentage picking each
    pct_picked_v1 = (n_picked_v1 / n_total) * 100,
    pct_picked_v2 = (n_picked_v2 / n_total) * 100,
    
    .groups = 'drop'
  )

print(trial_summary)

library(dplyr)
library(stringr)

# 1. Extract category from the Choice column for every row
category_counts <- data %>%
  mutate(
    chosen_category = case_when(
      str_detect(Choice, "natpos") ~ "natpos",
      str_detect(Choice, "natneg") ~ "natneg",
      str_detect(Choice, "citpos") ~ "citpos",
      str_detect(Choice, "citneg") ~ "citneg",
      TRUE ~ "other"
    )
  ) %>%
  # 2. Group by that category and count the picks
  group_by(chosen_category) %>%
  summarise(
    total_picks = n(),
    .groups = 'drop'
  ) %>%
  # 3. Calculate percentage of total picks
  mutate(
    pct_of_all_choices = (total_picks / sum(total_picks)) * 100
  ) %>%
  # 4. Sort to see the most picked at the top
  arrange(desc(total_picks))

print(category_counts)

library(dplyr)
library(stringr)
library(tidyr)

# 1. Simplify the data: One row per unique choice/trial
# This removes the extra rows created by the multiple emotion ratings
unique_choices <- data %>%
  select(Participant.Public.ID, videoset1, videoset2, Choice) %>%
  distinct() %>%
  mutate(
    v1_cat = case_when(
      str_detect(videoset1, "natpos") ~ "natpos",
      str_detect(videoset1, "natneg") ~ "natneg",
      str_detect(videoset1, "citpos") ~ "citpos",
      str_detect(videoset1, "citneg") ~ "citneg",
      TRUE ~ "other"
    ),
    v2_cat = case_when(
      str_detect(videoset2, "natpos") ~ "natpos",
      str_detect(videoset2, "natneg") ~ "natneg",
      str_detect(videoset2, "citpos") ~ "citpos",
      str_detect(videoset2, "citneg") ~ "citneg",
      TRUE ~ "other"
    ),
    picked_cat = case_when(
      str_detect(Choice, "natpos") ~ "natpos",
      str_detect(Choice, "natneg") ~ "natneg",
      str_detect(Choice, "citpos") ~ "citpos",
      str_detect(Choice, "citneg") ~ "citneg",
      TRUE ~ "other"
    ),
    # The category not picked is whichever one doesn't match the picked_cat
    not_picked_cat = if_else(picked_cat == v1_cat, v2_cat, v1_cat)
  )

# 2. Calculate Win Rate (%) per category
# (Total times picked / Total times that category was an available option)
win_rates <- unique_choices %>%
  pivot_longer(cols = c(v1_cat, v2_cat), values_to = "category_available") %>%
  group_by(category_available) %>%
  summarise(
    times_offered = n(),
    times_picked = sum(picked_cat == category_available, na.rm = TRUE),
    win_rate = (times_picked / times_offered) * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(win_rate))

# 3. Head-to-Head Matrix
# Shows exactly which category was sacrificed for another
comparison_matrix <- unique_choices %>%
  group_by(picked_cat, not_picked_cat) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = not_picked_cat, values_from = count, values_fill = 0)

print("--- Win Rates by Category (One row per unique choice) ---")
print(win_rates)

print("--- Comparison Matrix (Rows = Winner, Cols = Loser) ---")
print(comparison_matrix)

#####################################
# IMPORT & CLEAN QUESTIONNAIRE DATA #
#####################################

paths_questionnaire <- here("data", c(
  "data_exp_194853-v8_questionnaire-39o4.csv",                
  "data_exp_194853-v8_questionnaire-bvet.csv",
  "data_exp_194853-v8_questionnaire-ebow.csv",
  "data_exp_194853-v8_questionnaire-umjg.csv",
  "data_exp_194853-v8_questionnaire-w1tf.csv"
))

datalist_questionnaire <- list()
for (file_path in paths_questionnaire) {
  data_questionnaire <- read.csv(file_path, header = TRUE)
  
  data_questionnaire <- data_questionnaire[, c('Participant.Public.ID', 'Question', 'Response')]
  datalist_questionnaire[[length(datalist_questionnaire) + 1]] <- data_questionnaire
}

# Bind all as one dataset
data_questionnaire <- do.call(rbind, datalist_questionnaire)
data_questionnaire <- as.data.table(data_questionnaire)

# Clean data by removing empty strings and NA values
idx <- data_questionnaire$Question == '' | is.na(data_questionnaire$Question) 
data_questionnaire <- data_questionnaire[!idx, ]

# Remove duplicates based on Participant.Public.ID and Question
data_questionnaire <- unique(data_questionnaire, by = c('Participant.Public.ID', 'Question'))

# demographics 
unique_participants <- length(unique(data_questionnaire$Participant.Public.ID))
print(unique_participants)




