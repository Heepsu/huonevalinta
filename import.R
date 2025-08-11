library(data.table)
library(dplyr)
library(tidyr)
library(here)

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

data <- data %>%
  mutate(Object.Name = case_when(
    Object.Name == 'Rating Scale1' ~ 'Unpleasant-Pleasant',
    Object.Name == 'Rating Scale2' ~ 'Calm-Aroused',
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
    TRUE ~ Object.Name # This line keeps any other values in the column unchanged
  ))

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

# remove NA values 
idx <- data_questionnaire$Question == '' | is.na(data_questionnaire$Question) 
data_questionnaire <- data_questionnaire[!idx, ]

# convert to wide format 
data_wide <- data %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = row_id, values_from = Response)

data_questionnaire_wide <- data_questionnaire %>%
  pivot_wider(names_from = Question, values_from = Response)

merged_data <- left_join(data_wide, data_questionnaire_wide, by = "Participant.Public.ID")

