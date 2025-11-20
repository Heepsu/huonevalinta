# includes dplyr and ggplot2
library(tidyverse)

se <- function(x) {
  x_clean <- na.omit(x)
  if (length(x_clean) < 2) {
    return(NA) 
  }
  return(sd(x_clean) / sqrt(length(x_clean)))
}



# Calculate mean and standard error of the mean (SEM) for each Object.Name
mean_data <- data %>%
  group_by(Object.Name) %>%
  summarise(
    mean_response = mean(Response, na.rm = TRUE),
    std_err = se(Response),
    .groups = 'drop'
  )


