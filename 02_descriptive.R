# includes dplyr and ggplot2
library(tidyverse)

se <- function(x) {
  x_clean <- na.omit(x)
  if (length(x_clean) < 2) {
    return(NA) 
  }
  return(sd(x_clean) / sqrt(length(x_clean)))
}

data <- data %>%
  mutate(
    environment = substr(Choice, 1, 6)
  )

# Calculate mean and standard error of the mean (SEM) for each Object.Name
mean_data <- data %>%
  group_by(Object.Name, environment) %>%
  summarise(
    mean_response = mean(Response, na.rm = TRUE),
    std_err = se(Response),
    .groups = 'drop'
  )

write.csv(mean_data,
          file = "mean_data_huonevalinta.csv",
          row.names = FALSE)


#########
# PLOTS #
#########

target_emotions <- c("Anxiety", "Awe", "Excitement", "Fear", "Joy", "Liking")

mean_data_uni <- mean_data %>% 
  filter(`Object.Name` %in% target_emotions)

target_emotions2 <- c("Calm-Aroused", "Constricted Spacious", "Leave-Stay", "Relaxed-Tense", "Unpleasant-Pleasant", "Unsafe-Safe")

mean_data_bi <- mean_data %>% 
  filter(`Object.Name` %in% target_emotions2)

plot_mean_emotions <- ggplot(
  data = mean_data_bi, 
  aes(
    x = Object.Name,                 
    y = mean_response,              
    fill = environment           
  )
) +
  geom_bar(
    stat = "identity",           
    position = position_dodge(0.9) # Dodge/separate the bars for each environment
  ) +
  
  labs(
    title = "",
    x = "",
    y = "Mean Response",
    fill = "Environment"
  ) +
  
  theme_minimal() + 
  theme(
    # plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "bottom" 
  ) +
  
  
  scale_fill_brewer(palette = "Set2")

print(plot_mean_emotions)

############
# BOX PLOT # 
############

data_bi <- data %>% 
  filter(`Object.Name` %in% target_emotions2)

plot_raw_emotions_boxplot <- ggplot(
  data = data, 
  aes(
    x = Object.Name, 
    y = Response, 
    fill = environment 
  )
) +
  geom_boxplot(
    width = 0.8, 
    position = position_dodge(0.9) 
  ) +
  
  scale_y_continuous(
    limits = c(1, 9), 
    breaks = seq(1, 9, by = 1)
  ) +
  
  labs(
    title = "", 
    x = "",
    y = "Response", 
    fill = "Environment"
  ) +
  
  theme_minimal() + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1) # rotation of x-axis labels
  ) +
  
  scale_fill_brewer(palette = "Set2")

print(plot_raw_emotions_boxplot)

###

# mean response for each video
video_means <- data %>%
  group_by(Object.Name, environment, Choice) %>%
  summarize(mean_response = mean(Response, na.rm = TRUE), .groups = "drop")

video_means <- video_means %>% 
  filter(`Object.Name` %in% target_emotions2)

plot_raw_emotions_boxplot <- ggplot() +

  geom_boxplot(
    data = video_means,
    aes(x = Object.Name, y = mean_response, fill = environment),
    width = 0.8,
    position = position_dodge(0.9),
    outlier.shape = NA,
    alpha = 0.5
  ) +

  geom_jitter(
    data = video_means,
    aes(
      x = Object.Name, 
      y = mean_response, 
      fill = environment, 
      group = environment
    ),
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9),
    color = "black", 
    shape = 21,      
    size = 2,
    alpha = 0.8
  ) +
  scale_y_continuous(limits = c(1, 9), breaks = 1:9) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(plot_raw_emotions_boxplot)
