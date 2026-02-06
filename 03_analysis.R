# includes dplyr and ggplot2
library(tidyverse)
library(rlang) 
library(lme4)
library(lmerTest)
library(emmeans)



#######
# LMM #
#######

# Set 'natpos' as the reference level
data$environment <- relevel(
  factor(data$environment),
  ref = "natpos"
)

print("Environment category levels set (natpos is the reference):")
print(levels(data$environment))

#' Runs the LMM for a specific emotion scale 
#' @param data The input dataframe (dataImgOpp).
#' @param emotion_scale_value The specific emotion subscale to filter for (e.g., 'Active-Passive').
#' @return NULL (Saves output to a .txt file and prints to console).
run_lmm_analysis <- function(data, emotion_scale_value) {
  
  # Filter the data to include only the specified response scale
  data_filtered <- data %>%
    filter(Object.Name == emotion_scale_value)
  
  # Check if filtering resulted in empty data
  if (nrow(data_filtered) == 0) {
    stop(paste("Filtering for emotion scale '", emotion_scale_value, "' resulted in zero observations. Check your data and scale value."))
  }

  lmm_model <- lmer(
    Response ~ environment + (1 | Choice) + (1 | Participant.Public.ID),
    data = data_filtered
  )
  
  output_dir <- "LMM"
  dir.create(output_dir, showWarnings = FALSE) # Create directory if it doesn't exist
  
  # Sanitize filename (replace non-alphanumeric characters with underscore)
  filename_base <- gsub("[^[:alnum:]]", "_", emotion_scale_value)
  # Update: Prepend the determined environment suffix
  output_filepath <- paste0(output_dir, "/", filename_base, "_lmm_summary.txt")
  
  output_text <- capture.output({
    print(paste0("=============================================="))
    print(paste0("LMM Analysis Results for Scale: ", emotion_scale_value))
    print(paste0("=============================================="))
    
    print("Summary of the LMM model:")
    print(summary(lmm_model))
    
    print("Model Coefficients (Fixed Effects):")
    # 1. Intercept: The estimated mean 'Response' for 'naturebeautiful'
    # 2. env_category[other]: The estimated difference in mean 'Response'
    print(fixef(lmm_model))
    
    print("--- Post-hoc Pairwise Comparisons (Tukey Adjustment) ---")

    em_results <- emmeans(lmm_model, specs = pairwise ~ environment)
    
 
    print("Estimated Marginal Means:")
    print(em_results$emmeans)
    
   
    print("Pairwise Contrasts:")
    print(em_results$contrasts)
  })
  

  writeLines(output_text, con = output_filepath)
  cat(paste(output_text, collapse = "\n"), "\n")
  
}


scales_to_analyze <- c(
  'Calm-Aroused', 'Fear', 'Constricted Spacious', 
  'Relaxed-Tense', 'Unpleasant-Pleasant', 'Joy', 
  'Leave-Stay', 'Anxiety', 'Liking', 'Excitement', 'Unsafe-Safe', 'Awe'
)

for (scale in scales_to_analyze) {
  cat("\n============================================\n")
  cat(paste(" LMM for scale:", scale))
  cat("\n============================================\n")
  
  run_lmm_analysis(data = data, emotion_scale_value = scale)
  
  cat("\n\nAnalysis complete for scale:", scale, "\n")
}

####

library(dplyr)
library(lme4)
library(emmeans)
library(ggplot2)
library(tidyr)

# --- 1. Modified Function to Return Results ---
run_lmm_analysis <- function(data, emotion_scale_value) {
  
  data_filtered <- data %>% filter(Object.Name == emotion_scale_value)
  
  if (nrow(data_filtered) == 0) return(NULL)
  
  lmm_model <- lmer(
    Response ~ environment + (1 | Choice) + (1 | Participant.Public.ID),
    data = data_filtered
  )
  
  # Get Pairwise Comparisons
  em_results <- emmeans(lmm_model, specs = pairwise ~ environment)
  
  # Convert contrasts to a dataframe and add the Scale name
  pairwise_df <- as.data.frame(em_results$contrasts) %>%
    mutate(Scale = emotion_scale_value)
  
  # (Optional) Keep your text-saving logic here if you still want the .txt files
  output_dir <- "LMM"
  dir.create(output_dir, showWarnings = FALSE)
  filename_base <- gsub("[^[:alnum:]]", "_", emotion_scale_value)
  output_filepath <- paste0(output_dir, "/", filename_base, "_lmm_summary.txt")
  capture.output(summary(lmm_model), em_results, file = output_filepath)
  
  return(pairwise_df)
}

# --- 2. Run Analysis and Collect Data ---
scales_to_analyze <- c(
  'Calm-Aroused', 'Fear', 'Constricted Spacious', 
  'Relaxed-Tense', 'Unpleasant-Pleasant', 'Joy', 
  'Leave-Stay', 'Anxiety', 'Liking', 'Excitement', 'Unsafe-Safe', 'Awe'
)

all_results_list <- list()

for (scale in scales_to_analyze) {
  message("Analyzing scale: ", scale)
  res <- run_lmm_analysis(data = data, emotion_scale_value = scale)
  if (!is.null(res)) {
    all_results_list[[scale]] <- res
  }
}

# Combine all scales into one master dataframe
final_plot_data <- bind_rows(all_results_list)

# --- 3. Forest Plotting (Applying your specific style) ---
plot_ready_data <- final_plot_data %>%
  mutate(
    conf.low = estimate - (1.96 * SE),
    conf.high = estimate + (1.96 * SE),
    contrast = gsub(" - ", " vs ", contrast)
  )

# Define clean facet names (edit as needed)
facet_names <- c(
  'Constricted Spacious' = "Constricted-Spacious",
  'Leave-Stay' = "Stay-Leave"
)

forest_plot <- ggplot(plot_ready_data, aes(x = estimate, y = contrast)) +
  # Reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Error bars
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0.3, color = "steelblue") +
  # Point estimates
  geom_point(size = 2, color = "darkblue") +
  # Faceting by Scale
  facet_wrap(~Scale, scales = "free_y", ncol = 3,
             labeller = labeller(Scale = as_labeller(facet_names, default = label_value))) +
  theme_bw() +
  labs(
    title = "Forest Plot of Environmental Effects",
    subtitle = "Estimates with 95% Confidence Intervals (Bonferroni Adjusted)",
    x = "Estimate (Mean Difference)",
    y = ""
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    axis.text.y = element_text(size = 8),
    panel.spacing = unit(1, "lines")
  )

# Save the final result
ggsave("LMM/Master_Forest_Plot.pdf", plot = forest_plot, width = 14, height = 12)
