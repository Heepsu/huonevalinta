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
