# Load necessary libraries
library(ggplot2)    # For visualization
library(dplyr)      # For data manipulation
library(ggpubr)     # For combining plots
library(ggsci)      # For color scales


# Automation  -------------------------------------------------------------

# Load necessary libraries
# Load necessary libraries
library(dplyr)
library(lme4)
library(broom.mixed)  # For tidy model output
library(lmerTest)     # For p-values with linear mixed models


# Setting reference level for stable, CN cohort
Data_With_Slopes_March_12$Progressive_Change <- factor(Data_With_Slopes_March_12$Progressive_Change, 
                                                    levels = c("CN_to_CN", "MCI_to_MCI", "AD_to_AD", "CN_to_MCI_or_AD", "MCI_to_AD"))

# Define the unique combinations of hemi, label, and metric
hemi_values <- c("L", "R")
label_values <- c("CA1", "CA2", "CA3", "CA4", "DG", "Sub", "SRLM", "Cyst")
metric_values <- c("volume", "curvature", "gyrification", "thickness")

# Initialize an empty list to store model results
model_results_list <- list()

# Loop through each combination of hemi, label, and metric
for (hemi in hemi_values) {
  for (label in label_values) {
    for (metric in metric_values) {
      
      # Filter the dataset based on the current combination
      Subset_data <- Data_With_Slopes_March_12%>%
        filter(hemi == !!hemi, label == !!label, metric == !!metric)
      
      Subset_data_clean <- Subset_data %>%
        filter(!is.na(slope) & !is.na(Progressive_Change))
      
      # Print dimensions to check if the subsetting is correct
      print(paste("Subset dimensions for hemi:", hemi, "label:", label, "metric:", metric, "->", dim(Subset_data_clean)[1]))
      
      # Ensure we have enough data to fit the model
      if (nrow(Subset_data_clean) > 1) {
        # Loop through each cognitive domain (e.g., PHC_MEM, PHC_VSP, etc.)
        for (domain in c("PHC_MEM", "PHC_VSP", "PHC_LAN", "PHC_EXF")) {
          
          # Fit the regression model for the current domain
          model_formula <- as.formula(paste(domain, "~ scaled_residuals * Progressive_Change"))
          model <- try(lm(model_formula, data = Subset_data_clean), silent = TRUE)  # Use try to avoid stopping on errors
          
          # Check if the model fit succeeded
          if (!inherits(model, "try-error")) {
            # Extract the model summary as a tidy data frame
            model_summary <- broom::tidy(model)
            
            # Store the model summary in the list, include the domain in the name for clarity
            model_results_list[[paste(hemi, label, metric, domain, sep = "_")]] <- model_summary
          } else {
            print(paste("Model fit failed for hemi:", hemi, "label:", label, "metric:", metric, "domain:", domain))
          }
        }
        
      } else {
        print(paste("Not enough data to fit model for hemi:", hemi, "label:", label, "metric:", metric))
      }
    }
  }
}

# Combine all model results into a single data frame
model_results <- bind_rows(model_results_list, .id = "model_id")

# View the model results
print(model_results)

# Optionally, save the model results to a CSV file
write.csv(model_results, "Cognitive_Domain_Linear_Models_results.csv", row.names = FALSE)


# Run FDR From Already Made Models ---------------------------------------
library(readr)
read_csv("Cognitive_Domain_Linear_models_results.csv")

# Load necessary libraries
library(dplyr)

# Step 1: Load the results
model_results <- read.csv("Cognitive_Domain_Linear_Models_results.csv")


# Step 2: Apply FDR correction on the p-value column
model_results <- model_results %>%
  mutate(adj_p.value = p.adjust(p.value, method = "fdr"))

# Step 3: Sort results by adjusted p-value
sorted_results <- model_results %>%
  arrange(adj_p.value)

# Step 4: View the sorted results
print(sorted_results)

# Step 5: Optionally, save the sorted results to a new CSV file
write.csv(sorted_results, "Sorted_Interaction_Results_with_FDR.csv", row.names = FALSE)


Sorted_Interaction_Results_with_FDR <- read_csv("Sorted_Interaction_Results_with_FDR.csv")
# Plotting Results --------------------------------------------------------

library(ggplot2)


# Assuming the full dataset is stored in results_df
# Filter only significant results
significant_results <- Sorted_Interaction_Results_with_FDR

#Feb 2 Attempt with new dataset. 
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

# Assuming 'significant_results_clean' is your dataset
significant_results_clean <- significant_results %>%
  separate(model_id, into = c("Hemi", "Subfield", "Metric", "Other", "Domain"), sep = "_", remove = FALSE)
significant_results_clean <- significant_results_clean %>%
  filter(adj_p.value < 0.05)  # Adjust p-value threshold as needed

#terms to keep
terms_to_keep <- c("scaled_residuals:Progressive_ChangeAD_to_AD", "scaled_residuals:Progressive_ChangeMCI_to_MCI",
                   "scaled_residuals:Progressive_ChangeMCI_to_AD", "scaled_residuals:Progressive_ChangeCN_to_MCI_or_AD", "scaled_residuals" )
#filter out main terms beyond interaction
significant_results_clean <- significant_results_clean %>%
  filter(term %in% terms_to_keep)

write.csv(significant_results_clean, "Sorted_Interaction_Results_with_FDR_cleaned.csv", row.names = FALSE)


# Point Range Plot --------------------------------------------------------

library(dplyr)

significant_results_clean <- significant_results_clean %>%
  mutate(term = case_when(
    term == "scaled_residuals:Progressive_ChangeAD_to_AD" ~ "Stable AD", 
    term == "scaled_residuals:Progressive_ChangeMCI_to_MCI" ~ "Stable MCI", 
    term == "scaled_residuals:Progressive_ChangeMCI_to_AD" ~ "MCI to AD", 
    term == "scaled_residuals:Progressive_ChangeCN_to_MCI_or_AD" ~ "CN to MCI/AD",
    term == "scaled_residuals" ~ "Stable CN",  # Rename specific term
    TRUE ~ term  # Keep all other terms unchanged
  ))

write.csv(significant_results_clean, "only_interaction_terms.csv", row.names = FALSE)
          
significant_results_clean_L <- significant_results_clean %>%
  filter(Hemi == "L", adj_p.value < 0.05)

significant_results_clean_R <- significant_results_clean %>%
  filter(Hemi == "R", adj_p.value < 0.05)

# View result
head(df)

# Calculate range (effect size ± standard error)
filtered_data_L <- significant_results_clean_L %>%
  mutate(ymin = estimate - std.error, ymax = estimate + std.error)



#different modalities included

library(ggplot2)

# Check if the Metric column contains the correct values
table(filtered_data_L$Metric)  # This will help us see if "Volume" is correctly identified.

# Create a new column to distinguish Volume vs non-Volume
filtered_data_L$Volume_Group <- ifelse(filtered_data_L$Metric == "volume", "volume", "Other")


#different colors for modalities

library(ggplot2)

# Check if the Metric column contains the correct values
table(filtered_data_L$Metric)  # This will help us see if "volume" is correctly identified.

# Create a new column to distinguish Volume vs non-Volume
filtered_data_L$Volume_Group <- ifelse(filtered_data_L$Metric == "volume", "Volume", "Surface-Based Morphometry")

library(ggplot2)

# Check if the Metric column contains the correct values
table(filtered_data_L$Metric)  # This will help us see if "volume", "curvature", etc., are correctly identified.

# Reorder the terms as per your desired order
filtered_data_L$term <- factor(filtered_data_L$term, 
                               levels = c("MCI to AD", "CN to MCI/AD", "Stable AD",
                                          "Stable MCI", "Stable CN"))

# Now we can plot
ggplot(filtered_data_L, aes(x = term, y = estimate, color = Metric, shape = Domain)) +  
  geom_linerange(
    aes(ymin = ymin, ymax = ymax), 
    position = position_dodge(width = 0.6),  # Dodge lines for alignment
    size = 1, alpha = 0.5  # Adjust size and transparency of the lines
  ) +
  geom_point(
    position = position_dodge(width = 0.6),  # Dodge points for alignment
    size = 3, alpha = 0.8  # Adjust size for visibility
  ) +
  facet_wrap(~Subfield, scales = "free_y") +  # Faceting by subfield
  coord_flip() +  # Flip for readability
  theme_minimal() +  
  labs(
    title = "Effect Sizes for Interactions of Morphometry Slope with Diagnostic Category",
    x = "Interaction Term",
    y = "Beta (Effect Size)"
  ) +
  scale_color_jco() +  # Apply JCO color palette
  scale_shape_manual(values = c("MEM" = 16, "EXF" = 17, "LAN" = 18, "VSP" = 19)) +  # Different shapes for each cognitive domain
  scale_color_manual(
    values = c(
      "volume" = "darkblue", 
      "curvature" = "darkred", 
      "gyrification" = "darkgreen", 
      "thickness" = "orange"
    ),
    breaks = c("volume", "curvature", "gyrification", "thickness")  # Order the legend
  ) +  # Set colors for each metric
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust x-axis text angle for readability
    strip.text = element_text(size = 12),
    legend.position = "right",  # Keep the legend for color distinction
    panel.spacing = unit(1, "lines")  # Add space between facets for better layout
  )

#only CA1
filtered_data_L <- filtered_data_L %>% filter(Subfield == "CA1")

ggplot(filtered_data_L, aes(x = term, y = estimate, color = Metric, shape = Domain)) +  
  geom_linerange(
    aes(ymin = ymin, ymax = ymax), 
    position = position_dodge(width = 0.6),  # Dodge lines for alignment
    size = 1, alpha = 0.5  # Adjust size and transparency of the lines
  ) +
  geom_point(
    position = position_dodge(width = 0.6),  # Dodge points for alignment
    size = 3, alpha = 0.8  # Adjust size for visibility
  ) +
  facet_wrap(~Subfield, scales = "free_y") +  # Faceting by subfield
  coord_flip() +  # Flip for readability
  theme_minimal() +  
  labs(
    title = "Effect Sizes for Interactions of Morphometry with Diagnostic Category on Cognition",
    x = "Interaction Term",
    y = "Beta (Effect Size)"
  ) +
  scale_color_jco() +  # Apply JCO color palette
  scale_shape_manual(values = c("MEM" = 16, "EXF" = 17, "LAN" = 18, "VSP" = 19)) +  # Different shapes for each cognitive domain
  scale_color_manual(
    values = c(
      "volume" = "darkblue", 
      "curvature" = "darkred", 
      "gyrification" = "darkgreen", 
      "thickness" = "orange"
    ),
    breaks = c("volume", "curvature", "gyrification", "thickness")  # Order the legend
  ) +  # Set colors for each metric
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust x-axis text angle for readability
    strip.text = element_text(size = 12),
    legend.position = "right",  # Keep the legend for color distinction
    panel.spacing = unit(1, "lines")  # Add space between facets for better layout
  )
