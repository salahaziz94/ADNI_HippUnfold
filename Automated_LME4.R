
# LME4 with Results Storing -----------------------------------------------
# Load necessary libraries
library(dplyr)
library(lme4)
library(broom.mixed)  # For tidy model output
library(lmerTest)     # For p-values with linear mixed models
library(readr)

Data_With_Slopes_March_12 <- read_csv("Data_With_Slopes_March_12.csv")

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
      Subset_data <- Data_With_Slopes_March_12 %>%
        filter(hemi == !!hemi, label == !!label, metric == !!metric)
      
      # Clean the data by removing NA values
      Subset_data_clean <- Subset_data %>%
        filter(!is.na(Progressive_Change))
      
      # Print dimensions to check if the subsetting is correct
      print(paste("Subset dimensions for hemi:", hemi, "label:", label, "metric:", metric, "->", dim(Subset_data_clean)[1]))
      
      # Ensure we have enough data to fit the model
      if (nrow(Subset_data_clean) > 1) {
        
        # Conditionally add ETIV only for volume models
        if (metric == "volume") {
          lme4formula <- as.formula(paste("scaled_residuals ~ Progressive_Change * TimeSinceBL + Age + Education + Scanner_Site + Sex + ETIV_z + (1 + TimeSinceBL | PTID)"))
        } else {
          lme4formula <- as.formula(paste("scaled_residuals ~ Progressive_Change * TimeSinceBL + Age + Education + Scanner_Site + Sex + (1 + TimeSinceBL | PTID)"))
        }
        
        # Run the linear mixed-effects model using lmerTest for p-values
        model <- lmer(lme4formula, data = Subset_data_clean, control = lmerControl(optimizer = "bobyqa"), REML = FALSE)
        
        # Extract fixed effects (with p-values) and random effects
        fixed_effects <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) # Get fixed effects
        random_effects <- broom.mixed::tidy(model, effects = "ran_pars") # Get random effects
        
        # Combine both fixed and random effects and store in the results list
        combined_results <- bind_rows(
          mutate(fixed_effects, effect_type = "fixed"),
          mutate(random_effects, effect_type = "random")
        )
        
        # Store the combined results in the list
        model_results_list[[paste(hemi, label, metric, sep = "_")]] <- combined_results
        
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

write.csv(model_results, "Main_LMM_model_results.csv", row.names = FALSE)

Main_LMM_model_results <- read.csv("Main_LMM_model_results.csv")

Main_LMM_model_FDR_adjusted <- Main_LMM_model_results %>%
  mutate(adj_p_value = p.adjust(p.value, method = "fdr"))

write.csv(Main_LMM_model_FDR_adjusted, "Main_LMM_model_FDR_adjusted.csv", row.names = FALSE)

# With More Info From Random Effects --------------------------------------


# Loop through each combination of hemi, label, and metric for variances with ETIV (needs to be fixed)
for (hemi in hemi_values) {
  for (label in label_values) {
    for (metric in metric_values) {
      
      # Filter the dataset based on the current combination
      Subset_data <- Data_With_Slopes_March_12 %>%
        filter(hemi == !!hemi, label == !!label, metric == !!metric)
      
      # Clean the data by removing NA values
      Subset_data_clean <- Subset_data %>%
        filter(!is.na(Progressive_Change))
      
      # Print dimensions to check if the subsetting is correct
      print(paste("Subset dimensions for hemi:", hemi, "label:", label, "metric:", metric, "->", dim(Subset_data_clean)[1]))
      
      # Ensure we have enough data to fit the model
      if (nrow(Subset_data_clean) > 1) {
        
        # Conditionally add ETIV only for volume models
        if (metric == "volume") {
          lme4formula <- as.formula(paste("scaled_residuals ~ Progressive_Change * TimeSinceBL + Age + Education + Scanner_Site + Sex + ETIV_z + (1 + TimeSinceBL | PTID)"))
        } else {
          lme4formula <- as.formula(paste("scaled_residuals ~ Progressive_Change * TimeSinceBL + Age + Education + Scanner_Site + Sex + (1 + TimeSinceBL | PTID)"))
        }
        
        # Run the linear mixed-effects model using lmerTest for p-values
        model <- lmer(lme4formula, data = Subset_data_clean, control = lmerControl(optimizer = "bobyqa"))
        
        # Extract fixed effects (with p-values) and random effects
        fixed_effects <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) # Get fixed effects
        random_effects <- broom.mixed::tidy(model, effects = "ran_pars") # Get random effects
        
        # Additional: Extract variance-covariance matrix of random effects
        var_corr <- as.data.frame(VarCorr(model)) # Get variance-covariance matrix of random effects
        random_effect_values <- as.data.frame(ranef(model)) # Get actual random effects values
        
        # Combine both fixed and random effects and store in the results list
        combined_results <- bind_rows(
          mutate(fixed_effects, effect_type = "fixed"),
          mutate(random_effects, effect_type = "random"),
          mutate(var_corr, effect_type = "var_corr"),
          mutate(random_effect_values, effect_type = "random_values")
        )
        
        # Store the combined results in the list
        model_results_list[[paste(hemi, label, metric, sep = "_")]] <- combined_results
        
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

write.csv(model_results, "Main_LMM_model_results_with_Variances.csv", row.names = FALSE)

Main_LMM_model_results_with_Variances <- read.csv("Main_LMM_model_results_with_Variances.csv")

Main_LMM_model_FDR_adjusted <- Main_LMM_model_results_with_Variances %>%
  mutate(adj_p_value = p.adjust(p.value, method = "fdr"))

write.csv(Main_LMM_model_FDR_adjusted, "Main_LMM_model_FDR_adjusted_with_Variances.csv", row.names = FALSE)

# Trajectory Plot ---------------------------------------------------------


library(ggplot2)
library(ggsci)
library(cowplot)
library(dplyr)

# Define the specific label and hemisphere
target_hemi <- "L"  # Change to "R" for the right hemisphere
target_label <- "CA1"

# Define metrics to plot
metrics <- c("volume", "curvature", "gyrification", "thickness")

# Prepare the combined data frame
combined_data <- Data_With_Slopes_March_12 %>%
  filter(hemi == !!target_hemi, label == !!target_label) %>%
  filter(metric %in% metrics) %>%
  filter(!is.na(Progressive_Change))  # Remove rows with NA in Progressive_Change

# Initialize a list to store plots
plots <- list()



#lets automate this from above... (Correct version works with long_data_no_NAs)


# Define unique hemispheres and labels for subsetting
unique_hemispheres <- c("L", "R")
unique_labels <- c("CA1", "CA2", "CA3", "CA4", "Cyst", "DG", "SRLM", "Sub")

# Define metrics to plot
metrics <- c("volume", "curvature", "gyrification", "thickness")

# Initialize a list to store all plots
all_combined_plots <- list()






# Adding Slope Values -----------------------------------------------------
# Define unique hemispheres and labels for subsetting
library(ggrepel)
library(cowplot)
library(dplyr)
library(ggplot2)
library(ggsci)

# Define unique hemispheres and labels
unique_hemispheres <- c("L", "R")
unique_labels <- c("CA1", "CA2", "CA3", "CA4", "Cyst", "DG", "SRLM", "Sub")
metrics <- c("volume", "curvature", "gyrification", "thickness")

# Initialize a list to store all plots
all_combined_plots <- list()

# Loop through each combination of hemisphere and label
for (target_hemi in unique_hemispheres) {
  for (target_label in unique_labels) {
    # Prepare the combined data frame for the current hemisphere and label
    combined_data <- Data_With_Slopes_March_12 %>%
      filter(hemi == !!target_hemi, label == !!target_label) %>%
      filter(metric %in% metrics) %>%
      filter(!is.na(Progressive_Change))  # Remove rows with NA in Progressive_Change
    
    # Check if there is data to plot
    if (nrow(combined_data) == 0) {
      message(paste("No data for Hemisphere:", target_hemi, "Label:", target_label))
      next  # Skip to the next iteration if there's no data
    }
    
    # Initialize a list to store plots for this combination
    plots <- list()
    
    # Loop through each metric to create the plots
    for (i in seq_along(metrics)) {
      metric <- metrics[i]
      metric_title <- tools::toTitleCase(metric)
      
      # Filter data for the current metric
      metric_data <- combined_data %>% filter(metric == !!metric)
      
      if (nrow(metric_data) == 0) {
        message(paste("No data for Metric:", metric, "in Hemisphere:", target_hemi, "Label:", target_label))
        next  # Skip to the next iteration if there's no data for this metric
      }
      
      # Calculate slopes for each Progressive_Change group
      slopes <- metric_data %>%
        group_by(Progressive_Change) %>%
        summarise(
          slope_value = coef(lm(scaled_residuals ~ TimeSinceBL))["TimeSinceBL"],
          x_pos = mean(TimeSinceBL),  # Average x position for label placement
          y_pos = max(scaled_residuals) + 0.05 * diff(range(scaled_residuals))  # Position above the lines
        )
      
      # Create the plot for the current metric using raw data
      p <- ggplot(metric_data, 
                  aes(x = TimeSinceBL, y = scaled_residuals, color = Progressive_Change)) +
        geom_path(alpha = 0.1, aes(group = PTID)) +
        geom_smooth(method = "lm", se = FALSE, aes(group = Progressive_Change, color = Progressive_Change), linetype = "solid", linewidth = 1.2) +  # Add a linear fit line
        labs(title = paste0("(", LETTERS[i], ") ", metric_title),  # Add letter and metric title
             x = "Time Since Baseline", 
             y = "Residualized Metric") +
        theme_minimal() +
        scale_color_jco(name = "Progressive Group", 
                        labels = c("Stable CN", "Stable MCI", "Stable AD", "CN to MCI/AD", "MCI to AD")) +
        facet_wrap(~Progressive_Change, ncol = 5) +
        theme(strip.text = element_blank(), legend.position = "none")  # Remove strip text and legend
      
      # Add slope values directly on the plot without connecting lines
      p <- p + 
        geom_text_repel(data = slopes, 
                        aes(x = x_pos, y = y_pos, 
                            label = paste("Slope: ", round(slope_value, 4))),
                        size = 2.25, nudge_y = 0.1, segment.color = NA,  # No lines pointing to text
                        show.legend = FALSE) 
      
      plots[[i]] <- p
    }
    
    # Combine the plots for this combination
    if (length(plots) == 0) {
      message(paste("No plots created for Hemisphere:", target_hemi, "Label:", target_label))
      next  # Skip to the next iteration if no plots were created
    }
    
    combined_plots <- plot_grid(plotlist = plots, ncol = 2)
    
    # Create the legend using the first plot
    legend <- get_legend(plots[[1]] + theme(legend.position = "bottom"))
    
    # Add hemisphere and label annotation
    hemisphere_label <- ifelse(target_hemi == "L", "Hemisphere: Left | Label: ", "Hemisphere: Right | Label: ")
    hemisphere_label <- paste(hemisphere_label, target_label)
    
    # Create a blank plot to hold the annotation
    annotation_plot <- ggdraw() + 
      draw_label(hemisphere_label, size = 12, hjust = 0.5, vjust = 1.5)  # Increased size for better visibility
    
    # Combine the annotation plot and the combined plots
    final_plot <- plot_grid(annotation_plot, combined_plots, legend, ncol = 1, rel_heights = c(0.1, 1, 0.1))
    
    # Store the final plot for this combination
    all_combined_plots[[paste(target_hemi, target_label, sep = "_")]] <- final_plot
  }
}

# To view or save individual plots, you can iterate over `all_combined_plots`
for (plot_name in names(all_combined_plots)) {
  print(all_combined_plots[[plot_name]])
  # Optionally save each plot
  ggsave(paste0("plot_", plot_name, ".png"), plot = all_combined_plots[[plot_name]], width = 10, height = 8)
}



# saving slope values for comparisons  ------------------------------------

# Define the unique categories you're interested in
unique_hemispheres <- c("L", "R")
unique_labels <- c("CA1", "CA2", "CA3", "CA4", "Cyst", "DG", "SRLM", "Sub")
metrics <- c("volume", "curvature", "gyrification", "thickness")

# Create an empty list to store the results for each combination
slopes_list <- list()

# Loop through all combinations of hemisphere, label, and metric
for (target_hemi in unique_hemispheres) {
  for (target_label in unique_labels) {
    for (metric in metrics) {
      
      # Filter the data based on the current hemisphere, label, and metric
      combined_data <- Data_With_Slopes_March_12 %>%
        filter(hemi == target_hemi, label == target_label, metric == metric) %>%
        filter(!is.na(Progressive_Change))  # Remove rows with NA in Progressive_Change
      
      # Skip combinations where there is not enough data
      if (nrow(combined_data) > 1) {
        
        # Summarize the slope values for each group (Progressive_Change)
        slopes <- combined_data %>%
          group_by(Progressive_Change) %>%
          summarise(
            slope_value = coef(lm(scaled_residuals ~ TimeSinceBL))["TimeSinceBL"],
            x_pos = mean(TimeSinceBL),  # Average x position for label placement
            y_pos = max(scaled_residuals) + 0.05 * diff(range(scaled_residuals))  # Position above the lines
          )
        
        # Add grouping variables to the summary
        slopes$Hemisphere <- target_hemi
        slopes$Label <- target_label
        slopes$Metric <- metric
        
        # Append the result to the list
        slopes_list[[paste(target_hemi, target_label, metric, sep = "_")]] <- slopes
      }
    }
  }
}

# Combine the list of results into one data frame
slopes_df <- bind_rows(slopes_list)


write.csv(slopes_df, "slopes_for_comparisons_nov26.csv", row.names = FALSE)

# Perform ANOVA for each combination of Hemisphere, Label, and Metric
anova_results <- slopes_df %>%
  group_by(Hemisphere, Label, Metric) %>%
  do({
    model <- aov(slope_value ~ Progressive_Change, data = .)
    tidy_model <- broom::tidy(model)
    tidy_model
  })

# Inspect ANOVA results
print(anova_results)

# Perform pairwise comparisons (Tukey HSD) for each combination of Hemisphere, Label, and Metric
posthoc_results <- slopes_df %>%
  group_by(Hemisphere, Label, Metric) %>%
  do({
    model <- aov(slope_value ~ Progressive_Change, data = .)
    tukey_test <- TukeyHSD(model)
    tukey_results <- tidy(tukey_test)
    tukey_results
  })

# Inspect the post-hoc results
print(posthoc_results)



# Manually Checking Slope Values ------------------------------------------

# Define unique hemispheres and labels
unique_hemispheres <- c("L", "R")
unique_labels <- c("CA1", "CA2", "CA3", "CA4", "Cyst", "DG", "SRLM", "Sub")
metrics <- c("volume", "curvature", "gyrification", "thickness")

# Initialize a list to store all plots and slope values
all_combined_plots <- list()
all_slopes <- list()  # List to store slope values

# Loop through each combination of hemisphere and label
for (target_hemi in unique_hemispheres) {
  for (target_label in unique_labels) {
    # Prepare the combined data frame for the current hemisphere and label
    combined_data <- No_Duplicates_Jan_31_25 %>%
      filter(hemi == !!target_hemi, label == !!target_label) %>%
      filter(metric %in% metrics) %>%
      filter(!is.na(Progressive_Change))  # Remove rows with NA in Progressive_Change
    
    # Check if there is data to plot
    if (nrow(combined_data) == 0) {
      message(paste("No data for Hemisphere:", target_hemi, "Label:", target_label))
      next  # Skip to the next iteration if there's no data
    }
    
    # Initialize a list to store plots for this combination
    plots <- list()
    
    # Loop through each metric to create the plots
    for (i in seq_along(metrics)) {
      metric <- metrics[i]
      metric_title <- tools::toTitleCase(metric)
      
      # Filter data for the current metric
      metric_data <- combined_data %>% filter(metric == !!metric)
      
      if (nrow(metric_data) == 0) {
        message(paste("No data for Metric:", metric, "in Hemisphere:", target_hemi, "Label:", target_label))
        next  # Skip to the next iteration if there's no data for this metric
      }
      
      # Calculate slopes for each Progressive_Change group
      slopes <- metric_data %>%
        group_by(Progressive_Change) %>%
        summarise(
          slope_value = coef(lm(scaled_residuals ~ TimeSinceBL))["TimeSinceBL"],
          x_pos = mean(TimeSinceBL),  # Average x position for label placement
          y_pos = max(scaled_residuals) + 0.05 * diff(range(scaled_residuals))  # Position above the lines
        )
      
      # Store the slope values for inspection
      slopes$hemi <- target_hemi
      slopes$label <- target_label
      slopes$metric <- metric
      all_slopes[[length(all_slopes) + 1]] <- slopes  # Append slopes to the list
      
      # Create the plot for the current metric using raw data
      p <- ggplot(metric_data, 
                  aes(x = TimeSinceBL, y = scaled_residuals, color = Progressive_Change)) +
        geom_path(alpha = 0.1, aes(group = PTID)) +
        geom_smooth(method = "lm", se = FALSE, linetype = "solid", linewidth = 1.2) +  # Add a linear fit line
        labs(title = paste0("(", LETTERS[i], ") ", metric_title),  # Add letter and metric title
             x = "Time Since Baseline", 
             y = "Residualized Metric") +
        theme_minimal() +
        scale_color_jco(name = "Progressive Group", 
                        labels = c("Stable CN", "Stable MCI", "Stable AD", "CN to MCI/AD", "MCI to AD")) +
        facet_wrap(~Progressive_Change, ncol = 5) +
        theme(strip.text = element_blank(), legend.position = "none")  # Remove strip text and legend
      
      # Add slope values directly on the plot without connecting lines
      p <- p + 
        geom_text_repel(data = slopes, 
                        aes(x = x_pos, y = y_pos, 
                            label = paste("Slope: ", round(slope_value, 4))),
                        size = 2.25, nudge_y = 0.1, segment.color = NA,  # No lines pointing to text
                        show.legend = FALSE) 
      
      plots[[i]] <- p
    }
    
    # Combine the plots for this combination
    if (length(plots) == 0) {
      message(paste("No plots created for Hemisphere:", target_hemi, "Label:", target_label))
      next  # Skip to the next iteration if no plots were created
    }
    
    combined_plots <- plot_grid(plotlist = plots, ncol = 2)
    
    # Create the legend using the first plot
    legend <- get_legend(plots[[1]] + theme(legend.position = "bottom"))
    
    # Add hemisphere and label annotation
    hemisphere_label <- ifelse(target_hemi == "L", "Hemisphere: Left | Label: ", "Hemisphere: Right | Label: ")
    hemisphere_label <- paste(hemisphere_label, target_label)
    
    # Create a blank plot to hold the annotation
    annotation_plot <- ggdraw() + 
      draw_label(hemisphere_label, size = 12, hjust = 0.5, vjust = 1.5)  # Increased size for better visibility
    
    # Combine the annotation plot and the combined plots
    final_plot <- plot_grid(annotation_plot, combined_plots, legend, ncol = 1, rel_heights = c(0.1, 1, 0.1))
    
    # Store the final plot for this combination
    all_combined_plots[[paste(target_hemi, target_label, sep = "_")]] <- final_plot
  }
}

# Combine all slope values into a single data frame for inspection
slope_results <- bind_rows(all_slopes)

# View the slope results
print(slope_results)

# To view or save individual plots, you can iterate over all_combined_plots
for (plot_name in names(all_combined_plots)) {
  print(all_combined_plots[[plot_name]])
  # Optionally save each plot
  ggsave(paste0("plot_", plot_name, ".png"), plot = all_combined_plots[[plot_name]], width = 10, height = 8)
}


write.csv(slope_results, "slope_results_trajectory_plots.csv", row.names = FALSE)

# Jan 16 2025 Post Hoc Slopes ---------------------------------------------

library(emmeans)
library(dplyr)

# Initialize a list to store post-hoc comparison results
post_hoc_results <- list()

# Loop through each combination of hemisphere and label
for (target_hemi in unique_hemispheres) {
  for (target_label in unique_labels) {
    
    # Filter the data for the current hemisphere and label
    combined_data <- Data_With_Slopes_March_12 %>%
      filter(hemi == !!target_hemi, label == !!target_label) %>%
      filter(metric %in% metrics) %>%
      filter(!is.na(Progressive_Change))  # Remove rows with NA in Progressive_Change
    
    # Check if there is data to analyze
    if (nrow(combined_data) == 0) {
      message(paste("No data for Hemisphere:", target_hemi, "Label:", target_label))
      next
    }
    
    # Loop through each metric
    for (metric in metrics) {
      
      # Filter data for the current metric
      metric_data <- combined_data %>% filter(metric == !!metric)
      
      if (nrow(metric_data) == 0) {
        message(paste("No data for Metric:", metric, "in Hemisphere:", target_hemi, "Label:", target_label))
        next
      }
      
      # Fit the linear model for each group
      lm_model <- lm(scaled_residuals ~ Progressive_Change * TimeSinceBL, data = metric_data)
      
      # Perform pairwise post-hoc comparisons for slopes between the groups (using emmeans)
      emmeans_results <- emmeans(lm_model, pairwise ~ Progressive_Change, adjust = "FDR")
      
      # Store the post-hoc comparison results
      result_key <- paste(target_hemi, target_label, metric, sep = "_")
      post_hoc_results[[result_key]] <- list(
        summary = summary(emmeans_results$emmeans),
        comparisons = summary(emmeans_results$contrasts)
      )
      
      # Flag significant comparisons (p-value < 0.05)
      emmeans_comparisons <- as.data.frame(emmeans_results$contrasts)
      emmeans_comparisons$Significance <- ifelse(emmeans_comparisons$p.value < 0.05, "Significant", "Not Significant")
      
      # Save the summary (EMMEANS) results
      emmeans_summary <- as.data.frame(emmeans_results$emmeans)
      write.csv(emmeans_summary, file = paste0("emmeans_summary_", result_key, ".csv"), row.names = FALSE)
      
      # Save the pairwise comparisons (contrasts) results with significance flag
      write.csv(emmeans_comparisons, file = paste0("emmeans_comparisons_", result_key, ".csv"), row.names = FALSE)
    }
  }
}

# You can now add significance values manually by checking the saved CSVs.


# Putting Title and Legend Together (Jan 16) ------------------------------

# Loop through each combination of hemisphere and label
for (target_hemi in unique_hemispheres) {
  for (target_label in unique_labels) {
    # Prepare the combined data frame for the current hemisphere and label
    combined_data <- Data_With_Slopes_March_12 %>%
      filter(hemi == !!target_hemi, label == !!target_label) %>%
      filter(metric %in% metrics) %>%
      filter(!is.na(Progressive_Change))  # Remove rows with NA in Progressive_Change
    
    # Check if there is data to plot
    if (nrow(combined_data) == 0) {
      message(paste("No data for Hemisphere:", target_hemi, "Label:", target_label))
      next  # Skip to the next iteration if there's no data
    }
    
    # Initialize a list to store plots for this combination
    plots <- list()
    
    # Loop through each metric to create the plots
    for (i in seq_along(metrics)) {
      metric <- metrics[i]
      metric_title <- tools::toTitleCase(metric)
      
      # Filter data for the current metric
      metric_data <- combined_data %>% filter(metric == !!metric)
      
      if (nrow(metric_data) == 0) {
        message(paste("No data for Metric:", metric, "in Hemisphere:", target_hemi, "Label:", target_label))
        next  # Skip to the next iteration if there's no data for this metric
      }
      
      # Calculate slopes for each Progressive_Change group
      slopes <- metric_data %>%
        group_by(Progressive_Change) %>%
        summarise(
          slope_value = coef(lm(scaled_residuals ~ TimeSinceBL))["TimeSinceBL"],
          x_pos = mean(TimeSinceBL),  # Average x position for label placement
          y_pos = max(scaled_residuals) + 0.05 * diff(range(scaled_residuals))  # Position above the lines
        )
      
      # Create the plot for the current metric using raw data
      p <- ggplot(metric_data, 
                  aes(x = TimeSinceBL, y = scaled_residuals, color = Progressive_Change)) +
        geom_path(alpha = 0.1, aes(group = PTID)) +
        geom_smooth(method = "lm", se = FALSE, aes(group = Progressive_Change, color = Progressive_Change), linetype = "solid", linewidth = 1.2) +  # Add a linear fit line
        labs(title = paste0("(", LETTERS[i], ") ", metric_title),  # Add letter and metric title
             x = "Follow-up Years", 
             y = "Residualized Metric") +
        theme_minimal() +
        scale_color_jco(name = "Progressive Group", 
                        labels = c("Stable CN", "Stable MCI", "Stable AD", "CN to MCI/AD", "MCI to AD")) +
        facet_wrap(~Progressive_Change, ncol = 5) +
        theme(strip.text = element_blank(), legend.position = "none")  # Remove strip text and legend
      
      # Add slope values directly on the plot without connecting lines
      p <- p + 
        geom_text_repel(data = slopes, 
                        aes(x = x_pos, y = y_pos, 
                            label = paste("Slope: ", round(slope_value, 4))),
                        size = 2.25, nudge_y = 0.1, segment.color = NA,  # No lines pointing to text
                        show.legend = FALSE) 
      
      plots[[i]] <- p
    }
    
    # Combine the plots for this combination
    if (length(plots) == 0) {
      message(paste("No plots created for Hemisphere:", target_hemi, "Label:", target_label))
      next  # Skip to the next iteration if no plots were created
    }
    
    combined_plots <- plot_grid(plotlist = plots, ncol = 2)
    
    # Create the legend using the first plot
    legend <- get_legend(plots[[1]] + theme(legend.position = "top"))
    
    # Create a title for the plot
    title <- ggdraw() + 
      draw_label(
        paste("Longitudinal Change in", target_hemi, target_label, "Morphometry for Different Diagnostic Groups"), 
        size = 14, hjust = 0.5, vjust = 1.5
      )  # Title with appropriate size
    
    # Combine the title, plots, and legend
    final_plot <- plot_grid(title, legend, combined_plots, ncol = 1, rel_heights = c(0.1, 0.1, 1))
    
    # Store the final plot for this combination
    all_combined_plots[[paste(target_hemi, target_label, sep = "_")]] <- final_plot
  }
}

# To view or save individual plots, you can iterate over `all_combined_plots`
for (plot_name in names(all_combined_plots)) {
  print(all_combined_plots[[plot_name]])
  # Optionally save each plot
  ggsave(paste0("plot_", plot_name, ".png"), plot = all_combined_plots[[plot_name]], width = 10, height = 8)
}
