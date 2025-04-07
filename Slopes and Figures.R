#Put Effect Sizes from Main RQ Models into Dataframe


#re-doing this to account for slopes instead of effect sizes.

#As of March 12 re-doing slopes

library(dplyr)
library(broom)

library(dplyr)

# Compute slopes but retain all rows in df
slopes_df <- No_Duplicates_Jan_31_25 %>%
  filter(!is.na(scaled_residuals) & !is.na(TimeSinceBL)) %>%  # Remove NAs for modeling
  group_by(PTID, hemi, label, metric) %>%
  summarize(
    slope = ifelse(n() > 1, coef(lm(scaled_residuals ~ TimeSinceBL))[2], NA_real_),  # Compute slope if >1 time point
    .groups = "drop"
  )

# Merge back to the original dataframe
Data_With_Slopes_March_19 <- No_Duplicates_Jan_31_25 %>%
  left_join(slopes_df, by = c("PTID", "hemi", "label", "metric"))

#save
write.csv(Data_With_Slopes_March_19, "Data_With_Slopes_March_19.csv", row.names = FALSE)

slopes_df_example <- subset_data_example %>%
  filter(!is.na(scaled_residuals) & !is.na(TimeSinceBL)) %>%  # Remove NAs for modeling
  group_by(PTID, hemi, label, metric) %>%
  summarize(
    slope = ifelse(n() > 1, coef(lm(scaled_residuals ~ TimeSinceBL))[2], NA_real_),  # Compute slope if >1 time point
    .groups = "drop"
  )


#working below..
library(dplyr)
library(scales)
library(purrr)
library(dplyr)

# Step 1: Compute average slopes for each combination of hemi, label, metric, and Progressive_Change
avg_slopes <- Data_With_Slopes_March_19 %>%
  group_by(Progressive_Change, hemi, label, metric) %>%
  summarize(
    average_slope = mean(slope, na.rm = TRUE),  # Compute average slope
    .groups = 'drop'
  ) %>%
  filter(!is.na(average_slope) & Progressive_Change != "MCI_to_MCI")  # Remove rows where average_slope is NA


# Step 2: Compute global min, mid, max per metric from average_slope
metric_ranges <- avg_slopes %>%
  group_by(metric) %>%
  summarize(
    global_min = min(average_slope, na.rm = TRUE),
    global_mid = median(average_slope, na.rm = TRUE),  # Midpoint based on median
    global_max = max(average_slope, na.rm = TRUE),
    .groups = 'drop'
  )

# Step 3: Merge global min/mid/max with the average slopes
avg_slopes_with_global <- avg_slopes %>%
  left_join(metric_ranges, by = "metric")

# Step 4: Define the color mapping function
generate_blue_white_red <- function(value, min_val, mid_val, max_val) {
  if (is.na(value)) {
    return(NA_character_)  # Return NA for missing values
  }
  
  if (value <= mid_val) {
    norm_value <- scales::rescale(value, to = c(0, 0.5), from = c(min_val, mid_val))
  } else {
    norm_value <- scales::rescale(value, to = c(0.5, 1), from = c(mid_val, max_val))
  }
  
  color <- scales::col_numeric(
    palette = c("blue", "white", "red"),
    domain = c(0, 1)
  )(norm_value)
  
  return(color)
}

# Step 5: Apply color mapping
avg_slopes_with_global <- avg_slopes_with_global %>%
  mutate(color = pmap_chr(
    list(average_slope, global_min, global_mid, global_max),
    generate_blue_white_red
  ))

# Step 6: Convert color codes to RGB values
avg_slopes_with_global <- avg_slopes_with_global %>%
  mutate(
    Red = col2rgb(color)[1, ],
    Green = col2rgb(color)[2, ],
    Blue = col2rgb(color)[3, ]
  )

# Step 7: Write the data to a CSV
write.csv(avg_slopes_with_global, "avg_slopes_with_global.csv", row.names = FALSE)

