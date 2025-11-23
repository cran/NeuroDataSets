## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(NeuroDataSets)
library(dplyr)
library(ggplot2)

## ----NeuroDataSets-datasets,echo = TRUE,message = FALSE,warning = FALSE,results = 'markup'----


view_datasets_NeuroDataSets()



## ----patterns-subcortical-plot, fig.width=6, fig.height=4, out.width="100%"----

# Convert the dataset to long format using only base R + dplyr

long_data <- subcortical_patterns_tbl_df %>%
  select(Subcortical, everything()) %>%
  as.data.frame() %>%
  reshape(
    varying = names(.)[-1],
    v.names = "Value",
    timevar = "Condition",
    times = names(.)[-1],
    direction = "long"
  ) %>%
  select(Subcortical, Condition, Value)

# Create a heatmap
ggplot(long_data, aes(x = Condition, y = Subcortical, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  labs(
    title = "Subcortical Patterns by Condition",
    x = "Condition",
    y = "Subcortical Region",
    fill = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ----white-matter-plot, fig.width=6, fig.height=4.5, out.width="90%"----------

# Compute mean values using updated anonymous function syntax
summary_data <- WMpatterns_tbl_df %>%
  select(-WM) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
  as.data.frame()

# Reshape from wide to long format using base R
summary_data <- data.frame(
  Condition = names(summary_data),
  MeanValue = as.numeric(summary_data[1, ])
)

# Plot
ggplot(summary_data, aes(x = Condition, y = MeanValue, fill = Condition)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Value per Condition across White Matter Regions",
    x = "Condition",
    y = "Mean Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")  # Optional


## ----memory-hippocampus-plot, fig.width=6, fig.height=4.5, out.width="90%"----

# Lesion Size and Memory Score

ggplot(hippocampus_lesions_df, aes(x = lesion, y = memory)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "Relationship Between Lesion Size and Memory Score",
    x = "Lesion Size",
    y = "Memory Score"
  ) +
  theme_minimal()



