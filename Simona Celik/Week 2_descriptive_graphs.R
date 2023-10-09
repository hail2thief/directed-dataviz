### PURPOSE: Directed Reading, Fall 2023
### BY: Simona Celik
### DATE: October, 2023

# Load the required library
library(tidyverse)
library(here)
library(ggplot2)
library(dplyr)

# Read the final data file
df = read_rds(here("Simona Celik", "clean_data.rds"))

# Create the ggplot with the y-axis reversed and custom labels
ggplot(data = df, aes(x = GINI, y = CORRUPTION_INDEX)) +
  geom_point(aes(size = REGIME, color = REGION)) +
  labs(x = "GINI",
       y = "CORRUPTION INDEX",
       title = "Income Inequality and Corruption by Region") +
  theme_bw() +
  scale_y_continuous(breaks = c(25, 50, 75), 
                     labels = c("Higher Corruption", "Medium Corruption", "Lower Corruption"))

# Save the plot with a complete file path
ggsave(filename = here("Simona Celik", "gini_corruption.png"), width = 10, height = 6)

