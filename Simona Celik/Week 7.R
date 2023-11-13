# Load the required libraries
library(ggplot2)
library(margins)
library(stargazer)
library(here)

# Load the data 
load("C:/Users/simon/Downloads/elections.rda")
# View the structure of the loaded data
str(elections)

# Access specific components of the dataset
head(elections)  # View the first few rows

variable_names <- names(elections)

# Print the list of variable names
print(variable_names)

# Remove rows with NAs from the entire data frame
elections_cleaned <- na.omit(elections)

# Jitter plot for travel_time 
jitterplot <- ggplot(elections_cleaned, aes(x = factor(state), y = travel_time)) +
  geom_jitter(width = 0.3, height = 0.2, alpha = 0.3, size = 1) +  # Adjust alpha and size
  labs(title = "Jitter Plot for Travel Time by State",
       subtitle = "Visualizing the distribution of travel time across states",
       x = "State",
       y = "Travel Time (minutes)",  # Provide more clarity on the unit
       caption = "Data source: elections dataset") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  facet_wrap(~census_region, scales = "free_x", ncol = 2)  # Facet by census region for better comparison
print(jitterplot)

# Save the plot using ggsave
ggsave(filename = here("Simona Celik", "jitterplot.png"), plot = jitterplot, width = 10, height = 6)

# Create a scatterplot with smoothing line
scatterplot_with_smooth <- ggplot(elections_cleaned, aes(x = hh_income, y = per_gop_2020)) +
  geom_point(shape = 16, size = 3, color = "dodgerblue") +
  geom_smooth(method = "loess", se = FALSE, color = "orange") +  # Add a smoothing line
  labs(title = "Relationship between Household Income and GOP Vote Share in 2020",
       subtitle = "Scatterplot with Smoothing Line",
       x = "Household Income ($)",
       y = "GOP Vote Share 2020 (%)",
       caption = "Data source: elections dataset") +
  theme_minimal()

# Print the plot
print(scatterplot_with_smooth)

# Save the scatterplot using ggsave
ggsave(filename = here("Simona Celik", "scatterplot_with_smooth.png"), plot = scatterplot_with_smooth, width = 10, height = 6)

# Barplot with text over the bars for census_region
barplot_with_text <- ggplot(elections_cleaned, aes(x = factor(census_region))) +
  geom_bar(stat = "count", fill = "skyblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Barplot with Text Over the Bars for Census Region",
       subtitle = "Count of observations by census region",
       x = "Census Region",
       y = "Count",
       caption = "Data source: elections dataset") +
  theme_minimal()
print(barplot_with_text)

# Save the barplot using ggsave
ggsave(filename = here("Simona Celik", "barplot_with_text.png"), plot = barplot_with_text, width = 10, height = 6)

# Facet wrap for per_gop_2020 by census_region
facet_plot <- ggplot(elections_cleaned, aes(x = factor(census_region), y = per_gop_2020)) +
  geom_boxplot() +
  facet_wrap(~census_region, scales = "free_y", ncol = 2) +
  labs(title = "Facet Wrap Plot for GOP Vote Share 2020 by Census Region",
       subtitle = "Boxplots showing the distribution by region",
       x = "Census Region",
       y = "GOP Vote Share 2020",
       caption = "Data source: elections dataset") +
  theme_minimal()
print(facet_plot)

# Save the faceted plot using ggsave
ggsave(filename = here("Simona Celik", "facet_plot.png"), plot = facet_plot, width = 12, height = 8)

# Jitter plot for travel_time with facet_wrap
jitter_plot_facet <- ggplot(elections_cleaned, aes(x = factor(state), y = travel_time)) +
  geom_jitter(width = 0.3, height = 0.2, alpha = 0.3, size = 1) +
  labs(title = "Jitter Plot for Travel Time by State",
       subtitle = "Visualizing the distribution of travel time across states",
       x = "State",
       y = "Travel Time (minutes)",  # Provide more clarity on the unit
       caption = "Data source: elections dataset") +
  theme_minimal() +
  facet_wrap(~census_region, scales = "free_x", ncol = 2) +  # Facet by census region for better comparison
  theme(panel.background = element_rect(fill = "white"))  # Set the panel background to white

# Save the jitter plot with facet_wrap using ggsave
ggsave(filename = here("Simona Celik", "jitter_plot_facet.png"), plot = jitter_plot_facet, width = 12, height = 8)


# Scatterplot with labeled points for hh_income and per_gop_2020 with facet_wrap
scatterplot_facet <- ggplot(elections_cleaned, aes(x = hh_income, y = per_gop_2020, label = state)) +
  geom_point(shape = 16, size = 3, color = "dodgerblue") +
  geom_text_repel(nudge_x = 1000, nudge_y = 0.2, size = 3, box.padding = 0.5) +
  labs(title = "Relationship between Household Income and GOP Vote Share in 2020",
       subtitle = "Scatterplot with Labeled Points",
       x = "Household Income ($)",
       y = "GOP Vote Share 2020 (%)",
       caption = "Data source: elections dataset") +
  theme_minimal() +
  facet_wrap(~census_region, scales = "free_x", ncol = 2)  # Facet by census region for better comparison

# Save the scatterplot with smoothing line using ggsave
ggsave(filename = here("Simona Celik", "scatterplot_facet.png"), plot = scatterplot_facet, width = 10, height = 6)

# Barplot with text over the bars for census_region with facet_wrap
barplot_with_text_facet <- ggplot(elections_cleaned, aes(x = factor(census_region))) +
  geom_bar(stat = "count", fill = "skyblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Barplot with Text Over the Bars for Census Region",
       subtitle = "Count of observations by census region",
       x = "Census Region",
       y = "Count",
       caption = "Data source: elections dataset") +
  theme_minimal() +
  facet_wrap(~census_region, scales = "free_x", ncol = 2) +  # Facet by census region for better comparison
  theme(panel.background = element_rect(fill = "white"))  # Set the panel background to white

# Save the barplot with text over the bars using ggsave
ggsave(filename = here("Simona Celik", "barplot_with_text_facet.png"), plot = barplot_with_text_facet, width = 10, height = 6)

