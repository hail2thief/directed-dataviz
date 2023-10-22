### PURPOSE: Directed Reading, Fall 2023
### BY: Simona Celik
### DATE: October, 2023

# Load the required library
library(tidyverse)
library(here)
library(ggplot2)
library(dplyr)

# Read the final data file
df = read_rds(here("Simona Celik", "final_path.rds"))

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

# Create the bar plot showing proportion of people in public vs private sector, across countries
pct_public = df |> 
  group_by(COUNTRY, public_sector) |> 
  tally() |> 
  drop_na() |> 
  mutate(pct = n/sum(n))

ggplot(pct_public, aes(y = fct_rev(COUNTRY), x = pct, fill = public_sector)) + 
  geom_bar(position = "stack", stat = "identity", color = "white") + 
  theme_minimal() + 
  scale_fill_manual(values = c("private" = "blue", "public" = "orange")) +
  theme(legend.position = "top") +
  scale_x_continuous(labels = scales::percent) + 
  labs(x = NULL, y = NULL)

# Save the plot  
ggsave(file = here("Figures", "barplot_public_vs_private.png"), height = 10, width = 6)

# Filter out rows with NAs in both public_sector and income columns
df_filtered <- df[!is.na(df$public_sector) & !is.na(df$income), ]

# Calculate average income for a public sector
average_income_public <- mean(df_filtered$income[df_filtered$public_sector == "public"], na.rm = TRUE)

# Calculate average income for a private sector
average_income_private <- mean(df_filtered$income[df_filtered$public_sector == "private"], na.rm = TRUE)

# Create a data frame for plotting
average_incomes <- c(average_income_public, average_income_private)
sector_labels <- c("Public", "Private")
plot_data <- data.frame(Sector = sector_labels, Average_Income = average_incomes)

# Create a bar plot for average income by sector
bar_plot <- ggplot(data = plot_data, aes(x = Sector, y = Average_Income, fill = Sector)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Income by Sector",
    x = "Sector",
    y = "Average Income"
  ) +
  scale_fill_manual(values = c("blue", "green")) +
  geom_text(
    aes(label = round(Average_Income, 2)),
    position = position_stack(vjust = 0.5),
    size = 5,
    color = "white"
  ) +
  theme_minimal()

print(bar_plot)

# Save the plot using ggsave
ggsave(filename = here("Simona Celik", "barplot_income_by_sector.png"), plot = bar_plot,
       width = 10, height = 6)

# Remove NAs from the data frame
df_filtered <- drop_na(df, public_sector, REGION)

# Create a stacked barplot to show the distribution of public vs. private sector within regions
stacked_barplot <- ggplot(df_filtered, aes(x = REGION, fill = public_sector)) +
  geom_bar(position = "fill", na.rm = TRUE) +  # Add na.rm = TRUE to remove NAs
  labs(
    title = "Distribution of Public and Private Sectors by Region",
    x = "Region",
    y = "Proportion",
    fill = "Sector"
  ) +
  scale_fill_manual(values = c("blue", "orange")) +
  theme_minimal()

print(stacked_barplot)

# Save the stacked barplot
ggsave(filename = here("Simona Celik", "stacked_barplot.png"), plot = stacked_barplot,
       width = 10, height = 6)

# Filter out rows with missing values in the variables of interest
df_filtered <- df %>% select(CORRUPTION_INDEX, GINI, age, income) %>% drop_na()

# Calculate the correlation matrix
correlation_matrix <- cor(df_filtered)

# Convert the correlation matrix to a data frame
correlation_data <- as.data.frame(correlation_matrix)
correlation_data$Variable1 <- rownames(correlation_matrix)

# Reshape the data to long format
correlation_data_long <- pivot_longer(correlation_data, cols = -Variable1, names_to = "Variable2", values_to = "Correlation")

# Create a heat map to visualize the correlation matrix
heat_map <- ggplot(data = correlation_data_long, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Correlation Heat Map",
    x = "Variable",
    y = "Variable"
  ) +
  theme_minimal()

# Print the heat map
print(heat_map)

# Save the heat map
ggsave(filename = here("Simona Celik", "heat_map.png"), plot = heat_map,
       width = 10, height = 6)

# Visualize the joint influence of REGION and REGIME on support for democracy
nested_plot <- ggplot(data = df, aes(x = REGION, fill = REGIME)) +
  geom_bar(position = "fill") +
  labs(
    x = "REGION",
    y = "Proportion of Support for Democracy",
    title = "Support for Democracy by Region and Regime (Nested Area Plot)"
  )

print(nested_plot)

# Save the nested area plot using ggsave
ggsave(filename = here("Simona Celik", "nested_area_plot.png"), plot = nested_plot,
       width = 10, height = 6)

#Week 4

# Create a histogram for the "REGIME" variable
histogram_plot <- ggplot(df, aes(x = REGIME)) +
  geom_bar(fill = "blue", color = "black") +
  labs(
    x = "Regime Type",
    y = "Frequency",
    title = "Bar Plot of REGIME Variable"
  ) +
  theme_minimal()

print(histogram_plot)

# Save the plot using ggsave
ggsave(filename = here("Simona Celik", "histogram_regime.png"), plot = histogram_plot,
       width = 10, height = 6)

# Filter out rows with missing values in the 'income' variable
df_filtered <- df[!is.na(df$income), ]

# Create a density plot for 'income' with missing values removed
density_plot <- ggplot(df_filtered, aes(x = income)) +
  geom_density(fill = "lightgreen", color = "black") +
  labs(
    x = "Income",
    y = "Density",
    title = "Density Plot of Income (Missing Values Removed)"
  )

print(density_plot)

# Save the density plot using ggsave
ggsave(filename = here("Simona Celik", "density_plot_income.png"), plot = density_plot,
       width = 10, height = 6)

# Create a Grouped Histogram (Regime Type)
grouped_histogram_regime <- ggplot(df, aes(x = REGIME, fill = REGION)) +
  geom_bar(position = "dodge", color = "black") +
  labs(
    x = "Regime Type",
    y = "Frequency",
    title = "Grouped Histogram of REGIME Variable by Region"
  ) +
  theme_minimal()

print(grouped_histogram_regime)

# Save the grouped histogram using ggsave
ggsave(filename = here("Simona Celik", "grouped_histogram_regime.png"), plot = grouped_histogram_regime,
       width = 10, height = 6)

# Create a Group Density Plot (Income)

grouped_density_plot_income <- ggplot(df_filtered, aes(x = income, fill = REGION)) +
  geom_density(position = "dodge", color = "black") +
  labs(
    x = "Income",
    y = "Density",
    title = "Grouped Density Plot of Income (Missing Values Removed) by Region"
  )

print(grouped_density_plot_income)

# Save the grouped density plot using ggsave
ggsave(filename = here("Simona Celik", "grouped_density_plot_income.png"), plot = grouped_density_plot_income,
       width = 10, height = 6)

# Create the box plot showing distribution of corruption index across countries
corruption_index_plot <- ggplot(df, aes(x = COUNTRY, y = CORRUPTION_INDEX)) +
  geom_boxplot() +  
  labs(
    title = "Corruption Index by Country", 
    x = "Country",  
    y = "Corruption Index"  
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(corruption_index_plot)

# Save the box plot using ggsave
ggsave(filename = here("Simona Celik", "corruption_index_plot.png"), plot = corruption_index_plot,
       width = 10, height = 6)

# Create a violin plot for the 'CORRUPTION_INDEX' variable
violin_plot <- ggplot(df, aes(x = "Corruption Index", y = CORRUPTION_INDEX)) +
  geom_violin(fill = "lightblue") +
  labs(
    x = "Variable",
    y = "Corruption Index",
    title = "Violin Plot of Corruption Index"
  )

print(violin_plot)

# Save the violin plot using ggsave
ggsave(filename = here("Simona Celik", "violin_plot.png"), plot = violin_plot,
       width = 10, height = 6)

