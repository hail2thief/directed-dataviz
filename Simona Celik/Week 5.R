### PURPOSE: Directed Reading, Fall 2023, Week 5
### BY: Simona Celik
### DATE: October 27, 2023

# Load the required library
library(tidyverse)
library(here)
library(ggplot2)
library(dplyr)
library(corrplot)

load("C:/Users/simon/Downloads/gdp.rda")
# View the objects loaded from the .rda file
str(gdp)      # Using str() to view the structure of the object
summary(gdp)  # Using summary() to get a summary of the object
df <- gdp
View(df)
unique_countries <- unique(df$country)
print(unique_countries)

# Filter the data fo "Argentina" and "Costa Rica"
argentina <- df[df$country == "Argentina", ]

costa_rica <- df[df$country == "Costa Rica", ]

# Create a scatterplot to visualize the relationship between year and inflation rate
scatterplot <- ggplot(data = df, aes(x = year, y = e_miinflat)) +
  geom_point() +
  labs(x = "Year", y = "Inflation Rate") +
  ggtitle("Scatterplot of Year vs. Inflation Rate")

# Save the plot using ggsave
ggsave(filename = here("Simona Celik", "scatterplot.png"), plot = scatterplot, width = 10, height = 6)

# Create a scatterplot with ggplot for Costa Rica
ggplot(data = costa_rica, aes(x = year, y = e_mipopula)) +
  geom_point() +
  labs(x = "Year", y = "Population") +
  ggtitle("Scatterplot of Year vs. Population in Costa Rica")

# Save the plot with a complete file path
ggsave(filename = here("Simona Celik", "scatterplot_costa_rica.png"), width = 10, height = 6)


# Calculate the correlation matrix, excluding missing values
correlation_matrix <- cor(df[, c("e_miinflat", "e_mipopula", "e_miurbpop", "e_peinfmor", "e_pelifeex", "e_total_oil_income_pc", "e_cow_imports", "e_cow_exports")], use = "complete.obs")

# Create the correlogram
correlogram <- corrplot(correlation_matrix, method = "color", type = "lower", 
         tl.cex = 0.7, tl.col = "black", tl.srt = 45, 
         col = colorRampPalette(c("blue", "white", "red"))(100), 
         diag = FALSE, order = "hclust")

# Save the plot as a PNG file in the "Simona Celik" directory
png(filename = here("Simona Celik", "correlogram.png"), width = 800, height = 800)
dev.off()

# Create a time series plot for "Argentina" using ggplot2
argentina_time_series <- ggplot(data = argentina, aes(x = year, y = e_miinflat)) +
  geom_line() +
  labs(x = "Year", y = "Inflation Rate", title = "Time Series Plot of Inflation Rate for Argentina")

# Save the plot with ggsave
ggsave(filename = here("Simona Celik", "argentina_time_series.png"), plot = argentina_time_series, width = 8, height = 6)

# Create a time series plot for multiple response variables for Argentina 
time_series_plot_2 <- argentina %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = e_migdppc, color = "GDP per Capita"), linetype = "solid") +
  geom_line(aes(y = e_miinflat, color = "Inflation Rate"), linetype = "dashed") +
  labs(x = "Year", y = "Value", title = "Time Series of Multiple Response Variables") +
  scale_color_manual(values = c("GDP per Capita" = "blue", "Inflation Rate" = "red")) +
  scale_linetype_manual(values = c("GDP per Capita" = "solid", "Inflation Rate" = "dashed")) +
  theme_minimal()

# Save the plot 
ggsave(filename = here("Simona Celik", "argentina_time_series_2.png"), plot = time_series_plot_2, width = 8, height = 6, bg = "white")

# Create a new variable to store the country groups
df$country_group <- ifelse(df$country %in% c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
  "Ecuador", "Peru", "Paraguay", "Uruguay", "Venezuela"
), "South American Countries", "Central American and Caribbean Countries")

# Stacked line graph for total GDP and contributions for country groups
stacked_line_graph_group <- ggplot(df, aes(x = year)) +
  geom_area(aes(y = e_migdppc, fill = country_group)) +
  labs(x = "Year", y = "GDP per Capita", title = "Stacked Line Graph: Total GDP and Contributions Over Time for Country Groups") +
  scale_fill_brewer(palette = "Set1") +  # Customize the color palette
  theme_bw()  # Use a white background

# Save the plot with ggsave
ggsave(filename = here("Simona Celik", "stacked_line_graph_group.png"), plot = stacked_line_graph_group, width = 8, height = 6)

# Create a multiple time series plot for multiple countries 
multiple_time_series_plot <- ggplot(df, aes(x = year, y = e_migdppc, color = country)) +
  geom_line() +
  labs(x = "Year", y = "GDP per Capita", title = "GDP Per Capita Over Time for Selected South American Countries") +
  theme_bw() +  # Set the background to white
  scale_color_manual(values = c("Argentina" = "blue", "Brazil" = "red", "Chile" = "green", "Peru" = "purple"))

# Save the plot with ggsave
ggsave(filename = here("Simona Celik", "multiple_time_series_plot.png"), plot = multiple_time_series_plot, width = 8, height = 6)

