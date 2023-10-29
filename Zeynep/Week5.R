library(ggplot2)
install.packages("corrplot")


trade <- juanr::trade

# Tasks: make scatterplot, correlogram, time series, multiple time series

### Scatterplot

trade_2010 <- subset(trade, year == 2010)
trade_2010$log_exports <- log(trade_2010$exports)
trade_2010$log_imports <- log(trade_2010$imports)
trade_2010$log_gdp <- log(trade_2010$gdp)

ggplot(trade_2010,aes(x=log_exports, y=log_gdp) ) +
  geom_point() +
  labs(x= "Export", y= "GDP", title = "Worldwide Exports and GDP in 2010") +
  theme_bw()

ggsave(filename = "figures/Zeynep_scatterplot.pdf")

### Correlogram
library(dplyr)
library(corrplot)

# Exclude year and counrty variables

trade_2010_numeric <- trade_2010 %>% 
  select(-c(year, country))
trade_2010_numeric <- na.omit(trade_2010_numeric)

# Create a matrix for all correlations in our data

cor_matrix <- cor(trade_2010_numeric)

# Plot the Correlogram
corrplot(cor_matrix, method = "circle", type="upper", order="hclust")

# Correlogram using ggplot library

install.packages("GGally")
library(GGally)

ggcorr(trade_2010_numeric, label = TRUE) +
  labs(title = "Correlations for Trade Data")

ggsave(filename = "figures/Zeynep_Correlogram.pdf")

### Time Series Plot

trade_Qatar <- subset(trade, country == "Qatar") 

ggplot(trade_Qatar, aes(x = year, y = exports)) +
  geom_line(size = 1, color = "blue") +
  labs(title = "Qatar Exports Over Time",
       x = "Year",
       y = "Export") +
  theme_minimal()
ggsave(filename = "figures/Zeynep_TimeSeries.pdf")

### Multiple Time Series Plot

# Filter Data 
trade_multiple <- subset(trade, country %in% c("Qatar", "United States of America", "China", "Russia", "Brazil"))

# Plot
ggplot(trade_multiple, aes(x = year, y = exports, color = country)) +
  geom_line(size = 1,) +
  labs(title = "Exports Over Time",
       x = "Year",
       y = "Export") +
  theme_minimal()

#Save
ggsave(filename = "figures/Zeynep_MultipleTimeSeries.pdf")
