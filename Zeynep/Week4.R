library(ggplot2)
library(RColorBrewer)
library(juanr)
trade <- juanr::trade

#Clean dataframe and filter it with year 2010
trade_year <- subset(trade, year == 2010 & !is.na(pop) & !is.na(exports) & !is.na(imports))

#take log of exports and imports
trade_year$log_exports <- log(trade_year$exports)
trade_year$log_imports <- log(trade_year$imports)
trade_year$log_gdp <- log(trade_year$gdp)

#Plot a histogram (without a color)

ggplot(trade_year, aes(x=log_exports)) + geom_histogram() +
  labs(x="Export", title="Worldwide GDP and Exports in 2010")

ggsave(filename = "figures/Zeynep_histogram.pdf")

# Create some categorical groups(this dataset does not have any) for coloring histogram
library(dplyr)

trade_year<- trade_year %>% mutate(
  gdp_group = case_when(
    log_gdp <= 23 ~ "low",
    log_gdp > 23 & log_gdp <= 25 ~ "middle",
    log_gdp > 25 & log_gdp <= 27 ~ "upper middle",
    log_gdp > 27 ~ "high"
  )
)

# plot a colored(grouped) histogram

ggplot(trade_year, aes(x=log_exports, fill=gdp_group)) +
         geom_histogram() + 
         scale_fill_brewer(palette="Blues") +
  labs(x="Export", title="Worldwide GDP and Exports in 2010")

ggsave(filename = "figures/Zeynep_histogram_colored.pdf")

#create a density ridge plot
library(ggridges)

ggplot(trade_year, aes(y = gdp_group, x = log_exports)) + geom_density_ridges() +
  labs(x="Export", title="Worldwide GDP and Exports in 2010")
  
ggsave(filename = "figures/Zeynep_density_ridge.pdf")

#create a boxplot with color

ggplot(trade_year, aes(y=gdp_group, x=log_exports, color=gdp_group)) +
  geom_boxplot() +
  labs(x="Export", y="GDP", title="Worldwide GDP and Exports in 2010")

ggsave(filename = "figures/Zeynep_boxplot_colored.pdf")

#create a violin plot
ggplot(trade_year, aes(x =log_exports , y = gdp_group)) +
  geom_violin() +
  labs(x="Export", y="GDP", title="Worldwide GDP and Exports in 2010")
  
ggsave(filename = "figures/Zeynep_violinplot.pdf")

