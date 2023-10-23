#devtools::install_github("hail2thief/juanr")


library(tidyverse)
library(readr)
library(dplyr)


# Load Data (Trade data from Juan's R package)

load("/Users/zeyneponat/Desktop/UC Davis/COURSES/Data Visualization with Juan/Assignments/trade.rda")

# Clean dataframe

trade_year <- trade[trade$year == 2010,]
trade_year <- trade_year[!is.na(trade_year$pop), ]
trade_year <- trade_year[!is.na(trade_year$exports), ]
trade_year <- trade_year[!is.na(trade_year$imports), ]

# Plot data
ggplot(data = trade_year) 
ggplot(data = trade_year, aes(x= exports, y=imports)) + geom_point()
ggplot(data = trade_year, aes(x= exports, y=imports)) + geom_point() + labs(x="Export", y="Import", title="Worldwide Export vs Imports in 2010")
ggplot(data = trade_year, aes(x= exports, y=imports, size=pop)) + geom_point() + labs(x="Export", y="Import", title="Worldwide Export vs Imports 2010") 
ggplot(data = trade_year, aes(x= exports, y=imports, size=pop, color=gdp)) + geom_point()  + labs(x="Export", y="Import", title="Worldwide Export vs Imports 2010") 
ggplot(data = trade_year, aes(x= exports, y=imports, size=pop, color=gdp, label=country)) + geom_point()  + geom_text() + labs(x="Export", y="Import", title="Worldwide Export vs Imports 2010") 
ggplot(data = trade_year, aes(x= exports, y=imports, size=pop, color=gdp, label=country)) + geom_point()  + geom_text() + labs(x="Export", y="Import", title="Worldwide Export vs Imports 2010") + theme_linedraw()

#Exclude top three countries with extreme imports/exports and replot

trade_year <- trade_year[trade_year$exports<=700000, ]
ggplot(data = trade_year, aes(x= exports, y=imports, size=pop, color=gdp, label=country)) + geom_point()  + geom_text() + labs(x="Export", y="Import", title="Worldwide Export vs Imports 2010") + theme_linedraw()


