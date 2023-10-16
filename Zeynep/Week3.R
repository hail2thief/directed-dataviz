# Week 3

library(juanr)
trade <- juanr::trade

#Clean dataframe and filter it with year 2010
trade_year <- subset(trade, year == 2010 & !is.na(pop) & !is.na(exports) & !is.na(imports))

#take log of exports and imports
trade_year$log_exports <- log(trade_year$exports)
trade_year$log_imports <- log(trade_year$imports)

#Plot
library(ggplot2)
ggplot(data = trade_year, aes(x= log_exports, y=log_imports, size=pop, color=gdp)) + geom_point()  + geom_label(data = filter(trade_year, country == "Turkey"), aes(label = country)) + labs(x="Export", y="Import", title="Turkey in Global Export vs Imports 2010") + theme_bw()

#above code gave error (Error: object 'country' not found) . Let's try this:

trade_year$country1 <- trade_year$country
ggplot(data = trade_year, aes(x= log_exports, y=log_imports, size=pop, color=gdp)) + geom_point()  + geom_label(data = filter(trade_year, country1 == "Turkey"), aes(label = country1)) + labs(x="Export", y="Import", title="Turkey in Global Export vs Imports 2010") + theme_bw()

#Save the plot
ggsave(filename = "figures/turkey_trade.pdf")