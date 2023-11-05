## Week 6 
# Tasks: make scatterplot with smoothing line, coefficient plot (pointrange), marginal effect plots, regression table with 3 models using modelsummary package

install.packages("modelsummary")
library(ggplot2)

elections <- juanr::elections

View(elections)

### Scatterplot with smoothing line ###

ggplot(elections, aes(x = black, y = per_dem_2012)) + geom_point() + 
  geom_smooth(method = "lm") +
  labs( title = "% of Black People and Vote for Democratic Party in 2012") +
  theme_bw()

ggsave(filename = "figures/Zeynep Plots/Week_6/ScatterPlot_Smoothing.pdf")


### Coefficient Plot ###

# Create a model (regress per_dem_2020 on hh_income + black + female + per_dem_2016 + population)

m1 <- lm(per_dem_2012 ~ hh_income + pop + black  ,  data = elections)

#library(dotwhisker) and dwplot(m1) is another alternative but let's make it using ggplot

library(tidyverse)
library(broom)

# create a tibble
tidy_m1 <- tidy(m1)

# Create the coefficient plot
ggplot(tidy_m1, aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                  colour = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs( title = "Coefficient Plot for Vote for Democratic Party in 2012") +
  theme_bw()

ggsave(filename = "figures/Zeynep Plots/Week_6/CoefficientPlot.pdf")

### Marginal Effect Plot ###

install.packages("ggeffects")
library(ggeffects)

# Create another model

m2 <- lm(per_dem_2012 ~ black + pop , data = elections)

# get prediction values for each explanatory variable ( below plot function gave error when using more than one variable)

pred1 <- ggpredict(m2, terms = c("black [all]"))
pred2 <- ggpredict(m2, terms = c("pop [all]"))


ggplot(pred1, aes(x = x, y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  labs(x= "Black Population as %", y= "Vote for Democrat in 2012", title = "Marginal Effect of Black Population on Vote for Democratic Party") +
  theme_bw()

ggsave(filename = "figures/Zeynep Plots/Week_6/Margin_Effect_Black.pdf")

# Marginal Effect plot for district population

ggplot(pred2, aes(x = x, y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  labs(x= "District Population", y= "Vote for Democrat in 2012", title = "Marginal Effect of District Population on Vote for Democratic Party") +
  theme_bw()

ggsave(filename = "figures/Zeynep Plots/Week_6/Margin_Effect_Pop.pdf")

# alternative code for the same plots

library(sjPlot)
plot_model(m2, type = "pred", terms = "black")
plot_model(m2, type = "pred", terms = "pop")

### Regression Table ###

library(modelsummary)

# recall our previous models
m1 <- lm(per_dem_2012 ~ hh_income + pop + black  ,  data = elections)

m2 <- lm(per_dem_2012 ~ black + pop , data = elections)

m3 <- lm(per_dem_2012 ~ black , data = elections)

#Create a regression table
reg_table<-modelsummary(list(m1, m2, m3), title= "Regression Results for Democrat Vote Share in 2012", output = "figures/Zeynep Plots/Week_6/Regression_Table.png" )

#Below ggsave code did not work. I, then, add the output argument to the above code to save the table. that argument does not accept pdf format.
  #ggsave(filename = "figures/Zeynep Plots/Week_6/Regression_Table.pdf", plot = reg_table)
