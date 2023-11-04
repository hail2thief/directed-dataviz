### PURPOSE: Directed Reading, Fall 2023, Week 6
### BY: Simona Celik
### DATE: November 3, 2023

# Load the required libraries
library(ggplot2)
library(margins)
library(stargazer)

# Load the data 
load("C:/Users/simon/Downloads/elections.rda")
# View the structure of the loaded data
str(elections)

# Access specific components of the dataset
head(elections)  # View the first few rows

# Assuming your data frame is named "elections," you can use names()
variable_names <- names(elections)

# Print the list of variable names
print(variable_names)

#Make a Scatterplot with Smoothing Line
elections_scatterplot <- ggplot(elections, aes(x = black, y = per_dem_2020)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(x = "Percentage Black Population", y = "Percentage Democratic 2020")

#Save the scatterplot
ggsave("elections_scatterplot.png", plot = elections_scatterplot)

##An upward slope suggests a positive relationship, meaning that as the percentage of the Black population increases, the percentage of Democratic votes tends to increase. 

model <- lm(per_dem_2020 ~ black, data = elections)
coefficients <- tidy(model)

coefficient_plot <- ggplot(coefficients, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(x = "Coefficient", y = "Estimate") +
  coord_flip() +
  theme_minimal() +  # Use the minimal theme
  theme(panel.background = element_rect(fill = "white"))  # Set the background to white

# Save the coefficient plot as an image
ggsave("coefficient_plot.png", plot = coefficient_plot)

# Creating a ggplot for average marginal effects for the model with "black" and "hh_income" as predictors.

# Fit your regression model with "black" and "hh_income" asa predictors
model2 <- lm(per_dem_2020 ~ black + hh_income, data = elections)

# Calculate marginal effects for "black" and "hh_income"
marginal_effects <- margins(model2, variables = c("black", "hh_income"))

# Convert the summary to a tibble
marginal_effects_summary <- as_tibble(summary(marginal_effects))

# Adjust variable names manually
marginal_effects_summary$factor <- sub("^black:", "", marginal_effects_summary$factor)
marginal_effects_summary$factor <- sub("^hh_income:", "", marginal_effects_summary$factor)
marginal_effects_summary$factor <- sub("^`", "", marginal_effects_summary$factor)


# Create the marginal effect plot using ggplot2
p <- ggplot(data = marginal_effects_summary, aes(x = reorder(factor, AME),
                                                 y = AME, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, color = "gray80") +
  geom_pointrange() + coord_flip() +
  labs(x = NULL, y = "Average Marginal Effect")

# Save the plot as an image
ggsave("marginal_effect_plot.png", plot = p, width = 6, height = 4, units = "in")

# Regression Table with 3 Models Using the modelsummary Package
# Fit three linear regression models with different predictors
model1 <- lm(per_dem_2020 ~ black, data = elections)
model2 <- lm(per_dem_2020 ~ black + hh_income, data = elections)
model3 <- lm(per_dem_2020 ~ black + hh_income + travel_time, data = elections)

# Create a summary table for the three models
model_summary <- modelsummary(list(model1, model2, model3))
model_summary

# Save the regression summary as a table using stargazer
stargazer(model1, model2, model3, title = "Regression Models", type = "latex", 
          out = "model_summary.tex", 
          style = "default")  # You can change the style if needed

