#### POL298 Assignment 3 ####

library(ggplot2)
library(wesanderson)
library(devtools)
install_github("jorvlan/raincloudplots")
library(ggrain)
library(fivethirtyeight)
library(dplyr)

set.seed(1986)

# make a raincloud plot
data(congress_age)

filtered_data <- congress_age %>%
  filter(party %in% c('D', 'I', 'R')) %>%
  group_by(party, chamber) %>%
  filter(n() > 1)

RaincloudPlot <- ggplot(filtered_data, aes(x = party, y = age, fill = chamber)) + 
  geom_rain() +
  theme_minimal() +
  labs(title = "Age Distribution of Members of Congress by Party and Chamber",
       y = "Age",
       x = "Party",
       fill = "Chamber") +
  scale_x_discrete(labels = c(D = "Democrat", I = "Independent", R = "Republican")) +
  scale_fill_discrete(labels = c(house = "House", senate = "Senate"))
ggsave("RaincloudPlot.pdf", plot = RaincloudPlot)
