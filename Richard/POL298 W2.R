#### Data Viz W2 ####

library(ggplot2)
library(AppliedPredictiveModeling)

data(abalone)
head(abalone)

subsetabalone <- abalone[abalone$Type %in% c('M', 'F'), ]

ggplot(data = subsetabalone)

ggplot(data = subsetabalone, aes(x = LongestShell, y = WholeWeight))

ggplot(data = subsetabalone, aes(x = LongestShell, y = WholeWeight)) + geom_point()

ggplot(data = subsetabalone, aes(x = LongestShell, y = WholeWeight, size = Rings)) + 
  geom_point()

ggplot(data = subsetabalone, aes(x = LongestShell, y = WholeWeight, size = Rings, color = Type)) + 
  geom_point()

ggplot(data = subsetabalone, aes(x = LongestShell, y = WholeWeight, size = Rings, color = Type)) + 
  geom_point(alpha = 0.6) + labs(x = "Shell Length", y = "Total Weight", 
                                 title = "Growth of Abalone", size = "Age",
                                 color = "Sex")

ggplot(data = subsetabalone, aes(x = LongestShell, y = WholeWeight, size = Rings, color = Type)) + 
  geom_point(alpha = 0.6) + labs(x = "Shell Length", y = "Total Weight", 
                                 title = "Growth of Abalone", size = "Age",
                                 color = "Sex") + theme_classic()
