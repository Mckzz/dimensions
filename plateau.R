install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
install.packages("Hmisc")


is.data.frame(larva1b)
str(larva1b)

dime1b <- pivot_longer(larva1b, 
                       cols=c(`sac.area6`, `sac.area7`, `sac.area8`), 
                       names_to = "pH", values_to = "area")

str(dime1b)
print(dime1b, n=50)

dime1b$pH <- as.factor(dime1b$pH)

ggplot(data = dime1b, aes(min, area, group = pH)) +
  geom_line(aes(colour = factor(pH))) +
  labs(x = "min", y = "area") + 
  theme_classic()
