install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
install.packages("Hmisc")


is.data.frame(sqcapfccp)
str(sqcapfccp)

dime <- pivot_longer(sqcapfccp, cols=c(`sac.area6`, `sac.area7`, `sac.area8`), names_to = "pH", values_to = "area")

str(dime)
print(dime, n=50)

dime$pH <- as.factor(dime$pH)

ggplot(data = dime, aes(sec, area, group = pH)) +
  geom_line(aes(colour = factor(pH))) +
  labs(x = "sec", y = "area") + 
  theme(text = element_text(size = 15), 
        axis.text = element_text(size = 12), aspect.ratio = 0.75) +
  theme_classic()
