install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
rm(Hmisc)
install.packages("Hmisc")

####area plateaus

is.data.frame(larva1e_profiles)
str(larva1e_profiles)

dime1e <- pivot_longer(larva1e_profiles, 
                       cols=c(`sac.area6`, `sac.area7`, `sac.area8`), 
                       names_to = "pH", values_to = "area")

str(dime1e)
print(dime1e, n=50)

#so that $pH could be assigned colours
dime1e$pH <- as.factor(dime1e$pH)

ggplot(data = dime1c, aes(min, area, group = pH)) +
  geom_line(aes(colour = factor(pH))) +
  labs(x = "min", y = "area") + 
  theme_classic()
