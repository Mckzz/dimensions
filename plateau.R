install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
install.packages("Hmisc")

####area plateaus

is.data.frame(larva1c)
str(larva1c)

dime1c <- pivot_longer(larva1c, 
                       cols=c(`sac.area6`, `sac.area7`, `sac.area8`), 
                       names_to = "pH", values_to = "area")

str(dime1c)
print(dime1c, n=50)

#so that $pH could be assigned colours
dime1c$pH <- as.factor(dime1c$pH)

ggplot(data = dime1c, aes(min, area, group = pH)) +
  geom_line(aes(colour = factor(pH))) +
  labs(x = "min", y = "area") + 
  theme_classic()


#### line slope comparisons
rm(widthlong1c)

str(pcts_norm7)

#first pivots the withdths to long, leaving the areas as they were in excel data entry doc
widthlong <- pivot_longer(pcts_norm7, 
                         cols=c(`pH6w`, `pH7w`, `pH8w`), 
                         names_to = "pHwidth", values_to = "width")
#then pivots the areas in widthlong1c to make the fully long form dataframe
slopes <- pivot_longer(widthlong, 
                         cols=c(`pH6a`, `pH7a`, `pH8a`), 
                         names_to = "pHarea", values_to = "area")


str(slopes)
print(slopes, n=50)


slopes$pHwidth <- as.character(slopes$pHwidth)

slopes$pHwidth[slopes$pHwidth == 'pH6w'] <- '6'
slopes$pHwidth[slopes$pHwidth == 'pH7w'] <- '7'
slopes$pHwidth[slopes$pHwidth == 'pH8w'] <- '8'
slopes$pHwidth <- as.factor(slopes$pHwidth)

slopes$pHarea <- NULL







