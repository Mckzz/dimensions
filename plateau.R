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
print(pcts_norm7)

#first pivots the withdths to long, leaving the areas as they were in excel data entry doc
widthlong <- pivot_longer(pcts_norm7, 
                         cols=c(`pH6w`, `pH7w`, `pH8w`), 
                         names_to = "pHwidth", values_to = "width")

widthlong$pHwidth[widthlong$pHwidth == 'pH6w'] <- '6'
widthlong$pHwidth[widthlong$pHwidth == 'pH7w'] <- '7'
widthlong$pHwidth[widthlong$pHwidth == 'pH8w'] <- '8'
widthlong$pHwidth <- as.factor(widthlong$pHwidth)


print(widthlong, n= 30)

#then pivots the areas in widthlong1c to make the fully long form dataframe
slopes <- pivot_longer(widthlong, 
                         cols=c(`pH6a`, `pH7a`, `pH8a`), 
                         names_to = "pHarea", values_to = "area")


str(slopes)
print(slopes, n=50)

#so that the pH width categories can be renamed
slopes$pHwidth <- as.character(slopes$pHwidth)


#extract area columns and reorder
justarea <- slopes[ , c(1,4,5)]
print(justarea)
justarea <- arrange(justarea, pHarea)
print(justarea)
justarea <- justarea %>% rename(area1 = area)
print(justarea)

#bring area1 to slopes
slopes$area1 <- justarea$area1
print(slopes)

#don't need extra pH and area variables
slopes$pHarea <- NULL
slopes$area <- NULL

#gets rid of all duplicate rows
slopes <- unique( slopes[ ,  ] )
print(slopes)

#rename pHwidth to just "pH"
slopes <- slopes %>% rename(pH = pHwidth)

print(slopes)

slopes$pH <- as.numeric(slopes$pH)

ggplot(data = slopes, aes(x= pH)) +
  geom_line(aes(y= width, color= "red")) +
  geom_line(aes(y= area1, color= "blue")) +
  labs(x = "pH", y = "% change") + 
  labs(color="Dimension") +
  theme_classic()

