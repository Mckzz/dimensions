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




#### line slope comparisons (just high res method)
rm(norm7)
str(norm7)
print(norm7)

#first pivots the withdths to long, leaving the areas as they were in excel data entry doc
widthlong <- pivot_longer(norm7, 
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

print(slopes, n=25)

#so that the pH width categories can be renamed
slopes$pHwidth <- as.character(slopes$pHwidth)


#extract area columns and reorder
justarea <- slopes[ , c(1,4,5)]
justarea <- arrange(justarea, pHarea)
justarea <- justarea %>% rename(area1 = area)
print(justarea)

justarea <- arrange(justarea, larva)

#bring area1 to slopes
slopes$area1 <- justarea$area1

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

#making means
a.means <- slopes %>% 
  group_by(pH) %>% 
  summarise(a.m = mean(area1))

print(a.means)

w.means <- slopes %>% 
  group_by(pH) %>% 
  summarise(w.m = mean(width))

print(w.means)


means <- a.means
print(means)

summary(slopes)

#bring area1 to slopes
means$w.m <- w.means$w.m

#make sd values
width.sds <- slopes %>%
  group_by(pH) %>%
  dplyr::summarize(
    w.sd = sd(width)) 

print(width.dsd)

area.sds <- slopes %>%
  group_by(pH) %>%
  dplyr::summarize(
    a.sd = sd(area1)) 

print(area.sds)

#combine width and area sds
stdvs <- width.sds
stdvs$a.sd <- area.sds$a.sd

print(stdvs)

means$w.sd <- stdvs$w.sd
means$a.sd <- stdvs$a.sd

print(means)

end.means <- means[-2, ]
print(end.means)

ggplot(data = slopes, aes(x= pH)) +
  geom_line(aes(y= width, group= larva, color= "width")) +
  geom_line(aes(y= area1, group= larva, color= "area")) +
  geom_point(data = means, aes(pH, a.m)) +
  geom_point(data = means, aes(pH, w.m)) +
  geom_errorbar(data=end.means, mapping=aes(x=pH, ymin=a.m-a.sd, ymax=a.m+a.sd), width=0.05, size=0.5) +
  geom_errorbar(data=end.means, mapping=aes(x=pH, ymin=w.m-w.sd, ymax=w.m+w.sd), width=0.05, size=0.5) +
  labs(x = "pH", y = "% change") + 
  labs(color="Dimension") +
  theme_classic()

