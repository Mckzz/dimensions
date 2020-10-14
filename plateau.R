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

dime1c$pH <- as.factor(dime1c$pH)

ggplot(data = dime1c, aes(min, area, group = pH)) +
  geom_line(aes(colour = factor(pH))) +
  labs(x = "min", y = "area") + 
  theme_classic()


####line slope comparisons

str(larva1c_combined)

#first pivots the withdths to long, leaving the areas as they were in excel data entry doc
widthlong1c <- pivot_longer(larva1c_combined, 
                         cols=c(`pH6w`, `pH7w`, `pH8w`), 
                         names_to = "pH width", values_to = "width")
#then pivots the areas in widthlong1c 
slopes1c <- pivot_longer(widthlong1c, 
                         cols=c(`pH6a`, `pH7a`, `pH8a`), 
                         names_to = "pH area", values_to = "area")

str(slopes1c)
print(slopes1c, n=50)




