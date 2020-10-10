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
