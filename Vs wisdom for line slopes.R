####################### Vikram's wisdom ############################

library(tidyverse)
norm7 <- read_csv("./norm7.csv")

##### tidy up the data ####
## the end goal is 4 columns and 30 rows:
## 1) larva, 2) pH, 3) width, 4) area

## first pivot width measurements
tidy_widths <-
  pivot_longer(
    norm7,
    cols = c(`pH6w`, `pH7w`, `pH8w`),
    names_to = "pH", values_to = "width"
  ) %>%
  select(larva, pH, width) %>%
  mutate_at("pH", str_replace, "pH", "") %>% ## remove "pH"
  mutate_at("pH", str_replace, "w", "") ## remove "a"

print(tidy_widths)

## now areas
tidy_areas <-
  pivot_longer(
    norm7,
    cols = c(`pH6a`, `pH7a`, `pH8a`),
    names_to = "pH", values_to = "area"
  ) %>%
  select(larva, pH, area) %>%
  mutate_at("pH", str_replace, "pH", "") %>% ## remove "pH"
  mutate_at("pH", str_replace, "a", "") ## remove "a"

## join them together based on pH and larval identity
tidy_data <-
  left_join(tidy_widths, tidy_areas, by = c("larva", "pH")) %>%
  mutate(pH = as.double(pH)) ## keep pH as a numeric

print(tidy_data)

## this "tidy_data" should be the same as your "slopes" object
## let me know if I made a mistake!  

## compute means
means <-
  tidy_data %>%
  select(-larva) %>% ## exclude larva
  group_by(pH) %>% ## group by pH
  ## now compute mean and sd:
  summarize(across(everything(), 
                   tibble::lst(mean = mean, sd = sd))) 

print(means)
#### plotting ####
## plot with jitter
tidy_data %>%
  ggplot(aes(x= pH)) +
  geom_point(position = position_jitter(width = 0.03), 
             pch= 1, colour = "blue", 
             aes(y = width, group = larva)) +
  geom_point(position = position_jitter(width = 0.03), 
             pch= 1, colour = "red", 
             aes(y = area, group = larva)) +
  geom_line(data = means, size= 1, color= "red", 
            aes(pH, area_mean)) +
  geom_line(data = means, size= 1, color= "blue", 
            aes(pH, width_mean)) +
  geom_point(data = means, pch= 19, color= "red", size= 4, 
             aes(pH, area_mean)) +
  geom_point(data = means, pch= 19, color= "blue", size= 4, 
             aes(pH, width_mean)) +
  geom_errorbar(data = means, 
                mapping = aes(x = pH,
                              ymin = area_mean - area_sd,
                              ymax = area_mean + area_sd), 
                width = 0.05,
                size = 0.75) +
  geom_errorbar(data = means, 
                mapping = aes(x = pH,
                              ymin = width_mean - width_sd,
                              ymax = width_mean + width_sd), 
                width = 0.05,
                size = 0.75) +
  labs(x = "pH", y = "% change") +
  labs(color="Dimension") +
  theme_classic()

## no jitter
tidy_data %>%
  ggplot(aes(x= pH)) +
  geom_point(pch = 1, colour = "blue", 
             aes(y = width, group = larva)) +
  geom_point(pch = 1, colour = "red", 
             aes(y = area, group = larva)) +
  geom_line(data = means, size= 1, color= "red", 
            aes(pH, area_mean)) +
  geom_line(data = means, size= 1, color= "blue", 
            aes(pH, width_mean)) +
  geom_point(data = means, pch= 19, color= "red", size= 4, 
             aes(pH, area_mean)) +
  geom_point(data = means, pch= 19, color= "blue", size= 4, 
             aes(pH, width_mean)) +
  geom_errorbar(data = means, 
                mapping = aes(x = pH,
                              ymin = area_mean - area_sd,
                              ymax = area_mean + area_sd), 
                width = 0.05,
                size = 0.75) +
  geom_errorbar(data = means, 
                mapping = aes(x = pH,
                              ymin = width_mean - width_sd,
                              ymax = width_mean + width_sd), 
                width = 0.05,
                size = 0.75) +
  labs(x = "pH", y = "% change") +
  labs(color="Dimension") +
  theme_classic()


#### stats ####

## A few thoughts:

## given that all observations at pH 7 are 0 (reflecting how you 
## standardized things), I'd recommend against including them in
## a statistical model for now. If possible, would you be able
## to include these measurements (and rescale pH = 6, pH = 8 back
## to their original values)? I think it will actually help to have
## the data back in a more "raw" format.

## another thing is I would advise seeing if you can combine
## analyses of width and area together, since they were measured
## on the same individuals. Folding these two things into the same
## model will help guard against Type I (false positive) errors

## also, given that you have repeated measures from the same 
## individuals, I think it would be a good idea to incorporate
## individual identity into the statistical design if you can. We'll
## see if it actually impacts anything, tho

## I'll try to fold all of the above into the next few blocks of 
## code


## First remove pH = 7
stats_data <-
  tidy_data %>%
  filter(!pH == 7) %>%
  mutate(pH = as_factor(pH))

print(stats_data)

## Now "reshape" the data so that we can later analyze width
## and area jointly (this will hopefully make more sense at
## a later point)
install.packages("reshape2")
library(reshape2)
stats_data_reshaped <-
  reshape2::melt(
    stats_data,
    measure.vars = c("width", "area")
  )

str(stats_data_reshaped)
print(stats_data_reshaped)

## Let's try a simple model that does not account for individual
## larval identity
## Since the data were standardized so that pH 7 areas and widths
## are zero, I am going to fit a model without an intercept (using
## " -1" in the model formula). If you later add the actual values
## of pH = 7 back in, then you should not set up the model this way.
## We can only do this for now specifically because the data were
## centered at 0 beforehand.
mod1 <-
  lm(value ~ variable:pH - 1, 
     data = stats_data_reshaped)
summary(mod1)

## what this model is saying is that area is strongly affected but
## width is not (which is confirmed by the plot you had made). When
## pH = 6, area is reduced by ~17.34. When pH = 8, area is increased
## by ~14.69. Changes to width do not seem to be substantive enough 
## to be signficant (p-values > 0.05)

## let's see if we can incorporate individual identity and whether 
## that seems to matter:

## Fit a linear mixed model
install.packages("MCMCglmm")

mcmod <-
  MCMCglmm::MCMCglmm(
    value ~ variable:pH - 1, random = ~larva,
    data = stats_data_reshaped, scale = FALSE,
    nitt = 130000, thin = 100, burnin = 30000, 
    verbose = FALSE
  )
summary(mcmod)
## the main effects from this model (the post.mean in the summary 
## table) are very close to what we saw in the linear model

## how important are the random effects (individuals):
mean(mcmod$VCV[,1]/(mcmod$VCV[,1] + mcmod$VCV[,2]))
## very little (0.4%) of the residual variance is informed by
## the identity of the larvae; in other words, individual identity
## of the larvae does not have a strong effect on the data


## bonus plot!
## Here's a visualization of the model's effects (with 95% CIs):

## first gather the data
mcmod_model_dat <- 
  mcmod$Sol %>%
  as_tibble() %>%
  rename(pH6_width = `variablewidth:pH6`, 
         pH6_area = `variablearea:pH6`, 
         pH8_width = `variablewidth:pH8`,
         pH8_area = `variablearea:pH8`) %>%
  gather() 

## we'll use stat_halfeye from "tidybayes" in the plot
## it makes cool density plots of what the means of each
## category are estimated to be after the model has been fit
install.packages("tidybayes")
library(tidybayes)

## now the actual plot
mcmod_model_dat %>%
  ggplot(aes(x = value, y = key, fill = key)) +
  stat_halfeye(.width = 0.95, slab_colour = "black", 
               interval_size = 0.75, slab_size = 0.3) +
  scale_y_discrete(expand = c(0.05, 0))+
  xlab("effect estimate") +
  ylab("fixed effect") +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.ticks.x = element_line(size = 0.3),
    axis.ticks.y = element_blank(),
    axis.ticks.length = unit(0.1, "cm"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )






