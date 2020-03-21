## UofT iSchool INF2178 PS4
## Author: Zhaotian Li, Yonghao Li, Hongtianxu Hua
## Date: 20 Mar 2020

library(tidyverse)
library(broom)
library(ggplot2)
library(performance)

eitc <- read.csv("https://raw.githubusercontent.com/georgehua/INF2178-PS/master/PS4/eitc.csv")

skim(eitc, work, year, children)

eitc <- eitc %>% drop_na()
eitc %>% summarise_all(.funs = funs(sum(is.na(.))))

# the EITC went into effect in the year 1994
# The EITC only affects women with at least one child, so the
# treatment group will be all women with children.
eitc <- eitc %>%
  mutate(post93 = year >= 1994, 
         anykids = children >= 1)

ggplot(eitc, aes(year, work, color = anykids)) +
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 1994) +
  theme_minimal()

model = lm(work ~ anykids*post93, data = eitc)

tidy(model)

check_model(model)

model_performance(model)
