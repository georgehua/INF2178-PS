library(tidyverse)
library(ggplot2)
library(foreign)

library(skimr)

library(readstata13)
dat <- read.dta13("https://github.com/georgehua/INF2178-PS/blob/master/PS4/eitc.dta")

eitc <- read.dta("https://github.com/georgehua/INF2178-PS/blob/master/PS4/eitc.dta")


dataf = haven::read_dta('eitc.dta')
skim(dataf, work, year, children)

dataf = dataf %>%
  mutate(post93 = year >= 1994, 
         anykids = children >= 1)

ggplot(dataf, aes(post93, work, color = anykids)) +
  geom_jitter() +
  theme_minimal()

ggplot(dataf, aes(year, work, color = anykids)) +
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 1994) +
  theme_minimal()

model = lm(work ~ anykids*post93, data = dataf)
summary(model)

