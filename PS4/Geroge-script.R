library(tidyverse)
library(ggplot2)
library(foreign)

df <- read_csv2("bank-additional-full.csv")

eitc = read.dta("eitc.dta")
