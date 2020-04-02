#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# A Practical Introduction to Regression Discontinuity Designs
# Volume II
# Authors: Matias D. Cattaneo, Nicolas Idrobo and Rocio Titiunik
# Last update: 11-Apr-2018
#------------------------------------------------------------------------------#
# SOFTWARE WEBSITE: https://sites.google.com/site/rdpackages/
#------------------------------------------------------------------------------#
# TO INSTALL/DOWNLOAD R PACKAGES/FUNCTIONS:
# FOREIGN: install.packages('foreign')
# GGPLOT2: install.packages('ggplot2')
# GRID: install.packages('grid')
# LPDENSITY: install.packages('lpdensity')
# RDDENSITY: install.packages('rddensity')
# RDLOCRAND: install.packages('rdlocrand')
# RDROBUST: install.packages('rdrobust')
# TEACHINGDEMOS: install.packages('TeachingDemos')
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
rm(list=ls())

library(foreign)
library(ggplot2)
library(lpdensity)
library(rddensity)
library(rdrobust)
library(rdlocrand)
library(TeachingDemos)

options(width=300)
par(mar = rep(2, 4))

# A folder called "outputs" needs to be created in order to store 
# all of the figures, logs and tables. If the folder already exists,
# the user will get an error message but the code will not stop.
tryCatch(dir.create("outputs"))

# Loading the data and defining the main variables
data = read.dta("CIT_2019_Cambridge_education.dta")
nextGPA = data$nextGPA
left_school = data$left_school
nextGPA_nonorm = data$nextGPA_nonorm
X = data$X
T = data$T
T_X = T*X

#---------------------------------#
# Section 3                       #
# RD Designs with Discrete Scores #
#---------------------------------#
# Figure 3.1
# Histogram
tempdata = as.data.frame(X); colnames(tempdata) = c("v1");
p = ggplot(data=tempdata, aes(tempdata$v1))+
  geom_histogram(breaks=seq(-2.8, 0, by = 0.1), col="black", fill="blue", alpha = 1)+
  geom_histogram(breaks=seq(0, 1.6, by = 0.1), col="black", fill="red", alpha = 1)+
  labs(x="Score", y="Number of Observations")+geom_vline(xintercept=0, color="black")+
  theme_bw()
p
ggsave("./outputs/Vol-2-R_histogram.pdf", plot = p, width = 6, height = 5, units = "in")

# Code snippet 1
# Counting the number of observations with X different from missing
txtStart("./outputs/Vol-2-R_lso_countX.txt", commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
length(X)
txtStop()

# Code snippet 2
# Counting the unique values of X
txtStart("./outputs/Vol-2-R_lso_uniqueX.txt", commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
length(unique(X))
txtStop()

# Code snippet 3
# Using rddensity
txtStart("./outputs/Vol-2-R_lso_falsification_rddensity.txt", commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
out = rddensity(X)
summary(out)
txtStop()

# Code snippet 4
# Using rdrobust on hsgrade_pct
txtStart("./outputs/Vol-2-R_lso_falsification_rdrobust_hsgrade_pct.txt", commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
out = rdrobust(data$hsgrade_pct, X)
summary(out)
txtStop()

# Code snippet 5 (Figure 3.2)
# Using rdplot on hsgrade_pct
txtStart("./outputs/Vol-2-R_lso_falsification_rdplot_hsgrade_pct-COMMAND-ONLY.txt",
         commands=TRUE, results=FALSE, append=FALSE, visible.only=TRUE)
rdplot(data$hsgrade_pct, X, x.label = "Score", y.label = "", title="")
txtStop()

pdf("./outputs/Vol-2-R_lso_falsification_rdplot_hsgrade_pct.pdf")
rdplot(data$hsgrade_pct, X, x.label = "Score", y.label = "", title="")
dev.off()

# Table 3.3
summary(rdrobust(data$hsgrade_pct, X))
summary(rdrobust(data$totcredits_year1, X))
summary(rdrobust(data$age_at_entry, X))
summary(rdrobust(data$male, X))
summary(rdrobust(data$bpl_north_america, X))
summary(rdrobust(data$english, X))
summary(rdrobust(data$loc_campus1, X))
summary(rdrobust(data$loc_campus2, X))
summary(rdrobust(data$loc_campus3, X))

# Figure 3.3
pdf("./outputs/Vol-2-R_lso_falsification_rdplot_hsgrade_pct.pdf")
rdplot(data$hsgrade_pct, X, x.label = "Score", y.label = "", title="")
dev.off()

pdf("./outputs/Vol-2-R_lso_falsification_rdplot_totcredits_year1.pdf")
rdplot(data$totcredits_year1, X, x.label = "Score", y.label = "", title="")
dev.off()

pdf("./outputs/Vol-2-R_lso_falsification_rdplot_age_at_entry.pdf")
rdplot(data$age_at_entry, X, x.label = "Score", y.label = "", title="")
dev.off()

pdf("./outputs/Vol-2-R_lso_falsification_rdplot_english.pdf")
rdplot(data$english, X, x.label = "Score", y.label = "", title="")
dev.off()

pdf("./outputs/Vol-2-R_lso_falsification_rdplot_male.pdf")
rdplot(data$male, X, x.label = "Score", y.label = "", title="")
dev.off()

pdf("./outputs/Vol-2-R_lso_falsification_rdplot_bpl_north_america.pdf")
rdplot(data$bpl_north_america, X, x.label = "Score", y.label = "", title="")
dev.off()

# Code snippet 6 (Figure 3.4)
# Using rdplot on the outcome
txtStart("./outputs/Vol-2-R_lso3_rdplot_esmv.txt", commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
out = rdplot(nextGPA_nonorm, X,  binselect = 'esmv')
summary(out)
txtStop()

pdf("./outputs/Vol-2-R_lso3_rdplot_esmv.pdf")
rdplot(nextGPA_nonorm, X,  binselect = 'esmv', x.label = 'Score', y.label = 'Outcome', title = '')
dev.off()

# Code snippet 7
# Using rdrobust on the outcome
txtStart("./outputs/Vol-2-R_lso3_rdrobust_triangular_mserd_p1_rhofree_regterm1.txt",
         commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
out = rdrobust(nextGPA_nonorm, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# Code snippet 8
# Using rdrobust and showing the objects it returns
txtStart("./outputs/Vol-2-R_lso3_rdrobust_triangular_mserd_p1_rhofree_regterm1_namescoefsout_all.txt",
         commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
rdout = rdrobust(nextGPA_nonorm, X, kernel = 'triangular', p = 1, bwselect = 'mserd')
print(names(rdout))
print(rdout$beta_p_r)
print(rdout$beta_p_l)
txtStop()

# Code snippet 9
# Using rdrobust with clustered standard errors
txtStart("./outputs/Vol-2-R_lso3_rdrobust_triangular_mserd_p1_rhofree_regterm1_cluster.txt",
         commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
clustervar = X
out = rdrobust(nextGPA_nonorm, X, kernel = 'triangular', p = 1, bwselect = 'mserd', vce = 'hc0', cluster = clustervar)
summary(out)
txtStop()

# Code snippet 10
# Using rdrobust on the collapsed data
txtStart("./outputs/Vol-2-R_lso3_rdrobust_collapsed.txt", commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
data2 = data.frame(nextGPA_nonorm, X)
dim(data2)
collapsed = aggregate(nextGPA_nonorm ~ X, data = data2, mean)
dim(collapsed)
out = rdrobust(collapsed$nextGPA_nonorm, collapsed$X)
summary(out)
txtStop()

# Code snippet 11
# Binomial test with rdwinselect
txtStart("./outputs/Vol-2-R_lso_falsification_binomial.txt", commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
out = rdwinselect(X, wmin = 0.01, nwindows = 1, cutoff = 5.00000000000e-06)
txtStop()

# Binomial Test by Hand
txtStart("./outputs/Vol-2-R_lso_falsification_binomial_byhand.txt", commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
binom.test(77, 305, 1/2)
txtStop()

# Code snippet 13
# Using rdrandinf on hsgrade_pct
txtStart("./outputs/Vol-2-R_lso_falsification_rdrandinf_hsgrade_pct.txt", commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
out = rdrandinf(data$hsgrade_pct, X, wl = -0.005, wr = 0.01, seed = 50)
txtStop()

# Code snippet 14
# Using rdwinselect with covariates and windows with equal number of mass points to each side of the cutoff
txtStart("./outputs/Vol-2-R_lso_rdwinselect_consecutive_windows.txt", commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
Z = cbind(data$hsgrade_pct, data$totcredits_year1, data$age_at_entry, data$male,
          data$bpl_north_america, data$english, data$loc_campus1, data$loc_campus2, 
          data$loc_campus3)
colnames(Z) = c("hsgrade_pct", "totcredits_year1", "age_at_entry", "male",
                "bpl_north_america", "english", "loc_campus1", "loc_campus2",
                "loc_campus3")
out = rdwinselect(X, Z, p = 1, seed = 50, wmin = 0.01, wstep = 0.01, cutoff = 5.00000000000e-06)
txtStop()

# Code snippet 15
# Using rdrandinf on the Outcome
txtStart("./outputs/Vol-2-R_lso3_rdrandinf_adhocsmall_p0.txt", commands=TRUE, results=TRUE, append=FALSE, visible.only=TRUE)
out = rdrandinf(nextGPA_nonorm, X, wl = -0.005, wr = 0.01, seed = 50)
txtStop()
