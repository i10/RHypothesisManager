###
## Scenario:   Comparing task completion times in authoring tools
##
## Statistics: ANOVA assumptions, data transformations, one-way ANOVA, 
##             post hoc comparisons, nonparametric tests
###

## Independent-samples t-test

# read in a data file with task completion times (min) from 2 programming tools
design = read.csv("designtime.csv")
View(design)
design$Subject = factor(design$Subject) # convert to nominal factor
summary(design)

# view descriptive statistics by Tool
library(plyr)
ddply(design, ~ Tool, function(data) summary(data$Time))
ddply(design, ~ Tool, summarise, Time.mean=mean(Time), Time.sd=sd(Time))

# graph histograms and a boxplot
hist(design[design$Tool == "Illustrator",]$Time) # histogram
hist(design[design$Tool == "InDesign",]$Time) # histogram
plot(Time ~ Tool, data=design) # boxplot

# independent-samples t-test (suitable? maybe not, because...)
t.test(Time ~ Tool, data=design, var.equal=TRUE)


## Testing ANOVA assumptions

# Shapiro-Wilk normality test on response
shapiro.test(design[design$Tool == "Illustrator",]$Time)
shapiro.test(design[design$Tool == "InDesign",]$Time)

# but really what matters most is the residuals
m = aov(Time ~ Tool, data=design) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

# Kolmogorov-Smirnov test for log-normality
# fit the distribution to a lognormal to estimate fit parameters
# then supply those to a K-S test with the lognormal distribution fn (see ?plnorm)
# see ?Distributions for many other named probability distributions
library(MASS)
fit = fitdistr(design[design$Tool == "Illustrator",]$Time, "lognormal")$estimate
ks.test(design[design$Tool == "Illustrator",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)
fit = fitdistr(design[design$Tool == "InDesign",]$Time, "lognormal")$estimate
ks.test(design[design$Tool == "InDesign",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)

# tests for homoscedasticity (homogeneity of variance)
library(car)
leveneTest(Time ~ Tool, data=design, center=mean) # Levene's test
leveneTest(Time ~ Tool, data=design, center=median) # Brown-Forsythe test

# Welch t-test for unequal variances handles
# the violation of homoscedasticity. but not
# the violation of normality.
t.test(Time ~ Tool, data=design, var.equal=FALSE) # Welch t-test


## Data transformation

# create a new column in design defined as log(Time)
design$logTime = log(design$Time) # log transform
View(design) # verify

ddply(design, ~ Tool, function(data) summary(data$logTime))
ddply(design, ~ Tool, summarise, logTime.mean=mean(logTime), logTime.sd=sd(logTime))


# explore for intuition-building
hist(design[design$Tool == "Illustrator",]$logTime) # histogram
hist(design[design$Tool == "InDesign",]$logTime) # histogram
plot(logTime ~ Tool, data=design) # boxplot

# re-test for normality
shapiro.test(design[design$Tool == "Illustrator",]$logTime)
shapiro.test(design[design$Tool == "InDesign",]$logTime)
m = aov(logTime ~ Tool, data=design) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

# re-test for homoscedasticity
library(car)
leveneTest(logTime ~ Tool, data=design, center=median) # Brown-Forsythe test

# independent-samples t-test (now suitable for logTime)
t.test(logTime ~ Tool, data=design, var.equal=TRUE)


## Nonparametric equivalent of independent-samples t-test

# Mann-Whitney U test
library(coin)
wilcox_test(Time ~ Tool, data=design, distribution="exact")
wilcox_test(logTime ~ Tool, data=design, distribution="exact") # note: same result
