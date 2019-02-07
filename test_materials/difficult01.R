#Shape analysis
#paper Year1
#date: 24/02/2015
#
rm(list=ls())
library(lme4)
library(multcomp)

setwd("C:/Users/New person/Desktop/Geometric Morphom/My_data/analyses_2015-02/analysis_SHAPE_2013Project_10.2014-L508 PLANTSS")

#data
PCA <- read.table("raw.coord_aPC scores, CovMatrix, RAW-coord (n=349, LM30), Proc coord.txt", header=TRUE, sep="\t")

sym  <- read.table("shape_sym.txt", header=TRUE, sep="\t")
rot  <- read.table("shape_rot.txt", header=TRUE, sep="\t")
left <- read.table("shape_l-r.txt", header=TRUE, sep="\t")
up   <- read.table("shape_d-v.txt", header=TRUE, sep="\t")


# PCA - comparison between mating systems ---------------------------------
# data = PCA
# manova ------------------------------------------------------------------
Y <- as.matrix(PCA[8:63])
fit <- manova(Y ~ PCA$mat+PCA$cluster+PCA$pop+PCA$pla) #cannot assign random effects!
summary(fit, test="Pillai")
summary.aov(fit)[1:10] # results are confusing

# -> PC1 ------------------------------------------------------------------
m0 <-lmer ( PC1 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m1 <-lmer ( PC1 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m2 <-lmer ( PC1 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, PCA$pop,main='RANDOM - population')
plot(raw.residuals, PCA$pla,main='RANDOM - plant')
summary(m0)

# -> PC2 ------------------------------------------------------------------
m0 <-lmer ( PC2 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m1 <-lmer ( PC2 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m2 <-lmer ( PC2 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, PCA$pop,main='RANDOM - population')
plot(raw.residuals, PCA$pla,main='RANDOM - plant')
summary(m0)

# -> PC3 ------------------------------------------------------------------
m0 <-lmer ( PC3 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m1 <-lmer ( PC3 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m2 <-lmer ( PC3 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, PCA$pop,main='RANDOM - population')
plot(raw.residuals, PCA$pla,main='RANDOM - plant')
summary(m0)

# -> PC4 ------------------------------------------------------------------
m0 <-lmer ( PC4 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m1 <-lmer ( PC4 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m2 <-lmer ( PC4 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, PCA$pop,main='RANDOM - population')
plot(raw.residuals, PCA$pla,main='RANDOM - plant')
summary(m0)

# -> PC5 ------------------------------------------------------------------
m0 <-lmer ( PC5 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m1 <-lmer ( PC5 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m2 <-lmer ( PC5 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, PCA$pop,main='RANDOM - population')
plot(raw.residuals, PCA$pla,main='RANDOM - plant')
summary(m0)

# -> PC6 ------------------------------------------------------------------
m0 <-lmer ( PC6 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m1 <-lmer ( PC6 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m2 <-lmer ( PC6 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, PCA$pop,main='RANDOM - population')
plot(raw.residuals, PCA$pla,main='RANDOM - plant')
summary(m0)

# -> PC7 ------------------------------------------------------------------
m0 <-lmer ( PC7 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m1 <-lmer ( PC7 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m2 <-lmer ( PC7 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, PCA$pop,main='RANDOM - population')
plot(raw.residuals, PCA$pla,main='RANDOM - plant')
summary(m0)

# -> PC8 ------------------------------------------------------------------
m0 <-lmer ( PC8 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m1 <-lmer ( PC8 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m2 <-lmer ( PC8 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, PCA$pop,main='RANDOM - population')
plot(raw.residuals, PCA$pla,main='RANDOM - plant')
summary(m0)

# -> PC9 ------------------------------------------------------------------
m0 <-lmer ( PC9 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m1 <-lmer ( PC9 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m2 <-lmer ( PC9 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, PCA$pop,main='RANDOM - population')
plot(raw.residuals, PCA$pla,main='RANDOM - plant')
summary(m0)

# -> PC10 ------------------------------------------------------------------
m0 <-lmer ( PC10 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m1 <-lmer ( PC10 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)
m2 <-lmer ( PC10 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=PCA)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, PCA$pop,main='RANDOM - population')
plot(raw.residuals, PCA$pla,main='RANDOM - plant')
summary(m0)

# shape - sym -------------------------------------------------------------
m0 <-lmer ( PC1 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=sym)
m1 <-lmer ( PC1 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=sym)
m2 <-lmer ( PC1 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=sym)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, sym$pop,main='RANDOM - population')
plot(raw.residuals, sym$pla,main='RANDOM - plant')
summary(m0)

# shape - left-right ------------------------------------------------------
m0 <-lmer ( PC1 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=left)
m1 <-lmer ( PC1 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=left)
m2 <-lmer ( PC1 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=left)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, left$pop,main='RANDOM - population')
plot(raw.residuals, left$pla,main='RANDOM - plant')
summary(m0)

# shape - top-bottom ---------------------------------------------------------
m0 <-lmer ( PC1 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=up)
m1 <-lmer ( PC1 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=up)
m2 <-lmer ( PC1 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=up)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, up$pop,main='RANDOM - population')
plot(raw.residuals, up$pla,main='RANDOM - plant')
summary(m0)

# shape - rot -------------------------------------------------------------
m0 <-lmer ( PC1 ~
              + mat
            + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=rot)
m1 <-lmer ( PC1 ~
              #+ mat
              + cluster
            + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=rot)
m2 <-lmer ( PC1 ~
              #+ mat
              #+ cluster
              + (1|pop)
            + (1|pla)
            + (1|position)
            ,data=rot)

anova(m0,m1) # MATING SYSTEM
anova(m0,m2) # GENETIC CLUSTER

tukeytest <- glht(m0, linfct=mcp(cluster="Tukey"))
summary(tukeytest)

fitted<-fitted(m0)
raw.residuals<-residuals(m0)
norm.residuals<-(resid(m0, type="pearson"))
par(mfrow=c(2,2))
plot(fitted,raw.residuals, main="Symmetrical component")  #
abline(h=c(-1.96*sd(raw.residuals),0,1.96*sd(raw.residuals)))
plot(raw.residuals, norm.residuals)
plot(raw.residuals, up$pop,main='RANDOM - population')
plot(raw.residuals, up$pla,main='RANDOM - plant')
summary(m0)
