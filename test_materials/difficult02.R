library(lme4)

DATA <- read.csv("data_2015_PROJECT_GENTIANA_2016.12.14.csv")#, sep= ",", header= T, quote= "") # main dataset

DATAnpol <- subset(DATA, poll.obs1=="yes")
DATAcomp <- subset(DATA, poll.comp=="yes")

DATAmerged <- data.frame(plantID= as.factor(c(as.character(DATAcomp$id.short), as.character(DATAnpol$id.short))),
                         lat= c(DATAcomp$lat, DATAnpol$lat),
                         lon= c(DATAcomp$lon, DATAnpol$lon),
                         flower.sex= as.factor(c(as.character(rep("female",58)), as.character(DATAnpol$poll.flower.sex))),
                         treatment= c(rep("complementation",58), rep("natural",66)),
                         fruitherbivory= c("no","yes","no","no","no","no","yes","no","no","no","no","yes","no","no",
                                           "no","yes","no","no","no","yes","yes","no","no","no","no","no","no","yes",
                                           "no","yes","no","yes","no","yes","no","yes","no","no","no","no","no","no",
                                           "no","yes","no","no","no","yes","no","yes","no","no","no","no","no","yes",
                                           "no","no","no","yes","no","yes","yes","yes","yes","no","no","no","no","no",
                                           "no","no","no","yes","no","no","yes","no","no","no","yes","no","no","yes",
                                           "no","yes","no","no","yes","no","no","no","yes","yes","no","no","no","yes",
                                           "no","yes","no","no","no","no","no","no","no","no","no","yes","no","no","no",
                                           "yes","no","yes","no","no","no","no","yes","no","no","no"),
                         harvest.date= as.factor(c(as.character(DATAcomp$harvest.date), as.character(DATAnpol$harvest.date))),
                         pollination.date= as.factor(c(as.character(DATAcomp$poll.comp.date), as.character(DATAnpol$poll.obsv.1.date))),
                         seedset= c(DATAcomp$poll.comp.flower3.seed.set.developed.seeds, DATAnpol$poll.obsv.flower1.seed.set.developed.seeds),
                         totalseed= c(DATAcomp$poll.comp.flower3.seed.set.totalseed, DATAnpol$poll.obsv.flower1.seed.set.totalseed),
                         fl.length= c(DATAcomp$poll.comp.flower3.length, DATAnpol$poll.obsv.flower1.length),
                         fl.width=  c(DATAcomp$poll.comp.flower3.width, DATAnpol$poll.obsv.flower1.width)
)

# ATTENTION!!!!
# this is a complement to the main script: "script_2015_GentianaProject_v8_general.R"
#
# Including a dummy variable in the data
DATAmerged$dummy <- seq(1:length(DATAmerged[,1]))
#
# (7.1) STATISTICAL ANALYSES: [GLMER] approach (Poisson) ------------------
# -> e) seed set ----------------------------------------------------------
names(DATAmerged)
# ->> natural pollination + pollen complementation ------------------------
# TRIAL 1: entire merged data, including herbivory cases
Full <- glmer(seedset ~
              + treatment              # natural pollination or pollen complementation
              + fruitherbivory         # presence/absence of herbivory on fruit
              + scale(fl.length)       # corolla length + quadratic term
              + I(scale(fl.length)^2)  #
              + scale(fl.width)        # corolla width + quadratic term
              + I(scale(fl.width)^2)   #
              + treatment*fruitherbivory
              + treatment*scale(fl.length)
              + treatment*scale(fl.width)
              + fruitherbivory*scale(fl.length)
              + fruitherbivory*scale(fl.width)
              + scale(fl.length*fl.width)
              + (1|pollination.date)   # day of nat.pol or pol.comp (ie. treatment date)
              + (1|plant)
              #+ (1|dummy)
              , control=(glmerControl(optimizer=c("bobyqa"),optCtrl=list(maxfun=1e5)))
              , family=poisson
              , na.action=na.omit
              , data=DATAmerged)
summary(Full)
model_validation(Full)
dispersion_glmer(Full) # model fit is overdispersed!
# remove quadratic terms from original model

# fixed terms
Full <- glmer(seedset ~
                + treatment
              + fruitherbivory
              + scale(fl.length)
              + scale(fl.width)
              + (1|pollination.date)
              + (1|plant)
              #+ (1|dummy)
              , control=(glmerControl(optimizer=c("bobyqa"),optCtrl=list(maxfun=1e5)))
              , family=poisson
              , na.action=na.omit
              , data=DATAmerged)
drop1(Full, test="Chisq")

# TRIAL 2: removing herbivory cases (only data of entire fruits)
DATAseeds <- subset(DATAmerged, DATAmerged$fruitherbivory=="no") # 31 samples need to be removed
Full <- glmer.nb(seedset ~
                 + scale(fl.length)
                 + I(scale(fl.length)^2)
                 + scale(fl.width)
                 + I(scale(fl.width)^2)
                 + scale(fl.length*fl.width)
                 + (1|pollination.date)
                 , control=glmerControl(optimizer=c("bobyqa"),optCtrl=list(maxfun=1e10))
                 , na.action=na.omit
                 , data=subset(DATAseeds, DATAseeds$treatment=="complementation"))
# warnings()
# 1: In (function (par, fn, lower = -Inf, upper = Inf, control = list(),  ... :
#    NAs introduced by coercion to integer range
# 2: In optwrap(optimizer, devfun, start, rho$lower, control = control,  ... :
#    convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded
summary(Full)
dispersion_glmer(Full) # dispersion = 0.96
model_validation(Full)

# TRIAL 3: removing quadratic terms
Full <- glmer.nb(seedset ~
                   + scale(fl.length)
                 #+ I(scale(fl.length)^2)
                 + scale(fl.width)
                 #+ I(scale(fl.width)^2)
                 + scale(fl.length*fl.width)
                 + (1|pollination.date)
                 , control=glmerControl(optimizer=c("bobyqa"),optCtrl=list(maxfun=1e5))
                 , na.action=na.omit
                 , data=subset(DATAseeds, DATAseeds$treatment=="complementation"))
# Error in pwrssUpdate(pp, resp, tol = tolPwrss, GQmat = GQmat, compDev = compDev,  :
#                        pwrssUpdate did not converge in (maxit) iterations
# TRIAL 4: removing random term
Full <- glm.nb(seedset ~
                   + scale(fl.length)
                 + I(scale(fl.length)^2)
                 + scale(fl.width)
                 + I(scale(fl.width)^2)
                 + scale(fl.length*fl.width)
                 #+ (1|pollination.date)
                 #, control=glmerControl(optimizer=c("bobyqa"),optCtrl=list(maxfun=1e5))
                 , na.action=na.omit
                 , data=subset(DATAseeds, DATAseeds$treatment=="complementation"))
summary(Full)
model_validation(Full)

# e -----------------------------------------------------------------------
# TRIAL 5: based on merged seedset analyses, see section below of natural pollination + pollen complementation
# remove observations with seedset = 0 (which are 23 cases)

# CONCLUSION: random effect could not be fit as like for the natural pollination model
# previous runs did not produce warnings, althoug same code and data were used. They previously showed no significant effects either
# plot significant variables: length, width, and their interaction
# (7.3) STATISTICAL ANALYSES: [SPAMM] approach (Poisson) --------------------
# TO UPDATE!!!
# -> e) seed set ----------------------------------------------------------
# ->> natural pollination + pollen complementation ------------------------
spaMM_poisson_seedset_all <- corrHLfit(seedset ~
                                         + treatment
                                       + scale(fl.length)
                                       + scale(I(fl.length^2))
                                       + scale(fl.width)
                                       + scale(I(fl.width^2))
                                       + treatment*scale(fl.length)
                                       + treatment*scale(fl.width)
                                       #+ scale(fl.length*fl.width)
                                       + (1|pollination.date)
                                       + (1|plant)
                                       + Matern(1|lon+lat)
                                       , HLmethod="ML"
                                       , family=poisson()
                                       , data=DATAseeds)
summary(spaMM_poisson_seedset_all)
plot(spaMM_poisson_seedset_all)
# LRTs
t1 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + treatment
               + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=poisson())
t2 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=poisson())
t1 # NS
t2 # SIG
t3 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 #  + treatment
                 + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               #+ treatment*scale(fl.length)
               #+ treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=poisson())
t4 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + treatment
               #+ scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               #+ treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=poisson())
t5 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + treatment
               + scale(fl.length)
               #+ scale(I(fl.length^2))
               #+ scale(fl.width)
               + scale(I(fl.width^2))
               + treatment*scale(fl.length)
               #+ treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=poisson())
t6 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + treatment
               + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               #+ treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=poisson())
t7 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + treatment
               + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + treatment*scale(fl.length)
               #+ treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=poisson())

#ploting predictions for significant variables: treatment, length, width, width², treat.*width
length(fitted(spaMM_poisson_seedset_all))
DATAseeds$seedset
DATAseeds$seedset[c(1:27, 29:42, 44:67, 69:78, 80:82)] # removing points: 28, 43, 68, 79
par(mfrow=c(2,2))
plot(fitted(spaMM_poisson_seedset_all) ~ DATAseeds$treatment[c(1:27, 29:42, 44:67, 69:78, 80:82)], xlab="poll. treat")
# x <- data.frame(length=DATAseeds$fl.length[c(1:27, 29:42, 44:67, 69:78, 80:82)], seedset=DATAseeds$seedset[c(1:27, 29:42, 44:67, 69:78, 80:82)])
# x <- x[order(x$length),]
# points(x$length, fitted(lm(x$seedset ~ x$length+I(x$length^2))), type="l", col="red")
#

# 1:58 complementation (DATAseeds)
# 59:82 natural (DATAseeds)
# points removed: 28, 43, 68, 79
length(fitted(spaMM_poisson_seedset_all))
plot(fitted(spaMM_poisson_seedset_all)[1:56] ~ DATAseeds$fl.width[c(1:27, 29:42, 44:58)], pch=19, col="blue") # complemention
abline(lm(fitted(spaMM_poisson_seedset_all)[1:56] ~ DATAseeds$fl.width[c(1:27, 29:42, 44:58)]), col="blue") # complemention
points(fitted(spaMM_poisson_seedset_all)[57:78] ~ DATAseeds$fl.width[c(59:67, 69:78, 80:82)], pch=19, col="red") # natural
abline(lm(fitted(spaMM_poisson_seedset_all)[57:78] ~ DATAseeds$fl.width[c(59:67, 69:78, 80:82)]),  col="red") # natural
plot(fitted(spaMM_poisson_seedset_all) ~ DATAseeds$fl.length[c(1:27, 29:42, 44:67, 69:78, 80:82)], xlab="flower length (mm)")
abline(lm(fitted(spaMM_poisson_seedset_all) ~ DATAseeds$fl.length[c(1:27, 29:42, 44:67, 69:78, 80:82)]), col="red")

plot(fitted(spaMM_poisson_seedset_all) ~ DATAseeds$fl.width[c(1:27, 29:42, 44:67, 69:78, 80:82)], xlab="flower width (mm)")
abline(lm(fitted(spaMM_poisson_seedset_all) ~ DATAseeds$fl.width[c(1:27, 29:42, 44:67, 69:78, 80:82)]), col="red")

# ->> natural pollination -------------------------------------------------
spaMM_poisson_seedset_nat <- corrHLfit(seedset ~
                                         + scale(fl.length)
                                       + scale(I(fl.length^2))
                                       + scale(fl.width)
                                       + scale(I(fl.width^2))
                                       #+ scale(fl.length*fl.width)
                                       + (1|pollination.date)
                                       + Matern(1|lon+lat)
                                       , HLmethod="ML"
                                       , family=poisson()
                                       , data=na.omit(subset(DATAseeds, DATAseeds$treatment=="natural")))
summary(spaMM_poisson_seedset_nat)
plot(spaMM_poisson_seedset_nat)
# LRTs
t1 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="natural")), family=poisson())
t2 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="natural")), family=poisson())
t1 # SIG
t2 # NS
t3 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 #+ scale(fl.length)
                 + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="natural")), family=poisson())
t4 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               #+ scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="natural")), family=poisson())
#ploting predictions for significant variables: length, length²
length(fitted(spaMM_poisson_seedset_nat))

# ->> pollen complementation ----------------------------------------------
spaMM_poisson_seedset_com <- corrHLfit(seedset ~
                                         + scale(fl.length)
                                       + scale(I(fl.length^2))
                                       + scale(fl.width)
                                       + scale(I(fl.width^2))
                                       #+ scale(fl.length*fl.width)
                                       + (1|pollination.date)
                                       + Matern(1|lon+lat)
                                       , HLmethod="ML"
                                       , family=poisson()
                                       , data=na.omit(subset(DATAseeds, DATAseeds$treatment=="complementation")))
summary(spaMM_poisson_seedset_com)
plot(spaMM_poisson_seedset_com)
# LRTs
t1 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="complementation")), family=poisson())
t2 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="complementation")), family=poisson())
t1 # NS
t2 # NS
t3 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 #  + scale(fl.length)
                 #+ scale(I(fl.length^2))
                 + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="complementation")), family=poisson())
t4 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + scale(fl.length)
               #+ scale(I(fl.length^2))
               #+ scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="complementation")), family=poisson())

#ploting predictions for significant variables: none
# length(fitted(spaMM_poisson_seedset_com))

# -> e) seed set ----------------------------------------------------------
# ->> natural pollination + pollen complementation ------------------------
spaMM_neg.bin_seedset_all <- corrHLfit(seedset ~
                                         + treatment
                                       + scale(fl.length)
                                       + scale(I(fl.length^2))
                                       + scale(fl.width)
                                       + scale(I(fl.width^2))
                                       + treatment*scale(fl.length)
                                       + treatment*scale(fl.width)
                                       #+ scale(fl.length*fl.width)
                                       + (1|pollination.date)
                                       + (1|plant)
                                       + Matern(1|lon+lat)
                                       , HLmethod="ML"
                                       , family=negbin()
                                       , data=DATAseeds)
# ERROR MESSAGE: (convergence) lambda leverages numerically 1 were replaced by 1 - 1e-8
summary(spaMM_neg.bin_seedset_all)
plot(spaMM_neg.bin_seedset_all)
# LRTs
t1 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + treatment
               + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=negbin())
t2 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=negbin())
t1 # SIG
t2 # NS
t3 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 #+ treatment
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               #+ treatment*scale(fl.length)
               #+ treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=negbin())
t4 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + treatment
               #+ scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               #+ treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=negbin())
t5 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               #+ scale(fl.width)
               #+ scale(I(fl.width^2))
               + treatment*scale(fl.length)
               #+ treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=negbin())
t6 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               #+ treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=negbin())
t7 <- fixedLRT(null.formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + treatment*scale(fl.length)
               + treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + treatment
               + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + treatment*scale(fl.length)
               #+ treatment*scale(fl.width)
               + (1|pollination.date)
               + (1|plant)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=DATAseeds, family=negbin())

#ploting predictions for significant variables: length²
length(fitted(spaMM_neg.bin_seedset_all))
DATAseeds$seedset
DATAseeds$seedset[c(1:27, 29:42, 44:67, 69:78, 80:82)] # removing points: 28, 43, 68, 79
par(mfrow=c(2,2))
plot.new()
# plot(fitted(spaMM_neg.bin_seedset_all) ~ DATAseeds$treatment[c(1:27, 29:42, 44:67, 69:78, 80:82)], xlab="poll. treat")
plot.new()
# 1:58 complementation (DATAseeds)
# 59:82 natural (DATAseeds)
# points removed: 28, 43, 68, 79
# length(fitted(spaMM_neg.bin_seedset_all))
# plot(fitted(spaMM_neg.bin_seedset_all)[1:56] ~ DATAseeds$fl.width[c(1:27, 29:42, 44:58)], pch=19, col="blue") # complemention
# abline(lm(fitted(spaMM_neg.bin_seedset_all)[1:56] ~ DATAseeds$fl.width[c(1:27, 29:42, 44:58)]), col="blue") # complemention
# points(fitted(spaMM_neg.bin_seedset_all)[57:78] ~ DATAseeds$fl.width[c(59:67, 69:78, 80:82)], pch=19, col="red") # natural
# abline(lm(fitted(spaMM_neg.bin_seedset_all)[57:78] ~ DATAseeds$fl.width[c(59:67, 69:78, 80:82)]),  col="red") # natural

plot(fitted(spaMM_neg.bin_seedset_all) ~ DATAseeds$fl.length[c(1:27, 29:42, 44:67, 69:78, 80:82)], xlab="flower length (mm)")
abline(lm(fitted(spaMM_neg.bin_seedset_all) ~ DATAseeds$fl.length[c(1:27, 29:42, 44:67, 69:78, 80:82)]), col="red")

# plot(fitted(spaMM_neg.bin_seedset_all) ~ DATAseeds$fl.width[c(1:27, 29:42, 44:67, 69:78, 80:82)], xlab="flower width (mm)")
# abline(lm(fitted(spaMM_neg.bin_seedset_all) ~ DATAseeds$fl.width[c(1:27, 29:42, 44:67, 69:78, 80:82)]), col="red")
# x <- data.frame(width=DATAseeds$fl.width[c(1:27, 29:42, 44:67, 69:78, 80:82)], seedset=DATAseeds$seedset[c(1:27, 29:42, 44:67, 69:78, 80:82)])
# x <- x[order(x$width),]
# points(x$width, fitted(lm(x$seedset ~ x$width+I(x$width^2))), type="l", col="red")
#
# ->> natural pollination -------------------------------------------------
spaMM_neg.bin_seedset_nat <- corrHLfit(seedset ~
                                         + scale(fl.length)
                                       + scale(I(fl.length^2))
                                       + scale(fl.width)
                                       + scale(I(fl.width^2))
                                       #+ scale(fl.length*fl.width)
                                       + (1|pollination.date)
                                       + Matern(1|lon+lat)
                                       , HLmethod="ML"
                                       , family=negbin()
                                       , data=na.omit(subset(DATAseeds, DATAseeds$treatment=="natural")))
# ERROR MESSAGE: optim() message: ERROR: ABNORMAL_TERMINATION_IN_LNSRCH (convergence=52)
summary(spaMM_neg.bin_seedset_nat)
plot(spaMM_neg.bin_seedset_nat)
# LRTs
t1 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="natural")), family=negbin())
t2 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="natural")), family=negbin())
t1 # SIG
t2 # NS
t3 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 #  + scale(fl.length)
                 + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="natural")), family=negbin())
t4 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               #+ scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="natural")), family=negbin())

#ploting predictions for significant variables: length, length²
length(fitted(spaMM_neg.bin_seedset_nat))

# ->> pollen complementation ----------------------------------------------
spaMM_neg.bin_seedset_com <- corrHLfit(seedset ~
                                         + scale(fl.length)
                                       + scale(I(fl.length^2))
                                       + scale(fl.width)
                                       + scale(I(fl.width^2))
                                       #+ scale(fl.length*fl.width)
                                       + (1|pollination.date)
                                       + Matern(1|lon+lat)
                                       , HLmethod="ML"
                                       , family=negbin()
                                       , data=na.omit(subset(DATAseeds, DATAseeds$treatment=="complementation")))
summary(spaMM_neg.bin_seedset_com)
plot(spaMM_neg.bin_seedset_com)
# LRTs
t1 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="complementation")), family=negbin())
t2 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               + scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + scale(fl.length)
               + scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="complementation")), family=negbin())
t1 # NS
t2 # NS
t3 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 #  + scale(fl.length)
                 #+ scale(I(fl.length^2))
                 + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="complementation")), family=negbin())
t4 <- fixedLRT(null.formula= seedset ~
                 + scale(fl.length)
               #+ scale(I(fl.length^2))
               + scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               ,
               formula= seedset ~
                 + scale(fl.length)
               #+ scale(I(fl.length^2))
               #+ scale(fl.width)
               #+ scale(I(fl.width^2))
               + (1|pollination.date)
               + Matern(1|lon+lat)
               , HLmethod="ML", data=na.omit(subset(DATAseeds, DATAseeds$treatment=="complementation")), family=negbin())
