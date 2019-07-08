###
## Scenario:   Text entry on smartphone Sexs in different avatars
##
## Statistics: Factorial ANOVA, repeated measures ANOVA, main effects, 
##             interaction effects, the Aligned Rank Transform for 
##             "nonparametric ANOVAs"
###

# Mixed Factorial ANOVA on Words
# Note: "Mixed" here is not "mixed effects" as in LMMs.
# Those will be used farther down below. "Mixed" here is 
# mixing between-Ss and within-Ss factors. By contrast, 
# "mixed" in LMMs is mixing fixed and random effects,
# which we'll cover later. Mixed factorial designs are 
# also called "mixed designs" or "split-plot designs."
# It is easy to extrapolate to purely between-Ss or 
# within-Ss factorial designs from what we do here.

# read in data file of smartphone text entry by 24 people
avatar = read.csv("avatars.csv")
View(avatar)
avatar$Subject = factor(avatar$Subject) # convert to nominal factor
#avatar$SocialOrder = factor(avatar$SocialOrder) # convert to nominal factor
summary(avatar)

# explore the Words data
library(plyr)
ddply(avatar, ~ Clip * Social, function(data) summary(data$Valued))
ddply(avatar, ~ Clip * Social, summarise, Valued.mean=mean(Valued), Valued.sd=sd(Valued))

# histograms for two factors
hist(avatar[avatar$Sex == "iPhone" & avatar$avatar == "Sit",]$Words)
hist(avatar[avatar$Sex == "iPhone" & avatar$avatar == "Stand",]$Words)
hist(avatar[avatar$Sex == "iPhone" & avatar$avatar == "Walk",]$Words)
hist(avatar[avatar$Sex == "Galaxy" & avatar$avatar == "Sit",]$Words)
hist(avatar[avatar$Sex == "Galaxy" & avatar$avatar == "Stand",]$Words)
hist(avatar[avatar$Sex == "Galaxy" & avatar$avatar == "Walk",]$Words)
boxplot(Words ~ Sex * avatar, data=avatar, xlab="Sex.avatar", ylab="Words") # boxplots
with(avatar, interaction.plot(Clip, Social, Valued, ylim=c(0, max(avatar$Valued)))) # interaction plot

# test for a avatar order effect to ensure counterbalancing worked
library(ez)
m = ezANOVA(dv=Valued, within=c(ClipOrder, SocialOrder), wid=Subject, data=avatar)
m$Mauchly # n.s.
m$ANOVA 

# now perform the two-way mixed factorial repeated measures ANOVA
m = ezANOVA(dv=Words, between=Sex, within=avatar, wid=Subject, data=avatar)
m$Mauchly # sig. so use GGe correction
m$ANOVA
# note: "ges" in m$ANOVA is the generalized eta-squared measure
# of effect size, preferred to eta-squared or partial eta-squared. 
# see Bakeman (2005) in the References at ?ezANOVA.
# Now compute the corrected DFs for each corrected effect
pos = match(m$`Sphericity Corrections`$Effect, m$ANOVA$Effect) # positions of within-Ss efx in m$ANOVA
m$Sphericity$GGe.DFn = m$Sphericity$GGe * m$ANOVA$DFn[pos] # Greenhouse-Geisser
m$Sphericity$GGe.DFd = m$Sphericity$GGe * m$ANOVA$DFd[pos]
m$Sphericity$HFe.DFn = m$Sphericity$HFe * m$ANOVA$DFn[pos] # Huynh-Feldt
m$Sphericity$HFe.DFd = m$Sphericity$HFe * m$ANOVA$DFd[pos]
m$Sphericity # show results

# for completeness, note the above's uncorrected results are the same as from this
m = aov(Words ~ Sex * avatar + Error(Subject/avatar), data=avatar) # fit model
summary(m) # show table

# manual post hoc pairwise comparisons in light of sig. interaction
library(reshape2)
avatar.wide = dcast(avatar, Subject ~ Clip + Social, value.var="Valued") # go wide
View(avatar.wide)
iP = t.test(avatar.wide$iPhone ~ avatar, data=avatar.wide) # iPhone vs. Galaxy Words sitting
An = t.test(avatar.wide$Android ~ avatar, data=avatar.wide) # iPhone vs. Galaxy Words standing
p.adjust(c(iP$p.value, An$p.value), method="holm")

# just curious: also compare iPhone 'sit' and 'walk'
t.test(avatar.wide[avatar.wide$Sex == "iPhone",]$Sit, avatar.wide[avatar.wide$Sex == "iPhone",]$Walk, paired=TRUE)
boxplot(avatar.wide[avatar.wide$Sex == "iPhone",]$Sit, avatar.wide[avatar.wide$Sex == "iPhone",]$Walk,xlab="iPhone.Sit vs. iPhone.Walk", ylab="Words") # custom boxplot


## Nonparametric approach to factorial ANOVA
## The Aligned Rank Transform (ART) procedure
## http://depts.washington.edu/aimgroup/proj/art/

# explore the Error_Rate data
library(plyr)
ddply(avatar, ~ Sex * avatar, function(data) summary(data$Error_Rate))
ddply(avatar, ~ Sex * avatar, summarise, Error_Rate.mean=mean(Error_Rate), Error_Rate.sd=sd(Error_Rate))

# histograms, boxplots, and interaction plot
hist(avatar[avatar$Sex == "iPhone" & avatar$avatar == "Sit",]$Error_Rate)
hist(avatar[avatar$Sex == "iPhone" & avatar$avatar == "Stand",]$Error_Rate)
hist(avatar[avatar$Sex == "iPhone" & avatar$avatar == "Walk",]$Error_Rate)
hist(avatar[avatar$Sex == "Galaxy" & avatar$avatar == "Sit",]$Error_Rate)
hist(avatar[avatar$Sex == "Galaxy" & avatar$avatar == "Stand",]$Error_Rate)
hist(avatar[avatar$Sex == "Galaxy" & avatar$avatar == "Walk",]$Error_Rate)
boxplot(Error_Rate ~ Sex * avatar, data=avatar, xlab="Sex.avatar", ylab="Error_Rate") # boxplots
with(avatar, interaction.plot(avatar, Sex, Error_Rate, ylim=c(0, max(avatar$Error_Rate)))) # interaction?

# Aligned Rank Transform on Error_Rate
library(ARTool) # for art, artlm
m = art(Error_Rate ~ Sex * avatar + (1|Subject), data=avatar) # uses LMM
anova(m) # report anova
shapiro.test(residuals(m)) # normality?
qqnorm(residuals(m)); qqline(residuals(m)) # seems to conform

# conduct post hoc pairwise comparisons within each factor
with(avatar, interaction.plot(avatar, Sex, Error_Rate, ylim=c(0, max(avatar$Error_Rate)))) # for convenience
library(lsmeans) # for lsmeans
lsmeans(artlm(m, "Sex"), pairwise ~ Sex)
lsmeans(artlm(m, "avatar"), pairwise ~ avatar)
#lsmeans(artlm(m, "Sex : avatar"), pairwise ~ Sex : avatar) # don't do this in ART!

# the above contrast-testing method is invalid for cross-factor pairwise comparisons in ART.
# and you can't just grab aligned-ranks for manual t-tests. instead, use testInteractions 
# from the phia package to perform "interaction contrasts." see vignette("art-contrasts").
library(phia)
testInteractions(artlm(m, "Clip:Social"), pairwise=c("Clip", "Social"), adjustment="holm")
# in the output, A-B : C-D is interpreted as a difference-of-differences, i.e., the difference 
# between (A-B | C) and (A-B | D). in words, is the difference between A and B significantly 
# different in condition C from condition D?