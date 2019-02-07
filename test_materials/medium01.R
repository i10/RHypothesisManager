f4<-read.csv("/Users/LaurenMrkva/Downloads/flu4.csv")

#pd scale
scale<-f4[,7:10]
alpha(scale)

mod1<-lm(psychdistance~fluency,data=f4)
summary(mod1)
confint(mod1,method='Wald')

#different dimensions of psych distance
mod1a<-lm(spatialonly~fluency,data=f4)
summary(mod1a)
confint(mod1a,method='Wald')

mod1b<-lm(temporalonly~fluency,data=f4)
summary(mod1b)
confint(mod1b,method='Wald')

#when excluding people who correctly guessed research question
mod1c<-lm(pd_exclude~fluency,data=f4)
summary(mod1c)
confint(mod1c,method='Wald')

#controlling for all covariates
mod2<-lm(psychdistance~fluency+familiarity+temporal_squared+spatial_sqrt+construal+visualperspective+comprehension+valenceduring+intensityduring+taskunrelated+engagementtime+frustratednow+attentivescale_during,data=f4)
summary(mod2)
confint(mod2,method='Wald')

##fluency measures
#adjusting volume
mod3<-lm(adjustvolume~fluency,data=f4)
summary(mod3)
confint(mod3,method='Wald')

#self-reported fluency
mod3a<-lm(fluencySR~fluency,data=f4)
summary(mod3a)
confint(mod3a)

##manipulation didn't affect...
#objective distance
mod4a<-lm(spatial_sqrt~fluency,data=f4)
summary(mod4a)
confint(mod4a,method='Wald')

mod4b<-lm(temporal_squared~fluency,data=f4)
summary(mod4b)
confint(mod4b,method='Wald')

mod4c<-lm(objective_temporal~fluency,data=f4)
summary(mod4c)


#survey motivation, either measured as engagementtime spent on rest of survey or as self-reported attentiveness
mod5<-lm(engagementtime~fluency,data=f4)
summary(mod5)

mod5a<-lm(attentivescale_during~fluency,data=f4)
summary(mod5a)

#task-unrelated thoughts
mod7<-lm(taskunrelated~fluency,data=f4)
summary(mod7)

#emotional intensity and valence
mod8<-lm(intensityduring~fluency,data=f4)
summary(mod8)

mod8a<-lm(valenceduring~fluency,data=f4)
summary(mod8a)

#comprehension
mod9<-lm(comprehension~fluency,data=f4)
summary(mod9)

#Supp Material
m11a<-lm(familiarity~fluency,data=f4)
summary(m11a)
confint(m11a,method='Wald')

m11b<-lm(construal~fluency,data=f4)
summary(m11b)
confint(m11b,method='Wald')

m11c<-lm(visualperspective~fluency,data=f4)
summary(m11c)
confint(m11c,method='Wald')

m11d<-lm(comprehension~fluency,data=f4)
summary(m11d)
confint(m11d,method='Wald')

m11e<-lm(valenceduring~fluency,data=f4)
summary(m11e)
confint(m11e,method='Wald')

m11f<-lm(intensityduring~fluency,data=f4)
summary(m11f)
confint(m11f,method='Wald')

m11g<-lm(taskunrelated~fluency,data=f4)
summary(m11g)
confint(m11g,method='Wald')

m11h<-lm(engagementtime~fluency,data=f4)
summary(m11h)
confint(m11h,method='Wald')

m11i<-lm(frustratednow~fluency,data=f4)
summary(m11i)
confint(m11i,method='Wald')

m11j<-lm(attentivescale_during~fluency,data=f4)
summary(m11j)
confint(m11j,method='Wald')
