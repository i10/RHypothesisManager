library("lme4")
library("lmerTest")

#liking (it's only studies 1 and 2)
metaL<-read.csv("/Users/Owner/Desktop/data ms/Attention, emotion/meta_liking.csv")
agg7 <- lmer(liking ~ target*ArousalContrast+target*ValenceContrast +
               (target*ArousalContrast+target*ValenceContrast|ID) +
               (1|stim)+(1|study),
             data=metaL, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg7)
confint(agg7,method='Wald')


agg17 <- lmer(liking ~ target*baseArousal+target*baseValence +
                (target*baseArousal+target*baseValence|ID) +
                (target|stim)+(1|study),
              data=metaL, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg17)

agg18 <- lmer(realintensity ~ target*baseArousal+target*baseValence +
                (target*baseArousal+target*baseValence|ID) +
                (target|stim)+(1|study),
              data=metaL, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg18)

#liking, neutral
agg8 <- lmer(liking ~ target*posdummy+target*negdummy +
               (target*posdummy+target*negdummy|ID) +
               (target|stim)+(target|study),
             data=metaL, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg8)
confint(agg8,method='Wald')

#liking, positive
agg9 <- lmer(liking ~ target*negdummy+target*neutdummy +
               (target*negdummy+target*neutdummy|ID) +
               (target|stim)+(target|study),
             data=metaL, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg9)
confint(agg9,method='Wald')

#liking, negative
agg10 <- lmer(liking ~ target*posdummy+target*neutdummy +
                (target*posdummy+target*neutdummy|ID) +
                (target|stim)+(target|study),
              data=metaL, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg10)
confint(agg10,method='Wald')


#Exp 1, comparisons to baseline

seq11 <- lmer(intensity ~ targetdummy*ArousalContrast+targetdummy*ValenceContrast+nontargetdummy*ArousalContrast+nontargetdummy*ValenceContrast +
                (ArousalContrast+ValenceContrast|sub) +
                (1|IAPSImage),
              data=sequential, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(seq11)
confint(seq11,method='Wald')
#pos
seq11a <- lmer(intensity ~ targetdummy*dummyneg+targetdummy*dummyneut+nontargetdummy*dummyneg+nontargetdummy*dummyneut +
                (dummyneg+dummyneut|sub) +
                (1|IAPSImage),
              data=sequential, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(seq11a)
confint(seq11a,method='Wald')

#neut
seq11b <- lmer(intensity ~ targetdummy*dummyneg+targetdummy*dummypos+nontargetdummy*dummyneg+nontargetdummy*dummypos +
                 (dummyneg+dummypos|sub) +
                 (1|IAPSImage),
               data=sequential, verbose=T,
               control=lmerControl(optCtrl=list(maxfun=100000)))
summary(seq11b)
confint(seq11b,method='Wald')

#neg
seq11c <- lmer(intensity ~ targetdummy*dummypos+targetdummy*dummyneut+nontargetdummy*dummypos+nontargetdummy*dummyneut +
                 (dummypos+dummyneut|sub) +
                 (1|IAPSImage),
               data=sequential, verbose=T,
               control=lmerControl(optCtrl=list(maxfun=100000)))
summary(seq11c)
confint(seq11c,method='Wald')

#analyses involving exp 1c data only
ivm<-read.csv("/Users/Owner/Desktop/data ms/Attention, emotion/IVtoM.csv")


#distinctiveness Exp 1 (just 1c)
model2 <- lmer(distinctive ~ target*baselinearousal+target*baselinevalence +
                 (target*baselinearousal+target*baselinevalence|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(model2)

#matching distinctiveness measure (saturation and contrast)
model1 <- lmer(saturation ~ target*baselinearousal+target*baselinevalence +
                 (target*baselinearousal+target*baselinevalence|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(model1)

#I believe these are the final models for the simple effects, but I need to double-check this.
#neutral simple effect on self-report
model4 <- lmer(distinctive ~ target*dummypositive+target*dummynegative +
                 (target*dummypositive+target*dummynegative|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(model4)
#positive simple effect on self-report
model5 <- lmer(distinctive ~ target*dummyneutral+target*dummynegative +
                 (target*dummyneutral+target*dummynegative|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(model5)
#negative simple effect on self-report
model6 <- lmer(distinctive ~ target*dummypositive+target*dummyneutral +
                 (target*dummypositive+target*dummyneutral|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(model6)

#delay study (Experiment 1b)
delay<-read.csv("/Users/Owner/Desktop/data ms/Attention, emotion/Experiment3.csv")
#simples at session1
delay2 <- lmer(EmoIntensity ~ ArousalContrast*Target*dummytime2 + ValenceContrast*Target*dummytime2 +
                 (ArousalContrast*Target*dummytime2 + ValenceContrast*Target|ID) +
                 (Target|stim),
               data=delay, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(delay2)

#simple at delay session (supplemental material)
delay3 <- lmer(EmoIntensity ~ ArousalContrast*Target*dummytime1 + ValenceContrast*Target*dummytime1 +
                 (ArousalContrast*Target*dummytime1 + ValenceContrast*Target*dummytime1|ID) +
                 (Target|stim),
               data=delay, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(delay3)
confint(delay3,method='Wald')
confint(delay2,method='Wald')

#across both sessions (supplemental material)
delay1 <- lmer(EmoIntensity ~ ArousalContrast*Target*Time1or2 + ValenceContrast*Target*Time1or2 +
                 (ArousalContrast*Target*Time1or2 + ValenceContrast*Target|ID) +
                 (Target|stim),
               data=delay, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(delay1)
confint(delay1,method='Wald')