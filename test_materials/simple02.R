######## Post-hoc Tests ##########

library(foreign)
library(lsmeans)
library(car)

d <- read.spss("AB_unbalanced.sav", to.data.frame=TRUE)
d$A <- as.factor(d$A)
d$B <- as.factor(d$B)

m1 <- lm(y ~ A * B, data=d)

m1.lsm <- lsmeans(m1, c("A")) ## enspricht estimated marginal means in SPSS

contrast(m1.lsm, method="pairwise") ## tukey post-hoc comparisons
contrast(m1.lsm, method="eff") ## effect contrasts
contrast(m1.lsm, method="del.eff")

with(d, tapply(y, A, mean)) ## sample means, not the same in unbalanced designs with interactions

Anova(m1, type="III")

melr <- effectLite("y", x="A", k="B", data=d)
melr@results@adjmeans

## graphics
m1 <- lm(y ~ A*B, data=d)
m1.lsm <- lsmeans(m1, c("A","B"))
lsmip(m1.lsm, A ~ B)

summary(lsmeans(m1, c("A","B")))

