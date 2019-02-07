alphabets = read.csv("alphabets.csv")

View(alphabets)

alphabets$Subject = factor(alphabets$Subject)

summary(alphabets)

plot(WPM ~ Alphabet, data=alphabets)

m = aov(WPM ~ Alphabet, data=alphabets)

shapiro.test(residuals(m))
qqnorm(residuals(m))
qqline(residuals(m))

# Add a logarithmised column and test
alphabets$logWPM = log(alphabets$WPM)

shapiro.test(alphabets[alphabets$Alphabet == "EdgeWrite",]$logWPM)

m = aov(logWPM ~ Alphabet, data=alphabets)

shapiro.test(residuals(m))
qqnorm(residuals(m))
qqline(residuals(m))

library(car)
leveneTest(WPM ~ Alphabet, data=alphabets, center=median)

library(multcomp)
summary(glht(m, mcp(Alphabet="Tukey")), test=adjusted(type="holm"))

library(lsmeans)
summary(glht(m, lsm(pairwise ~ Alphabet)), test=adjusted(type="holm"))

library(coin)
kruskal_test(WPM ~ Alphabet, data=alphabets, distribution="asymptotic")
kruskal_test(logWPM ~ Alphabet, data=alphabets, distribution="asymptotic")
