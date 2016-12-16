#bring in first sampling
fc2.first <- read.csv("/Users/Sara/Desktop/firstdates.week123.csv", header=T)
head(fc2.first)
summary(fc2.first)
fc2.first$Week<- as.factor(fc2.first$Week)
is.factor(fc2.first$Week)
is.factor(fc2.first$Treatment)
is.factor(fc2.first$Date)
library(ggplot2)
library(MASS)

# try poisson glm on eggs
egg.glm<-glm(Eggs~Treatment+Date+Distance+Direction+Aphids, 
             data=fc2.first, family=poisson)
summary(egg.glm)
anova(egg.glm, test="Rao")

#try glm negbin on eggs
egg.glm.nb<-glm.nb(Eggs~Treatment+Date+Distance+Direction+Aphids, data=fc2.first)
anova(egg.glm.nb, test="Rao")
summary(egg.glm.nb)

#let's look at the aphids - glm poisson
aphid.glm<-glm(Aphids~Treatment+Date+Distance+Direction+Eggs, 
             data=fc2.first, family=poisson)
summary(aphid.glm)
anova(aphid.glm, test="Rao")

#lets look at the aphids with negbin
aphid.glm.nb<-glm.nb(Aphids~Treatment+Date+Distance+Direction+Eggs, data=fc2.first)
anova(aphid.glm.nb, test="Rao")
summary(aphid.glm.nb)


###Second Sampling###
#pull in data
fc2.second <- read.csv("/Users/Sara/Desktop/seconddates.week123.csv", header=T)
head(fc2.second)
summary(fc2.second)
fc2.second$Week<- as.factor(fc2.second$Week)
is.factor(fc2.second$Week)
is.factor(fc2.second$Treatment)
is.factor(fc2.second$Date)

#lets check eggs with poisson glm
egg.glm.2nd<-glm(Eggs~Treatment+Date+Distance+Direction+Aphids, 
             data=fc2.second, family=poisson)
summary(egg.glm.2nd)
anova(egg.glm.2nd, test="Rao")

#try glm negbin on eggs
egg.glm.nb.2nd<-glm.nb(Eggs~Treatment+Date+Distance+Direction+Aphids, data=fc2.second)
anova(egg.glm.nb.2nd, test="Rao")
summary(egg.glm.nb.2nd)

#let's look at the aphids - glm poisson
aphid.glm.2nd<-glm(Aphids~Treatment+Date+Distance+Direction+Eggs, 
               data=fc2.second, family=poisson)
summary(aphid.glm.2nd)
anova(aphid.glm.2nd, test="Rao")

#lets look at the aphids with negbin
aphid.glm.nb.2nd<-glm.nb(Aphids~Treatment+Date+Distance+Direction+Eggs, data=fc2.second)
anova(aphid.glm.nb.2nd, test="Rao")
summary(aphid.glm.nb.2nd)
