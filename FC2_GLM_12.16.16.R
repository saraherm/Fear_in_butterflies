#bring in first sampling
fc2.first <- read.csv("https://raw.githubusercontent.com/saraherm/Fear_in_butterflies/master/firstdates.week123.csv", header=T)
head(fc2.first)
summary(fc2.first)
fc2.first$Week<- as.factor(fc2.first$Week)
is.factor(fc2.first$Week)
is.factor(fc2.first$Treatment)
is.factor(fc2.first$Date)
library(ggplot2)
library(MASS)
library(plyr)

# try poisson glm on eggs
egg.glm<-glm(Eggs~Treatment+Week+Distance+Direction+Aphids, 
             data=fc2.first, family=poisson)
summary(egg.glm)
anova(egg.glm, test="Rao")


#try glm negbin on eggs
egg.glm.nb<-glm.nb(Eggs~Treatment+Week+Distance+Direction+Aphids, data=fc2.first)
summary(egg.glm.nb)
anova(egg.glm.nb, test="Rao")

#looks like negbin model has better fit, but direction is probably not very important. 
#let's drop direction and see what that does
#try glm negbin on eggs, dropping direction factor
egg.glm.nb<-glm.nb(Eggs~Treatment+Week+Distance+Aphids, data=fc2.first)
summary(egg.glm.nb)
anova(egg.glm.nb, test="Rao")

#it looks like drpping the direction term was a good call- but I think Distance is probably
#an additional source of noise. Let's cull out Distance= 4

fc2.first<-fc2.first[which(fc2.first$Distance != 4),]

#and now let's repeat the analysis to see if that helps clear things up a bit
#note the AIC will go down regardless (fewer data, less noise left over)
#so this is more of an eyeball thing

egg.glm.nb<-glm.nb(Eggs~Treatment+Week+Distance+Aphids, data=fc2.first)
summary(egg.glm.nb)
anova(egg.glm.nb, test="Rao")

#so when we do that, we see a treatment effect but we lose the effect of aphids entirely.
#this is interesting- it suggests that aphids have influnce farther away from the cue
#but nearer to the focal plant, the cue dominates.

#let's see what the actual means are in this case

#first overall:
egg.first.means<-ddply(fc2.first, .(Treatment), summarize, 
                       N=length(Eggs),
                       mean=mean(Eggs), 
                       sd   = sd(Eggs),
                       se   = sd / sqrt(N) )
egg.first.means

#then by week
egg.first.means.week<-ddply(fc2.first, .(Week, Treatment), summarize, 
                           N=length(Eggs),
                           mean=mean(Eggs), 
                           sd   = sd(Eggs),
                           se   = sd / sqrt(N) )
egg.first.means.week

#hmm. So overall, it looks like there's more eggs in predator and chemical, but if we
#look at it within a week, it's all over the place. I'm not sure what to do with these data :(

###########

#let's look at the aphids - glm poisson
aphid.glm<-glm(Aphids~Treatment+Week+Distance+Direction+Eggs, 
             data=fc2.first, family=poisson)
summary(aphid.glm)
anova(aphid.glm, test="Rao")

#lets look at the aphids with negbin
aphid.glm.nb<-glm.nb(Aphids~Treatment+Week+Distance+Direction+Eggs, data=fc2.first)
summary(aphid.glm.nb)
anova(aphid.glm.nb, test="Rao")

#the negbin is preforming much better for the aphids- use it. 
#it looks like the drection /distance factor can be dropped entirely here
# start with distance as it seems to be the poorest performer

aphid.glm.nb<-glm.nb(Aphids~Treatment+Week+Direction+Eggs, data=fc2.first)
summary(aphid.glm.nb)
anova(aphid.glm.nb, test="Rao")

#we lose a tiny bit with the AIC when we drop distance but not much. Let's try dropping 
# direction as well and see what happens
aphid.glm.nb<-glm.nb(Aphids~Treatment+Week+Eggs, data=fc2.first)
summary(aphid.glm.nb)
anova(aphid.glm.nb, test="Rao")

#and what happens when we drop eggs?
aphid.glm.nb<-glm.nb(Aphids~Treatment+Week, data=fc2.first)
summary(aphid.glm.nb)
anova(aphid.glm.nb, test="Rao")

#looks like the simplest model is the best

# so it seems that these data suggest that aphids inconsistently affect eggs, but eggs don't
#affect aphids

#as above, let's see what the actual means are in this case

#first overall:
Aphid.first.means<-ddply(fc2.first, .(Treatment), summarize, 
                       N=length(Aphids),
                       mean=mean(Aphids), 
                       sd   = sd(Aphids),
                       se   = sd / sqrt(N) )
Aphid.first.means

#then by week
Aphid.first.means.week<-ddply(fc2.first, .(Week, Treatment), summarize, 
                            N=length(Aphids),
                            mean=mean(Aphids), 
                            sd   = sd(Aphids),
                            se   = sd / sqrt(N) )
Aphid.first.means.week



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
