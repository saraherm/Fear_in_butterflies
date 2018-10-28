#analysis for dissertation chapter 3 - Field evaluation of the impact of 
#predators and predator cues on colonization and host use by herbivorous prey

#bring in focal plant data, for sampling date 1
focal.first <- read.csv("https://raw.githubusercontent.com/saraherm/Fear_in_butterflies/master/FC2_pieriseggs_focal_sampling1.csv", header=T)
head(focal.first)
summary(focal.first)
is.factor(focal.first$week)
is.factor(focal.first$treatment)
is.factor(focal.first$date)
library(ggplot2)
library(MASS)
library(plyr)

#let's look at the means
egg.focal.first.means<-ddply(focal.first, .(treatment), summarize, 
                       N=length(eggs),
                       mean=mean(eggs), 
                       sd   = sd(eggs),
                       se   = sd / sqrt(N) )
egg.focal.first.means

#evaluate the effect of treatment and week on egg numbers
egg.glm.nb<-glm.nb(eggs~treatment+week+aphids, data=focal.first)
summary(egg.glm.nb)
anova(egg.glm.nb, test="Rao")

#week and aphids don't matter, let's drop them
egg.glm.nb<-glm.nb(eggs~treatment, data=focal.first)
summary(egg.glm.nb)
anova(egg.glm.nb, test="Rao")

# try poisson glm on eggs
egg.glm<-glm(eggs~treatment, 
             data=focal.first, family=poisson)
summary(egg.glm)
anova(egg.glm, test="Rao")
#AIC is a tiny bit better with this model, and here, the effect of 
#treatment becomes marginally significant - fewest eggs in predator treatment

#attempt at graphing these data
#first, sum of eggs
barplot(focal.first$eggs, main= "Eggs on Treatment Plants", 
        xlab="Treatment", ylab="Average Eggs")
fig1 <- ggplot(data=focal.first, aes(x=treatment, y=eggs)) + geom_bar(stat="identity")
fig1
#now, averages with SE - just kidding, cant figure it out right now









#moving on to second sampling on focal plants
#bring in focal plant data, for sampling date 2
focal.second <- read.csv("https://raw.githubusercontent.com/saraherm/Fear_in_butterflies/master/FC2_pieriseggs_focal_sampling2.csv", header=T)
head(focal.second)
summary(focal.second)
is.factor(focal.second$week)
is.factor(focal.second$treatment)
is.factor(focal.second$date)

#let's look at the means
egg.focal.second.means<-ddply(focal.second, .(treatment), summarize, 
                             N=length(eggs),
                             mean=mean(eggs), 
                             sd   = sd(eggs),
                             se   = sd / sqrt(N) )
egg.focal.second.means

#evaluate the effect of treatment and week on egg numbers
egg.glm.nb2<-glm.nb(eggs~treatment+week+aphids, data=focal.second)
summary(egg.glm.nb2)
anova(egg.glm.nb2, test="Rao")

#week and aphids don't matter, let's drop them
egg.glm.nb2<-glm.nb(eggs~treatment, data=focal.second)
summary(egg.glm.nb2)
anova(egg.glm.nb2, test="Rao")

# try poisson glm on eggs
egg.glm2<-glm(eggs~treatment, 
             data=focal.second, family=poisson)
summary(egg.glm2)
anova(egg.glm2, test="Rao")
#AIC is a tiny bit better with this model as well









#moving on to the larger data set. Let's bring in all distances 
#(except focal plants!) for sampling 1
allbutfocal.first <- read.csv("https://raw.githubusercontent.com/saraherm/Fear_in_butterflies/master/FC2_pierisaphid_allbutfocal_sampling1.csv", header=T)
head(allbutfocal.first)
summary(allbutfocal.first)

# try poisson glm on eggs to evaluate the effect of treatment and week on egg numbers
egg.glm3<-glm(eggs~treatment+week+distance, 
              data=allbutfocal.first, family=poisson)
summary(egg.glm3)
anova(egg.glm3, test="Rao")

#try neg bin
egg.glm.nb3<-glm.nb(eggs~treatment+week+distance, data=allbutfocal.first)
summary(egg.glm.nb3)
anova(egg.glm.nb3, test="Rao")
#week and distance are highly significant, but not treatment. 









#let's do all distances (except focal plants!) for sampling 2
allbutfocal.second <- read.csv("https://raw.githubusercontent.com/saraherm/Fear_in_butterflies/master/FC2_pierisaphid_allbutfocal_sampling2.csv", header=T)
head(allbutfocal.second)
summary(allbutfocal.second)

# try poisson glm on eggs to evaluate the effect of treatment and week on egg numbers
egg.glm4<-glm(eggs~treatment+week+distance, 
              data=allbutfocal.second, family=poisson)
summary(egg.glm4)
anova(egg.glm4, test="Rao")

#try neg bin
egg.glm.nb4<-glm.nb(eggs~treatment+week+distance, data=allbutfocal.second)
summary(egg.glm.nb4)
anova(egg.glm.nb4, test="Rao")
#week and distance are again highly significant, but not treatment. 









#ok - time to work with the aphids.
#evaluate the effect of treatment and week on aphid numbers
aphids.glm.nb<-glm.nb(aphids~treatment+week, data=focal.first)
summary(aphids.glm.nb)
anova(aphids.glm.nb, test="Rao")

# try poisson glm on aphids
aphids.glm<-glm(aphids~treatment+week, 
             data=focal.first, family=poisson)
summary(aphids.glm)
anova(aphids.glm, test="Rao")
#AIC is way worse with this model, but treatment becomes significant? 
#there is a ton of variation here, becuase of week but there are more aphids 
#in predator and chemical trt compared to control

#based on redisudal deviance also, the neg bin model wins.









#moving on to second sampling on focal plants
#evaluate the effect of treatment and week on aphid numbers
aphids.glm.nb2<-glm.nb(aphids~treatment+week, data=focal.second)
summary(aphids.glm.nb2)
anova(aphids.glm.nb2, test="Rao")

# try poisson glm on eggs
aphids.glm2<-glm(aphids~treatment+week, 
              data=focal.second, family=poisson)
summary(aphids.glm2)
anova(aphids.glm2, test="Rao")
#same problem as above with this one with residual deviance and higher AIC

#again, negbin wins







#moving on to the larger data set
# try poisson glm on aphids to evaluate the effect of treatment and week on aphids numbers
aphids.glm3<-glm(aphids~treatment+week, 
              data=allbutfocal.first, family=poisson)
summary(aphids.glm3)
anova(aphids.glm3, test="Rao")

#try neg bin
aphids.glm.nb3<-glm.nb(aphids~treatment+week, data=allbutfocal.first)
summary(aphids.glm.nb3)
anova(aphids.glm.nb3, test="Rao")
#week and treatment are significant but this model fit isnt great?









#let's do all distances (except focal plants!) for sampling 2
# try poisson glm on aphids to evaluate the effect of treatment and week on aphids numbers
aphids.glm4<-glm(aphids~treatment+week+distance, 
              data=allbutfocal.second, family=poisson)
summary(aphids.glm4)
anova(aphids.glm4, test="Rao")

#try neg bin
aphids.glm.nb4<-glm.nb(aphids~treatment+week, data=allbutfocal.second)
summary(aphids.glm.nb4)
anova(aphids.glm.nb4, test="Rao")
#week is crazy significant but treatment only marginally here and the 
#null deviane is way higher than DF









#try all distances, sample date 1
#data file - FC2_pierisaphid_alldistances_sampling1.csv

alldist.first <- read.csv("https://raw.githubusercontent.com/saraherm/Fear_in_butterflies/master/FC2_pierisaphid_alldistances_sampling1.csv", header=T)
head(alldist.first)
summary(alldist.first)

#neg bin on eggs
eggs.glm.nb5<-glm.nb(eggs~treatment+week+distance, data=alldist.first)
summary(eggs.glm.nb5)
anova(eggs.glm.nb5, test="Rao")

#neg bin on aphids
aphids.glm.nb5<-glm.nb(aphids~treatment+week+distance, data=alldist.first)
summary(aphids.glm.nb5)
anova(aphids.glm.nb5, test="Rao")
#treatment is significant here.null deviance higher than df....









#try all distances, sample date 2
#data file - FC2_pierisaphid_alldistances_sampling2.csv

alldist.second <- read.csv("https://raw.githubusercontent.com/saraherm/Fear_in_butterflies/master/FC2_pierisaphid_alldistances_sampling2.csv", header=T)
head(alldist.second)
summary(alldist.second)

#neg bin on eggs
eggs.glm.nb6<-glm.nb(eggs~treatment+week+distance, data=alldist.second)
summary(eggs.glm.nb6)
anova(eggs.glm.nb6, test="Rao")

#neg bin on aphids
aphids.glm.nb6<-glm.nb(aphids~treatment+week+distance, data=alldist.second)
summary(aphids.glm.nb6)
anova(aphids.glm.nb6, test="Rao")
#treatment is marginal here.null deviance WAY higher than df....



