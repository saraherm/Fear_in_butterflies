##analysis for what will be included in data chapter 3 - Field evaluation of the impact of 
#predators and predator cues on colonization and host use by herbivorous prey

library(ggplot2)
library(MASS)
library(plyr)

######################
######## EGGS ########
######################
#all distances (except focal plants!) for sampling 1
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



########let's do all distances (except focal plants!) for sampling 2########
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



######################
######## APHIDS ######
######################
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





######################
####### YIELD ########
######################
#take a look at yield?
# FC2_CollardYield_2016_alldata.csv
yield <- read.csv("https://raw.githubusercontent.com/saraherm/Fear_in_butterflies/master/FC2_CollardYield_2016_alldata.csv", header=T)
head(yield)
summary(yield)

yield.aov <- aov(yield$avg.per.plant~yield$treat)
summary(yield.aov)
#marginal effect of treatment on yield.