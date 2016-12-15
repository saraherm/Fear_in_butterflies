#working to glm the crap out of this data. let's bring it in
fc2<-read.table(file="https://github.com/saraherm/Fear_in_butterflies/blob/master/FC2_alldates_noweather_12.14.16.csv", header=TRUE)
setwd("/Users/Sara/Desktop/")
library(ggplot2)
library(MASS)
options(scipen=999) #Prevent scientific notation from writing in console and graphical output
fc2 <- read.csv("/Users/Sara/Desktop/FC2_alldates_noweather_12.14.16.csv", header=T)
head(fc2)
summary(fc2)

#make sure treatment, date, day after deploy and week are treated as factors
fc2$Treatment<-as.factor(fc2$Treatment)
summary(fc2$Treatment)
is.factor(fc2$Treatment)
fc2$Date<-as.factor(fc2$Date)
summary(fc2$Date)
is.factor(fc2$Day.after.deploy)
fc2$Day.after.deploy<-as.factor(fc2$Day.after.deploy)
summary(fc2$Day.after.deploy)
is.factor(fc2$Day.after.deploy)
fc2$Week<-as.factor(fc2$Week)
summary(fc2$Week)
is.factor(fc2$Week)

#Facet the data set into two, one with first samples, one with second
#cant figure this out just yet..

#Try GLM with poisson...on eggs
egg.glm<-glm(Eggs~Treatment+Date+Distance.m.+Direction+Aphids+Week+Day.after.deploy, 
                data=fc2, family=poisson)
summary(egg.glm)

#try negative bionmial on eggs
egg.glm.nb<-glm.nb(Eggs~Treatment+Date+Distance.m.+Direction+Aphids+Week, data=fc2)
anova(egg.glm.nb, test="Rao")
summary(egg.glm.nb)

egg.glm.nb.summary<-ddply(fc2, .(Treatment, Date, Distance.m., Aphids), summarize, 
                            N=length(Eggs),
                            mean=mean(Eggs),
                            sd   = sd(Eggs),
                            se   = sd / sqrt(N) )


#glm with poisson on aphids
aphid.glm<-glm(Aphids~Treatment+Date+Distance.m.+Direction+Eggs+Week+Day.after.deploy, 
               data=fc2, family=poisson)
summary(aphid.glm)


#try negative binomial on aphids
aphid.glm.nb<-glm.nb(Aphids~Treatment+Date+Distance.m.+Direction+Eggs+Week, data=fc2)
anova(aphid.glm.nb, test="Rao")
summary(aphid.glm.nb)
