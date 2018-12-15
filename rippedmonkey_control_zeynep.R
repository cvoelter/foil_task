ripmon2<-read.csv("ripped_monkey_control16.csv", header=T)
str(ripmon2)
attach(ripmon2)
library(lme4)

#preprocessing of data, scale categorical variables and define others as factor
z.trialno<-as.vector(scale(trialno))
z.phase2=as.vector(scale(phase2))
z.age=as.vector(scale(age))

trialtype<-relevel(trialtype, ref = "stick")
summary(ripmon2)

#coding dummy variables before centering the slopes
trialtype.food<-as.numeric(trialtype==levels(trialtype)[2])
sex.m<-as.numeric(sex==levels(sex)[2])
table(sex.m)
table(trialtype.food)

#centering the slopes
trialtype.food.c<-trialtype.food-mean(trialtype.food)
sex.m.c<-sex.m -mean(sex.m)

# running the control and then the full model

contr<-glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))
full=glmer(correct ~ trialtype*z.phase2+z.trialno+z.age+sex+(1|boxtype)+(1+z.trialno+z.phase2+trialtype.food.c|id),data=ripmon2, family=binomial, control=contr)
summary(full) #only trial type has a sig effect. no effect of phase.
null=glmer(correct~(1|boxtype)+(1+z.trialno+z.phase2+trialtype.food.c|id),data=ripmon2, family=binomial, control=contr)

anova(null,full,test="Chisq")
#the comparison of the null and the full model does not suggest a difference. We retain Ho

#I will check the reduced model without the interaction anyway

contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))
full2=glmer(correct ~ trialtype+z.phase2+z.trialno+z.age+sex+(1|boxtype)+(1+z.trialno+z.phase2+trialtype.food.c|id),data=ripmon2, family=binomial, control=contr)
summary(full2)

round(summary(full2)$coefficients,2)
anova(null,full2,test="Chisq")

#there is no effect of phase2, but food trials are easier
#the model without the interaction does not improve compared to the null hypothesis.
#this means that there is no effect of trialtype, phase or age and sex on monkeys' performance in the pattern control.

install.packages("multcomp")
library(multcomp)
xx=glht(full2, linfct = mcp(trialtype = c("food-stick = 0")))
summary(xx)

#they do better in food trials


