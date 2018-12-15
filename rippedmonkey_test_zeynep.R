ripmon1<-read.csv("ripped_monkey_test16.csv", header=T)
str(ripmon1)
attach(ripmon1)
library(lme4)

#preprocessing of data, scale categorical variables and define others as factor
#We scale variables so that the differences in the range of the variables do not influence the parameter estimations?
z.trialno<-as.vector(scale(trialno))
z.phase2=as.vector(scale(phase2))
z.age=as.vector(scale(age))

trialtype<-relevel(trialtype, ref = "stick")
summary(ripmon1)
apply(X=xx>0, MARGIN=2, FUN=sum)

#coding dummy variables before centering the slopes
trialtype.food<-as.numeric(trialtype==levels(trialtype)[2])
sex.m<-as.numeric(sex==levels(sex)[2])
table(sex.m)
table(trialtype.food)

#centering the slopes: p-values of the factors can be influenced by the choice of reference category.
#by centering the factor for the random slope components the p-values should be the same irrespective of the choice of the reference level
trialtype.food.c<-trialtype.food-mean(trialtype.food)
sex.m.c<-sex.m -mean(sex.m)

# running the control (due to convergence issues) and then the full model

contr<-glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))
full=glmer(correct ~ trialtype*z.phase2+z.trialno+age+sex+(1|boxtype)+(1+z.trialno+z.phase2+trialtype.food.c|id),data=ripmon1, family=binomial, control=contr)
summary(full) 
#sig effect of trialtype and sex (better in food trials than stick and males are better than females)
#no difference between the test and transfer trials, no interaction of phase and trialtype!

#the null model starts with 1 because we want to fix the intercept at 1?
null=glmer(correct~(1|boxtype)+(1+z.trialno+z.phase2+trialtype.food.c|id),data=ripmon1, family=binomial, control=contr)
anova(null,full,test="Chisq") 
drop1(full, test="Chisq") #drop command gives a better estimate of p values
#there is a sig difference between the null and full so I retain the full model

#but the interaction is not significant, so I remove it from the model
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))
full2=glmer(correct ~ trialtype+z.phase2+z.trialno+age+sex+(1|boxtype)+(1+z.trialno+z.phase2+trialtype.food.c|id),data=ripmon1, family=binomial, control=contr)
summary(full2)

round(summary(full2)$coefficients,2)
#still the same effects without the interaction in the model

#there is no effect of phase2, but food trials are easier, a sig effect of sex

install.packages("multcomp")
library(multcomp)
xx=glht(full2, linfct = mcp(trialtype = c("food-stick = 0")))
summary(xx)
xx2=glht(full2, linfct=mcp(sex=c("m-f=0")))
summary(xx2)
#they do better in food trials and males do better- this is possible to check using only means

