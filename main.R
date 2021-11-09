# Title     : 2020 RARF Data
# Objective : Identify appropriate model for Pesticide Treatment
# Created by: Kartik Jevaji
# Created on: 7/24/2021

# rep is 1-4 group for each treatment # assuming this is over time (rep 4 is later than rep 1)
# treatment is Agrimek, azera, blackhawk, control, coragen, mustang.maxx
# head 1-25 is block within rep group
# dseed is count of damaged seeds
#install.packages("lme4")
library("lme4")

rarf <- read.table("RARF 2020 R.txt", header=T)
attach(rarf)
#summary(rarf)

# factor Treatment
TreatmentLevels <- c("AgriMek", "Azera", "Blackhawk", "Control", "Coragen", "Mustang.Maxx")
Treatment <- factor(rarf$Treatment, TreatmentLevels)
#summary(Treatment)

# factor Damage
Damagedunfactored <- factor(ifelse(rarf$Dseed > 0, "YES", "NO"))
factorlevels <- c("NO", "YES")
Damaged <- factor(Damagedunfactored, factorlevels)
#summary(Damaged)
#contrasts(Damaged)

# factor Rep
repLevels <- c("1", "2", "3", "4")
repFactor <- factor(rarf$Rep, repLevels)
#summary(repFactor)

#factor Head
headLevels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18",
                "19", "20", "21", "22", "23", "24", "25")
headFactor <- factor(rarf$Head, headLevels)
#summary(headFactor)
#create frame with factored data
glmframe <- data.frame(Damaged, Treatment, repFactor, headFactor)
summary(glmframe)

# kruskal-wallis non parametric tests
# A collection of data samples are independent if they come from unrelated populations
# and the samples do not affect each other.
# Using the Kruskal-Wallis Test, we can decide whether the population distributions are identical without
# assuming them to follow the normal distribution.
kruskal.test(Treatment ~ Damaged) # Kruskal-Wallis chi-squared = 22.661, df = 1, p-value = 1.932e-06
#kruskal.test(repFactor ~ Damaged) # Kruskal-Wallis chi-squared = 0.1341, df = 1, p-value = 0.7142
kruskal.test(headFactor ~ Damaged) # Kruskal-Wallis chi-squared = 4.379, df = 1, p-value = 0.03638 // might want to factor on head

# Logistic Regression Models (Smaller AIC is better)
# issues: headFactor and repFactor are not independent, need random effects model if including both in analysis
glmframe <- data.frame(Damaged, Treatment, repFactor, headFactor)
SuggestedModel <- glm(Damaged ~ Treatment, binomial) # Coragen and Mustang.Maxx are significant
summary(SuggestedModel) # AIC 462.15 on 599 degrees of freedom
exp(cbind(OddsRatio = coef(SuggestedModel), confint(SuggestedModel))) # code to get odds ratios and confidence intervals
anova(SuggestedModel, test = "Chisq")

qqnorm(resid(SuggestedModel))
hist(resid(SuggestedModel))
plot(fitted(SuggestedModel), resid(SuggestedModel))
plot(predict(SuggestedModel), resid(SuggestedModel))

#alternativeModel <- glm(Damaged ~ Treatment+headFactor, binomial, data = glmframe) # Assumptions MET. Coragen and Mustang.Maxx are significant
#summary(alternativeModel) # AIC 481.67
#anova(alternativeModel, test = "Chisq") ## can drop headFactor from model

#alternativeModel2 <- glm(Damaged ~ Treatment+repFactor, binomial, data = glmframe)
#summary(alternativeModel2) # AIC 464.89
#anova(alternativeModel2, test = "Chisq") ## can drop repFactor from model

#alternativeModel3 <- glm(Damaged ~ Treatment+repFactor+headFactor,binomial,data=glmframe)
#summary(alternativeModel3) # IGNORING assumptions because of not enough data # AIC 461.31
#anova(alternativeModel3, test = "Chisq")


#doNotUse <- glm(Damaged ~ Treatment+rarf$Rep+rarf$Head,binomial)  ## INVALID assumptions use only for comparing
#summary(doNotUse) # uses unfactored data AIC 461.31

# Generalized Linear Models, fixed effect TreatmentFactor, random effects: repFactor nested headFactor
## reponse ~ treatment + (1|a/b) formula for nested
### singular fit on all models
##glmframe <- data.frame(Damaged, Treatment, repFactor, headFactor)
##glm4 <- glmer(Damaged ~ 1 + Treatment + (1|repFactor/headFactor), binomial, data = glmframe) # random intercept with fixed mean
##summary(glm4)
##glm5 <- glmer(Damaged ~ Treatment + (Treatment|repFactor/headFactor), binomial, data = glmframe) # correlated random intercept with slope
##summary(glm5)
#glm6 <- glmer(Damaged ~ Treatment + (Treatment||repFactor/headFactor), binomial, data = glmframe, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000))) # uncorrelated random intercept with slope
#summary(glm6)
## reponse ~ treatment + (1|a/b)


rarf2 <- data.frame(repFactor, headFactor, Treatment, Damaged)
#summary(rarf2)
Control <- subset(rarf2, rarf2$Treatment == 'Control')
#summary(Control) # n = 100, p(damaged) = 18/100
Agrimek <- subset(rarf2, rarf2$Treatment == "AgriMek")
#summary(Agrimek) # n = 100, p(damaged) = 19/100
Azera <- subset(rarf2, rarf2$Treatment == "Azera")
#summary(Azera) # p(damaged) = 28
Blackhawk <- subset(rarf2, rarf2$Treatment == "Blackhawk")
#summary(Blackhawk) # pdamaged = 12
Coragen <- subset(rarf2, rarf2$Treatment == "Coragen")
#summary(Coragen) #pdamaged = 3
Mustang <- subset(rarf2, rarf2$Treatment == "Mustang.Maxx")
#summary(Mustang) # pdamaged = 5

# kruskal-wallis tests for whether control is equal to each treatment
# kruskal.test(Control$Damaged ~ Agrimek$Damaged) # Kruskal-Wallis chi-squared = 0.87879, df = 1, p-value = 0.3485
# kruskal.test(Control$Damaged ~ Azera$Damaged) # Kruskal-Wallis chi-squared = 0.00053233, df = 1, p-value = 0.9816
# kruskal.test(Control$Damaged ~ Blackhawk$Damaged) # Kruskal-Wallis chi-squared = 2.9634, df = 1, p-value = 0.08517
# kruskal.test(Control$Damaged ~ Coragen$Damaged) # Kruskal-Wallis chi-squared = 0.48772, df = 1, p-value = 0.4849
# kruskal.test(Control$Damaged ~ Mustang$Damaged) # Kruskal-Wallis chi-squared = 1.7086, df = 1, p-value = 0.1912


#rarf3 <- data.frame(rbind(Agrimek,Azera,Blackhawk,Coragen,Mustang))
#alternativeModel4 <- glm(Damaged ~ Treatment, binomial, data=rarf3) # this model is without control effect
#summary(alternativeModel4) AIC 365.87 on 499 degrees of freedom
## exp(cbind(OddsRatio = coef(alternativeModel4), confint(alternativeModel4))) # code to get odds ratios and confidence intervals


## i'm assuming this is what is meant with poisson. quasipoisson?
#summary(glm(formula = Dseed ~ Treatment+Rep+Head, data = rarf, family = poisson)) # AIC 634.37 - higher than df, bad fit
#summary(glm(formula = Dseed ~ Treatment+Rep+Head, data = rarf, family = quasipoisson))



# citations
# http://www.r-tutor.com/elementary-statistics/non-parametric-methods/kruskal-wallis-test
# https://stats.stackexchange.com/questions/79360/mixed-effects-model-with-nesting/79392
# https://stats.stackexchange.com/questions/197977/analyzing-nested-categorical-data
# https://www.jstatsoft.org/article/view/v067i01/v67i01.pdf Fitting Linear Mixed-Effects Models using lme4
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#should-i-treat-factor-xxx-as-fixed-or-random
# Whether you explicitly specify a random effect as nested or not depends (in part) on the way the levels of the random effects are coded.
# If the ‘lower-level’ random effect is coded with unique levels, then the two syntaxes (1|a/b) (or (1|a)+(1|a:b)) and (1|a)+(1|b) are equivalent.
# If the lower-level random effect has the same labels within each larger group (e.g. blocks 1, 2, 3, 4 within sites A, B, and C) then the explicit nesting (1|a/b) is required.
# It seems to be considered best practice to code the nested level uniquely (e.g. A1, A2, …, B1, B2, …) so that confusion between nested and crossed effects is less likely.

# https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/
# https://rpsychologist.com/r-guide-longitudinal-lme-lmer


# FUTURE IDEAS
# https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
# https://cran.r-project.org/web/packages/nnet/nnet.pdf multinomial regression
# https://cran.r-project.org/web/packages/blme/blme.pdf bayesnian Generalized Linear Mixed Effects models
# https://stats.stackexchange.com/questions/132677/binomial-glmm-with-a-categorical-variable-with-full-successes Bayesnian
# https://stats.stackexchange.com/questions/391180/specifying-and-interpreting-mixed-effects-logistic-regression/391233#391233



