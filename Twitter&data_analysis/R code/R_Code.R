#memory clear
rm(list = ls())

##First way
set.seed(234)
nums<-rpois(6,4)
gender<-sample(c("Male", "Female"), size=6, replace = TRUE)
product<-sample(c("A", "B", "C"), size=6, replace = TRUE)
data_set<-cbind(gender,product,nums)
data<-as.data.frame(data_set)
library(plyr)
df <- rename(data, c("nums" = "price"))

#Second way
dataset <- expand.grid(gender=c("female", "male"), product=c("A","B","C"))
set.seed(234)
price <- rpois(6, 4)
(ProductPrice <- cbind(dataset,price))

#Find the best distribution for the data##################
library(vcd)
data(WomenQueue)
head(WomenQueue)

barplot(WomenQueue, main="Women Queue Depature Distribution",xlab="Counts",ylab="Frequency")

Women_Ord=Ord_plot(obj=WomenQueue,main="Women Queue")
Women_Ord
#Since slope is Negative and Intercept is positive, it is determined that this is a Binomial Distribution.



#Step1- Contingency tables
#Two-way tables
data("Hospital", package="vcd")
Hospital

#Two-way tables (Question 1)
#Find the row proportions for this table, i.e., the proportions of patients with differing length of stay for each value of visit frequency.
#What do you see there that relates to the question of independence of the table variables?
prop.table(Hospital,1)

#Two-way tables (Question 2)
#Carry out a \(\chi^2\) test for association between the two variables. What do you conclude?
chisq.test(Hospital)
#As the p-value is smaller than the .05 significance level, we reject the null hypothesis. 
#Therefore variables are associated, there is no dependency.


#Two-way tables (Question 3)
#Say you donâ t trust the asymptotic p-value from the test because the sample size is relatively small. See help(chisq.test)for how to get a Monte Carlo simulated p-value. Do it.

chisq.test(Hospital, simulate.p.value = TRUE)
#Still under the Monte Carlo Simulation, we see that the p-value is smaller than alpha.
#It can be reassured that the variables are associated.

#Two-way tables (Question 4) Extra Credit
#Use MASS::loglm() to carry out the standard \(\chi^2\) test
library(MASS)
Hospital.mod <- loglm(~1 + 2, data = Hospital, fitted = TRUE)
Hospital.mod



#Loglinear Model
#Survival on the Titanic
data("Titanic")
Titanic

#Loglinear Model (Question 1)
Titanic.nonzero <- Titanic + 0.5
Titanic.glm1 <- glm(Freq~ Class + Sex + Age, data= Titanic, subset= (Freq> 0), family= poisson)
Titanic.glm2 <- glm(Freq~ Class + Sex + Age, data= Titanic.nonzero, family= poisson)
summary(Titanic.glm1)
summary(Titanic.glm2)

#Loglinear Model (Question 2)
Titanic.mod0 <- loglm(~ 1*2*3 + 4, data=Titanic)
mosaic(Titanic.mod0, main="Model [AGC][S]", labeling = labeling_residuals)

#We find that:
#There is strong association between non-survival to crew and 3rd class. It seemed that higher class is more likely to be saved and survived at last.
#Regardiless of the class type, children is more likely to survive. It seemed that children is first helped and saved.
#For first class, it seemed that female is more likely to survive.

#Loglinear Model (Question 3) Extra Credit
Titanic.mod1 <- loglm(~ 1*2*3 + 1*4 + 2*4 + 3*4, data=Titanic)
# Or we can write (1+2+3)*4
mosaic(Titanic.mod1, main="Model [AGC][AS][GS][CS]", labeling = labeling_residuals)
#After we take possible association between gender, age, and class with survivual, mosaic plot showed that remaining association reduced a lot. 
#Many association has been captured when we have considered 1-1 association.


anova(Titanic.mod0, Titanic.mod1, test="chisq")
#From ANOVA test, we find that the association decreased a lot after we add different association combination to the model.


#Logistic Regression
#Volunteering for a psychology experiment
library(car)
data(Cowles, package="car")

#Logistic regression (Question 1)
scatterplotMatrix(Cowles)
#From scatter plot, it seemed that male and female volunteer not equally often, 
#but the decision to volunteer seemed to be related to extraversion.

#Logistic regression (Question 2)
Cowles.mod0 <- glm(volunteer ~ . , data = Cowles, family = binomial)
summary(Cowles.mod0)
#From the result, it seemed male is slightly unlikely to volunteer, 
#and there is positive correlation between volunteer and extraversion.

#While other factors are constant, if it is male, there is 20.96% decrease of the odds of volunteer.
#While other factors are constant, the increase of extraversion will lead to a 6.86% increase of the odds of volunteer.

Anova(Cowles.mod0)
#From ANOVA result, there seemed to be association between features.


#Logistic regression (Question 3)
Cowles.mod1 <- glm(volunteer ~ (sex + neuroticism + extraversion)^2, data=Cowles, family=binomial)
summary(Cowles.mod1)

Anova(Cowles.mod1)
#It seemed that sex is still one important feature. The combination of neuroticisim and extraversion was significant as well.

Cowles.mod <- glm(volunteer ~ sex + neuroticism * extraversion, data = Cowles, family = binomial)
summary(Cowles.mod)
#From the result, we verified that all factors, including the interaction factor, are statistically significant.

anova(Cowles.mod0, Cowles.mod1, Cowles.mod)
#It seemed that it is benifical to add interaction factor to the model. However, it seemed that we needn't add the interaction of all factors. 
#Only the interaction of neuroticism and extraversion is enough.
library(effects)
Cowles.effect <- allEffects(Cowles.mod, xlevels = list(extraversion = seq(0, 24, 8)))
plot(Cowles.effect, multiline=TRUE)

#Effect plot showed similar conclusion. Male and Female are not equally often for volunteer. 
#The different groups of combination of extraversion and neuroticism indicated different probability of volunteer.