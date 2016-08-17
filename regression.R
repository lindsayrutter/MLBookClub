# Clocks
clocksDat = read.csv("clocks.csv")
plot(clocksDat,pch=16)

clocksLinAge = lm(Price ~ Age, data=clocksDat)
summary(clocksLinAge)

intercept = coef(clocksLinAge)["(Intercept)"]
age = coef(clocksLinAge)["Age"]
qplot(clocksDat$Age,clocksDat$Price) + geom_abline(intercept = intercept, slope = age) + xlim(-10,200) + ylim(-1000,2200)

# 1) What is the interpretation for the intercept?

# 2) What is the interpretation for the slope?

# 3) How much would we predict a 175 year old clock would cost?

clocksLinAgeBid = lm(Price ~ Age + Bidders, data=clocksDat)
summary(clocksLinAgeBid)

# 4) What is the interpretation for the intercept?

# 5) What is the interpretation for the age slope?

# 6) What is the interpretation for the bidders slope?

# 7) How much would we predict a 194 year old clock would cost if there are 5 bidders?

clocksLinAgeBidInt = lm(Price ~ Age + Bidders + Age*Bidders, data=clocksDat)
summary(clocksLinAgeBidInt)

# 8) What is the interpretation for the age slope?

# 9) What is the interpretation for the number of bidders slope?

# 10) How much would we predict a 194 year old clock would cost if there are 5 bidders?

# 11) Which model would you use? Simple linear regression? Multiple linear regression without interaction term? Or, multiple regression with interaction term?

# Lobsters
lobsterDat = read.csv("lobster.csv")
colnames(lobsterDat) = c("carpaceLength","survive")

lobsterLogit <- glm(survive ~ carpaceLength, data = lobsterDat, family = "binomial")
lobsterLogit

lobsterIntercept = coef(lobsterLogit)["(Intercept)"]
lobsterCarpaceLength = coef(lobsterLogit)["carpaceLength"]

carpaceLengths = seq(0,60,0.1)
survive = 1/(1+exp(-(lobsterIntercept+lobsterCarpaceLength*carpaceLengths)))
qplot(carpaceLengths,survive)

# 12) What is the probability of surviving if carpaceLength is 30mm?
# 1/(1+exp(-(B0+B1*x)))
# prob30 = 1/(1+exp(-(lobsterIntercept+lobsterCarpaceLength*30)))

# 13) What is the probability of surviving if carpaceLength is 31mm?
# prob31 = 1/(1+exp(-(lobsterIntercept+lobsterCarpaceLength*31)))

# 14) What is the probability of surviving if carpaceLength is 55mm?
# prob55 = 1/(1+exp(-(lobsterIntercept+lobsterCarpaceLength*55)))

# 15) What are the odds of survival if carpaceLength is 30mm?
# odd30 = prob30/(1-prob30) # 0.13439

# 16) What are the odds of survival if carpaceLength is 31mm?
# odd31 = prob31/(1-prob31) # 0.16330

# 17) What is the odds ratio?
# OR = exp((lobsterIntercept+lobsterCarpaceLength*31))/exp((lobsterIntercept+lobsterCarpaceLength*30))
# OR = exp((lobsterIntercept+lobsterCarpaceLength*18))/exp((lobsterIntercept+lobsterCarpaceLength*17))
# OR = odd31/odd30

# 18) What is the interpretation of the odds ratio?

################### TITANIC ##################

titanicDat = read.csv('titanic.csv',header=T,na.strings=c(""))
dim(titanicDat)
head(titanicDat)

# Look for NA values
sapply(titanicDat,function(x) sum(is.na(x)))
sapply(titanicDat, function(x) length(unique(x)))

library(Amelia)
missmap(titanicDat, main = "Missing values vs observed")

titanicDat <- subset(titanicDat,select=c(2,3,5,6,7,8,10,12))
# Replace missing values for age with average age 29.69912
titanicDat$Age[is.na(titanicDat$Age)] <- mean(titanicDat$Age,na.rm=T)

is.factor(titanicDat$Sex)
is.factor(titanicDat$Embarked)
contrasts(titanicDat$Sex)
contrasts(titanicDat$Embarked)

titanicDat <- titanicDat[!is.na(titanicDat$Embarked),]
rownames(titanicDat) <- NULL

trainDat <- titanicDat[1:800,]
testDat <- titanicDat[801:889,]

titanicLogit <- glm(Survived ~ Sex, data = titanicDat, family = "binomial")
titanicLogit

titanicIntercept = coef(titanicLogit)["(Intercept)"]
#Pclass = coef(titanicLogit)["Pclass"]
Sexmale = coef(titanicLogit)["Sexmale"]
#Age = coef(titanicLogit)["Age"]
#SibSp = coef(titanicLogit)["SibSp"]
#Parch = coef(titanicLogit)["Parch"]
#Fare = coef(titanicLogit)["Fare"]
#EmbarkedQ = coef(titanicLogit)["EmbarkedQ"]
#EmbarkedS = coef(titanicLogit)["EmbarkedS"]

# 19) What is the probability of surviving if male?
# probM = 1/(1+exp(-(titanicIntercept+Sexmale*1)))

# 20) What is the probability of surviving if female?
# probF = 1/(1+exp(-(titanicIntercept+Sexmale*0)))

# We can double-check these probabilities
# survivedM = sum(titanicDat$Sex=='male'&titanicDat$Survived==1)
# diedM = sum(titanicDat$Sex=='male'&titanicDat$Survived==0)
# survivedF = sum(titanicDat$Sex=='female'&titanicDat$Survived==1)
# diedF = sum(titanicDat$Sex=='female'&titanicDat$Survived==0)

# 21) What are the odds of surviving if male? 
# survivedM/(survivedM+diedM)
# survivedM/(survivedM+diedM)/(1-survivedM/(survivedM+diedM))

# 22) What are the odds of surviving if female?
# survivedF/(survivedF+diedF)
# survivedF/(survivedF+diedF)/(1-survivedF/(survivedF+diedF))

# 23) What is the odds ratio?
# OR = exp((titanicIntercept+Sexmale*1))/exp((titanicIntercept+Sexmale*0)) #0.08167
# OR = exp(Sexmale) #0.08167

# 24) What is the interpretation of the odds ratio?

####################################

titanicLogitFull <- glm(Survived ~ ., data = titanicDat, family = "binomial")

summary(titanicLogitFull)

titanicIntercept = coef(titanicLogit)["(Intercept)"]
Pclass = coef(titanicLogit)["Pclass"]
Sexmale = coef(titanicLogit)["Sexmale"]
Age = coef(titanicLogit)["Age"]
SibSp = coef(titanicLogit)["SibSp"]
Parch = coef(titanicLogit)["Parch"]
Fare = coef(titanicLogit)["Fare"]
EmbarkedQ = coef(titanicLogit)["EmbarkedQ"]
EmbarkedS = coef(titanicLogit)["EmbarkedS"]

# 25) What is the interpretation of the odds ratio for age?

# 26) What is the interpretation of the odds ratio for class?

anova(titanicLogitFull, test="Chisq")

fitted.results <- predict(titanicLogitFull,newdata=subset(testDat,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testDat$Survived)
print(paste('Accuracy',1-misClasificError))
