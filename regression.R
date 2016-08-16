# Clocks
clocksDat = read.csv("clocks.csv")
plot(clocksDat,pch=16)

clocksLinAge = lm(Price ~ Age, data=clocksDat)
summary(clocksLinAge)

intercept = coef(clocksLinAge)["(Intercept)"]
age = coef(clocksLinAge)["Age"]
qplot(clocksDat$Age,clocksDat$Price) + geom_abline(intercept = intercept, slope = age) + xlim(-10,200) + ylim(-1000,2200)

# What is the interpretation for the intercept?
# A clock that is zero years old will cost on average -$191.66

# What is the interpretation for the slope?
# A one year increase in clock age will correspond on average to a $10.48 increase in price.

# How much would we predict a 175 year old clock would cost?
# intercept + age*175 = 1642.184

clocksLinAgeBid = lm(Price ~ Age + Bidders, data=clocksDat)
summary(clocksLinAgeBid)

# What is the interpretation for the intercept?
# A clock that is zero years old and has no bidders will cost on average -$1336.72.

# What is the interpretation for the age slope?
# All things equal, a one year increase in clock age will correspond on average to a $12.7362 increase in price.

# What is the interpretation for the bidders slope?
# All things equal, one more bidder will correspond on average to a $85.8151 increase in price

clocksLinAgeBidInt = lm(Price ~ Age + Bidders + Age*Bidders, data=clocksDat)
summary(clocksLinAgeBidInt)

# How to interpret these values?

# A one year increase in clock age will correspond on average to a $323.62 + $1.2979*(Number of bidders) increase in price.

# A one bidder increase will correspond on average to a $229.3455 + $1.2979*(Age) increase in price.

# Which is the best model?

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

# What is the probability of surviving if carpaceLength is 30mm?
# 1/(1+exp(-(B0+B1*x)))
prob30 = 1/(1+exp(-(lobsterIntercept+lobsterCarpaceLength*30)))

# What is the probability of surviving if carpaceLength is 31mm?
prob31 = 1/(1+exp(-(lobsterIntercept+lobsterCarpaceLength*31)))

# What is the probability of surviving if carpaceLength is 55mm?
prob55 = 1/(1+exp(-(lobsterIntercept+lobsterCarpaceLength*55)))

# Odds ratio
OR = exp((lobsterIntercept+lobsterCarpaceLength*31))/exp((lobsterIntercept+lobsterCarpaceLength*30))
OR = exp((lobsterIntercept+lobsterCarpaceLength*18))/exp((lobsterIntercept+lobsterCarpaceLength*17))

# Odd of surival if carpaceLength is 30mm?
odd30 = prob30/(1-prob30) # 0.13439

# Odd of surival if carpaceLength is 31mm?
odd31 = prob31/(1-prob31) # 0.16330

# Odds ratio
odd31/odd30

# Interpretation
# All things equal, a 1mm increase in carpace length will have 21.5% increase in the odds of survival.

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

# What is the probability of surviving if male?
probM = 1/(1+exp(-(titanicIntercept+Sexmale*1)))

# What is the probability of surviving if female?
probF = 1/(1+exp(-(titanicIntercept+Sexmale*0)))

# We can double-check these probabilities
survivedM = sum(titanicDat$Sex=='male'&titanicDat$Survived==1)
diedM = sum(titanicDat$Sex=='male'&titanicDat$Survived==0)
survivedF = sum(titanicDat$Sex=='female'&titanicDat$Survived==1)
diedF = sum(titanicDat$Sex=='female'&titanicDat$Survived==0)

survivedM/(survivedM+diedM) #0.1889
survivedF/(survivedF+diedF) #0.7404

# Odds ratio
OR = exp((titanicIntercept+Sexmale*1))/exp((titanicIntercept+Sexmale*0)) #0.08167
OR = exp(Sexmale) #0.08167

# male odd of survival 
survivedM/(survivedM+diedM)/(1-survivedM/(survivedM+diedM)) # 0.232

# female odd of survival 
survivedF/(survivedF+diedF)/(1-survivedF/(survivedF+diedF)) # 2.8518

# Interpretation
# All things equal, males are associated with a 91.83% decrease in the odds of survival than females.

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

# Interpretation
# All things equal, a person one year older is associated with a 4% decrease in the odds of survival.

# Interpretation
# All things equal, one decrease in class is associated with a 33% decrease in the odds of survival.

anova(titanicLogitFull, test="Chisq")

###
fitted.results <- predict(titanicLogitFull,newdata=subset(testDat,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testDat$Survived)
print(paste('Accuracy',1-misClasificError))
