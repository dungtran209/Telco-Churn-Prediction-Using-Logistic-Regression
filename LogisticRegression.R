rm(list=ls())

# run sessionInfo() in order to see what packages and data sets are attached 
# Please make sure that  unnecessary packages and datasets are unloaded to avoid masking
sessionInfo()

setwd("C:\\xxx")
getwd()
data<-read.csv("xxx.csv", na.strings="")
class(data)
dim(data)
names(data)
str(data)
attach(data)
vector1<-c("TotalIntlCalls", 
           "TotalDayCalls", 
           "TotalEveMinutes", 
           "TotalIntlMinutes", 
           "TotalDayMinutes", 
           "TotalEveCalls",
           "NumbervMailMessages", 
           "CustomerServiceCalls", 
           "TotalCall",
           "TotalNightCalls",
           "TotalNightMinutes")
data[PhoneService=="No", vector1]<-0

# those variables with factor values and still missing values
vector2<-c("InternationalPlan", "VoiceMailPlan")
# replace the missing values of no phone service customers with "No"
data[PhoneService=="No", vector2]<-"No"

#randomly select the some numbers from the list of 10
# This is being done to randomly select whose customers will have the line, but never use it
set.seed(123)
size<-sample(c(1:10),1)
missing_value_index<-unique(which(is.na(data),1)[,1])
m<-sample(missing_value_index,size=size)

# replace the missing values of variables corresponding to the randomly selected rows
data[m,vector1]<-0
data[m,vector2]<-"No"

# impute the missing values for the rest of the customers in the missing_value_index
missing_value_index<-missing_value_index[!missing_value_index %in% m]

require(imputeMissings)
data<-impute(data)

(final_summary<-summary(data))

# Trying to compare the outcome of the imputation of the missing values
raw_summary[,"TotalIntlCalls"]

detach(data)

# Need to recode "No service" to "No" in Multiples lines, OnlineSecurity, OnlineBackup,
#DeviceProtection, TechSupport, StreamingTV, StreamingMovies
require(plyr)
recode_columns<-c(10:15)
for(i in 1:ncol(data[recode_columns])){
  data[,recode_columns][,i]<-as.factor(mapvalues(data[,recode_columns][,i], from =c("No internet service"), to = c("No")))
}
# Replace "No phone service" with "No"    
data$MultipleLines<-as.factor(mapvalues(data$MultipleLines, from=c("No phone service"), to =c("No")))

# Replace the values in SeniorCitizen variable from 0 and 1 to Yes and No
data$SeniorCitizen<-as.factor(mapvalues(data$SeniorCitizen, from=c("0","1"), to=c("No", "Yes")))


summary(data$tenure)
#do we need to group the values into 6 levels?
tenure_bin<-function(tenure){
  if (tenure>=0 & tenure<= 12){
    return('0-12 Mnth')
  }else if (tenure>12 & tenure<=24){
    return('12-24 Mnth')
  }else if (tenure>24 & tenure<=36){
    return ('24-36 Mnth')
  }else if (tenure>36 & tenure<=48){
    return ('36-48 Mnth')
  }else if (tenure>48 & tenure<=60){
    return ('48-60 Mnth')
  }else if (tenure>60 ){
    return ('60 Mnth and greater')
  }}
data$tenure_bin<-sapply(data$tenure, tenure_bin)
data$tenure_bin<-as.factor(data$tenure_bin)
data$tenure<-NULL


#recode variable InternetService to two level "Yes" and "No"
data$InternetService<-as.factor(mapvalues(data$InternetService, from=c("DSL","Fiber optic"), to =c("Yes","Yes")))


#remove the customer ID column
data$customerID<-NULL

summary(data)


#Split data into train and test

ind <- sample.int(n=nrow(data),size=nrow(data)*0.70, replace=FALSE)
train <- data[ind,]
test <- data[-ind,]
y_train<-train$Churn
train$Churn<-NULL
y_test<-test$Churn
test$Churn<-NULL

#Test whether there are no intersects
intersect(train, test)

LR <- glm(formula = y_train~., data = train, family = binomial("logit"))
names(LR)
summary(LR)

require(AUC)

LRstep <- step(object = LR, direction = 'both')
names(LRstep)
predLRstep <- as.numeric(predict(LRstep, test,type = 'response'))
(auc_step <- AUC::auc(roc(predLRstep, y_test)))
summary(LRstep)


fit.null<-glm(y_train~0, data=train, family=binomial("logit"))
fit1<-glm(y_train~PhoneService,data=train, family=binomial("logit"))
fit2<-glm(y_train~PhoneService+InternetService,data=train, family=binomial("logit"))

summary(fit.null)
summary(fit1)
summary(fit2)

#AIC
AIC(fit.null)
AIC(fit1)
AIC(fit2)


#BIC
BIC(fit.null)
BIC(fit1)
BIC(fit2)

#Cross-validates summ of squares
CVSS <- function(model) {
  return(sum((model$residuals/(1 - hatvalues(model)))^2))
}

CVSS(fit.null)
CVSS(fit1)
CVSS(fit2)


fit3<-glm(y_train~PhoneService+InternetService+Contract, data=train, family=binomial("logit"))
fit4<-glm(y_train~PhoneService+InternetService+Contract+PaymentMethod,data=train, family=binomial("logit"))
fit5<-glm(y_train~PhoneService+InternetService+Contract+PaymentMethod+MonthlyCharges,data=train, family=binomial("logit"))

summary(fit3)
summary(fit4)
summary(fit5)

#AIC
AIC(fit3)
AIC(fit4)
AIC(fit5)


#BIC
BIC(fit3)
BIC(fit4)
BIC(fit5)

CVSS(fit3)
CVSS(fit4)
CVSS(fit5)


fit6<-glm(y_train~PhoneService+InternetService+Contract+PaymentMethod+MonthlyCharges+CustomerServiceCalls, data=train, family=binomial("logit"))
fit7<-glm(y_train~PhoneService+InternetService+Contract+PaymentMethod+MonthlyCharges+CustomerServiceCalls+TotalCall,data=train, family=binomial("logit"))
fit8<-glm(y_train~PhoneService+InternetService+Contract+PaymentMethod+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin,data=train, family=binomial("logit"))

summary(fit6)
summary(fit7)
summary(fit8)

#AIC
AIC(fit6)
AIC(fit7)
AIC(fit8)


#BIC
BIC(fit6)
BIC(fit7)
BIC(fit8)

CVSS(fit6)
CVSS(fit7)
CVSS(fit8)


#remove Contract and PmtMethod from the model
fit9<-glm(y_train~PhoneService+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall,data=train, family=binomial("logit"))
summary(fit9)
AIC(fit9)
BIC(fit9)
CVSS(fit9)

fit10<-glm(y_train~PhoneService+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin,data=train, family=binomial("logit"))
summary(fit10)
AIC(fit10)
BIC(fit10)
CVSS(fit10)

# add StreamingTV
fit11<-glm(y_train~PhoneService+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV,data=train, family=binomial("logit"))
summary(fit11)
AIC(fit11)
BIC(fit11)
CVSS(fit11)


#Add Streaming Movies
fit11<-glm(y_train~PhoneService+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+StreamingMovies,data=train, family=binomial("logit"))
summary(fit11)
AIC(fit11)
BIC(fit11)
CVSS(fit11)
#remove StreamingMovies
#Add InternationalPlan

fit12<-glm(y_train~PhoneService+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+InternationalPlan,data=train, family=binomial("logit"))
summary(fit12)
AIC(fit12)
BIC(fit12)
CVSS(fit12)
#remove InternationalPlan
#Add VoiceMailPLan
fit13<-glm(y_train~PhoneService+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))
summary(fit13)
AIC(fit13)
BIC(fit13)
CVSS(fit12)
#remove PhoneService
fit14<-glm(y_train~InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))
summary(fit14)
AIC(fit14)
BIC(fit14)
CVSS(fit14)
#Add OnlineSecurity
fit15<-glm(y_train~InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan+OnlineSecurity,data=train, family=binomial("logit"))
summary(fit15)
AIC(fit15)
BIC(fit15)
CVSS(fit15)
#remove OnlineSecurity
#Add gender
fit16<-glm(y_train~gender+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))
summary(fit16)
AIC(fit16)
BIC(fit16)
CVSS(fit16)
#Add SeniorCitizen
fit17<-glm(y_train~SeniorCitizen+gender+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))
summary(fit17)
AIC(fit17)
BIC(fit17)
CVSS(fit17)
#remove SeniorSitizen and gender
#Add dependents
fit18<-glm(y_train~Dependents+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))
summary(fit18)
AIC(fit18)
BIC(fit18)
CVSS(fit18)
# CVSS jumped up
#remove Dependents
#Tech Support
fit19<-glm(y_train~TechSupport+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))
summary(fit19)
AIC(fit19)
BIC(fit19)
CVSS(fit19)
#remove TechSupport
#run the model
fit20<-glm(y_train~InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))
summary(fit20)
AIC(fit20)
BIC(fit20)
CVSS(fit20)
#remove StreamingTV and VoiceMailPlan
fit21<-glm(y_train~InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin,data=train, family=binomial("logit"))
summary(fit21)
AIC(fit21)
BIC(fit21)
CVSS(fit21)

pred.fit <- as.numeric(predict(fit21,test, type='response'))
require(AUC)
(auc_lr <- auc(roc(pred.fit, y_test))) 
