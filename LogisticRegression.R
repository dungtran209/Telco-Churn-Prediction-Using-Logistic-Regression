rm(list=ls())

setwd("C:\\Users\\Asus\\Desktop\\MSc BA\\Predictive Analytics and Modelling of Data\\Term Project")
getwd()


data <- read.csv("Edinburgh_test.csv",  na.strings = "")

#######DATA UNDERSTANDING & DATA PREPARATION ###########
########################################################

class(data)
str(data)
colnames(data)
summary(data)

data$customerID <- as.character(data$customerID)
data$SeniorCitizen <- as.factor(data$SeniorCitizen)

# NA values
sapply(data, function(x) sum (is.na(x)))
#retutrns 13columns*21 NA values in each
rowSums(is.na(data))
#returns 21 row with 13 NA vallues in each

# How to deal with missing values?
# Among 21 these observations, there are 11 customers with no phone service and 10 customers who have phone service

attach(data)

# Create the vectors that contains all the variables with the missing values
vector1 <- c("TotalIntlCalls","TotalDayCalls", "TotalEveMinutes","TotalIntlMinutes","TotalDayMinutes","TotalEveCalls","NumbervMailMessages",
             "CustomerServiceCalls","TotalCall", "TotalNightCalls", "TotalNightMinutes")
vector2 <- c("InternationalPlan","VoiceMailPlan")

# Replace the missing values of No phone service customers with 0 or "No"
data[PhoneService == "No", vector1] <- 0
data[PhoneService == "No", vector2] <- "No"

# With 10 remaining customers, randomly replace 3 with 0/"No" and impute the others with median/mode
set.seed(123)

missingvalue <- which(is.na(data),1)
missingvalue_index <- unique(missingvalue[,1])
m <- sample(missingvalue_index,3)
data[m, vector1] <- 0
data[m, vector2] <- "No"

# Impute the missing values for the rest of the customers in the missingvalue_index with the median/mode
require(imputeMissings)
data <- impute(data)

detach(data)

# Need to recode "No service" to "No" in Multiples lines, OnlineSecurity, OnlineBackup,
#DeviceProtection, TechSupport, StreamingTV, StreamingMovies
install.packages("plyr")
require(plyr)

# Replace "No internet service" with "No"
recode_columns<-c("OnlineSecurity", "OnlineBackup","DeviceProtection", "TechSupport", "StreamingTV","StreamingMovies")
for(i in 1:ncol(data[recode_columns])){
  data[,recode_columns][,i]<-as.factor(mapvalues(data[,recode_columns][,i], from =c("No internet service"), to = c("No")))
}
#recode variable InternetService to two level "Yes" and "No"
data$InternetService<-as.factor(mapvalues(data$InternetService, from=c("DSL","Fiber optic"), to =c("Yes","Yes")))

# Replace "No phone service" with "No"    
data$MultipleLines<-as.factor(mapvalues(data$MultipleLines, from=c("No phone service"), to =c("No")))

# Replace the values in SeniorCitizen variable from 0 and 1 to Yes and No
data$SeniorCitizen<-as.factor(mapvalues(data$SeniorCitizen, from=c("0","1"), to=c("No", "Yes")))

data$Churn<-as.factor(mapvalues(data$Churn, from=c("No","Yes"), to=c("0", "1")))


summary(data$tenure)

#do we need to group the values into 6 levels?
tenure_bin<-function(tenure){
  if (tenure>=0 & tenure<= 12){
    return('0-12 Month')
  }else if (tenure>12 & tenure<=24){
    return('12-24 Month')
  }else if (tenure>24 & tenure<=36){
    return ('24-36 Month')
  }else if (tenure>36 & tenure<=48){
    return ('36-48 Month')
  }else if (tenure>48 & tenure<=60){
    return ('48-60 Month')
  }else if (tenure>60 ){
    return ('60 Month & More')
  }}

data$tenure_bin<-sapply(data$tenure, tenure_bin)
data$tenure_bin<-as.factor(data$tenure_bin)

#Remove the columns which are not neccessary for analysis
data$customerID<-NULL
data$tenure<-NULL

str(data)
#data[,vector1]<-NULL
#data[,vector2]<-NULL
cor(data[,sapply(data,is.numeric)])

#Correlation between numeric variables
correlation_matrix<-cor(data[,sapply(data,is.numeric)])
install.packages("corrplot")
require(corrplot)
corrplot(correlation_matrix, main="\n\nCorrelation Plot (Numeric Variables)", method="number")
#Monthly Charges and Total Charges are highly correlated 

#Do we need to remove one of them? Total Charges?
data$TotalCharges<-NULL
data$TotalCall <- NULL

##Calculate the average minute of Calls
data1 <- data

data1$AvgIntlMin <- ifelse(data$TotalIntlCalls == 0,0,data$TotalIntlMinutes/data$TotalIntlCalls)
data1$AvgDayMin <- ifelse(data$TotalDayCalls == 0,0,data$TotalDayMinutes/data$TotalDayCalls)
data1$AvgEveMin <- ifelse(data$TotalEveCalls == 0,0,data$TotalEveMinutes/data$TotalEveCalls)
data1$AvgNightMin <- ifelse(data$TotalNightCalls == 0,0,data$TotalNightMinutes/data$TotalNightCalls)
vector3 <- c("TotalIntlCalls", "TotalDayCalls", "TotalEveMinutes","TotalIntlMinutes","TotalDayMinutes","TotalEveCalls", "TotalNightCalls", "TotalNightMinutes")
vector4 <- c("AvgIntlMin", "AvgDayMin", "AvgEveMin", "AvgNightMin ")

data1[,vector3] <- NULL
data2 <- data1
data2[,vector4] <- NULL


correlation_matrix<-cor(data[,sapply(data,is.numeric)])
correlation_matrix1<-cor(data1[,sapply(data1,is.numeric)])
corrplot(correlation_matrix1, main="\n\nCorrelation Plot Data1 (Numeric Variables)", method="number")


#Visualize the numerics variables 
install.packages("ggplot2")
install.packages("gridExtra")
require(ggplot2)
require(gridExtra)


#Visualize the categorical variables 

p1 <- ggplot(data, aes(x = gender, ..count..)) + xlab(NULL) + ggtitle("Gender") + geom_bar(aes(fill=Churn),position = "fill")
p2 <- ggplot(data, aes(x = SeniorCitizen, ..count..)) + xlab(NULL) + ggtitle("Senior Citizen") + geom_bar(aes(fill=Churn),position = "fill")
p3 <- ggplot(data, aes(x = Partner, ..count..)) + xlab(NULL) + ggtitle("Partner") + geom_bar(aes(fill=Churn),position = "fill")
p4 <- ggplot(data, aes(x = Dependents, ..count..)) + xlab(NULL) + ggtitle("Dependents") + geom_bar(aes(fill=Churn),position = "fill")
grid.arrange(p1, p2, p3, p4, nrow=2)

p5 <- ggplot(data, aes(x = PhoneService, ..count..)) + xlab(NULL) + ggtitle("PhoneService") + geom_bar(aes(fill=Churn),position = "fill")
p6 <- ggplot(data, aes(x = MultipleLines, ..count..)) + xlab(NULL) + ggtitle("MultipleLines") + geom_bar(aes(fill=Churn),position = "fill")
p7 <- ggplot(data, aes(x = InternetService, ..count..)) + xlab(NULL) + ggtitle("InternetService") + geom_bar(aes(fill=Churn),position = "fill")
p8 <- ggplot(data, aes(x = OnlineSecurity, ..count..)) + xlab(NULL) + ggtitle("OnlineSecurity") + geom_bar(aes(fill=Churn),position = "fill")
grid.arrange(p5, p6, p7, p8, nrow=2)

p9 <- ggplot(data, aes(x = OnlineBackup, ..count..)) + xlab(NULL) + ggtitle("OnlineBackup") + geom_bar(aes(fill=Churn),position = "fill")
p10 <- ggplot(data, aes(x = DeviceProtection, ..count..)) + xlab(NULL) + ggtitle("DeviceProtection") + geom_bar(aes(fill=Churn),position = "fill")
p11 <- ggplot(data, aes(x = TechSupport, ..count..)) + xlab(NULL) + ggtitle("TechSupport") + geom_bar(aes(fill=Churn),position = "fill")
p12 <- ggplot(data, aes(x = StreamingTV, ..count..)) + xlab(NULL) + ggtitle("StreamingTV") + geom_bar(aes(fill=Churn),position = "fill")
grid.arrange(p9, p10, p11, p12, nrow=2)

p13 <- ggplot(data, aes(x = StreamingMovies, ..count..)) + xlab(NULL) + ggtitle("StreamingMovies") + geom_bar(aes(fill=Churn),position = "fill")
p14 <- ggplot(data, aes(x = Contract, ..count..)) + xlab(NULL) + ggtitle("Contract") + geom_bar(aes(fill=Churn),position = "fill")
p15 <- ggplot(data, aes(x = PaperlessBilling, ..count..)) + xlab(NULL) + ggtitle("PaperlessBilling") + geom_bar(aes(fill=Churn),position = "fill")
p16 <- ggplot(data, aes(x = PaymentMethod, ..count..)) + xlab(NULL) + ggtitle("PaymentMethod") + geom_bar(aes(fill=Churn),position = "fill")
grid.arrange(p13, p14, p15, p16, nrow=2)


p17 <- ggplot(data, aes(x = tenure_bin, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Tenure") + geom_bar(aes(fill=Churn),position = "fill") +theme(legend.position = 'none')+scale_fill_manual(values=c("#00668F","#009CDB"))
p18 <- ggplot(data, aes(x = InternationalPlan, ..count..)) + xlab(NULL) + ylab("Proportion") +ggtitle("International Plan") + geom_bar(aes(fill=Churn),position = "fill") +scale_fill_manual(values=c("#00668F","#009CDB"))
p19 <- ggplot(data, aes(x = VoiceMailPlan, ..count..)) + xlab(NULL) + ylab("Proportion") +ggtitle("Voice Mail Plan") + geom_bar(aes(fill=Churn),position = "fill") +scale_fill_manual(values=c("#00668F","#009CDB"))
grid.arrange(p17, p18, p19, layout_matrix = matrix(c(1,2,1,3), nrow =2) )


#Visualize the numerical variables 

p20 <- ggplot(data, aes(x = Churn, y = TotalIntlCalls, fill = Churn))+ xlab(NULL) + ylab(NULL) + ggtitle("Total Intl Calls") + geom_boxplot()
p21 <- ggplot(data, aes(x = Churn, y = TotalDayCalls, fill = Churn))+ xlab(NULL) + ylab(NULL) + ggtitle("Total Day Calls") + geom_boxplot()
p22 <- ggplot(data, aes(x = Churn, y = TotalEveCalls, fill = Churn))+ xlab(NULL) + ylab(NULL) + ggtitle("Total Eve Calls") + geom_boxplot()
p23 <- ggplot(data, aes(x = Churn, y = TotalNightCalls, fill = Churn))+ xlab(NULL) + ylab(NULL) + ggtitle("Total Night Calls") + geom_boxplot()
grid.arrange(p20, p21, p22, p23, nrow=2)

p24 <- ggplot(data, aes(x = Churn, y = TotalIntlMinutes, fill = Churn))+ xlab(NULL) + ylab(NULL) + ggtitle("Total Intl Minutes") + geom_boxplot()
p25 <- ggplot(data, aes(x = Churn, y = TotalDayMinutes, fill = Churn))+ xlab(NULL) + ylab(NULL) + ggtitle("Total Day Minutes") + geom_boxplot()
p26 <- ggplot(data, aes(x = Churn, y = TotalEveMinutes, fill = Churn))+ xlab(NULL) + ylab(NULL) + ggtitle("Total Eve Minutes") + geom_boxplot()
p27 <- ggplot(data, aes(x = Churn, y = TotalNightMinutes, fill = Churn))+ xlab(NULL) + ylab(NULL) + ggtitle("Total Night Minutes") + geom_boxplot()
grid.arrange(p24, p25, p26, p27, nrow=2)

p28 <- ggplot(data, aes(x = Churn, y = NumbervMailMessages, fill = Churn))+ xlab(NULL) + ylab(NULL) + ggtitle("Number Mail Messages") + geom_boxplot()
p29 <- ggplot(data, aes(x = Churn, y = CustomerServiceCalls, fill = Churn))+ xlab(NULL) + ylab(NULL) + ggtitle("Customer Service Calls") + geom_boxplot()
p30 <- ggplot(data, aes(x = Churn, y = TotalCall, fill = Churn))+ xlab(NULL) + ylab(NULL) + ggtitle("Total Calls") + geom_boxplot()
grid.arrange(p28, p29, p30, nrow=2)

plot(data$TotalIntlCalls, data$Churn)




auc.LR <- matrix(0,29,10)
install.packages("AUC")
require(AUC)


######## SPLIT DATA ############
################################

#Split data set into training set (75%) and test set (25%)
for (j in 1:10){
set.seed(123+j)

#Create indicators for the train/val/test set
ind <-sample(x =1:nrow(data), size = nrow(data),replace = FALSE)
trainind <- ind[1:round(length(ind)*.7)]
testind <- ind[round(length(ind)*.7+1):length(ind)] 

#Test whether there are no intersects
intersect(trainind,testind)

#Create the sets and separate the response
train <- data[trainind,]
y_train <- train$Churn
train$Churn <- NULL

test <- data[testind,]
y_test <- test$Churn
test$Churn <- NULL

intersect(row.names(train), row.names(test))

#Check the distribution of training and test sets
table(y_train)
table(y_test)
table(y_train)/length(y_train)
table(y_test)/length(y_test)


######### MODELLING ############
################################

LR <- glm(formula = y_train~., data = train, family = binomial("logit"))

#LR1 <- glm(y_train~MonthlyCharges+NumbervMailMessages+CustomerServiceCalls+AvgIntlMin+AvgDayMin+AvgEveMin+AvgNightMin,data = train, family = binomial("logit"))
#LR2 <- glm(Churn~gender+SeniorCitizen+Partner)

pred.LR <- as.numeric(predict(LR,test, type='response'))
auc.LR[1,j] <- AUC::auc(roc(pred.LR, y_test))
#plot(roc(pred.LR,y_test))


LRstep <- step(object = LR, direction = 'both')
pred.LRstep <- as.numeric(predict(LRstep,test, type='response'))
auc.LR[2,j] <- AUC::auc(roc(pred.LRstep, y_test))


fit.null<-glm(y_train~0, data=train, family=binomial("logit"))
fit1<-glm(y_train~PhoneService,data=train, family=binomial("logit"))
fit2<-glm(y_train~PhoneService+InternetService,data=train, family=binomial("logit"))

pred.fitnull <- as.numeric(predict(fit.null,test, type='response'))
auc.LR[3,j] <- AUC::auc(roc(pred.fitnull, y_test))

pred.fit1 <- as.numeric(predict(fit1,test, type='response'))
auc.LR[4,j] <- AUC::auc(roc(pred.fit1, y_test))

pred.fit2 <- as.numeric(predict(fit2,test, type='response'))
auc.LR[5,j] <- AUC::auc(roc(pred.fit2, y_test))


fit3<-glm(y_train~PhoneService+InternetService+Contract, data=train, family=binomial("logit"))
fit4<-glm(y_train~PhoneService+InternetService+Contract+PaymentMethod,data=train, family=binomial("logit"))
fit5<-glm(y_train~PhoneService+InternetService+Contract+PaymentMethod+MonthlyCharges,data=train, family=binomial("logit"))

pred.fit3 <- as.numeric(predict(fit3,test, type='response'))
auc.LR[6,j] <- AUC::auc(roc(pred.fit3, y_test))

pred.fit4 <- as.numeric(predict(fit4,test, type='response'))
auc.LR[7,j] <- AUC::auc(roc(pred.fit4, y_test))

pred.fit5 <- as.numeric(predict(fit5,test, type='response'))
auc.LR[8,j] <- AUC::auc(roc(pred.fit5, y_test))


fit6<-glm(y_train~PhoneService+InternetService+Contract+PaymentMethod+MonthlyCharges+CustomerServiceCalls, data=train, family=binomial("logit"))
fit7<-glm(y_train~PhoneService+InternetService+Contract+PaymentMethod+MonthlyCharges+CustomerServiceCalls+TotalCall,data=train, family=binomial("logit"))
fit8<-glm(y_train~PhoneService+InternetService+Contract+PaymentMethod+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin,data=train, family=binomial("logit"))

pred.fit6 <- as.numeric(predict(fit6,test, type='response'))
auc.LR[9,j] <- AUC::auc(roc(pred.fit6, y_test))

pred.fit7 <- as.numeric(predict(fit7,test, type='response'))
auc.LR[10,j] <- AUC::auc(roc(pred.fit7, y_test))

pred.fit8 <- as.numeric(predict(fit8,test, type='response'))
auc.LR[11,j] <- AUC::auc(roc(pred.fit8, y_test))


#remove Contract and PmtMethod from the model
fit9<-glm(y_train~PhoneService+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall,data=train, family=binomial("logit"))



fit10<-glm(y_train~PhoneService+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin,data=train, family=binomial("logit"))


# add StreamingTV

#Add Streaming Movies
fit11<-glm(y_train~PhoneService+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+StreamingMovies,data=train, family=binomial("logit"))

pred.fit9 <- as.numeric(predict(fit9,test, type='response'))
auc.LR[12,j] <- AUC::auc(roc(pred.fit9, y_test))

pred.fit10 <- as.numeric(predict(fit10,test, type='response'))
auc.LR[13,j] <- AUC::auc(roc(pred.fit10, y_test))

pred.fit11 <- as.numeric(predict(fit11,test, type='response'))
auc.LR[14,j] <- AUC::auc(roc(pred.fit11, y_test))


#remove StreamingMovies
#Add InternationalPlan

fit12<-glm(y_train~PhoneService+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+InternationalPlan,data=train, family=binomial("logit"))

#remove InternationalPlan
#Add VoiceMailPLan
fit13<-glm(y_train~PhoneService+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))

#remove PhoneService
fit14<-glm(y_train~InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))

#Add OnlineSecurity
fit15<-glm(y_train~InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan+OnlineSecurity,data=train, family=binomial("logit"))

#remove OnlineSecurity
#Add gender
fit16<-glm(y_train~gender+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))

#Add SeniorCitizen
fit17<-glm(y_train~SeniorCitizen+gender+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))

#remove SeniorSitizen and gender
#Add dependents
fit18<-glm(y_train~Dependents+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))

# CVSS jumped up
#remove Dependents
#Tech Support
fit19<-glm(y_train~TechSupport+InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))

#remove TechSupport
#run the model
fit20<-glm(y_train~InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin+StreamingTV+VoiceMailPlan,data=train, family=binomial("logit"))

#remove StreamingTV and VoiceMailPlan
fit21<-glm(y_train~InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin,data=train, family=binomial("logit"))

#Sensitivity Analysis
fit22<-glm(y_train~InternetService+CustomerServiceCalls+TotalCall+tenure_bin,data=train, family=binomial("logit")) #Remove Monthly Charges
fit23<-glm(y_train~MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin,data=train, family=binomial("logit")) #Remove InternetService
fit24<-glm(y_train~InternetService+MonthlyCharges+TotalCall+tenure_bin,data=train, family=binomial("logit")) #Remove CustomerServiceCall
fit25<-glm(y_train~InternetService+MonthlyCharges+CustomerServiceCalls+tenure_bin,data=train, family=binomial("logit")) #Remove TotalCall
fit26<-glm(y_train~InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall,data=train, family=binomial("logit")) #Remove TenureBin



pred.fit12 <- as.numeric(predict(fit12,test, type='response'))
auc.LR[15,j] <- AUC::auc(roc(pred.fit12, y_test))

pred.fit13 <- as.numeric(predict(fit13,test, type='response'))
auc.LR[16,j] <- AUC::auc(roc(pred.fit13, y_test))

pred.fit14 <- as.numeric(predict(fit14,test, type='response'))
auc.LR[17,j] <- AUC::auc(roc(pred.fit14, y_test))

pred.fit15 <- as.numeric(predict(fit15,test, type='response'))
auc.LR[18,j] <- AUC::auc(roc(pred.fit15, y_test))

pred.fit16 <- as.numeric(predict(fit16,test, type='response'))
auc.LR[19,j] <- AUC::auc(roc(pred.fit16, y_test))

pred.fit17 <- as.numeric(predict(fit17,test, type='response'))
auc.LR[20,j] <- AUC::auc(roc(pred.fit17, y_test))

pred.fit18 <- as.numeric(predict(fit18,test, type='response'))
auc.LR[21,j] <- AUC::auc(roc(pred.fit18, y_test))

pred.fit19 <- as.numeric(predict(fit19,test, type='response'))
auc.LR[22,j] <- AUC::auc(roc(pred.fit19, y_test))

pred.fit20 <- as.numeric(predict(fit20,test, type='response'))
auc.LR[23,j] <- AUC::auc(roc(pred.fit20, y_test))

pred.fit21 <- as.numeric(predict(fit21,test, type='response'))
auc.LR[24,j] <- AUC::auc(roc(pred.fit21, y_test))

pred.fit22 <- as.numeric(predict(fit22,test, type='response'))
auc.LR[25,j] <- AUC::auc(roc(pred.fit22, y_test))
pred.fit23 <- as.numeric(predict(fit23,test, type='response'))
auc.LR[26,j] <- AUC::auc(roc(pred.fit23, y_test))
pred.fit24 <- as.numeric(predict(fit24,test, type='response'))
auc.LR[27,j] <- AUC::auc(roc(pred.fit24, y_test))
pred.fit25 <- as.numeric(predict(fit25,test, type='response'))
auc.LR[28,j] <- AUC::auc(roc(pred.fit25, y_test))
pred.fit26 <- as.numeric(predict(fit26,test, type='response'))
auc.LR[29,j] <- AUC::auc(roc(pred.fit26, y_test))
}

#k-fold Cross-validation (k=10)

require(caret)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(1)
glmModel <- train(Churn~InternetService+MonthlyCharges+CustomerServiceCalls+TotalCall+tenure_bin, data = train, method = 'glm', family=binomial("logit"), trControl = fitControl)
summary(glmModel)

#The log odds ratio
coefficients(fit21)
#The odds ratio
exp(coefficients(fit21))


pred.fit <- as.numeric(predict(fit21,test, type='response'))
(auc_lr <- AUC::auc(roc(pred.fit, y_test)))
plot(roc(pred.fit21,y_test))
plot(roc(pred.fit22,y_test))
plot(roc(pred.fit23,y_test))
plot(roc(pred.fit24,y_test))
plot(roc(pred.fit25,y_test))
plot(roc(pred.fit26,y_test))

AUC::sensitivity(pred.fit21,y_test)


#make a prediction
predLR1 <- as.numeric(predict(fit21,test, type='response'))
predLR2 <- ifelse(predLR1 > 0.5, 1,0)
mean(predLR2 == y_test)


if (!require('glmnet')) {   
  install.packages('glmnet',
                   repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require('glmnet') 
}

lasso <- c("InternetService", "MonthlyCharges", "CustomerServiceCalls", "TotalCall", "tenure_bin")
train1= train[,lasso]

#Build a regularized logistic regression model
#Set alpha=0 if you want ridge regression
(LRl1 <- glmnet(x=data.matrix(train1),y=y_train,
                family="binomial"))

#Plot the lambda paramater
plot(LRl1, xvar= 'lambda')

#Look at the coefficients of the LR model for high and low values of lambda
coef(LRl1)[,1:2]
coef(LRl1)[,58:59]

predLRl1 <- predict(LRl1,newx=data.matrix(val),
                    type="response",
                    s=LRl1$lambda[1])
AUC::auc(roc(as.numeric(predLRl1),y_val))

predLRl1 <- predict(LRl1,newx=data.matrix(train),
                    type="response",
                    s=LRl1$lambda[1])
AUC::auc(roc(as.numeric(predLRl1),y_train))
