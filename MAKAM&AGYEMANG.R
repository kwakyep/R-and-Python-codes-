## Created by Peter K, MS June 1,2020
rm(list=ls())
setwd("~/Documents/Project NWFP")
library(readxl)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(caret)
library(DMwR) 
library(corrplot)
DBD <- read_excel("FINALB1.xls")

head(DBD)
str(DBD)
summary(DBD)

glimpse(DBD)

####Creating a copy of the original datafram so I can edit it & keep track of changes from original

DBDa<- as.data.frame(DBD)


str(DBD)
table(DBD$NoShow)
str(DBDa)

DBDa$Month <-as.factor(DBDa$Month)
DBDa$AMPM <-as.factor(DBDa$AMPM)
DBDa$Sex <-factor(DBDa$Sex, levels = c('female','male'))
DBDa$Appt_Type <- as.factor(DBDa$Appt_Type)
DBDa$ProviderName <- as.factor(DBDa$ProviderName)
DBDa$NoShow <- as.factor(DBDa$NoShow)
DBDa$Primary_Insurance_Name <-  as.factor(DBDa$Primary_Insurance_Name)
DBDa$Date <- as.Date(DBDa$Date)
# DBDa$Time <-as_datetime(DBDa$Time)
# DBDa$Time <-as.factor(DBDa$Time)
glimpse(DBDa)

summary(DBDa)
str(DBDa)

range(DBDa$Age)
hist(DBDa$Age)


ggplot(DBDa, aes(x=NoShow, fill= ProviderName)) + geom_bar(position = "fill")

summary(DBDa)


DBDa %>% group_by(ProviderName) %>% filter(n()=50) %>% 
  ggplot(DBDa,aes(x=ProviderName, fill= NoShow)) + 
  geom_bar()+coord_flip()


# DBDa  %>%  group_by(Appt_Type,Sex,NoShow)  %>% 
#   summarise(total.count=n()) %>% 
#   filter(total.count >5) %>%
#   ggplot(DBDa,aes(x=ProviderName, fill= NoShow)) + 
#   geom_bar()+coord_flip()

A <- cor(balDBDa)
corrplot(A,method="circle")

set.seed(1234)
train = sample(1:nrow(DBDa),0.85*nrow(DBDa))
test = -train
testdata = DBDa[test,]
traindata = DBDa[train,]

library(randomForest)

rf = randomForest(NoShow~.-Date-Time-Primary_Insurance_Name-DOB,ntree=1000, mtry = 4, data =DBDa, importance = T,na.action =na.exclude )
plot(rf)

varImpPlot(rf,main = "Variable Importance")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='Primary_Insurance_Name',which.class = "1")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='Sex',which.class = "1")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='Month',which.class = "1")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='AMPM',which.class = "1")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='ProviderName',which.class = "1")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='Sex',which.class = "2")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='Month',which.class = "2")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='AMPM',which.class = "2")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='ProviderName',which.class = "2")


######Balancing Data

# nswlogit <- glm(NoShow~.,data = traindata, family = "binomial")
balDBDa <- as.data.frame(DBDa)
balDBDa$Date <-NULL
balDBDa$Time <-NULL
balDBDa$Primary_Insurance_Name<-NULL
balDBDa$DOB <-NULL


folds <- createFolds(factor(balDBDa$NoShow), k = 5, list = FALSE)

TrainingDataIndex <- createDataPartition(balDBDa$NoShow, p=0.75, list = FALSE)
# Create Training Data 
trainingData <- balDBDa[TrainingDataIndex,]
testData <- balDBDa[-TrainingDataIndex,]
TrainingParameters <- trainControl(method = "repeatedcv", number = 5, repeats=5)


balancedata <-SMOTE(NoShow~.,trainingData,perc.over = 200, k = 10,perc.under = 250,na.action =na.exclude)

table(balancedata$NoShow)

rf = randomForest(NoShow~.,ntree=1000, mtry = 4, data =balancedata, importance = T,na.action =na.exclude )
plot(rf)

varImpPlot(rf,main = "Variable Importance")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='Sex',which.class = "1")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='Month',which.class = "1")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='AMPM',which.class = "1")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='ProviderName',which.class = "1")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='PAppt_Type',which.class = "1")

partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='Sex',which.class = "2")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='Month',which.class = "2")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='AMPM',which.class = "2")
partialPlot(x=rf, pred.data = as.data.frame(traindata),x.var='ProviderName',which.class = "2")



# as.data.frame(table(balancedata$attemptedsuicideyouth))


########################################### MODELS FOR ACCURACY, SENSITIVITY, RECALL ETC

####### Logistic Regression
data_ctrl <- trainControl(method = "cv", number = 5)

model_caret <- train(NoShow~.,   # model to fit
                     data = balDBDa,                        
                     trControl = data_ctrl,# folds
                     method = "glm",# specifying regression model
                     na.action = na.pass)# pass missing data to model - some models wi
print(model_caret)
glmpred <- predict(model_caret,testData)
cmGLM <-confusionMatrix(glmpred, testData$NoShow)
cmGLM

model_caret$finalModel
model_caret$resample


####### Bagging
model_bagging <- train(NoShow~.,   # model to fit
                       data = balDBDa,                        
                       trControl = data_ctrl,# folds
                       method = "treebag",# specifying regression model
                       na.action = na.pass)# pass missing data to model - some models wi
print(model_bagging)
bagpred <- predict(model_bagging,testData)
cmBag <-confusionMatrix(bagpred, testData$NoShow)
cmBag

model_bagging$finalModel
model_bagging$resample

####### RandomForest
#Number randomly variable selected is mtry
mtry <- sqrt(ncol(balDBDa))
tunegrid <- expand.grid(.mtry=mtry)
model_rf <- train(
  NoShow~.,   
  data = balDBDa,                        
  trControl = data_ctrl,
  tuneGrid = tunegrid,
  method = "rf",
  importance = TRUE,
)
print(model_rf)

rfpred <- predict(model_rf,testData)
cmRf <-confusionMatrix(rfpred, testData$NoShow)
cmRf

model_rf$finalModel
model_rf$resample

varimp_rf <-varImp(model_rf)
plot(varimp_rf, main = "Variable Importance with Random Forest (No-Shows)")


############### Repeating steps with balanced data now 

####### Logistic Regression
b_data_ctrl <- trainControl(method = "cv", number = 5)

b_model_caret <- train(NoShow~.,   # model to fit
                     data = balancedata,                        
                     trControl = data_ctrl,# folds
                     method = "glm",# specifying regression model
                     na.action = na.pass)# pass missing data to model - some models wi
print(b_model_caret)
b_glmpred <- predict(b_model_caret,testData)
b_cmGLM <-confusionMatrix(glmpred, testData$NoShow)
b_cmGLM

b_model_caret$finalModel
b_model_caret$resample


####### Bagging
b_model_bagging <- train(NoShow~.,   # model to fit
                       data = balancedata,                        
                       trControl = data_ctrl,# folds
                       method = "treebag",# specifying regression model
                       na.action = na.pass)# pass missing data to model - some models wi
print(b_model_bagging)
b_bagpred <- predict(b_model_bagging,testData)
b_cmBag <-confusionMatrix(b_bagpred, testData$NoShow)
b_cmBag

b_model_bagging$finalModel
b_model_bagging$resample

####### RandomForest
#Number randomly variable selected is mtry
b_mtry <- sqrt(ncol(balancedata))
b_tunegrid <- expand.grid(.mtry=mtry)
b_model_rf <- train(
  NoShow~.,   
  data = balancedata,                        
  trControl = data_ctrl,
  tuneGrid = tunegrid,
  method = "rf",
  importance = TRUE,
)
print(b_model_rf)

b_rfpred <- predict(b_model_rf,testData)
b_cmRf <-confusionMatrix(b_rfpred, testData$NoShow)
b_cmRf

b_model_rf$finalModel
b_model_rf$resample

b_varimp_rf <-varImp(b_model_rf)
plot(b_varimp_rf, main = "Variable Importance with Random Forest (No-Show)")

save(list=ls(all=TRUE), file ="Kwakye_NWFP.RData")


