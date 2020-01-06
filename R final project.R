#Written by Peter Kwakye & Yan Chen December 10 2019
#Analysis of polarizability values of molecules and their representation in terms of their molecular desscriptors 
features <-read.csv("~/Downloads/R final project/2019-03-28 CE451-551 L18 v5.0 tutorial/data/molorg_features.csv")
pol<-read.csv("~/Downloads/R final project/2019-03-28 CE451-551 L18 v5.0 tutorial/data/molorg_pol.csv", sep="")
features $polarizability<-pol$pol_Bohr3
features<-na.omit(features)
features$Psi_i_1s<-NULL ###constant 0
library(glmnet)
library(caret)
require(ISLR)
require(caTools)
library(randomForest)
library(gbm)
library(leaps)
library(ggplot2)
###histogram
hist(features $polarizability)
##scatterplot with correlated variables
plot(features $SpPos_B.p.,features $polarizability)
plot(features $VE2sign_L,features $polarizability)
plot(features $MW,features $polarizability)
set.seed(12345)
train=sample(1:nrow(features),round(0.80*nrow(features)))
test=-train
testdata=features [test,]
traindata=features [train,]
results1<-lm(polarizability~SpPos_B.p., traindata) ###only using SpPos_B.p.
summary(results1)
pred1<-predict(results1, testdata)
lm_test_RMSE1<-sqrt(mean((testdata$polarizability-pred1)^2)) ##12.14067

results2<-lm(polarizability~VE2sign_L, traindata) ###only using SpPos_B.p.
summary(results2)
pred2<-predict(results2, testdata)
lm_test_RMSE2<-sqrt(mean((testdata$polarizability-pred2)^2)) ##19.46847


results<-lm(polarizability~., traindata) ##use all variables
summary(results)
pred<-predict(results, testdata)
lm_test_RMSE<-sqrt(mean((testdata$polarizability-pred)^2))  ###19.79608



###ridge
X<-as.matrix(features[,-c(700)])
Y<-features$polarizability
ridge.mod=glmnet(X,Y,alpha=0)
plot(ridge.mod)
ridgetrain<-sample(1:nrow(X),round(0.8*nrow(X)))

cv.out.ridge<-cv.glmnet(X[ridgetrain,],Y[ridgetrain],alpha=0)
plot(cv.out.ridge)
bestlamridge<-cv.out.ridge$lambda.min
bestlamridge
ridge.predcoef<-predict(cv.out.ridge,s=bestlamridge,type="coefficients")
ridge.predresp<-predict(cv.out.ridge,s=bestlamridge,newx=X[-ridgetrain,],type='response')

ridge_test_RMSE<-sqrt(mean((ridge.predresp-Y[-ridgetrain])^2))####4.833469

####lasso
lasso.mod=glmnet(X,Y,alpha=1)
plot(lasso.mod)
lassotrain<-sample(1:nrow(X),round(0.8*nrow(X)))

cv.out.lasso<-cv.glmnet(X[lassotrain,],Y[lassotrain],alpha=1)
plot(cv.out.lasso)
bestlamlasso<-cv.out.lasso$lambda.min
bestlamlasso
lasso.predcoef<-predict(cv.out.lasso,s=bestlamlasso,type="coefficients")
lasso.predresp<-predict(cv.out.lasso,s=bestlamlasso,newx=X[-lassotrain,],type='response')

lasso_test_RMSE<-sqrt(mean((lasso.predresp-Y[-lassotrain])^2))###1.269062

##pcr
library(pls)
pcr.train=sample(1:nrow(features),round(0.80*nrow(features)))
pcr.test=-pcr.train
y.pcr.test=features$polarizability[pcr.test]
y.pcr.train=features$polarizability[pcr.train]
pcr.fit=pcr(polarizability~., data=features, subset=pcr.train, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="RMSEP")
pcr.pred.test=predict(pcr.fit,features[pcr.test,],ncomp=120)
pcr_test_RMSE<-sqrt(mean((pcr.pred.test-y.pcr.test)^2))### 5.88577


###pls
pls.train=sample(1:nrow(features),round(0.80*nrow(features)))
pls.test=-pls.train
y.pls.test=features$polarizability[pls.test]
y.pls.train=features$polarizability[pls.train]
pls.fit=plsr(polarizability~., data=features, subset=pcr.train, scale=TRUE, validation="CV")
summary(pls.fit)

validationplot(pls.fit,val.type="RMSEP")
pls.pred.test=predict(pls.fit,features[pls.test,],ncomp=45)
pls_test_RMSE<-sqrt(mean((pls.pred.test-y.pls.test)^2))####5.003426


###random forest
rf.fit<-randomForest(polarizability~., data=traindata, n.tree=10000)
rf_y_hat<-predict(rf.fit,newdata =testdata,type='response')
rf_test_RMSE<-sqrt(mean((testdata$polarizability-rf_y_hat)^2))###8.682586


###bagging
bag.fit<-randomForest(polarizability~., data=traindata, n.tree=10000,mtry=700)
bag_y_hat<-predict(bag.fit,newdata = testdata,type='response')
bag_test_RMSE<-sqrt(mean((testdata$polarizability-bag_y_hat)^2))###8.815865

###boosting not between 0 and 1
# boost.fit<-gbm(polarizability~., data=traindata,n.trees=1000,shrinkage=.1,interaction.depth=3,distribution='adaboost')
# boost_y_hat<-predict(boost.fit,newdata=testdata,n.trees=1000,type='response')
# boosting.1_test_RMSE<-sqrt(mean((testdata$polarizability-boost_y_hat)^2))

##best subset selection
best_subset<- regsubsets(polarizability~., data = traindata, nvmax = 700, method=("exhaustive"),really.big=T)
best_summary<-summary(best_subset)
test_matrix<-model.matrix(polarizability~., data=traindata)
test.error=c()
##length(test.error)=656
for (i in 1:656) {
  coeffi=coef(best_subset, id=i)
  pred.test<-test_matrix[,names(coeffi)]%*%coeffi
  test.error[i]<-mean((testdata$polarizability-pred.test)^2)
}
plot(1:length(test.error),test.error, xlab='number of variables', ylab='test MSE',main='test MSE')
which.min(test.error)
test.error[1]##MSE=3728.864
coef(best_subset, id=1)### MW
coef(best_subset, id=2)##AMW MW
coef(best_subset, id=3)##AMW MW Sv
