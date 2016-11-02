#Test.R#
library(gbm)
library(h2o)
library(xgboost)
library(data.table)

#model & data#
Sift_test<-readRDS("Sift_test.RData")
fit_gb<-readRDS("fit_gb.RData")
model<-readRDS("model.RData")
fit_ada<-readRDS("fit_ada.RData")
clf<-readRDS("clf.RData")
model1<-readRDS("model1.RData")
#GBM#
y_hat_gbm<-predict(fit_gb,newdata=data.frame(Sift_test[,c(1:5033)]))
for(i in 1:length(y_hat_gbm))
{if(y_hat_gbm[i]>=0)
{y_hat_gbm[i]=1}
 else{y_hat_gbm[i]=0}}

#deep learning: tanh#
test_h2o<-as.h2o(data.frame(Sift_test[,-5034]))
h2o_yhat_test <- h2o.predict(model, test_h2o)
y_hat_dl <- as.data.frame(h2o_yhat_test)
y_hat_dl<-y_hat_dl[,1]
for(i in 1:length(y_hat_dl))
{if(y_hat_dl[i]>=0.5)
{y_hat_dl[i]=1}
  else{y_hat_dl[i]=0}}

#Adaboost#
y_hat_ada<-predict(fit_ada,newdata=data.frame(Sift_test[,c(1:5033)]))
for(i in 1:length(y_hat_ada))
{if(y_hat_ada[i]>=0)
{y_hat_ada[i]=1}
  else{y_hat_ada[i]=0}}

#Xgboost#
test_relevance <- predict(clf,Sift_test[,1:(dim(Sift_test)[2]-1)],
                            ntreelimit =clf$bestInd, missing=NA)
p <- ifelse(test_relevance>0.5,1,0)

#deep learning: Maxout#
h2o_yhat_test1 <- h2o.predict(model1, test_h2o)
y_hat_dl_m <- as.data.frame(h2o_yhat_test1)
y_hat_dl_m<-y_hat_dl_m[,1]
for(i in 1:length(y_hat_dl_m))
{if(y_hat_dl_m[i]>=0.5)
{y_hat_dl_m[i]=1}
  else{y_hat_dl_m[i]=0}}

#EM: Majority votes#
temp<-y_hat_ada+y_hat_dl+y_hat_gbm+p+y_hat_dl_m
m_v<-vector(length=length(temp))
for(i in 1:length(m_v))
{ if(temp[i]>=3)
{ m_v[i]=1}
  else{m_v[i]=0}}

saveRDS(m_v,"testresult.RData")
                        
