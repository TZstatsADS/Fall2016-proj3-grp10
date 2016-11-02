#GBM#
library(gbm)
Sift<-read.csv("C:/Users/Owner/Desktop/ADS/Project3/SIFT - Color - Texture.csv",header=T)
Sift<-na.omit(Sift[,-1])
Sift<-t(Sift)
y<-vector(length=2000)
y[1:1000]<-1
y[1001:2000]<-0
Sift<-cbind(Sift,y)
ind_train<-sample(2000,1600)
ind_test<-c(1:2000)[-ind_train]
Sift_train<-Sift[ind_train,]
Sift_test<-Sift[ind_test,]
fit_gb<-gbm(y~.,data=data.frame(Sift_train),distribution="bernoulli",n.trees=100,shrinkage=1,interaction.depth=2,cv.folds=5)
#train error#
train_error_gbm<-predict(fit_gb,newdata=data.frame(Sift_train[,c(1:5033)]))
for(i in 1:length(train_error_gbm))
{if(train_error_gbm[i]>=0)
{train_error_gbm[i]=1}
  else{train_error_gbm[i]=0}}
sum(train_error_gbm==Sift_train[,5034])/length(train_error_gbm)
#test error#
y_hat_gbm<-predict(fit_gb,newdata=data.frame(Sift_test[,c(1:5033)]))
for(i in 1:length(y_hat_gbm))
{if(y_hat_gbm[i]>=0)
{y_hat_gbm[i]=1}
  else{y_hat_gbm[i]=0}}
sum(y_hat_gbm==Sift_test[,5034])/length(y_hat_gbm)



#deep learning: tanh#
library(h2o)
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
dat_h2o <- as.h2o(data.frame(Sift_train))
model <- 
  h2o.deeplearning(x = 1:5033,  # column numbers for predictors
                   y = 5034,   # column number for label
                   dat_h2o, # data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5,0.5,0.5), # % for nodes dropout
                   balance_classes = FALSE, 
                   hidden = c(50,50,50,50,50), # five layers of 50 nodes
                   epochs = 100,
                   nfolds=5,  #cross-valid part
                   fold_assignment =c("Random"),
                   keep_cross_validation_predictions = TRUE,
                   keep_cross_validation_fold_assignment = TRUE)
test_h2o<-as.h2o(data.frame(Sift_test[,-5034]))
h2o_yhat_test <- h2o.predict(model, test_h2o)
y_hat_dl <- as.data.frame(h2o_yhat_test)
y_hat_dl<-y_hat_dl[,1]
for(i in 1:length(y_hat_dl))
{if(y_hat_dl[i]>=0.5)
{y_hat_dl[i]=1}
  else{y_hat_dl[i]=0}}
sum(y_hat_dl==Sift_test[,5034])/length(y_hat_dl)


#Adaboost#
fit_ada<-gbm(y~.,data=data.frame(Sift_train),distribution="adaboost",n.trees=100,shrinkage=1,cv.folds=5)
#train error#
train_error_ada<-predict(fit_ada,newdata=data.frame(Sift_train[,c(1:5033)]))
for(i in 1:length(train_error_ada))
{if(train_error_ada[i]>=0)
{train_error_ada[i]=1}
  else{train_error_ada[i]=0}}
sum(train_error_ada==Sift_train[,5034])/length(train_error_ada)
#test error#
y_hat_ada<-predict(fit_ada,newdata=data.frame(Sift_test[,c(1:5033)]))
for(i in 1:length(y_hat_ada))
{if(y_hat_ada[i]>=0)
{y_hat_ada[i]=1}
  else{y_hat_ada[i]=0}}
sum(y_hat_ada==Sift_test[,5034])/length(y_hat_ada)

#Xgboost#
library(xgboost)
library(data.table)
#sift_naive <- fread("SIFT - Color - Texture.csv")
#sift_naive <- data.frame(t(sift_naive)[-1,]) 
#colnames(sift_naive) <- paste("feature",c(1:dim(sift_naive)[2]))
#sift_naive$class <- ifelse(grepl("chicken",rownames(sift_naive)),1,0)
#sift_naive <- sift_naive[sample(1:2000),]
#train_naive <- sift_naive[1:round(nrow(sift_naive)*0.8),] 
#test_naive <- sift_naive[1601:2000,]



h<-sample(nrow(Sift_train),200)
dval<-xgb.DMatrix(data=Sift_train[h,1:(dim(Sift)[2]-1)],
                  label=Sift_train[h,dim(Sift)[2]], missing=NA)
dtrain<-xgb.DMatrix(data=Sift_train[-h,1:(dim(Sift)[2]-1)],
                    label=Sift_train[-h,dim(Sift)[2]], missing=NA)
watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eta                 = 0.025, 
                max_depth           = 6, 
                subsample           = 0.7, 
                colsample_bytree    = 0.9, 
                # eval_metric         = "rmse",
                metrics={'error'},
                min_child_weight    = 6
)

clf <- xgb.train(data = dtrain, 
                 params               = param, 
                 nrounds              = 1000, #300
                 verbose              = 0,#2
                 #watchlist            = watchlist,
                 early.stop.round     = NULL, 
                 maximize            = FALSE,
                 print.every.n        = 1
)
#clf$bestScore
#cat("Submit file\n")
test_relevance <- predict(clf,Sift_test[,1:(dim(Sift)[2]-1)],
                          ntreelimit =clf$bestInd, missing=NA)
p <- ifelse(test_relevance>0.5,1,0)
sum(p==Sift_test[,dim(Sift)[2]])/length(p)

#deep learning: Maxout#
model1 <- 
  h2o.deeplearning(x = 1:5033,  # column numbers for predictors
                   y = 5034,   # column number for label
                   dat_h2o, # data in H2O format
                   activation = "MaxoutWithDropout", # or 'Max'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5,0.5,0.5), # % for nodes dropout
                   balance_classes = FALSE, 
                   hidden = c(50,50,50,50,50), # five layers of 50 nodes
                   epochs = 100,
                   nfolds=5,  #cross-valid part
                   fold_assignment =c("Random"),
                   keep_cross_validation_predictions = TRUE,
                   keep_cross_validation_fold_assignment = TRUE)
h2o_yhat_test1 <- h2o.predict(model1, test_h2o)
y_hat_dl_m <- as.data.frame(h2o_yhat_test1)
y_hat_dl_m<-y_hat_dl_m[,1]
for(i in 1:length(y_hat_dl_m))
{if(y_hat_dl_m[i]>=0.5)
{y_hat_dl_m[i]=1}
  else{y_hat_dl_m[i]=0}}
sum(y_hat_dl_m==Sift_test[,5034])/length(y_hat_dl_m)





#EM-Majority Votes#
temp<-y_hat_ada+y_hat_dl+y_hat_gbm+p+y_hat_dl_m
m_v<-vector(length=length(temp))
for(i in 1:length(m_v))
{ if(temp[i]>=3)
{ m_v[i]=1}
  else{m_v[i]=0}}
sum(m_v==Sift_test[,5034])/length(m_v)
   

