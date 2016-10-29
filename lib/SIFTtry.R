#SIFT try#
#gbm#
library(gbm)
Sift<-read.csv(file.choose(),header=T)
Sift<-t(Sift)
y<-vector(length=2000)
y[1:1000]<-0
y[1001:2000]<-1
Sift_1<-cbind(Sift,y)
ind_train<-sample(2000,1000)
ind_test<-c(1:2000)[-ind_train]
Sift_train<-Sift_1[ind_train,]
Sift_test<-Sift_1[ind_test,]
fit_gb<-gbm(y~.,data=data.frame(Sift_train),distribution="bernoulli",n.trees=2000,interaction.depth=2,cv.folds=10)
y_hat<-predict(fit_gb,newdata=data.frame(Sift_test[,c(1:5000)]),n.tree=2000)
for(i in 1:length(y_hat))
{if(y_hat[i]>=0)
 {y_hat[i]=1}
else{y_hat[i]=0}}
sum(y_hat==Sift_test[,5001])/length(y_hat)


#h2o#
library(h2o)
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
dat_h2o <- as.h2o(data.frame(Sift_train))
model <- 
  h2o.deeplearning(x = 1:5000,  # column numbers for predictors
                   y = 5001,   # column number for label
                   dat_h2o, # data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5,0.5,0.5), # % for nodes dropout
                   balance_classes = FALSE, 
                   hidden = c(50,50,50,50,50), # five layers of 50 nodes
                   epochs = 100)
                   #nfolds=5,  #cross-valid part
                   #fold_assignment =c("Random"),
                   #keep_cross_validation_predictions = TRUE,
                   #keep_cross_validation_fold_assignment = TRUE)
test_h2o<-as.h2o(data.frame(Sift_test[,-5001]))
h2o_yhat_test <- h2o.predict(model, test_h2o)
df_yhat_test <- as.data.frame(h2o_yhat_test)
df_yhat_test<-df_yhat_test[,1]
for(i in 1:length(df_yhat_test))
{if(df_yhat_test[i]>=0.5)
{df_yhat_test[i]=1}
  else{df_yhat_test[i]=0}}
sum(df_yhat_test==Sift_test[,5001])/length(df_yhat_test)



#Plus Color data:#
Sift_color<-read.csv(file.choose(),header=T)
Sift_color<-Sift_color[,-1]
Sift_color<-t(Sift_color)
y<-vector(length=2000)
y[1:1000]<-0
y[1001:2000]<-1
Sift_2<-cbind(Sift_color,y)
ind_train<-sample(2000,1000)
ind_test<-c(1:2000)[-ind_train]
Sift_train2<-Sift_2[ind_train,]
Sift_test2<-Sift_2[ind_test,]
library(h2o)
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
dat_h2o <- as.h2o(data.frame(Sift_train2))
model <- 
  h2o.deeplearning(x = 1:5001,  # column numbers for predictors
                   y = 5002,   # column number for label
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
test_h2o<-as.h2o(data.frame(Sift_test2[,-5002]))
h2o_yhat_test <- h2o.predict(model, test_h2o)
df_yhat_test <- as.data.frame(h2o_yhat_test)
df_yhat_test<-df_yhat_test[,1]
for(i in 1:length(df_yhat_test))
{if(df_yhat_test[i]>=0.5)
{df_yhat_test[i]=1}
  else{df_yhat_test[i]=0}}
sum(df_yhat_test==Sift_test[,5001])/length(df_yhat_test)
