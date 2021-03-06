setwd("C:/Users/CATHY/OneDrive/Documents/2016-2017 Junior/Applied Data Science/Project 3/")
library("dplyr")
library("data.table")
library("caret")
library("ROCR")
library("e1071")
library("data.table")
library("kernlab")
library("party")
library("h2o")
library("adabag")
library("plotly")
library("gbm")

sift <- fread("./Fall2016-proj3-grp10-master/data/final_feature2.csv")
#sift<-read.csv("./Fall2016-proj3-grp10-master/data/final_feature2.csv", header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
sift2 <- sift

#use this to refresh the data
sift <- sift2
colnames(sift) <- paste("feature",c(1:ncol(sift)))
sift <- subset(sift,select = -`feature 1`)
colnames(sift) <- paste("feature",c(1:ncol(sift)))
sift<-t(sift)
# sift <- as.matrix(sift)
# sift <- sift[,-1]
# sift <- t(sift)
# sift <- na.omit(sift)
sift <- as.data.frame(sift)

#sift$class <- ifelse(grepl("chicken",rownames(sift)),1,0)
a.vector <- rep(1,1000)
a.vector <- c(a.vector,rep(0,1000))
sift$class<- a.vector

this <- colSums(subset(sift,select = -class))
while(min(this) < 0.0001){
  print("here")
  that <- as.integer(which.min(this))
  print(that)
  sift <- sift[,-that]
  this <- this[-that]
}


#dividing training and test sample
t_x <- sample(nrow(sift),nrow(sift)*0.20)
X_dataset <- sift[-t_x,]
X_test_dataset <- sift[t_x,]
X_train <- subset(sift[-t_x,], select= -class )
X_test <- subset(sift[t_x,],select = -class)
Y_train <- sift$class[-t_x]
Y_test <- sift$class[t_x]


fit.gbm1 <- gbm(class ~ ., data=X_dataset, dist="adaboost", n.tree = 400,shrinkage = 1)

pred_gbm <- predict(fit.gbm1, X_test,n.tree=10)
for(i in 1:length(pred_gbm)){
  if(pred_gbm[i] < 0){
    pred_gbm[i] = 0
  }
  if(pred_gbm[i] > 0){
     pred_gbm[i] = 1
  }
}
table(pred_gbm,Y_test)
sum(pred_gbm==Y_test)/length(Y_test)

# #svm_poly...accuracy rate 50.3% 47.667%, 42,667%
# model_svm_poly <- svm(class~., data = X_dataset,method = "C-classification", kernel = "polynomial", cost = 0.1, gamma = 0.1)
# pred_svm_poly <- predict(model_svm_poly, X_test)
# table(pred_svm_poly,Y_test)
# sum(pred_svm_poly==Y_test)/length(Y_test)
# 
# #svm_line...accuracy rate 68.667% 68% 70%
# model_svm_line <- svm(class~., data = X_dataset,method = "C-classification", kernel = "linear", cost = 0.1, gamma = 0.1)
# pred_svm_line <- predict(model_svm_line, X_test)
# sum(pred_svm_line==Y_test)/length(Y_test)
# 
# #kernlab_poly...accuracy rate 68.667% 68%
# model_kern_poly <- ksvm(class~., data = X_dataset,type = "C-svc",kernel = "polydot")
# pred_kern_poly <- predict(model_kern_poly, X_test)
# sum(pred_kern_poly==Y_test)/length(Y_test)
# 
#kernlab_poly...accuracy rate 68.667% 68%
model_kern_line <- ksvm(class~., data = X_dataset,type = "C-svc",kernel = "vanilladot")
pred_kern_line <- predict(model_kern_line, X_test)
sum(pred_kern_line==Y_test)/length(Y_test)
 
#tree...accuracy rate 58% 61.3% ... with color 74% ... with texture 75%
model_tree <- ctree(class ~ ., data = X_dataset)
pred_tree <- predict(model_tree,X_test)
sum(pred_tree==Y_test)/length(Y_test)



sift$class <- factor(sift$class,labels = c("chicken","dog"))
#adaboost 74% 71% 74% ... with color 84% ... with texture 
model_adaboost_color_texture <- boosting(formula = class~., data= X_dataset, boos=TRUE,mfinal=100)
save(model_adaboost_color_texture, file = "model_adaboost_color_texture.Rdata")
pred_adaboost <- predict(model_adaboost_color_texture,X_test)
sum(pred_adaboost$class==Y_test)/length(Y_test)
 
# model_adaboost$importance
# error_adaboost <- errorevol(model_adaboost,X_test_dataset)
# x<-c(1:100)
# ada_table <- rbind(x,error_adaboost$error)
# ada_table <- t(ada_table)
# ada_table <- as.data.frame(ada_table)
# colnames(ada_table) <- c("iteration","error_rate")
# plot(x=ada_table$iteration,y=ada_table$error_rate)

