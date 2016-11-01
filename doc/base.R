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

sift <- fread("./Fall2016-proj3-grp10-master/data/sift_features.csv")
sift2 <- sift

#use this to refresh the data
sift <- sift2
sift <- as.matrix(sift)
sift <- t(sift)
colnames(sift) <- paste("feature",c(1:5000))
sift <- data.frame(sift)
sift$class <- ifelse(grepl("chicken",rownames(sift)),1,0)
sift$class <- factor(sift$class,labels = c("chicken","dog"))

this <- colSums(subset(sift,select = -class))
while(min(this) < 0.0001){
  print("here")
  that <- as.integer(which.min(this))
  print(that)
  sift <- sift[,-that]
  this <- this[-that]
}

#dividing training and test sample
t_x <- sample(nrow(sift),nrow(sift)*0.15)
X_dataset <- sift[-t_x,]
X_test_dataset <- sift[t_x,]
X_train <- subset(sift[-t_x,], select= -class )
X_test <- subset(sift[t_x,],select = -class)
Y_train <- sift$class[-t_x]
Y_test <- sift$class[t_x]

#svm_poly...accuracy rate 47.667%
model_svm_poly <- svm(class~., data = X_dataset,method = "C-classification", kernel = "polynomial", cost = 0.1, gamma = 0.1)
pred_svm_poly <- predict(model_svm_poly, X_test)
sum(pred_svm_poly==Y_test)/length(Y_test)

#svm_line...accuracy rate 68%
model_svm_line <- svm(class~., data = X_dataset,method = "C-classification", kernel = "linear", cost = 0.1, gamma = 0.1)
pred_svm_line <- predict(model_svm_line, X_test)
sum(pred_svm_line==Y_test)/length(Y_test)

#kernlab_poly...accuracy rate 68%
model_kern_poly <- ksvm(class~., data = X_dataset,type = "C-svc",kernel = "polydot")
pred_kern_poly <- predict(model_kern_poly, X_test)
sum(pred_kern_poly==Y_test)/length(Y_test)

#kernlab_poly...accuracy rate 68%
model_kern_line <- ksvm(class~., data = X_dataset,type = "C-svc",kernel = "vanilladot")
pred_kern_line <- predict(model_kern_line, X_test)
sum(pred_kern_line==Y_test)/length(Y_test)

#tree...accuracy rate 61.3%
model_tree <- ctree(class ~ ., data = X_dataset)
pred_tree <- predict(model_tree,X_test)
sum(pred_tree==Y_test)/length(Y_test)

#glm...does not converge
model_gml <- glm(formula = class~. ,family = binomial(link = "logit"),data = X_dataset)
#pred_gml <- predict(model_gml,X_test)
#sum(pred_gml==Y_test)/length(Y_test)

#adaboost 74%
model_adaboost <- boosting(formula = class~.,data= X_dataset , boos=TRUE)
pred_adaboost <- predict(model_adaboost,X_test)
sum(pred_adaboost$class==Y_test)/length(Y_test)

model_adaboost$importance
error_adaboost <- errorevol(model_adaboost,X_test_dataset)
x<-c(1:100)
ada_table <- rbind(x,error_adaboost$error)
ada_table <- t(ada_table)
ada_table <- as.data.frame(ada_table)
colnames(ada_table) <- c("iteration","error_rate")
plot(x=ada_table$iteration,y=ada_table$error_rate)






#####THIS SECTION IS NOT YET COMPLETED
#deep learning h2o
localH2O <- h2o.init(ip = "localhost", port = 50002)#does not work for me
sift.h2o <- as.h2o(localH2O, sift.r, key="iris.h2o")

#convert this line: sift.hex = h2o.importFile(localH2O, path = prosPath, key = "sift.hex")
s <- h2o.runif(sift.hex)
summary(s)

sift.train = sift.hex[s <= 0.8,]
sift.train = h2o.assign(sift.train, "sift.train")
sift.test = sift.hex[s > 0.8,]
sift.test = h2o.assign(sift.test, "sift.test")
nrow(sift.train) + nrow(sift.test)

h2o.gbm(y = dependent, x = independent, data = australia.hex, n.trees
        = 15, interaction.depth = 5,
        n.minobsinnode = 2, shrinkage = 0.01, distribution= "multinomial")

