setwd("/Users/zuonianyao/Documents/GR5243/Project3")
library("dplyr")
library("data.table")
library("caret")
library("ROCR")

sift <- fread("sift_features.csv")
sift <- as.matrix(sift)
sift <- t(sift)
colnames(sift) <- paste("feature",c(1:5000))
sift <- data.frame(sift)
sift$class <- ifelse(grepl("chicken",rownames(sift)),1,0)
train <- sift[1:round(nrow(sift)*0.8),] 
test <- sift[1601:2000,]
# tree <- train(class~.,data=train,method ="rpart",trControl = trainControl(method = "cv"))
# p <- predict(tree,test)

table(p,test$class)
fit <- glm(formula = class~. ,family = binomial(link = "logit"),data = train)
p <- ifelse(p>0.5,1,0)
summary(fit)
