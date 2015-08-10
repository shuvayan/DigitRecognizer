library(caTools)
digits = read.csv('C:/Users/DELL/Documents/R/Kaggle/DigitRecognizer/train.csv', header=TRUE)
split = sample.split(digits$label, SplitRatio=.7)
train = subset(digits, split==TRUE)
test = subset(digits, split==FALSE)

library(rpart)
cart = rpart(label ~ ., data=train, method="class", control = rpart.control(minbucket=10))
cartPredict = predict(cart, newdata=test, type="class")
cartTable = table(test$label, cartPredict)
sum(diag(cartTable))/nrow(test)

library(randomForest)
train$label = factor(train$label)
test$label = factor(test$label)
combi <- rbind(train,test)
randomForest_1 = randomForest(label ~ ., data=combi, nodesize=5,ntree=500,do.trace = T)
randomForest_2 = randomForest(label ~ ., data=combi, nodesize=5,ntree=500,do.trace = T)
randomForest_3 = randomForest(label ~ ., data=combi, nodesize=5,ntree=500,do.trace = T)
rf.all <- combine(randomForest_1,randomForest_2,randomForest_3)
# 
# randomForestPredict = predict(randomForest, newdata=test)
# head(randomForestPredict)
# randomForestTable = table(test$label, randomForestPredict)
# randomForestTable
# sum(diag(randomForestTable))/nrow(test)

realTest = read.csv('C:/Users/DELL/Documents/R/Kaggle/DigitRecognizer/test.csv')
randomForestPredict = predict(rf.all,realTest)
predictions <- data.frame(ImageId = 1:nrow(realTest),Label = randomForestPredict)
write.csv(predictions, file = "rf_benchmark.csv",row.names = F)

#KNN:
knn <- kknn(label ~ .,train = train,test = realTest,k = 11)
knn_predict <- predict(knn,realTest)
predictions_KNN <- data.frame(ImageId = 1:nrow(realTest),Label = knn_predict)
write.csv(predictions, file = "knn_benchmark.csv",row.names = F)

# C5.0:
c50 <- C5.0(label ~ .,data = train,trials = 5)
c50_predict <- predict(c50,realTest)
predictions_c50 <- data.frame(ImageId = 1:nrow(realTest),Label = c50_predict)
write.csv(predictions_c50, file = "c50_benchmark.csv",row.names = F)

# Conditional inference tree:
ctree <- cforest(label ~ .,data = train,controls=cforest_unbiased(ntree=2000, mtry=3))