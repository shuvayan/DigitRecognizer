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
randomForest = randomForest(label ~ ., data=train, nodesize=20,ntree=200, do.trace=TRUE)
# 
# randomForestPredict = predict(randomForest, newdata=test)
# head(randomForestPredict)
# randomForestTable = table(test$label, randomForestPredict)
# randomForestTable
# sum(diag(randomForestTable))/nrow(test)

realTest = read.csv('C:/Users/DELL/Documents/R/Kaggle/DigitRecognizer/test.csv')
randomForestPredict = predict(randomForest,realTest)
predictions <- data.frame(ImageId = 1:nrow(realTest),Label = randomForestPredict)
write.csv(predictions, file = "rf_benchmark.csv",row.names = F)

#KNN:
knn <- kknn(label ~ .,train = train,test = realTest,k = 11)
knn_predict <- predict(knn,realTest)
predictions_KNN <- data.frame(ImageId = 1:nrow(realTest),Label = knn_predict)
write.csv(predictions, file = "knn_benchmark.csv",row.names = F)

