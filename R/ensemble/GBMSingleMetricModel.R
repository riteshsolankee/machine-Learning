

library(caret)

singleMatric <- read.csv("C:/Temp/singlemetricmodel.csv")

## Remove Metric Name data with value 'avg'
smDF <- singleMatric[singleMatric$MetricName != 'avg', ]


#Split data to have maximum of 1000 categorical variable
smDF$factNum <- as.numeric(smDF$MetricName)

smDF1st <- smDF[smDF$factNum < 1000,]
## Remove redundant columns
# MetricTime
# CnvDate
# factNum

colnames(smDF)
smDF1000 <- smDF1st[, c(
  "CnvDateOnly",
  "CnvHour",
  "CnvMinute",
  "CnvMonth",
  "CnvWeekday",
  "CnvYear",
  "MetricName",
  "MetricValue")]

## Split data into traning and test
smDF1000$MetricName <- as.character(smDF1000$MetricName)
smDF1000$MetricName <- as.factor(smDF1000$MetricName)
index <- createDataPartition(y=smDF1000$MetricValue, p=0.7, list=FALSE)
training <- smDF1000[index,]
testing <- smDF1000[-index,]

## Created GBM Model

library(gbm)
gbmModel = gbm(MetricValue ~., training,
          n.trees=1000,
          shrinkage=0.01,
          distribution="gaussian",
          interaction.depth=7,
          bag.fraction=0.9,
          cv.fold=10,
          n.minobsinnode = 50
          )

best.iter <- gbm.perf(gbmModel,method="OOB")
print(best.iter)


# check performance using a 50% heldout test set
best.iter <- gbm.perf(gbmModel,method="test")
print(best.iter)
# check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbmModel,method="cv")
print(best.iter)

summary(gbmModel,n.trees=1)
summary(gbmModel,n.trees=best.iter)

preds <- predict(gbmModel, newdata = testing, best.iter)


testing$Predictions <- predict(gbmModel, newdata = testing, n.trees = 1000)
testing$BestPredictions <- predict(gbmModel, newdata = testing, best.iter)


plot(gbmModel,1,best.iter)


print(sum((testing$MetricValue-testing$Predictions)^2))

getwd()
write.csv(training, file = "training.csv")
write.csv(testing, file = "MyData.csv")
save(gbmModel, file="gbmModel.Rdata")
load("gbmModel.Rdata")
