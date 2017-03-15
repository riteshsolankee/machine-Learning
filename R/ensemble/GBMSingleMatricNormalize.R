
library(caret)

singleMatric <- read.csv("C:/Temp/singlemetricmodel.csv")

## Remove Metric Name data with value 'avg'
smDF <- singleMatric[singleMatric$MetricName != 'avg', ]


#Split data to have maximum of 1000 categorical variable
smDF$factNum <- as.numeric(smDF$MetricName)

smDF1st <- smDF[smDF$factNum < 1000,]
smDF2st <- smDF[smDF$factNum >= 1000 & smDF$factNum < 2000,]
## Remove redundant columns
# MetricTime
# CnvDate
# factNum

colnames(smDF)

## Next 1000
smDF2000 <- smDF2st[, c(
  "CnvDateOnly",
  "CnvHour",
  "CnvMinute",
  "CnvMonth",
  "CnvWeekday",
  "CnvYear",
  "MetricName",
  "MetricValue")]

## Start - Normalize metricValue
library(data.table)

df <- data.table(smDF2000)
df[, Mean:=mean(MetricValue), by=list(MetricName)]
df[, SD:=sd(MetricValue), by=list(MetricName)]
df[, NormValue:=((MetricValue - Mean)/SD)]
nrow(df)
View(df)

df <- as.data.frame.matrix(df)

singleValueDF <- df[is.na(df$SD), ]
noVariationDF <- df[df$SD == 0, ]

df <- df[!is.na(df$SD), ]
df <- df[df$SD != 0, ] 
nrow(df)
View(df)
## End - normalization



## Split data into traning and test
df$MetricName <- as.character(df$MetricName)
df$MetricName <- as.factor(df$MetricName)
index <- createDataPartition(y=df$MetricValue, p=0.7, list=FALSE)
training2 <- df[index,]
testing2 <- df[-index,]

str(training2)
colnames(training2)
#View(testing2)
## Created GBM Model
trainData <- training2[, c("CnvDateOnly", "CnvHour","CnvMinute", "CnvWeekday", "MetricName", "NormValue")]
library(gbm)
training2$MetricName <- as.character(training2$MetricName)
training2$MetricName <- as.factor(training2$MetricName)
summary(training2)

gbmModel2Norm = gbm(NormValue ~ ., 
                    trainData,
               n.trees=500,
               shrinkage=0.01,
               distribution="gaussian",
               interaction.depth=7,
               bag.fraction=0.9,
               cv.fold=10,
               n.minobsinnode = 50
)

best.iter2 <- gbm.perf(gbmModel2Norm,method="OOB")
print(best.iter2)

str(testing2)
str(trainData)

testing2$preds <- predict(gbmModel2Norm, newdata = testing2, best.iter2)

dfTest <- data.table(testing2)

dfTest[, DeNormValue:=((preds*SD + Mean))]

dfTest <- as.data.frame.matrix(dfTest)

View(dfTest)

summary(gbmModel2Norm,n.trees=1)
summary(gbmModel2Norm,n.trees=best.iter2)



plot(gbmModel2Norm,1,best.iter2)


colnames(dfTest)

finalResult <- dfTest[, c("CnvDateOnly", "CnvHour", "CnvMinute", "CnvMonth", "CnvWeekday", "CnvYear", "MetricName", "MetricValue", "DeNormValue")]

View(finalResult)
write.csv(finalResult, file = "testResult.csv")
write.csv(trainData, file = "training.csv")
save(gbmModel2Norm, file="gbmModel2Norm.Rdata")

getwd()

#print(sum((testing2$MetricValue-testing2$Predictions)^2))

