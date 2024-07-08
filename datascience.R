install.packages("tidyverse")
install.packages("magrittr")
install.packages("dplyr")
install.packages("repr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("caret")
install.packages('e1071', dependencies=TRUE)
install.packages("pROC")



library(tidyverse)
library(magrittr)
library(dplyr)
library(repr)
library(ggplot2)
library(corrplot)
library(caret)
library(e1071)
library(pROC)

#Data Exploration
data = read.csv("D:\\Semester 3\\Data Science\\Course Project\\DataSets\\dataset_1.csv")
head(data)
tail(data)
ncol(data)
nrow(data)
colnames(data)
summary(data)


#Data Transformation

data2 = data %>%
  mutate(sex = ifelse(sex == 1, "MALE", "FEMALE"),
         fbs = ifelse(fbs == 1, ">120", "<=120"),
         exang = ifelse(exang == 1, "YES" ,"NO"),
         cp = ifelse(cp == 1, "ATYPICAL ANGINA",
                     ifelse(cp == 2, "NON-ANGINAL PAIN", "ASYMPTOMATIC")),
         restecg = ifelse(restecg == 0, "NORMAL",
                          ifelse(restecg == 1, "ABNORMALITY", "PROBABLE OR DEFINITE")),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         target = ifelse(target == 1, "YES", "NO")
  ) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())



#Data Visualization
options(repr.plot.width =6, repr.plot.height =3)

ggplot(data2, aes(x=target, fill=target))+
  geom_bar()+
  xlab("Heart Disease")+
  ylab("Count")+
  ggtitle("Presence & Absence of Heart Disease")+
  scale_fill_discrete(name= 'Heart Disease', labels =c("Absence", "Presence"))

prop.table(table(data2$target))


data2 %>%
  group_by(age) %>%
  count() %>%
  filter(n>10) %>%
  ggplot()+
  geom_col(aes(age, n), fill = 'blue')+
  ggtitle("Age Analysis")+
  xlab("Age")+
  ylab("Agecount")


data2 %>%
  ggplot(aes(x=sex, y=trestbps))+
  geom_boxplot(fill ='yellow')+
  xlab('sex')+
  ylab('BP')+
  facet_grid(~cp)


#Getting Correlations

cor_heart = cor(data[, 1:14])

cor_heart

corrplot(cor_heart, method ='square', type='upper')



#Data Cleaning

s = sum(is.na(data2))
s


#Training and Testing Data

set.seed(10)

intrainRows = createDataPartition(data2$target, p=0.7,list=FALSE)
trainData = data2[intrainRows,]
testData = data2[-intrainRows,]


nrow(trainData)/(nrow(trainData)+nrow(testData)) #check percentage



#AUC = list()
Accuracy = list()

set.seed(10)

 logRegModel = train(target ~ ., data=trainData, method = "glm" , family = "binomial")
logRegPrediction = predict(logRegModel, testData)
logRegPredictionProb <- predict(logRegModel, testData, type='prob')[2]
logRegConfMat <- confusionMatrix(logRegPrediction, testData[,"target"])

#AUC$logReg <- roc(as.numeric(testData$target),as.numeric(as.matrix((logRegPredictionProb))))$auc
Accuracy$logReg <- logRegConfMat$overall['Accuracy']

Accuracy
logRegConfMat

