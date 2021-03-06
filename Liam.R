library(tidyverse)
library(caret)
library(rpart)
library(rattle)
library(ggplot2)
library(haven)
library(gganimate)
library(Hmisc)
infile<-read_csv("ModelFileV4.csv")
df<-data.frame(infile)%>%
  select(-c(Cohort, First, Last, TargetGPA, ID, GPA, major, sat, College, race))%>%
  drop_na
df$target <- factor(df$Target10p)
df<-df%>%
  selec
t(-c(Target10p))%>%
  # mutate(race=recode(race, '6 Pacific Isl'='Other', '5 NativeAm'='Other','9 Unkn US'='Other'))%>%
  filter(degree!="a NDG, UG")%>%
  filter(midtermgpa!="#N/A")%>%
  mutate(midtermgpa=as.numeric(midtermgpa))%>%
  mutate(MathClass=recode(MathClass,'E201'='201','226'='Other','350'='Other','122'='Other', '223'='Other'))%>%
  mutate(MathProf=recode(MathProf, "Salzillo"="Salzillo & Olinsky & Lamere & Toce", "Olinsky"="Salzillo & Olinsky & Lamere & Toce", "Lamere"="Salzillo & Olinsky & Lamere & Toce", "Toce"="Salzillo & Olinsky & Lamere & Toce"))
#select(c(midtermgpa,target))

#df %>%
#  group_by(MathProf) %>%
#  summarise(num=length(MathProf))

rcorr(as.matrix(df))

# Split the data into 70% training and 30% testing
#set.seed(12341220)
splitIndex <- createDataPartition(df$target, p = .70, list = FALSE)
train <- df[ splitIndex,]
test <- df[-splitIndex,]

train0 = train[train$target == '0',]
train1 = train[train$target == '1',]
n0 = nrow(train0)
n1 = nrow(train1)
train00 = train0[sample(1:n0, n1),]
train_under = rbind(train00, train1)

modelrpart2 <- train(target~.,data =train_under, method = "rpart2")
predrpart2=predict(modelrpart2,test)
cmRpart2=confusionMatrix(predrpart2, test$target, positive="1")
cmRpart2

modelranger <- train(target~.,data =train_under, method = "ranger")
predranger=predict(modelranger,test)
cmRanger=confusionMatrix(predranger, test$target, positive="1")
cmRanger

modellogitboost <- train(target~.,data =train_under, method = "LogitBoost")
predlogitboost=predict(modellogitboost,test)
cmlogreg=confusionMatrix(predlogitboost, test$target, positive="1")
cmlogreg

modellmt <- train(target~.,data =train_under, method = "LMT")
predlmt=predict(modellmt,test)
cmLMT=confusionMatrix(predlmt, test$target, positive="1")
cmLMT

modelnnet <- train(target~.,data =train_under, method = "nnet")
prednnet=predict(modelnnet,test)
cmNnet=confusionMatrix(prednnet, test$target, positive="1")
cmNnet

modelknn <- train(target~.,data =train_under, method = "knn")
predknn=predict(modelknn,test)
cmKnn=confusionMatrix(predknn, test$target, positive="1")
cmKnn

modeladaboost <- train(target~.,data =train_under, method = "adaboost")
predadaboost=predict(modeladaboost,test)
cmadaboost=confusionMatrix(predadaboost, test$target, positive="1")
cmadaboost
modellvq <- train(target~.,data =train_under, method = "lvq")
predlvq=predict(modellvq,test)
cmlvq=confusionMatrix(predlvq, test$target, positive="1")
cmlvq
modellda <- train(target~.,data =train_under, method = "lda")
predlda=predict(modellda,test)
cmlda=confusionMatrix(predlda, test$target, positive="1")

modelwsrf <- train(target~.,data =train_under, method = "wsrf")
predwsrf=predict(modelwsrf,test)
cmrf2=confusionMatrix(predwsrf, test$target, positive="1")
cmrf2

Outputs<-data.frame(c("LMT","LogitBoost","Nnet","Random Forest","Rpart2","Knn","adaboost", "lvq", "lda"),c(cmLMT$overall["Accuracy"], cmlogreg$overall["Accuracy"], cmNnet$overall["Accuracy"], cmRanger$overall["Accuracy"], cmRpart2$overall["Accuracy"], cmKnn$overall["Accuracy"], cmadaboost$overall["Accuracy"], cmlvq$overall["Accuracy"], cmlda$overall["Accuracy"]), c(cmLMT$byClass["Balanced Accuracy"], cmlogreg$byClass["Balanced Accuracy"], cmNnet$byClass["Balanced Accuracy"], cmRanger$byClass["Balanced Accuracy"], cmRpart2$byClass["Balanced Accuracy"], cmKnn$byClass["Balanced Accuracy"], cmadaboost$byClass["Balanced Accuracy"], cmlvq$byClass["Balanced Accuracy"], cmlda$byClass["Balanced Accuracy"]))
names(Outputs)<-c("model", "accuracy", "balanced accuracy")
Outputs

outs=data.frame(test$target)
run_model <- function() {
  train0 = train[train$target == '0',]
  train1 = train[train$target == '1',]
  n0 = nrow(train0)
  n1 = nrow(train1)
  train00 = train0[sample(1:n0, n1),]
  train_under = rbind(train00, train1)
  model <- train(target~.,data =train_under, method = "LMT")
  pred=predict(model,test)
  return(pred)
}

for (iter in 1:100){
  r <- NULL
  while( is.null(r) ) {
    try(
      r <- run_model()
      
    )
  }
  outs<-cbind(outs,r)
  print(iter)
}
namefix<-c('target','iter1','iter2','iter3','iter4','iter5','iter6','iter7','iter8','iter9','iter10','iter11','iter12','iter13','iter14','iter15','iter16','iter17','iter18','iter19','iter20','iter21','iter22','iter23','iter24','iter25','iter26','iter27','iter28','iter29','iter30','iter31','iter32','iter33','iter34','iter35','iter36','iter37','iter38','iter39','iter40','iter41','iter42','iter43','iter44','iter45','iter46','iter47','iter48','iter49','iter50','iter51','iter52','iter53','iter54','iter55','iter56','iter57','iter58','iter59','iter60','iter61','iter62','iter63','iter64','iter65','iter66','iter67','iter68','iter69','iter70','iter71','iter72','iter73','iter74','iter75','iter76','iter77','iter78','iter79','iter80','iter81','iter82','iter83','iter84','iter85','iter86','iter87','iter88','iter89','iter90','iter91','iter92','iter93','iter94','iter95','iter96','iter97','iter98','iter99','iter100')
names(outs)<-namefix
outs<-outs%>%
  select(-c(target))
outs<-lapply(outs,as.numeric)
outs<-data.frame(outs)
outs<-rowSums(outs)
results<-outs/100
results<-results-1
resultsX<-data.frame(results)
resultsX<-resultsX%>%
  mutate(results=cut(results, breaks=c(-Inf, 0.5, Inf), labels = c('0','1')))
cm=confusionMatrix(resultsX$results, test$target, positive = "1")
