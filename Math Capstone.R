'Importation of Data'
CollegeData = read.csv("C://Users/student/Documents/CleanData.csv")
sapply(CleanData, function(x) sum(is.na(x)))

install.packages("Hmisc")
library(Hmisc)
df <- data.frame(t(RedCleanData),row.names=NULL)
summary(df)

rcorr(df, type="pearson")

class(CleanData)


install.packages("mice")
library(mice)
imp <- mice(CleanData, seed = 51009, print = FALSE)
mi1 <- with(data = CleanData, expr = lm(ADM_RATE_ALL ~ ACTCMMID + SAT_AVG_ALL))
mi0 <- with(data = CleanData, expr = lm(ADM_RATE_ALL ~ age + hyp))
D2(mi1, mi0)
md.pattern(CleanData)
install.packages("VIM")
library(VIM)

ImputedData <- mice(RedCleanData,m=2000,maxit=,meth='pmm',seed=500)
summary(tempData)

'Attempt at dbscan'
library(dbscan)



'Splitting Testing and Training'
set.seed(99)
rows <- sample(nrow(CollegeData1))
rCollegeData1 <- CollegeData1[rows, ]
split <- round(nrow(CollegeData1) * 0.80)
train <- CollegeData1[1:split, ]
test <- CollegeData1[(split + 1):nrow(CollegeData1), ]

install.packages("ranger")
install.packages("caret")
library(caret)
x <- c("HCM2", "PREDDEG")
model_lm<-lm(ADM_RATE ~ ., train)
model_rf<-train(train["HCM2", "PREDDEG"],train["ADM_RATE"],method='rf')

RedCleanData <- select_if(is.numeric()) %>% rcorr(RedCleanData)

library(ranger)
rang <- ranger(ADM_RATE ~ HCS2+PREDDEG,data=train)

pp <- predict(rang,test)

cm=confusionMatrix(data = pp$predictions, reference = test.s[,1], positive = "x1")


