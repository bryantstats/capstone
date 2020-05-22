library(tidyverse)
library(ranger)
library(broom)
library(pROC)
library(glmnet)
library(caret)
library(MASS)
sapply(RedCleanData, function(x) sum(is.na(x)))

install.packages("Hmisc")
library(Hmisc)
summary(df)
summary(RedCleanData)
library(mice)
imp <- mice(RedCleanData, method = "cart", m = 1)
ImputedData <- complete(imp)
sapply(ImputedData, function(x) sum(is.na(x)))


'Splitting Testing and Training'
set.seed(99)
rows <- sample(nrow(ImputedData))
rImputedData <- ImputedData[rows, ]
split <- round(nrow(ImputedData) * 0.80)
train <- ImputedData[1:split, ]
test <- ImputedData[(split + 1):nrow(ImputedData), ]

library(caret)
library(ranger)
ImputedData1 <- ImputedData[c(-8, -47)]

'Splitting Testing and Training'
set.seed(99)
rows <- sample(nrow(ImputedData1))
rImputedData1 <- ImputedData1[rows, ]
split <- round(nrow(ImputedData1) * 0.80)
train_numeric <- ImputedData1[1:split, ]
test_numeric <- ImputedData1[(split + 1):nrow(ImputedData), ]

rang <- ranger(ADM_RATE_ALL ~ .,data=train_numeric)
pp <- predict(rang,test_numeric)

mod8 <- train(ADM_RATE_ALL ~ ., data = train_numeric, method = "glmnet",
              trControl = trainControl(method = "cv", number = 10))

mod8
mod8.pred <- predict(mod8, test_numeric)

xdollar <- c(scale_x_continuous(labels = dollar,
                                breaks = seq(0, 130000, 25000),
                                limits = c(0, NA)))

ydollar <- c(scale_y_continuous(labels = dollar,
                                breaks = seq(0, 130000, 25000),
                                limits = c(0, NA)))

titling <- theme(plot.title = element_text(hjust = 0.5,
                                           face = "bold"),
                 axis.title.x = element_text(face = "bold"),
                 axis.title.y = element_text(face = "bold"))
ggplot(data = RedCleanData) +
  geom_boxplot(mapping = aes(x = "", y = GRAD_DEBT_MDN)) +
  labs(title = "Median Debt of \na College's Graduates",
       x = NULL,
       y = "Median Earnings in USD") +
    titling
ggplot(data=RedCleanData) +
scatter
  labs(title = "Median Earnings of Graduates against Admission Rate of Colleges",
       x = "ADM_RATE_ALL",
       y = "GRAD_DEBT_MDN",
       color = "HBCU")
