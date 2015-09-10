setwd("C:/Course/ORF525Statistical Learning/Project")
adults<-read.csv("income.csv",header=F)
head(adults)
names(adults)
#change column names
colnames(adults)<-c("Age","workclass", "fnlwgt", "education", "education.num","marital.status", "occupation", "relationship", "race", "sex", "capital.gain","capital.loss", "hour.per.wk", "native.country", "income.class")
#choose the first 16280 rows as the train data and the rest as test data
train <- adults[1:16280, ]
test <- adults[16281:length(adults), ]
#Convert into factors
test$income.class <- as.factor(test$income.class)
test$workclass <- as.factor(test$workclass)
test$education <- as.factor(test$education)
test$marital.status <- as.factor(test$marital.status)
test$occupation <- as.factor(test$occupation)
test$relationship <- as.factor(test$relationship)
test$race <- as.factor(test$race)
test$sex <- as.factor(test$sex)
test$native.country <- as.factor(test$native.country)

train$income.class <- as.factor(train$income.class)
train$workclass <- as.factor(train$workclass)
train$education <- as.factor(train$education)
train$marital.status <- as.factor(train$marital.status)
train$occupation <- as.factor(train$occupation)
train$relationship <- as.factor(train$relationship)
train$race <- as.factor(train$race)
train$sex <- as.factor(train$sex)
train$native.country <- as.factor(train$native.country)

#define a function to display the prediction performance
confusion <- function(actual, predicted, names = NULL, printit = TRUE, prior = NULL) 
  {
  if (is.null(names))
  names <- levels(actual)
tab <- table(actual, predicted)
acctab <- t(apply(tab, 1, function(x) x/sum(x)))
dimnames(acctab) <- list(Actual = names, 'Predicted (cv)' = names)
if (is.null(prior)) { relnum <- table(actual)
prior <- relnum/sum(relnum)
acc <- sum(tab[row(tab) == col(tab)])/sum(tab)
} 
else { acc <- sum(prior * diag(acctab))
names(prior) <- names
}
if (printit)
print(round(c('Overall accuracy' = acc, 'Prior frequency' = prior),
            4))
if (printit) { cat("nnConfusion matrix", "nn")
print(round(acctab, 4))
}
invisible(acctab)
}

#manually choose variables that tends to have a large impact on income
#10 models are constructed in total
model1 = glm(income.class ~ Age + education.num + marital.status + occupation +
               race + sex, family = binomial, data = train)
pred.model1 <- round(predict(model1, newdata = test, type = "response"))
confusion(test$income.class, pred.model1)

model2 = glm(income.class ~ Age + workclass + education.num + marital.status +
               occupation + relationship + race + sex + native.country, family = binomial,
             data = train)
pred.model2 <- round(predict(model2, newdata = test, type = "response"))
confusion(test$income.class, pred.model2)

model3 = glm(income.class ~ Age + education.num + marital.status + occupation +
               relationship + race + sex + native.country, family = binomial, data = train)
pred.model3 <- round(predict(model3, newdata = test, type = "response"))
confusion(test$income.class, pred.model3)

model4 = glm(income.class ~ Age + workclass + education.num + marital.status +
               occupation + relationship + race + sex, family = binomial, data = train)
pred.model4 <- round(predict(model4, newdata = test, type = "response"))
confusion(test$income.class, pred.model4)

model5 = glm(income.class ~ Age + education.num + occupation + race + sex, family = binomial,data = train)
pred.model5 <- round(predict(model5, newdata = test, type = "response"))
confusion(test$income.class, pred.model5)

model6 = glm(income.class ~ education.num + marital.status + occupation + race +
               sex, family = binomial, data = train)
pred.model6 <- round(predict(model6, newdata = test, type = "response"))
confusion(test$income.class, pred.model6)

model7 = glm(income.class ~ Age + fnlwgt + education.num + marital.status +
               occupation + race + sex, family = binomial, data = train)
pred.model7 <- round(predict(model7, newdata = test, type = "response"))
confusion(test$income.class, pred.model7)

model8 = glm(income.class ~ Age + fnlwgt + education + education.num + marital.status +
               occupation + race + sex + hour.per.wk, family = binomial, data = train)
pred.model8 <- round(predict(model8, newdata = test, type = "response"))
confusion(test$income.class, pred.model8)

model9 = glm(income.class ~ Age + workclass + education.num + marital.status +
               occupation + relationship + race + sex + hour.per.wk, family = binomial,
             data = train)
pred.model9 <- round(predict(model9, newdata = test, type = "response"))
confusion(test$income.class, pred.model9)

model10 = glm(income.class ~ Age + workclass + fnlwgt + education + education.num +
                marital.status + occupation + relationship + race + sex + capital.gain +
                capital.loss + hour.per.wk + native.country, family = binomial, data = train)
pred.model10 <- round(predict(model10, newdata = test, type = "response"))
confusion(test$income.class, pred.model10)

#bootstrap
prior <- table(train$income.class)
prior <- prior/sum(prior)
index <- train[sample(nrow(train), replace = TRUE), ]
boot.log <- glm(income.class ~ Age + education.num + marital.status + occupation +
                  race + sex, family = binomial, data = index)
pred.model <- round(predict(boot.log, newdata = test[1:dim(index)[1], ], type = "response"))
cmat <- confusion(index$income.class, pred.model)
print(c(acc = round(prior[1] * cmat[1, 1] + prior[2] * cmat[2, 2], 4)))

