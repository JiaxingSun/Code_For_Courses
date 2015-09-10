setwd("C:/Course/ORF525Statistical Learning/Project")
adults<-read.csv("income.csv",header=F)
colnames(adults)<-c("Age","workclass", "fnlwgt", "education", "education.num","marital.status", "occupation", "relationship", "race", "sex", "capital.gain","capital.loss", "hour.per.wk", "native.country", "income.class")
train <- adults[1:16280, ]
test <- adults[16281:length(adults), ]
#convert into factors
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

#construct the model with training data
library(MASS)
adult.lda <- lda(income.class ~., data = train)
null <- lda(income.class ~ Age, data = train)
adults.lda <- lda(income.class ~ ., data = train)
names(train)
adults.hat <- predict(adults.lda)
tabtrain <- table(train$income.class, adults.hat$class)
conTrain <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]))
dimnames(conTrain) <- list(Actual = c("No", "Yes"), Predicted (cv) = c("No",
                                                                         "Yes"))
print(round(conTrain, 3))
step(null, scope = list(lower = null, upper = adult.lda), direction = "forward")
step(adult.lda, data = train, direction = "backward")

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

# Check model performance with test data
test.hat <- predict(adult.lda, test)
confusion(test$income.class, test.hat$class)
## And with CV
adultsCV.lda <- lda(income.class ~ ., data = train, CV = TRUE)
head(tr)
tab <- table(train$income.class, adultsCV.lda$class)
conCV1 <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]))
dimnames(conCV1) <- list(Actual = c("No", "Yes"), "Predicted (cv)" = c("No","Yes"))
print(round(conCV1, 3))

library(lattice)
densityplot(~adults.hat$x, groups = train$type)
adultsCV.lda <- lda(income.class ~ ., data = train, CV = TRUE)

confusion(train$income.class, adultsCV.lda$class)
sort(train$income.class)[12383]
## 12383 -> <=50K
length(train$income.class)
class(train$income.class)
16280 - 12383
head(train)
3897/16280
12383/16280
a <- sort(adults$income.class)
a[24720]
length(a)
less.than <- 24720/length(a)
more.than <- (length(a) - 24720)/length(a)
1 - less.than
prior <- c(0.7591904, 0.2408096)
adultsCVp.lda <- lda(income.class ~ ., data = train, CV = TRUE, prior = prior)
confusion(train$income.class, adultsCVp.lda$class, prior = c(0.7606265, 0.2393735))
dim(test)
Pima.tr
prior <- table(train$income.class)
prior <- prior/sum(prior)
index <- sample(1:dim(train)[1], replace = TRUE)
boot.lda <- lda(income.class ~ ., data = train[index, ], CV = TRUE)
cmat <- confusion(train[index, "income.class"], boot.lda$class, printit = FALSE)
print(c(acc = round(prior[1] * cmat[1, 1] + prior[2] * cmat[2, 2], 4)))
class(boot.lda$class)
AIC(adultsCVp.lda)
length(train[index, "income.class"])
length(boot.lda$class)
# Can compare with logistic model. Model diagnostics and CV. Remove
# variables that are highly collinear. The lasso can help us to do this!
# The elastic net will help us to both remove collinear variables and
# maximize coefficient shrinkage.