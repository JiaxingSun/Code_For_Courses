setwd("C:/Course/ORF525Statistical Learning/Project")
adults<-read.csv("income.csv",header=F)
sapply(Pima.tr, class)
colnames(adults) <- c("Age", "workclass", "fnlwgt", "education", "education.num",
                      "marital.status", "occupation", "relationship", "race", "sex", "capital.gain",
                      "capital.loss", "hour.per.wk", "native.country", "income.class")
sapply(adults, shapiro.test)
head(adults)
sort(train$native.country)[900:1000]
attach(adults)
head(sex)
head(adults)
dim(adults)
length(sex)
workclass[35:45]
Age[35:45]
income.class[1:30]
# Build matrices for glmnet
require(useful)
fnlwgt[1:30]
income.class <- as.factor(income.class)
adults$income <- with(adults, income.class == "<=50K")
adults$income[1:30]
head(adults)
dim(adults)
adults <- adults[, -16]
adultsx <- build.x(income.class ~ Age + workclass + fnlwgt + education + education.num +
                     marital.status + occupation + relationship + race + sex + capital.gain +
                     capital.loss + hour.per.wk + native.country - 1, data = adults, contrasts = FALSE)
topleft(adultsx, c = 10)
topright(adultsx)
bottomleft(adultsx)
dim(adultsx)
adultsy <- build.y(income.class ~ Age + workclass + fnlwgt + education + education.num +
                     marital.status + occupation + relationship + race + sex + capital.gain +
                     capital.loss + hour.per.wk + native.country - 1, data = adults)
head(adultsy)
tail(adultsy)
length(adultsy)
require(glmnet)
################################ lasso model###########################
set.seed(1863561)
adultsCV1 <- cv.glmnet(x = adultsx, y = adultsy, family = "binomial", nfold = 5)
adultsCV1$lambda.min
adultsCV1$lambda.1se
plot(adultsCV1)
length(coef(adultsCV1, s = "lambda.1se"))
plot(adultsCV1$glmnet.fit, xvar = "lambda")
abline(v = log(c(adultsCV1$lambda.min, adultsCV1$lambda.1se)), lty = 2, lwd = 2)
predict(adultsCV3, newx = adultsx[1:10, ], s = "lambda.1se", type = "class")
################################ ridge model###########################
set.seed(1863561)
adultsCV2 <- cv.glmnet(x = adultsx, y = adultsy, family = "binomial", nfold = 5,
                       alpha = 0)
adultsCV2$lambda.min
log(adultsCV2$lambda.1se)
adultsCV2$lambda.1se
coef(adultsCV2, s = "lambda.1se")
plot(adultsCV2)
plot(adultsCV2$glmnet.fit, xvar = "lambda")
abline(v = log(c(adultsCV2$lambda.min, adultsCV2$lambda.1se)), lty = 2, lwd = 2)
# find optimum alpha and lambda
require(parallel)
require(doParallel)
# only consider alpha > 0.5
set.seed(1830834)
folder <- sample(rep(x = 1:5, length.out = nrow(adultsx)))
alphy <- seq(from = 0.5, to = 1, by = 0.05)
set.seed(9034874)
c_new <- makeCluster(2)
registerDoParallel(c_new)
before <- Sys.time()
a.Double <- foreach(i = 1:length(alphy), .errorhandling = "remove", .inorder = FALSE,
                    .multicombine = TRUE, .export = c("adultsx", "adultsy", "alphy", "folder"),
                    .packages = "glmnet") %dopar% { print(alphy[i])
cv.glmnet(x = adultsx, y = adultsy, family = "binomial", nfolds = 5, foldid = folder,
          alpha = alphy[i])
}
after <- Sys.time()
stopCluster(c_new)
after - before
a.Double
sapply(a.Double, class)

extractGlmnetInfo <- function(object) { lambdaMin <- object$lambda.min
lambda1se <- object$lambda.1se
whichMin <- which(object$lambda == lambdaMin)
which1se <- which(object$lambda == lambda1se)
data.frame(lambda.min = lambdaMin, error.min = object$cvm[whichMin], lambda.1se = lambda1se,
           error.1se = object$cvm[which1se])
}
alphaInfo <- Reduce(rbind, lapply(a.Double, extractGlmnetInfo))
alphaInfo$Alpha <- alphy
alphaInfo
require(reshape2)
require(stringr)
require(ggplot2)
alphaMelt <- melt(alphaInfo, id.vars = "Alpha", value.name = "Value", variable.name = "Measure")
alphaMelt$Type <- str_extract(string = alphaMelt$Measure, pattern = "(min)|(1se)")
alphaMelt$Measure <- str_replace(string = alphaMelt$Measure, pattern = "nn.(min|1se)",
                                 replacement = "")
alphaCast <- dcast(alphaMelt, Alpha + Type ~ Measure, value.var = "Value")
plota <- ggplot(alphaCast, aes(x = Alpha, y = error)) + geom_line(aes(group = Type),
  col = "midnightblue") + facet_wrap(~Type, scales = "free_y", ncol = 1) +
  geom_point(aes(size = lambda), col = "midnightblue")
plota + theme(axis.text.x = element_text(face = "bold", size = 14), axis.text.y = element_text(face = "bold",
size = 14)) + theme(axis.title.x = element_text(face = "bold", size = 20), axis.title.y = element_text(face = "bold", size = 20))
## Therefore optimal alpha is .9
## Get coefficients for optimum model
set.seed(9405782)
adultsCV3 <- cv.glmnet(x = adultsx, y = adultsy, family = "binomial", nfold = 5,
                       alpha = alphaInfo$Alpha[which.min(alphaInfo$error.1se)])
plot(adultsCV3)
plot(adultsCV3$glmnet.fit, xvar = "lambda")
abline(v = log(c(adultsCV3$lambda.min, adultsCV3$lambda.1se)), lty = 2, lwd = 2)
theCoef <- as.matrix(coef(adultsCV3, s = "lambda.1se"))
coefDF <- data.frame(Value = theCoef, Coefficient = rownames(theCoef))
require(ggthemes)
coefDF <- coefDF[nonzeroCoef(coef(adultsCV3, s = "lambda.1se")), ]
plotb <- ggplot(coefDF, aes(x = X1, y = reorder(Coefficient, X1))) + geom_vline(xintercept = 0,color = "grey12", linetype = 2, lwd = 1) + geom_point(color = "blue") +labs(x = "Value", y = "Coefficient", title = "Coefficient Plot") + theme_economist()
plotb + theme(axis.title.y = element_text(vjust = 0.25)) + theme(axis.title.x = element_text(face = "bold",size = 20), axis.title.y = element_text(face = "bold", size = 20))
adultshat <- predict(adultsCV3, newx = adultsx[1:20, ], s = "lambda.min", type = "class")
dim(adultsx)
# Bootstrap
class(train)
index <- sample(train, replace = TRUE)
names(index)
trnx <- build.x(income.class ~ Age + workclass + fnlwgt + education + education.num +
                  marital.status + occupation + relationship + race + sex + capital.gain +
                  capital.loss + hour.per.wk + native.country - 1, data = index, contrasts = FALSE)
trny <- build.y(income.class ~ Age + workclass + fnlwgt + education + education.num +
                  marital.status + occupation + relationship + race + sex + capital.gain +
                  capital.loss + hour.per.wk + native.country - 1, data = index)
tstx <- build.x(income.class ~ Age + workclass + fnlwgt + education + education.num +
                  marital.status + occupation + relationship + race + sex + capital.gain +
                  capital.loss + hour.per.wk + native.country - 1, data = test, contrasts = FALSE)

adultsCV3 <- cv.glmnet(x = trnx, y = trny, family = "binomial", nfold = 5, alpha = alphaInfo$Alpha[which.min(alphaInfo$adultshat <- predict(adultsCV3, newx = tstx, s = "lambda.1se", type = "class")
tab <- table(test$income.class, adultshat[, 1])
conCV1 <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]))
dimnames(conCV1) <- list(Actual = c("No", "Yes"), _Predicted (cv)_ = c("No", "Yes"))
print(round(conCV1, 3))
confusion(test$income.class, adultshat[, 1])
                                                                                                             