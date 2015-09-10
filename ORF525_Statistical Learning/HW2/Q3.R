train[,41]=as.factor(train[,41])
nb=naiveBayes(train.y~.,data=train)
nb$apiori
nb$tables
test.x=data.frame(test.x)
preds=predict(nb, test.x,type="class")
preds=as.numeric(preds)-1

error = preds - test.y
errorrate = mean(abs(error))
positive = error[test.y>0]
negative = error[test.y==0]
pos.error = mean(abs(positive))
neg.error = mean(abs(negative))
test.error = mean(c(pos.error, neg.error))
test.error
list(error_rate=errorrate, pos_error=pos.error, neg_error=neg.error, test_error=test.error)


for (i in 4:41){
	train[,i]=as.factor(train[,i])
}
nb=naiveBayes(train.y~.,data=train)
nb$apiori
nb$tables
test.x=data.frame(test.x)
for (i in 4:40)
{ 
	test.x[,i]=as.factor(test.x[,i]) 
}
preds=predict(nb, test.x,type="class")
preprob=predict(nb, test.x,type="raw")
preds=as.numeric(preds)
preds=preds-1
error = preds - test.y
positive = error[test.y>0]
negative = error[test.y==0]
pos.error = mean(abs(positive))
neg.error = mean(abs(negative))
pos.error
neg.error
test.error = mean(c(pos.error, neg.error))
test.error
