################ XGBoost v0.0 #################
#First model doing nothing to check data capacity only
#data reading
math.tra = read.csv2("data/pv1math-tra.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst.csv", sep=";", dec=",", quote="\"", encoding='utf-8')

#save training labels
labels = math.tra$PV1MATH
train = as.matrix( math.tra[,-dim(math.tra)[2]] )
test = as.matrix( math.tst )

numberOfClasses <- max(labels)+1
param <- list("objective" = "multi:softmax",
              "eval_metric" = "merror",
              "num_class" = numberOfClasses)
cv.nround <- 25
cv.nfold <- 5

require(xgboost)
#training with Cross Validation
xgbst.cv =xgboost::xgb.cv(param=param, data=train, label=labels, nfold=cv.nfold, nrounds=cv.nround)

#train + test
xgbModel =  xgboost::xgboost(param=param, data=train, label=labels, nrounds=cv.nround)
prediction = predict(xgbModel, test)
#_____________________________________________#


######## Submission code ##########
submission = data.frame(ID=c(1:length(prediction)), Prediction=prediction, row.names=NULL)
write.table(submission, "./submission/m2-xgbCapacity.csv", row.names=FALSE, quote=FALSE, sep=",")
