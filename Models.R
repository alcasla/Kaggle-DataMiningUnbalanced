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
#param <- list("objective" = "multi:softmax",
#              "eval_metric" = "merror",
#              "num_class" = numberOfClasses)
param <- list("objective" = "multi:softprob",
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


################ GBM v0.0 #################
#Model without preprocessing
#data reading
math.tra = read.csv2("data/pv1math-tra.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst.csv", sep=";", dec=",", quote="\"", encoding='utf-8')

#structures, labels, train and test matrixs
labels = math.tra$PV1MATH
train = as.matrix( math.tra[,-dim(math.tra)[2]] )
test = as.matrix( math.tst )

require(caret)
#Control structure to training
fitControl = caret::trainControl(method="repeatedcv", number=4, repeats=4)

set.seed(123456)
gbm = caret::train(train, labels, method="gbm", trControl=fitControl, verbose=F)
prediction = predict(gbm, test)
#_____________________________________________ 0.81784

################ GBM v1.0 #################
#Model without preprocessing
#data reading
math.tra = read.csv2("data/pv1math-tra-outliersV1.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst-outliersV1.csv", sep=";", dec=",", quote="\"", encoding='utf-8')

#structures, labels, train and test matrixs
labels = math.tra$PV1MATH
train = as.matrix( math.tra[,-dim(math.tra)[2]] )
test = as.matrix( math.tst )

require(caret)
#Control structure to training
fitControl = caret::trainControl(method="repeatedcv", number=4, repeats=4)

set.seed(123456)
gbm = caret::train(train, labels, method="gbm", trControl=fitControl, verbose=F)
prediction = predict(gbm, test)
#_____________________________________________ 0.81452

################ SVM v1.0 #################
#Model without preprocessing
#data reading
math.tra = read.csv2("data/pv1math-tra.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst.csv", sep=";", dec=",", quote="\"", encoding='utf-8')

math.tra[,c(1,2,3,4,5,6,8)] = sapply(c(1,2,3,4,5,6,8), as.numeric)
math.tst[,c(1,2,3,4,5,6,8)] = sapply(c(1,2,3,4,5,6,8), as.numeric)

require(e1071)
svmModel = e1071::svm(PV1MATH ~ ., data=math.tra, type="eps")
prediction = predict(svmModel, math.tst, type="probs")
#_____________________________________________ 0.82279

################ SVM v2.0 #################
#Model removing outliers V2
#data reading
math.tra = read.csv2("data/pv1math-tra-outliersV2.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst-outliersV2.csv", sep=";", dec=",", quote="\"", encoding='utf-8')

math.tra[,c(1,2,3,4,5,6,8)] = sapply(c(1,2,3,4,5,6,8), as.numeric)
math.tst[,c(1,2,3,4,5,6,8)] = sapply(c(1,2,3,4,5,6,8), as.numeric)

require(e1071)
svmModel = e1071::svm(PV1MATH ~ ., data=math.tra, type="eps")
prediction = predict(svmModel, math.tst, type="probs")
#_____________________________________________ 0.80790

################ SVM v1.1 #################
#Model without preprocessing
#data reading
math.tra = read.csv2("data/pv1math-tra.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst.csv", sep=";", dec=",", quote="\"", encoding='utf-8')

require(e1071)
svmModel = e1071::svm(as.factor(PV1MATH) ~ ., data=math.tra, probability=T)
prediction = predict(svmModel, math.tst, probability=T)
prediction = attr(prediction, "probabilities")[,2]
#_____________________________________________ 0.82293

################ SVM v2.1 #################
#Model with data transformation Centered
#data reading
math.tra = read.csv2("data/pv1math-tra-centered.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst-centered.csv", sep=";", dec=",", quote="\"", encoding='utf-8')

require(e1071)
svmModel = e1071::svm(as.factor(PV1MATH) ~ ., data=math.tra, probability=T)
prediction = predict(svmModel, math.tst, probability=T)
prediction = attr(prediction, "probabilities")[,2]
#_____________________________________________ 0.82293

################ SVM v2.2 #################
#Model with data transformation Centered and Scaled
#data reading
math.tra = read.csv2("data/pv1math-tra-centerScaled.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst-centerScaled.csv", sep=";", dec=",", quote="\"", encoding='utf-8')

require(e1071)
svmModel = e1071::svm(as.factor(PV1MATH) ~ ., data=math.tra, probability=T)
prediction = predict(svmModel, math.tst, probability=T)
prediction = attr(prediction, "probabilities")[,2]
#_____________________________________________ 0.82293


######## Submission code ##########
submission = data.frame(ID=c(1:length(prediction)), Prediction=prediction, row.names=NULL)
write.table(submission, "./submission/m5-svm2.2.csv", row.names=FALSE, quote=FALSE, sep=",")
