require(FSelector)

#data reading
math.tra = read.csv2("data/pv1math-tra-centerScaled.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst-centerScaled.csv", sep=";", dec=",", quote="\"", encoding='utf-8')

##### check discrete attributes against class by chi-squared rank correlation
weightsChi = FSelector::chi.squared(PV1MATH~.,math.tra[,c(1,2,3,4,5,6,8,23)])
#            attr_importance
#ST04Q01        0.0000000
#ST15Q01        0.1831259
#ST19Q01        0.1345920
#misced         0.1949517
#fisced         0.1708762
#CLCUSE1        0.1462875
#FAMSTRUC       0.0000000

#Generate both dataset without first feature ST04Q01
math.tra = math.tra[,-1]
math.tst = math.tst[,-1]
#save data
write.csv2(math.tra, "./data/pv1math-tra-ftS.ST04Q01.csv", row.names=F)
write.csv2(math.tst, "./data/pv1math-tst-ftS.ST04Q01.csv", row.names=F)

#Generate both dataset without last feature FAMSTRUC
math.tra = math.tra[,-8]
math.tst = math.tst[,-8]
#save data
write.csv2(math.tra, "./data/pv1math-tra-ftS.FAMSTRUC.csv", row.names=F)
write.csv2(math.tst, "./data/pv1math-tst-ftS.FAMSTRUC.csv", row.names=F)
# __________________________________________________________________________


require(FSelector)

#data reading
math.tra = read.csv2("data/pv1math-tra-centerScaled.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst-centerScaled.csv", sep=";", dec=",", quote="\"", encoding='utf-8')

###### Check correlation between cuantitaive variables by Pearson´s correlation
weigthsCor = FSelector::linear.correlation(PV1MATH~., math.tra)
#          attr_importance
# ...
# BELONG       0.028735568
# STUDREL      0.063544976
# ATTLNACT     0.000517021
# ATSCHL       0.026137152
# ...

#Generate both dataset without last feature ATTLNACT
math.tra = math.tra[,-11]
math.tst = math.tst[,-11]
#save data
write.csv2(math.tra, "./data/pv1math-tra-ftS.ATTLNACT.csv", row.names=F)
write.csv2(math.tst, "./data/pv1math-tst-ftS.ATTLNACT.csv", row.names=F)
# __________________________________________________________________________


require(FSelector)

#data reading
math.tra = read.csv2("data/pv1math-tra-centerScaled.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst-centerScaled.csv", sep=";", dec=",", quote="\"", encoding='utf-8')

###### Aproximation filter: relief - check distante between intances
#Normalize dataset in range [0,1]
math.tra = apply(math.tra, 2, function(x) x + (-1 * min(x)) )
math.tra = apply(math.tra, 2, function(x) x / max(x) )
math.tra = as.data.frame(math.tra)

weightsRelief = FSelector::relief(PV1MATH~., math.tra, neighbours.count=6, sample.size=20)
print(weightsRelief)

#This weights could be more representatives with SVM model due to it is based on distances like svm
#           attr_importance
# ST04Q01     0.0000000000  *
# ST15Q01    -0.0289517148
# ST19Q01     0.0247741183
# misced     -0.0156416982
# fisced     -0.0224910133
# CLCUSE1     0.0207665404
# ESCS       -0.0011171943              *
# FAMSTRUC   -0.0068493151
# BELONG      0.0184784741
# STUDREL    -0.0094036016
# ATTLNACT    0.0348404018
# ATSCHL      0.0028114449  *
# ANXMAT      0.0187589008
# FAILMAT     0.0135356004
# INSTMOT    -0.0257399081
# INTMAT     -0.0277746547
# SMATBEH     0.0169353253
# MATHEFF     0.0080243548
# MATINTFC    0.0006705710          *
# MATWKETH    0.0002355927      *
# SCMAT      -0.0073350821
# SUBNORM     0.0030072193

#Generate both dataset without features ATSCHL and ST04Q01
math.tra = math.tra[,-c(1,12)]
math.tst = math.tst[,-c(1,12)]
#save data
write.csv2(math.tra, "./data/pv1math-tra-ftS.ST04Q01.ATSCHL.csv", row.names=F)
write.csv2(math.tst, "./data/pv1math-tst-ftS.ST04Q01.ATSCHL.csv", row.names=F)
#____________________________


#Generate both dataset without features ATSCHL, fisced, BELONG, ST04Q01 - DO WITH WEIGHTS WITHOUT NORMALIZE DATASET (WRONG)
math.tra = math.tra[,-c(1,5,9,12)]
math.tst = math.tst[,-c(1,5,9,12)]
#save data
write.csv2(math.tra, "./data/pv1math-tra-ftS.ST04Q01.misced.BELONG.ATSCHL.csv", row.names=F)
write.csv2(math.tst, "./data/pv1math-tst-ftS.ST04Q01.misced.BELONG.ATSCHL.csv", row.names=F)
#____________________________

#Generate both dataset without features ST04Q01, ATSCHL, and MATWKETH
math.tra = math.tra[,-c(1,12,20)]
math.tst = math.tst[,-c(1,12,20)]
#save data
write.csv2(math.tra, "./data/pv1math-tra-ftS.ST04Q01.ATSCHL.MATWKETH.csv", row.names=F)
write.csv2(math.tst, "./data/pv1math-tst-ftS.ST04Q01.ATSCHL.MATWKETH.csv", row.names=F)
# ____________________________

#Generate both dataset without features ST04Q01, ATSCHL, and MATINTFC
math.tra = math.tra[,-c(1,12,19)]
math.tst = math.tst[,-c(1,12,19)]
#save data
write.csv2(math.tra, "./data/pv1math-tra-ftS.ST04Q01.ATSCHL.MATINTFC.csv", row.names=F)
write.csv2(math.tst, "./data/pv1math-tst-ftS.ST04Q01.ATSCHL.MATINTFC.csv", row.names=F)
# ____________________________

#Generate both dataset without features ST04Q01, ESCS, and ATSCHL
math.tra = math.tra[,-c(1,7,12)]
math.tst = math.tst[,-c(1,7,12)]
#save data
write.csv2(math.tra, "./data/pv1math-tra-ftS.ST04Q01.ESCS.ATSCHL.csv", row.names=F)
write.csv2(math.tst, "./data/pv1math-tst-ftS.ST04Q01.ESCS.ATSCHL.csv", row.names=F)
# __________________________________________________________________________


require(FSelector)
require(rpart)
require(pROC)
#data reading
math.tra = read.csv2("data/pv1math-tra-ftS.ST04Q01.ATSCHL.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst-ftS.ST04Q01.ATSCHL.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tra$PV1MATH = as.factor(math.tra$PV1MATH)
################ exhaustive search ################
evaluator = function(subset, k=5){
  splits = runif(nrow(math.tra))
  
  results = sapply(1:k, function(i){
      test.idx = (splits >= ((i-1)/k) & (splits < (i/k)))
      train.idx = !test.idx
      
      test = math.tra[test.idx, , drop=F]
      train = math.tra[train.idx, , drop=F]
      
      tree = rpart(as.simple.formula(subset,"PV1MATH"), train)
      
      #error.rate = sum(test$PV1MATH != predict(tree, test, type="c")) / nrow(test)   #Acuraccy
      error.rate = auc(roc(as.numeric(test$PV1MATH)-1, predict(tree, test)[,2]))
      #return(1-error.rate)
      return(error.rate)
  })
  
  print(subset)
  print(mean(results))
  return(mean(results))
}

      sink("myfile.txt", append=TRUE)   #Redirect output to myfile.txt
subset = FSelector::exhaustive.search(names(math.tra[,-21]), evaluator)
      sink()                            #Get back output to terminal
f = as.simple.formula(subset, "PV1MATH")
print(f)


#After study variable combinations
math.tra = math.tra[,-c(1,2,4,9)]
math.tst = math.tst[,-c(1,2,4,9)]
#save data
write.csv2(math.tra, "./data/pv1math-tra-ftS.exaus1.csv", row.names=F)
write.csv2(math.tst, "./data/pv1math-tst-ftS.exaus1.csv", row.names=F)
#________________________________

#After study variable combinations
math.tra = math.tra[,-c(1,2,4,9,10,14,17)]
math.tst = math.tst[,-c(1,2,4,9,10,14,17)]
#save data
write.csv2(math.tra, "./data/pv1math-tra-ftS.exaus2.csv", row.names=F)
write.csv2(math.tst, "./data/pv1math-tst-ftS.exaus2.csv", row.names=F)
#__________________________________________________




