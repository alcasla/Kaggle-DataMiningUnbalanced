#data reading
math.tra = read.csv2("data/pv1math-tra-centerScaled.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst-centerScaled.csv", sep=";", dec=",", quote="\"", encoding='utf-8')

require(FSelector)

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




# 9.1.5. Aproximación filter: relief, page 46


