#data reading
math.tra = read.csv2("data/pv1math-tra.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst.csv", sep=";", dec=",", quote="\"", encoding='utf-8')


####### V1 detect outliers to continuous features - Package OUTLIERS ########### 
require(outliers)
outliers = outliers::outlier(math.tra[,-c(1,2,3,4,5,6,8,23)])
print(outliers)

#Detect outlier instance and draw for ESCS
iOutliers = list(which(math.tra$ESCS == -4.2800))
ggplot(data=math.tra) +
  geom_point(mapping = aes(x=ESCS, y=SUBNORM, color=(math.tra$ESCS==-4.2800), shape=(math.tra$ESCS==-4.2800)))
#Detect outlier instance and draw for BELONG
iOutliers = c(iOutliers, which(math.tra$BELONG == -3.0000))
ggplot(data=math.tra) +
  geom_point(mapping = aes(x=BELONG, y=SUBNORM, color=(math.tra$BELONG == -3.0000), shape=(math.tra$BELONG == -3.0000)))
#Detect outlier instance and draw for STUDREL
iOutliers = c(iOutliers, which(math.tra$STUDREL == -3.1100))
ggplot(data=math.tra) +
  geom_point(mapping = aes(x=STUDREL, y=SUBNORM, color=(math.tra$STUDREL == -3.1100), shape=(math.tra$STUDREL == -3.1100)))
#Detect outlier instance and draw for ATTLNACT
iOutliers = c(iOutliers, which(math.tra$ATTLNACT == -3.3758))
ggplot(data=math.tra) +
  geom_point(mapping = aes(x=ATTLNACT, y=SUBNORM, color=(math.tra$ATTLNACT == -3.3758), shape=(math.tra$ATTLNACT == -3.3758)))
#Detect outlier instance and draw for ATSCHL
iOutliers = c(iOutliers, which(math.tra$ATSCHL == -2.9900))
ggplot(data=math.tra) +
  geom_point(mapping = aes(x=ATSCHL, y=SUBNORM, color=(math.tra$ATSCHL == -2.9900), shape=(math.tra$ATSCHL == -2.9900)))
#Detect outlier instance and draw for SMATBEH
iOutliers = c(iOutliers, which(math.tra$SMATBEH == 4.4249))
ggplot(data=math.tra) +
  geom_point(mapping = aes(x=SMATBEH, y=SUBNORM, color=(math.tra$SMATBEH == 4.4249), shape=(math.tra$SMATBEH == 4.4249)))
#Detect outlier instance and draw for MATHEFF
iOutliers = c(iOutliers, which(math.tra$MATHEFF == -3.7500))
ggplot(data=math.tra) +
  geom_point(mapping = aes(x=MATHEFF, y=SUBNORM, color=(math.tra$MATHEFF == -3.7500), shape=(math.tra$MATHEFF == -3.7500)))
#Detect outlier instance and draw for SUBNORM
iOutliers = c(iOutliers, which(math.tra$SUBNORM == -4.2456))
ggplot(data=math.tra) +
  geom_point(mapping = aes(x=SUBNORM, y=SMATBEH, color=(math.tra$SUBNORM == -4.2456), shape=(math.tra$SUBNORM == -4.2456)))

#remove outliers from dataset
iOutliers = sapply(1:length(iOutliers), function(x) iOutliers[[x]])
math.tra = math.tra[-iOutliers,]


#save data
write.csv2(math.tra, "./data/pv1math-tra-outliersV1.csv", row.names=F)
write.csv2(math.tst, "./data/pv1math-tst-outliersV1.csv", row.names=F)
#____________________________________________________________________________



####### V2 detect outliers to continuous features - Package OUTLIERS ###########
require(mvoutlier)

#detect highest outlier ratio
uni = uni.plot(math.tra[,c(7,9,10,11,12,13,14,15)], symb=T, alpha=0.005)
outliers = which(uni$md > 6)
uni = uni.plot(math.tra[,c(16,17,18,19,20,21,22)], symb=T, alpha=0.005)
outliers = c(outliers, which(uni$md > 6))
#Plot outlier value and histogram
par(mfrow=c(1,2))
hist(uni$md)
plot(uni$md)
par(mfrow=c(1,1))

#get new train set
math.tra = math.tra[-outliers,]


#save data
write.csv2(math.tra, "./data/pv1math-tra-outliersV2.csv", row.names=F)
write.csv2(math.tst, "./data/pv1math-tst-outliersV2.csv", row.names=F)
#____________________________________________________________________________


