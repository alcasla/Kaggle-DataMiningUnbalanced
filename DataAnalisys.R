#data reading
math.tra = read.csv2("data/pv1math-tra.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst.csv", sep=";", dec=",", quote="\"", encoding='utf-8')


#general information
dim(math.tra);  dim(math.tst)
summary(math.tra)
head(math.tra, n=5)     #show first row values
str(math.tra)


#Calcule number of NAs (missing values) to each feature
#     dataset - the full dataset
#     return a number list of NAs with the same features order in the dataset
numNA = function(dataset){
  cat('Number of NAs by feature')
  sapply(1:dim(dataset)[2], function(x){
    cat(labels(dataset)[[2]][x], sum(which(is.na(dataset[,x]))), '\n')
    return( sum(which(is.na(dataset[,x]))) )
  } )
}
#_______________________________


#Boxplot  TERMINARRRRRRR********************///////////////***************************------------------
boxplot(math.tra, main="Dataset math boxplot", las=2, cex.axis=0.8)
boxplot(math.tra, main="Dataset math boxplot", las=2, cex.axis=0.8, outline=F)

#Add mark to test and combine all data
#separar instancias del train cambiarle los nombres a todas las variables y si eso ordenarlas
math.tra.0 = math.tra[which(math.tra$PV1MATH==0),]
math.tra.1 = math.tra[which(math.tra$PV1MATH==1),]

test = cbind(math.tst, PV1MATH=rep(2, dim(math.tst)[1]))
fullData = rbind(math.tra, test)

require(ggplot2)
require(reshape)
fullDataPlane <- melt(fullData, id.var = "PV1MATH")
ggplot(data=fullDataPlane, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=PV1MATH))
#_______________________________


#Study class PV1MATH
table(math.tra$PV1MATH)     #table labels
table(math.tra$PV1MATH)[[1]]/table(math.tra$PV1MATH)[[2]]
#_______________________________


#Calcule percentage to each label
#     feature - whatever feature from a dataset, specifically categoric features
ClassPercentages = function(feature){
  labels = unique(feature)
  cat('Percentages from labels:', labels, '\n')
  sapply(labels, function(x) sum( feature == x ) / length(feature))
}
ClassPercentages(math.tra$PV1MATH)    #percentage labels from class
#_______________________________

#Visualize feature distributuions
require(ggplot2)
require(gridExtra)
math.tra$PV1MATH = as.factor(math.tra$PV1MATH)

#Histogram from integer variables
plotFeatureVsClass = function(x) {
  ggplot(data = math.tra) + ggtitle(paste("Histograma", names(math.tra)[x], "Vs Class")) +
    geom_bar(mapping = aes(x=math.tra[,x], fill=PV1MATH)) +
    scale_fill_manual(values=c("#446677", "#66CC99")) +
    theme(axis.text.x = element_text(hjust = 1, size=8)) + 
    xlab(names(math.tra[x]))
}
#4x4 hist features with class color
#Integer features
grid.arrange(plotFeatureVsClass(1), plotFeatureVsClass(2), plotFeatureVsClass(3), plotFeatureVsClass(4), nrow=2, ncol=2)
grid.arrange(plotFeatureVsClass(5), plotFeatureVsClass(6), plotFeatureVsClass(8), nrow=2, ncol=2)
#Decimal features
grid.arrange(plotFeatureVsClass(7), plotFeatureVsClass(9), plotFeatureVsClass(10), plotFeatureVsClass(11), nrow=2, ncol=2)
grid.arrange(plotFeatureVsClass(12), plotFeatureVsClass(13), plotFeatureVsClass(14), plotFeatureVsClass(15), nrow=2, ncol=2)
grid.arrange(plotFeatureVsClass(16), plotFeatureVsClass(17), plotFeatureVsClass(18), plotFeatureVsClass(19), nrow=2, ncol=2)
grid.arrange(plotFeatureVsClass(20), plotFeatureVsClass(21), plotFeatureVsClass(22), nrow=2, ncol=2)
#_______________________________


#Plot from decimal variables
cor(math.tra)   #look for a feature correlated with class for good visualizatoin

plotXY = function(ft1, ft2, dataset){
  ggplot(data=dataset) + 
    geom_point(mapping = aes(x=dataset[,ft1], y=dataset[,ft2], color=dataset[ ,dim(dataset)[2]] )) +
    xlab(paste(names(dataset)[ft1])) +
    ylab(paste(names(dataset)[ft2])) #+
    #theme(legend.position = "none")
}
grid.arrange(plotXY(7, 18, math.tra), plotXY(9, 18, math.tra), plotXY(10, 18, math.tra), plotXY(11, 18, math.tra), 
             nrow=2, ncol=2)
grid.arrange(plotXY(12, 18, math.tra), plotXY(13, 18, math.tra), plotXY(14, 18, math.tra), plotXY(15, 18, math.tra), 
             nrow=2, ncol=2)
grid.arrange(plotXY(16, 18, math.tra), plotXY(17, 18, math.tra), plotXY(19, 18, math.tra), plotXY(20, 18, math.tra), 
             nrow=2, ncol=2)
grid.arrange(plotXY(21, 18, math.tra), plotXY(22, 18, math.tra), nrow=2, ncol=2)
#_______________________________


