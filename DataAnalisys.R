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

table(math.tra$PV1MATH)     #table class
apply(math.tra, 2, function(x) table(x, math.tra$PV1MATH))    #table each feature against class

#Calcule percentage to each label
#     feature - whatever feature from a dataset, specifically categoric features
ClassPercentages = function(feature){
  labels = unique(feature)
  cat('Percentages from labels:', labels, '\n')
  sapply(labels, function(x) sum( feature == x ) / length(feature))
}

ClassPercentages(math.tra$PV1MATH)    #percentage labels from class


#save data
write.csv2(math.tra, "./data/pv1math-tra.csv", row.names=F)
write.csv2(math.tst, "./data/pv1math-tst.csv", row.names=F)
