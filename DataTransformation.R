#data reading
math.tra = read.csv2("data/pv1math-tra.csv", sep=";", dec=",", quote="\"", encoding='utf-8')
math.tst = read.csv2("data/pv1math-tst.csv", sep=";", dec=",", quote="\"", encoding='utf-8')

require(caret)

#---------------- Center data ---------------
#Calcule centering over dataset
math.tra.centering = caret::preProcess(math.tra[,-23], method="center")
#get centered data
math.tra.centered = predict(math.tra.centering, math.tra[,-23])
    math.tra.centered = cbind(math.tra.centered, PV1MATH =math.tra$PV1MATH)
math.tst.centered = predict(math.tra.centering, math.tst)

#save data
write.csv2(math.tra.centered, "./data/pv1math-tra-centered.csv", row.names=F)
write.csv2(math.tst.centered, "./data/pv1math-tst-centered.csv", row.names=F)
#___________________________________________

#---------- Center and Scale data -----------
#Calcule centering and scale over train dataset
math.tra.centSca = caret::preProcess(math.tra[,-23], method=c("center","scale"))
#get centered and scale data
math.tra.CenterScaled = predict(math.tra.centSca, math.tra[,-23])
    math.tra.CenterScaled = cbind(math.tra.CenterScaled, PV1MATH =math.tra$PV1MATH)
math.tst.CenterScaled = predict(math.tra.centSca, math.tst)

#save data
write.csv2(math.tra.CenterScaled, "./data/pv1math-tra-centerScaled.csv", row.names=F)
write.csv2(math.tst.CenterScaled, "./data/pv1math-tst-centerScaled.csv", row.names=F)
#___________________________________________
