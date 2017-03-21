# imbalanced.R
# Implementation and evaluation of imbalanced classification techniques 
# Programming code courtesy by Sarah Vluymans, Sarah.Vluymans@UGent.be

## load the subclus dataset
subclus <- read.table("subclus.txt", sep=",")
colnames(subclus) <- c("Att1", "Att2", "Class")

# determine the imbalance ratio
unique(subclus$Class)
nClass0 <- sum(subclus$Class == 0)    #number of instances from 0 class
nClass1 <- sum(subclus$Class == 1)    #number of instances from 1 class
IR <- nClass1 / nClass0       #rate=5, for each instance of 0 class there are 5 of 1 class
IR

# visualize the data distribution, plot and coloring points
plot(subclus$Att1, subclus$Att2)
points(subclus[subclus$Class==0,1],subclus[subclus$Class==0,2],col="red")
points(subclus[subclus$Class==1,1],subclus[subclus$Class==1,2],col="blue")  

# Set up the dataset for 5 fold cross validation.
# Make sure to respect the class imbalance in the folds.
pos <- (1:dim(subclus)[1])[subclus$Class==0]      #positivo and negative instance positions
neg <- (1:dim(subclus)[1])[subclus$Class==1]

CVperm_pos <- matrix(sample(pos,length(pos)), ncol=5, byrow=T)    #change representation to matrix dividing instances 
CVperm_neg <- matrix(sample(neg,length(neg)), ncol=5, byrow=T)      #between five cols to do 5k-fold

CVperm <- rbind(CVperm_pos, CVperm_neg)     #combine classes split into 5 cols


# Base performance of 3NN
library(class)
knn.pred = NULL
for( i in 1:5){     #for each col classify and insert into an object
  predictions <- knn(subclus[-CVperm[,i], -3], subclus[CVperm[,i], -3], subclus[-CVperm[,i], 3], k = 3)
  knn.pred <- c(knn.pred, predictions)
}
acc <- sum((subclus$Class[as.vector(CVperm)] == 0 & knn.pred == 1) 
           | (subclus$Class[as.vector(CVperm)] == 1 & knn.pred == 2)) / (nClass0 + nClass1) #calculate accuracy
tpr <- sum(subclus$Class[as.vector(CVperm)] == 0 & knn.pred == 1) / nClass0     #calculate true positive rate
tnr <- sum(subclus$Class[as.vector(CVperm)] == 1 & knn.pred == 2) / nClass1     #calculate true negative rate
gmean <- sqrt(tpr * tnr)      #geometric mean

#Aplicado sobre el dataset subclus obtengo una medida de accuracy del 0.936 y una media geométrica de 0.869.
#Estas medidas me da a enterder que por su diferencia se está acertando segun la primera en un buen ratio pero 
#debido al desbalanceo presente en el problema no hay un acierto equilibrado, luego estamos desfavoreciendo a la 
#clase minoritaria.


require('class')

# 1. ROS
knn.pred = NULL
for( i in 1:5){
  #get data for 5-FCV from last partitions
  train <- subclus[-CVperm[,i], -3]
  classes.train <- subclus[-CVperm[,i], 3]  #get classes from train cases
  test  <- subclus[CVperm[,i], -3]
  
  # randomly oversample the minority class (class 0)
  minority.indices <- (1:dim(train)[1])[classes.train == 0]   #get index from each minority class case
  to.add <- dim(train)[1] - 2 * length(minority.indices)      #number of casos to balance classes
  duplicate <- sample(minority.indices, to.add, replace = T)  #vector to insert cases
  for( j in 1:length(duplicate)){   #add each case
    train <- rbind(train, train[duplicate[j],])
    classes.train <- c(classes.train, 0)
  }  
  
  # use the modified training set to make predictions
  predictions <-  knn(train, test, classes.train, k = 3)
  knn.pred <- c(knn.pred, predictions)
}
tpr.ROS <- sum(subclus$Class[as.vector(CVperm)] == 0 & knn.pred == 1) / nClass0     #hits class 0
tnr.ROS <- sum(subclus$Class[as.vector(CVperm)] == 1 & knn.pred == 2) / nClass1     #hits class 1
gmean.ROS <- sqrt(tpr.ROS * tnr.ROS)

#Tras entrenar con un conjunto de datos balanceado mejora la media geométrica hasta 0.9128 mejorando 4.5% de aciertos en test.
#Claramente hay una mejora frente a un entrenamiento desbalanceado con un ratio de 5.


# 2. RUS
knn.pred = NULL
for( i in 1:5){
  
  train <- subclus[-CVperm[,i], -3]
  classes.train <- subclus[-CVperm[,i], 3] 
  test  <- subclus[CVperm[,i], -3]
  
  # randomly undersample the minority class (class 1)
  majority.indices <- (1:dim(train)[1])[classes.train == 1]     #index of class 1 cases
  to.remove <- 2* length(majority.indices) - dim(train)[1]      #number of cases to remove
  remove <- sample(majority.indices, to.remove, replace = F)
  train <- train[-remove,] 
  classes.train <- classes.train[-remove]       #get data without inbalance
  
  # use the modified training set to make predictions
  predictions <-  knn(train, test, classes.train, k = 3)
  knn.pred <- c(knn.pred, predictions)
}
tpr.RUS <- sum(subclus$Class[as.vector(CVperm)] == 0 & knn.pred == 1) / nClass0
tnr.RUS <- sum(subclus$Class[as.vector(CVperm)] == 1 & knn.pred == 2) / nClass1
gmean.RUS <- sqrt(tpr.RUS * tnr.RUS)

#El resultado de reducir la clase mayoritaria NO MEJORA respecto a no hacer nada, 0.8675944 frente a 0.869, y es peor 
#que en caso de reproducir la clase minoritaria, su explicación es lógica y es devido a que eliminamos elementos válidos 
#para su aprendizaje, mientras que en ROS añadimos elementos, aunque repetidos, pero no quitamos información útil que al
#fin y al cabo ayuda al modelo a su aprendizaje.

# Visualization (RUS on the full dataset)
subclus.RUS <- subclus
majority.indices <- (1:dim(subclus.RUS)[1])[subclus.RUS$Class == 1] 
to.remove <- 2 * length(majority.indices) - dim(subclus.RUS)[1]       
remove <- sample(majority.indices, to.remove, replace = F)
subclus.RUS <- subclus.RUS[-remove,]


#Data preprocessing visualization comparing
par(mfrow=c(2,1))

#preprcessed data
plot(subclus.RUS$Att1, subclus.RUS$Att2)
points(subclus.RUS[subclus.RUS$Class==0,1],subclus.RUS[subclus.RUS$Class==0,2],col="red")
points(subclus.RUS[subclus.RUS$Class==1,1],subclus.RUS[subclus.RUS$Class==1,2],col="blue") 

#original data
plot(subclus$Att1, subclus$Att2)
points(subclus[subclus$Class==0,1],subclus[subclus$Class==0,2],col="red")
points(subclus[subclus$Class==1,1],subclus[subclus$Class==1,2],col="blue")

par(mfrow=c(1,1))


# 2.4.1 Distance function
distance <- function(i, j, data){
  sum <- 0
  for(f in 1:dim(data)[2]){
    if(is.factor(data[,f])){ # nominal feature
      if(data[i,f] != data[j,f]){
        sum <- sum + 1
      }
    } else {
      sum <- sum + (data[i,f] - data[j,f]) * (data[i,f] - data[j,f])
    }
  }
  sum <- sqrt(sum)
  return(sum)
}


# 2.4.2 Get 5 nearest neighbors
getNeighbors <- function(x, minority.instances, train){
  #get distances between case and each instance
  distances <- as.data.frame(lapply(1:length(minority.instances), distance, j=x, data=train))
  names(distances) <- minority.instances      #name to identify later
  
  #delete row of distance whit itself
  if(length(which(names(distances)==x)))
    distances <- distances[-c(x)]
  
  distances <- sort(distances)      #sort ascending by distance
  mins <- names(distances[1:5])     #get index from 5 min distances
  return(mins)
}

#test code for getNeighbors
minorityInd <- (1:dim(subclus)[1])[subclus$Class == 0]
x <- 1;
data <- subclus
nearestNeighbors <- getNeighbors(case, minorityInd, data)


#2.4.3 Create synthetic instance from 5NearestNeighbors
syntheticInstance <- function(x, nearestNeighbors, data){
  nn <- nearestNeighbors[round(runif(1, 1,length(nearestNeighbors)))]     #select random nearest neighbor
  #print(paste("SyntheticInstance: base case", x, "neighbor selected", nn));
  
  i1 <- data[x,];   #instance and random rearest neighbor
  i2 <- data[nn,];
  syntetic <- i1;
  
  #print("i1:");       #######
  #print(i1);          #######
  #print("i2:");       #######
  #print(i2);          #######
  
  rndm <- runif(1);   #random [0,1]
  for(f in 1:(dim(data)[2]-1)){   #for each feature
    if(is.factor(data[,f])){    # nominal feature
      if(round(runif(1)==0)){ syntetic[,f] = i1[,f]; }  #put random nominal value
      else { syntetic[,f] = i2[,f]; }
    }else {                     # numerical feature
      dis = abs(i1[,f]-i2[,f]);
      if(i1[,f] < i2[,f])
        { syntetic[,f] = i1[,f] + (dis*rndm); }
      else
        { syntetic[,f] = i2[,f] + (dis*rndm); }
    }
  }
  
  #print("syntetic:");   #######
  #print(syntetic);      #######
  
  return(syntetic)
}

#test code for syntheticInstance
newIns <- syntheticInstance(x, nearestNeighbors, data)


#Unbalanced ratio function - Binary class 0 - 1
# return ratio [0,1]
unbalancedRatio <- function(data, minority.class){
  instances0 <- (1:dim(data)[1])[data[,dim(data)[2]] == 0]
  instances1 <- (1:dim(data)[1])[data[,dim(data)[2]] == 1]
  
  if(minority.class == 0){
    ratio = length(instances0)/length(instances1);
  } else{
    ratio = length(instances1)/length(instances0);
  }
  
  return(ratio)
}

#2.4.4 SMOTE
#Assume:  class in last column 
#         unbalanced ratio is for minority regarding to majority class
#         binary class, 0 or 1
#PARAMETERS
#         data = full dataset
# ratioDesired = unbalanced ratio desired [0,1]
SMOTE <- function(data, ratioDesired){
  #get instances index from each class - 0 and 1 
  instances0 <- (1:dim(data)[1])[data[,dim(data)[2]] == 0]
  instances1 <- (1:dim(data)[1])[data[,dim(data)[2]] == 1]
  
  #get index from minority class
  if(length(instances0) < length(instances1)){
    minority.instances <- instances0;
    minority.class = 0;
  } else{
    minority.instances <- instances1;
    minority.class = 1;
  }
  
  #calcule unbalanced ratio
  ratio = unbalancedRatio(data, minority.class);
  ratioInit = ratio;
  
  out <- data;
  #generate instances until equal ratios
  while(ratio < ratioDesired)
  {
    x = sample(minority.instances, 1);
        #print(paste("Instance X: ", x));
    nearestNeighbors <- getNeighbors(x, minority.instances, data);
    synthetic <- syntheticInstance(x, nearestNeighbors, data);
    synthetic <- round(synthetic, digits=3);      #round, too digits
    out = rbind(out, synthetic);              #add instance to dataset
    
    #calcule unbalanced ratio
    ratio = unbalancedRatio(out, minority.class);
  }
  
  #algorithm  feedback
  print(paste("Inicio: ", length(instances0), "instances class 0 -", length(instances1), "instances class 1."));
  print(paste("Unbalanced ratio: ", ratioInit, "with minority class ", minority.class));
  print(paste("Final: ", length((1:dim(out)[1])[out[,dim(out)[2]] == 0]), "instances class 0-", 
        length((1:dim(out)[1])[out[,dim(out)[2]] == 1]), "instances class 1."));
  
  return(out);
}

#balance dataset until minority class (0) has half instances than majority class
data <- SMOTE(data, 0.8);


#####CHECK RESULT SMOTE
# Set up the dataset for 5 fold cross validation.
nClass0 <- sum(data$Class == 0)    #number of instances from 0 class
nClass1 <- sum(data$Class == 1)    #number of instances from 1 class

pos <- (1:dim(data)[1])[data$Class==0]      #positivo and negative instance positions
neg <- (1:dim(data)[1])[data$Class==1]

CVperm_pos <- matrix(sample(pos,length(pos)), ncol=5, byrow=T)    #change representation to matrix dividing instances 
CVperm_neg <- matrix(sample(neg,length(neg)), ncol=5, byrow=T)      #between five cols to do 5k-fold

CVperm <- rbind(CVperm_pos, CVperm_neg)     #combine classes split into 5 cols

SMOTE.pred = NULL
require(class)
for( i in 1:5){
  
  train <- data[-CVperm[,i], -3]
  classes.train <- data[-CVperm[,i], 3] 
  test  <- data[CVperm[,i], -3]
  
  # use the modified training set to make predictions
  predictions <-  knn(train, test, classes.train, k = 3)
  SMOTE.pred <- c(SMOTE.pred, predictions)
}
tpr.SMOTE <- sum(data$Class[as.vector(CVperm)] == 0 & SMOTE.pred == 1) / nClass0
tnr.SMOTE <- sum(data$Class[as.vector(CVperm)] == 1 & SMOTE.pred == 2) / nClass1
gmean.SMOTE <- sqrt(tpr.SMOTE * tnr.SMOTE)

#VISUALIZATION
#Data preprocessing visualization comparing
par(mfrow=c(2,1))
#preprcessed data SMOTE
plot(data$Att1, data$Att2, main="Subclus SMOTE")
points(data[data$Class==0,1],data[data$Class==0,2],col="red")
points(data[data$Class==1,1],data[data$Class==1,2],col="blue") 
#original data
plot(subclus$Att1, subclus$Att2, main="Subclus Original")
points(subclus[subclus$Class==0,1],subclus[subclus$Class==0,2],col="red")
points(subclus[subclus$Class==1,1],subclus[subclus$Class==1,2],col="blue")
par(mfrow=c(1,1))

#Aplicado nuestro método propio SMOTE (ratioDesired=0.8) obtenemos un resultado para la media geométrica de 0.9628499 dejando así la mejor
#marca entre todo los métodos aplicados, así quedan los resultados:
#   Método        GMEAN
#   SMOTE         0.963
#   ROS           0.912
#   3NN           0.869
#   RUS           0.867

#Conclusión: Claramente los métodos generadores de instancias sintéticas ganan a los métodos conservadores o a los
#eliminadores de instancias. Generar instancias sintéticas a partir de las existentes mejora el resultado más de un 5% 
#frente a replicar de forma aleatoria. Supongo que la diferencia no es mayor por la posibilidad de crear instancias poco
#adecuadas en las fronteras al utilizar como vecinos cercanos instancias algo ruidosas. Aún así los resultados son 
#satisfactorios.


###############################################################
#OPTIONAL - UNBALANCED PACKaGE
subclus <- read.table("subclus.txt", sep=",")
colnames(subclus) <- c("Att1", "Att2", "Class")

require(unbalanced);

#code majority class as 0 and minority as 1
subclusTS = subclus
index0 = which(subclus$Class==0)
index1 = which(subclus$Class==1)
subclusTS[index0,3] = 1
subclusTS[index1,3] = 0

#Apply TomekLinks - dataTS
subclusTS = ubTomek(subclusTS[,1:2], subclusTS$Class)
subclusTS = data.frame(subclusTS$X, class = as.factor(subclusTS$Y))
#Unbalanced Ratio before smote
length(which(subclusTS$class==0)) / length(which(subclusTS$class==1))

#Apply SMOTE - dataTS
subclusTS = ubSMOTE(subclusTS[,-3], subclusTS[,3], perc.over=200)
subclusTS = data.frame(subclusTS$X, class = as.factor(subclusTS$Y))
#Unbalanced Ratio after smote
length(which(subclusTS$class==0)) / length(which(subclusTS$class==1))

#Apply TomekLinks - dataTS
subclusTS = ubTomek(subclusTS[,1:2], subclusTS$class)
subclusTS = data.frame(subclusTS$X, class = as.factor(subclusTS$Y))
#Unbalanced Ratio before smote
length(which(subclusTS$class==0)) / length(which(subclusTS$class==1))


#####CHECK RESULT TOMEKLINKS + SMOTE
# Set up the dataset for 5 fold cross validation.
nClass0 <- sum(subclusTS$class == 0)    #number of instances from 0 class
nClass1 <- sum(subclusTS$class == 1)    #number of instances from 1 class

pos <- (1:dim(subclusTS)[1])[subclusTS$class==0]      #positivo and negative instance positions
neg <- (1:dim(subclusTS)[1])[subclusTS$class==1]

CVperm_pos <- matrix(sample(pos,length(pos)), ncol=5, byrow=T)    #change representation to matrix dividing instances 
CVperm_neg <- matrix(sample(neg,length(neg)), ncol=5, byrow=T)      #between five cols to do 5k-fold

CVperm <- rbind(CVperm_pos, CVperm_neg)     #combine classes split into 5 cols

SMOTE.pred = NULL
require(class)
for( i in 1:5){
  
  train <- subclusTS[-CVperm[,i], -3]
  classes.train <- subclusTS[-CVperm[,i], 3] 
  test  <- subclusTS[CVperm[,i], -3]
  
  # use the modified training set to make predictions
  predictions <-  knn(train, test, classes.train, k = 3)
  SMOTE.pred <- c(SMOTE.pred, predictions)
}
tpr.TOMEKSMOTE <- sum(subclusTS$class[as.vector(CVperm)] == 0 & SMOTE.pred == 1) / nClass0
tnr.TOMEKSMOTE <- sum(subclusTS$class[as.vector(CVperm)] == 1 & SMOTE.pred == 2) / nClass1
gmean.TOMEKSMOTE <- sqrt(tpr.TOMEKSMOTE * tnr.TOMEKSMOTE)

#gmean =  0.9345498 aplicando TomekLinks + Smote
#         0.9608181 aplicando TomekLinks + Smote + TomekLinks
#Se nota claramente como limpiar la frontera de instancias que pueden afectar negativamente al clasificador mejora 
#el resultado en casi un 3%.

#VISUALIZATION
#Data preprocessing visualization comparing
par(mfrow=c(2,1))
#preprcessed data SMOTE
plot(subclusTS$Att1, subclusTS$Att2, main="Subclus TomekLinks + SMOTE + TomekLinks")
points(subclusTS[subclusTS$class==0,1],subclusTS[subclusTS$class==0,2],col="blue")
points(subclusTS[subclusTS$class==1,1],subclusTS[subclusTS$class==1,2],col="red") 
#original data
plot(subclus$Att1, subclus$Att2, main="Subclus Original")
points(subclus[subclus$Class==0,1],subclus[subclus$Class==0,2],col="red")
points(subclus[subclus$Class==1,1],subclus[subclus$Class==1,2],col="blue")
par(mfrow=c(1,1))

#####################
#SOMTE + ENN
subclus <- read.table("subclus.txt", sep=",")
colnames(subclus) <- c("Att1", "Att2", "Class")

require(unbalanced);

#code majority class as 0 and minority as 1
subclusSE = subclus
index0 = which(subclus$Class==0)
index1 = which(subclus$Class==1)
subclusSE[index0,3] = 1
subclusSE[index1,3] = 0

#Apply SMOTE - dataSE
subclusSE$Class = as.factor(subclusSE$Class)
subclusSE = ubSMOTE(subclusSE[,-3], subclusSE[,3], perc.over=200)
subclusSE = data.frame(subclusSE$X, class = as.factor(subclusSE$Y))
#Unbalanced Ratio after smote
length(which(subclusSE$class==0)) / length(which(subclusSE$class==1))

#Apply ENN - dataSE
subclusSE = ubENN(subclusSE[,-3], subclusSE[,3])
subclusSE = data.frame(subclusSE$X, class = as.factor(subclusSE$Y))
#Unbalanced Ratio after ENN
length(which(subclusSE$class==0)) / length(which(subclusSE$class==1))

#####CHECK RESULT SMOTE + ENN
# Set up the dataset for 5 fold cross validation.
nClass0 <- sum(subclusSE$class == 0)    #number of instances from 0 class
nClass1 <- sum(subclusSE$class == 1)    #number of instances from 1 class

pos <- (1:dim(subclusSE)[1])[subclusSE$class==0]      #positivo and negative instance positions
pos <- pos[1:length(pos)-1]
neg <- (1:dim(subclusSE)[1])[subclusSE$class==1]

CVperm_pos <- matrix(sample(pos,length(pos)), ncol=5, byrow=T)    #change representation to matrix dividing instances 
CVperm_neg <- matrix(sample(neg,length(neg)), ncol=5, byrow=T)      #between five cols to do 5k-fold

CVperm <- rbind(CVperm_pos, CVperm_neg)     #combine classes split into 5 cols

SMOTE.pred = NULL
require(class)
for( i in 1:5){
  
  train <- subclusSE[-CVperm[,i], -3]
  classes.train <- subclusSE[-CVperm[,i], 3] 
  test  <- subclusSE[CVperm[,i], -3]
  
  # use the modified training set to make predictions
  predictions <-  knn(train, test, classes.train, k = 3)
  SMOTE.pred <- c(SMOTE.pred, predictions)
}
tpr.SMOTEENN <- sum(subclusSE$class[as.vector(CVperm)] == 0 & SMOTE.pred == 1) / nClass0
tnr.SMOTEENN <- sum(subclusSE$class[as.vector(CVperm)] == 1 & SMOTE.pred == 2) / nClass1
gmean.ENNSMOTE <- sqrt(tpr.SMOTEENN * tnr.SMOTEENN)

#gmean =  0.9345498 aplicando Smote + ENN

#VISUALIZATION
#Data preprocessing visualization comparing
par(mfrow=c(2,1))
#preprcessed data SMOTE
plot(subclusSE$Att1, subclusSE$Att2, main="Subclus SMOTE + ENN")
points(subclusSE[subclusSE$class==0,1],subclusSE[subclusSE$class==0,2],col="blue")
points(subclusSE[subclusSE$class==1,1],subclusSE[subclusSE$class==1,2],col="red") 
#original data
plot(subclus$Att1, subclus$Att2, main="Subclus Original")
points(subclus[subclus$Class==0,1],subclus[subclus$Class==0,2],col="red")
points(subclus[subclus$Class==1,1],subclus[subclus$Class==1,2],col="blue")
par(mfrow=c(1,1))

