data("iris")
indexes <- c(1:40,51:90,101:140)

learningSet <- iris[indexes,1:4]
learningLabel <- iris[indexes,5]

testSet <- iris[-indexes,1:4]
testLabel <- iris[-indexes,5]

########Function "distance" that calculates the distance between two vectors
#input : 2 vectors
#output : the distance between the two vectors

euclud_dist <- function(vec1,vec2){
  
  if(length(vec1)!=length(vec2))
    return(NULL)
  
  sum <- 0
  for(i in 1:length(vec1))
    sum <- sum + (vec1[i]-vec2[i])^2
  
  return(sqrt(sum))
}

manhat_dist <- function(vec1,vec2){
  
  if(length(vec1)!=length(vec2))
    return(NULL)
  
  sum <- 0
  for(i in 1:length(vec1))
    sum <- sum + abs(vec1[i]-vec2[i])
  
  return(sum)
}

dist_element <- function(set,element,dist_name){
  
  result <- c()
  if(dist_name =="manhattan"){
    for(i in 1:length(set[,1]))
      result[i] <- manhat_dist(set[i,],element)
  }else{
    for(i in 1:length(set[,1]))
      result[i] <- euclud_dist(set[i,],element)
  }
  return (result)
}


dist_matrix <- function(matrix1,matrix2,dist_name){
  
  n <- length(matrix1[,1])
  m <- length(matrix2[,1])
  result <- matrix(nrow = m ,ncol = n)
  
  if(dist_name == "manhattan"){  
    for(i in 1:n){
      for(j in 1:m) {
        result[j,i] <- manhat_dist(matrix1[i,],matrix2[j,])
      }
    }
  }else{
    for(i in 1:n){
      for(j in 1:m) {
        result[j,i] <- euclud_dist(matrix1[i,],matrix2[j,])
      }
    }
    
  }
  return(result)
}


dist_same_matrix <- function(matrix,dist_name){
  
  n <- length(matrix[,1])
  result <- matrix(nrow=n,ncol=n)
  
  for(i in 1:n)
    result[i,i] <- 0
  
  if(dist_name == "manhattan"){
    for(i in 1:(n-1)){
      for(j in (i+1):n){
        dist <- manhat_dist(matrix[i,],matrix[j,])
        result [i,j] <- dist
        result [j,i] <- dist
      }
    }
  }else 
  {for(i in 1:(n-1)){
    for(j in (i+1):n){
      dist <- euclud_dist(matrix[i,],matrix[j,])
      result [i,j] <- dist
      result [j,i] <- dist
    }
  }
  }
  return(result)
} 



k_nn <- function(learnSet,learnLabel,testSet,vec_k,test_type,dist_name){
  
  dist_matrix <- matrix()
  
  if(test_type =="apprentissage" || test_type =="cross_validation" ){
    dist_matrix <- dist_same_matrix(learnSet,dist_name)
  }else{
    if(test_type =="test")
      dist_matrix <- dist_matrix(learnSet,testSet,dist_name)
    else
      return("Unkown parametre for test_type : ")
  }
  
  n <- length(dist_matrix[,1])
  m <- length(vec_k)
  
  for(i in 1:n)
    dist_matrix[i,] <- order(dist_matrix[i,])
  
  if(test_type == "cross_validation")
    dist_matrix <- dist_matrix[,2:length(dist_matrix[1,])]
  
  #matrix of data prediction
  matrix_k <- matrix(nrow = n,ncol = m)
  
  #All classes in label set
  levelsClass <- levels(learnLabel)
  
  #countClass count the occurence of class for a certain k 
  countClass <- c() 
  
  for(i in 1:n){
    for(x in levelsClass)
      countClass [x] <- 0
    j <- 1
    
    for(l in 1:length(vec_k)){
      for(k in j:vec_k[l]){
        countClass[learnLabel[dist_matrix[i,k]]] <- countClass[learnLabel[dist_matrix[i,k]]] + 1
      }
      
      x <- which(countClass == max(countClass))
      if(length(x)>1)
        matrix_k [i,l] <- names(sample(x,1))
      else
        matrix_k [i,l] <- names(x)
      #names(which.max(countClass))
      j <- vec_k[l] + 1
    }
  }
  
  return(matrix_k)
}

#v <- c(2)
#u <- k_nn(learningSet,learningLabel,testSet, v,"test","")


right_class <- function (matrix,label){
  
  right_class <- c()
  
  for(i in 1:length(matrix[1,]))
    right_class[i] <- 0
  
  for(i in 1:length(matrix[1,])){
    for(j in 1:length(matrix[,1])){
      if(matrix[j,i] == label[j] )
        right_class[i] <- right_class[i] + 1 
    }
    right_class[i] <- (right_class[i] / length(matrix[,1]))*100 
  }
  
  return(right_class)
}

k=c(1,2,5,10,20,40)
apprentissage <- k_nn(learningSet,learningLabel,testSet,k,test_type ="apprentissage",dist_name="Eucludean")
test <- k_nn(learningSet,learningLabel,testSet,k,test_type ="test",dist_name="Eucludean")
cross_validation <- k_nn(learningSet,learningLabel,testSet,k,test_type ="cross_validation",dist_name="Eucludean")

x <- k
y1 <- right_class(apprentissage,learningLabel)
y2 <- right_class(test,testLabel)
y3 <- right_class(cross_validation,learningLabel)


#plot(iris[,1:4],bg=c("red","green3","blue")[iris[,5]],
#     pch=c(21,25,24)[iris[,5]],main="Les iris de Fisher",
#     labels=c("Longueur\nsépale","Largeur\nsépale",
#              "Longueur\npétale","Largeur\npétale"))

plot(x, y1, type = "n", ylim = range(c(y1, y2, y3)), xlab = "parametre K", ylab = "rate of good classification %")
lines(x, y1, col = "blue")
lines(x, y2, col = "green")
lines(x, y3, col = "red")
legend(27, 99, legend=c("apprentissage rate", "test rate","cross validation rate"),
       col=c("blue", "green","red"), lty=1, cex=0.7)


install.packages(class)
library(class)



#for(i in 1:length(k) ){
#x <- knn(learningSet, testSet, learningLabel, k = k[i], l = 0, prob = FALSE, use.all = TRUE)
#print(right_class(as.matrix(x),testLabel))}

#print(y2)

#print(class(testLabel[1]))
