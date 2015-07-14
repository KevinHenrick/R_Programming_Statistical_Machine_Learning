									
########################### Problem 1###############################
library(MASS)
a <- matrix(c(1,2,-1), 3, 1)
n <- 100

Compute_Alpha <- function(a){
#This function computes the Alpha value of the a matrix (nxd). Alpha is equal to one over the summation of all the values in the last row of a.
  sum <- 0
  for (i in 1:dim(a)[2]) {
    sum <- sum + a[dim(a)[1],i]}
  return (1/sum)
}
Generate_e <- function (Alpha, H) {
 #This function creates the orthoganal vector e by inputting the coefficient (Alpha) that is multiplied within the linear combination of the orthoganal vector to a, and the hyperplane (H).
  E <- matrix(0, dim(H)[1], 1)
  for(i in 1:dim(H)[1]){
    for(j in 1:dim(H)[2]){
      E[i,1] <- E[i,1] + H[i,j]}}  
  return (E*Alpha)
}

Cut_Vector_by_1 <- function (e){
  #This function takes in a vector e with d+1 rows, cuts the last row, and returns a vector h with d rows.
  h <- matrix(0, dim(e)[1]-1, 1)
  for (i in 1:dim(e)[1]-1) {
    h[i,1] <- e[i,1]}
  return (h)
}
Norm_Value <- function(V) {  
 #This function calculates the Norm of a vector V by summing the squares of each component, and taking the square root.
  sum = 0
  for (i in 1:dim(V)[1]){
    sum = sum + V[i]^2
  }  
  return(sqrt(sum))
}
Generate_Data <- function(n, a, h, v_H){
  #This function generates the data and returns S (an n by d matrix) that lists the datapoints in the rows, and variable dimensions in the columns.
  S <- matrix(0, dim(a)[1]-1, n)
  for(i in 1:floor(n/2)){
    S[ , i] <- h*(1+ runif(1, -5 , 5)) + v_H*(1 + runif(1, 0, Norm_Value(v_H)))                                    
  }
  for(i in  floor(n/2):n) {
    S[ , i] <- h*(1+ runif(1, -5, 5)) - v_H*(1 + runif(1, 0, Norm_Value(v_H)))   }
  return (S)}
fakedata <- function (a, n){
   #This is the fakedata function that creates the linearly separable two-class dataset. 
    H <- Null(a)
   Alpha <- Compute_Alpha(a)  
   e <- Generate_e (Alpha, H)
   h <- Cut_Vector_by_1(e)
  v_H <- Cut_Vector_by_1(a)
  List <- Generate_Data(n, a, h, v_H)
  return (List)}

Generate_c <- function(n){
#This function creates an equal amount of two class data (1 thru floor(n/2) is labelled class +1 and floor(n/2) thru n is labelled class -1). The values are listed an n-row column vector.
  c <- matrix(0, n, 1) 
  for (i in 1:floor(n/2)){
    c[i]<- 1
  }
  for (i in floor(n/2):n){
    c[i]<- -1
  }
   return(c)}

S <- fakedata (a, n)
Y <- Generate_c (n)


########################### Problem 2###############################
Add_Row_of_Ones <- function(S){
  #This function adds a last row of 1's as elements within every column of the Matrix S outputted by the fakedata function.
S_with_Ones <- matrix(1,dim(S)[1]+1,dim(S)[2])
  for(i in 1:dim(S)[1]){
    S_with_Ones[i,] <- S[i,] 
  }  
  return(S_with_Ones)
}

z <- matrix(a, 1, 3)
Classify_Data <- function(S,z){
  #This function classifies all of the datapoints within S that have a scalar product with z that is greater than zero as being 1, and less than zero being -1. It returns a list Y of the classified datapoints.
  S_with_Ones <- Add_Row_of_Ones(S)
  Y <- matrix(0, dim(S)[2], 1)
  for(i in 1:dim(S_with_Ones)[2]){
    if(z%*%S_with_Ones[,i] >= 0){
      Y[i,1]= 1    }  
    else{
     Y[i,1]= -1    }}
  return(Y)}



########################### Problem 3###############################
Part a) Indicator <- function(z, S, Y){
#This function generates the Indicator function for f[Xi] != Yi. It will be equal to 1 when f[Xi] != Yi, and will be equal to zero when f[Xi] = Yi.
 Indicator <- 0
if ((((S%*%z>0) && (Y== -1))||((S%*%z<0) && (Y==1)))) {
    Indicator <- 1 
  }
  else {
    Indicator <- 0
  }
   return(Indicator)
}

Generate_Cost <-function(z,S,Y){
  #This function calculates the Perceptron Cost Function of fitting the hyperplane, which serves as the classifier for the linearly separable two-class dataset. 
  Cost <- 0 
  for(i in 1:dim(S)[2]){ 
    Cost <- Cost + abs(S[,i]%*%z) * Indicator(z,S[,i],Y[i])   
  } 
  return(Cost)}




Generate_Gradient<-function(z,S,Y){  
  #This function calculates the Gradient for the Cost Function.
  Gradient <- matrix(0,dim(S)[1],1) 
  for(i in 1:dim(S)[2]){  
    Gradient <- Gradient + Indicator(z,S[,i],Y[i])*Y[i]*S[,i] 
  }
  return(Gradient) }
S <- Add_Row_of_Ones(S)
Y<- Classify_Data(S, z)

Perceptrain_Batch<-function(S,Y){
#This function runs the Perceptrain Batch Algorithm with Alpha = (1/k), where k is the current iteration number.
  z <- matrix(1,dim(S)[1],1)
 k <- 1
 Cost <- Generate_Cost(z,S,Y)
  while(Cost!=0){
    	Gradient <- Generate_Gradient(z,S,Y)
z <- z + (1/k)*Gradient
z_history <- z
  	 Cost <- Generate_Cost(z,S,Y)
    	k <- k+1
#The following line of code is executed when z_history is plotted. Otherwise, it’s commented out.   abline(z[3,1]/z[2,1], -z[1,1]/z[2,1], col="light grey")
    	 print(z_history) }
    return(z) }
Part b) 
Obtain_Increment <- function (S, z, Y){
#This function checks the scalar product of S and z for each column of S, and if it’s greater than zero, returns the value of the vector within S multiplied by 1. If the scalar product of S and z is less than zero, the vector is multiplied by -1.
  Increment <- (-1)
  Last <- 0
 if (S%*%z>0){
    Increment <- 1  } 
  if (Last != Y){
 Last <- 1}
  return (Increment*S*Last)
}

Perceptrain_Fixed <- function(S,Y){  
  #This function runs the Perceptrain Fixed Increment Algorithm with Alpha = 1, where k is the current iteration number, and returns the normal vector z to the hyperplane.
    z <- matrix(1,dim(S)[1],1)  
  k <- 1 
  Cost <- Generate_Cost(z,S,Y) 
  while(Cost!=0){ 
    for(i in 1:dim(S)[2]){ 
      z <- z - Obtain_Increment(S[,i],z, Y[i]) 
      Cost <- Generate_Cost(z,S,Y)
      k <- k+1
}
   #The following line is executed when the z_history is plotted against the training data. Otherwise, it’s commented out.
    abline(z[3,1]/z[2,1], -z[1,1]/z[2,1], col="light grey")
  } return(z) 
    abline(z[3,1]/z[2,1], -z[1,1]/z[2,1], col="light grey")

  }
















########################### Problem 4###############################
#Generating a 3-d random vector a (called a_Test):

a_Test <- matrix(c(runif(1, 1, 5), runif(1, 1, 10), runif(1, 1, 3)), 3, 1)
H_Test <- Null (a_Test)
Alpha <- Compute_Alpha(a_Test)
e_Test <- Generate_e(Alpha, H_Test)
h_Test <- Cut_Vector_by_1(e_Test)
v_H_Test <- Cut_Vector_by_1(a_Test)



#Running the fakedata and classification functions on the 3-d vector a_Test and 100 data-points, respectively:

S_Test <- fakedata(a_Test, 100)
Y_Test <- Generate_c (100)

#Adding a row of Ones to the output of the fakedata function in order to run the Perceptrain Algorithms:
S_Test <- Add_Row_of_Ones(S_Test)

#Running the two Perceptrain Algorithms (Batch and then Fixed Increment, respectively):
z_Batch <- matrix(Perceptrain_Batch(S_Test,Y_Test), 1, 3)
z_Fixed <- matrix(Perceptrain_Fixed(S_Test,Y_Test), 1, 3)

######################### Problem 5###############################
#Converting the data and vectors into their corresponding 2-d vector representations in order to plot:

x_y_Batch <- c((z_Batch[1]/z_Batch[3]),(z_Batch[2]/z_Batch[3]))
x_y_Fixed <- c((z_Fixed[1]/z_Fixed[3]),(z_Fixed[2]/z_Fixed[3]))

#Plotting the data and 2-d vector representation of the normal vector (in dark blue) to the hyperplane classifier using the Perceptron Batched Algorithm. +1 Data Values are green, and -1 Data Values are red.
plot(0, 0, asp=1, xlim=c(-2*x_y_Batch[1],2*x_y_Batch[1]), ylim=c(-2*x_y_Batch[2], 2*x_y_Batch[2]), col=0)
grid()
abline(h=0, col="blue")
abline(v=0, col="blue")
arrows(0,0, x_y_Batch[1], x_y_Batch[2], length=.2, col="dark blue",lwd=3)
abline(0, h_Test[2,1]/h_Test[1,1])
for(j in 1:dim(S_Test)[2]){
  if (Y_Test[j]== 1){
    points(S_Test[2,j],S_Test[1,j],col="green")}
  else{
    points(S_Test[2,j],S_Test[1,j],col="red")
   }
}



#Plotting the data and 2-d vector representation of the normal vector (in dark green) to the hyperplane classifier using the Perceptron Fixed-Increment Algorithm. +1 Data Values are blue, and -1 Data Values are red.
plot(0, 0, asp=1, xlim=c(-2*x_y_Fixed[1],2*x_y_Fixed[1]), ylim=c(-2*x_y_Fixed[2], 2*x_y_Fixed[2]), col=0)
grid()
abline(h=0, col="blue")
abline(v=0, col="blue")
arrows(0,0, x_y_Fixed[1], x_y_Fixed[2], length=.2, col="dark green",lwd=3)
abline(0, h_Test[2,1]/h_Test[1,1])
for(j in 1:dim(S_Test)[2]){
  if (Y_Test[j]== 1){
    points(S_Test[2,j],S_Test[1,j],col="blue")}
  else{
    points(S_Test[2,j],S_Test[1,j],col="red")
  }
}










#Batch Algorithm Plots:

#Plot 1: The Output of the Batch Perceptron Algorithm is shown with Training Data and z_History (Evolution of the Hyperplanes in Gray). The Red line is the Classifier Hyper plane, and the Blue Arrow is the Normal Vector.

#Plot 2: The Output of the Batch Perceptron Algorithm is shown with the Test Data Set and the Classifier Hyperplane (in Red). The Normal Vector to the Hyperplane is represented by the Blue Arrow.

#Fixed Increment Algorithm Plots:

#Plot 3: The Output of the Fixed Increment Perceptron Algorithm is shown with Training Data and z_History (Evolution of the Hyperplanes in Gray). The Red line is the classifier hyper plane, and the Blue arrow is the normal vector.

#Plot 4: The Output of the Fixed Increment Perceptron Algorithm is shown with the Test Data Set and the Classifier Hyperplane (in Red). The Normal Vector to the Hyperplane is represented by the Blue Arrow.