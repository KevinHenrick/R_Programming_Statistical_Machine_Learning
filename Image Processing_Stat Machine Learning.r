#Kevin Henrick Data Mining Homework 4
setwd("C:\\Users\\Kevin\\Desktop\\Homework 4 Data Mining")
H <- matrix(readBin("histograms.bin", "double", 640000), 40000, 16) #Matrix of input
#histograms
#Check to see that the bin file was properly loaded:
dim(H) #[1] 40000 16 printed.
rowSums(H) #The sum of each row within the outputted matrix is 121.
#Therefore, the bin file was indeed correctly loaded.
############################ Part 1) #####################################
#Implementing the EM Algorithm:
Small_Constant <- .01
H <- H + Small_Constant
MultinomialEM <- function(H,k,t){

 n <- dim(H)[1]
 d <- dim(H)[2]
 k_randomly_chosen_histograms <- sample(1:n, k, replace=FALSE)

 #Compute the Thetas - they are parameter vectors of a multinomial distribution and can
be regarded as the center of a cluster; they are computed as
 #the weighted averages of all features assigned to the cluster.

 #Initializing the Theta's:
 Theta_old <- matrix (1, k, d)

 for (j in 1:k){

 Theta_old[j, ] <- H[k_randomly_chosen_histograms[j], ] /
(norm(as.matrix(H[k_randomly_chosen_histograms[j] , ]), "F"))

 }

 a_old <- (1/k)*matrix(1, n, k)
 c <- 100


 while(c > t){

 #Part a) - Assignment Step:
 phi <- exp(H %*% t(log(Theta_old)) )
 a_new <- phi / rowSums(phi)
 rows_not_nan <- !is.nan(a_new)[,1]

 #Part b) - Model Adjustment Step:
 b = t(a_new[rows_not_nan,]) %*% H[rows_not_nan,]
 Theta_new <- b / (rowSums(b))


 #Part c) - Computing the measure of change of the assignments:
 c <- norm(Theta_new - Theta_old, type = "O") + norm(a_new - a_old, type = "O")
 a_old <- a_new
 Theta_old <- Theta_new


 print(c)


 }

 return(max.col(a_new))

}
########################## Part 2 #################################
# Running the algorithm for k =3, 4, and 5, respectively:
t <- .01 #Threshold parameter. This parameter sets the required minimum difference
between datapoints that move between clusters at each iteration.
#For k=3
M_k3 <- MultinomialEM(H, k=3, t)
#For k=4
M_k4 <- MultinomialEM(H, k=4, t)
#For k=5
M_k5 <- MultinomialEM(H, k=5, t)

######################### Part 3 #####################################
# Showing the Original Image:
Image_Matrix <- matrix((H %*% matrix(1:16,16,1)/121), 200, 200) #Making
the matrix 200x200, and assigning weights to each element by the position.
Image_Matrix <- Image_Matrix[ ,ncol(Image_Matrix):1] #Reversing the
image to be in the appropriate format.
image(Image_Matrix, col=grey(seq(0, 1, length = 256)),main="The Original Image")
# Please note that the display of the original image as captured in R is shown below:
# Showing the Clustered Image using the outputs of the MultinomialEM algorithm:
#For k=3
Matrix_k3 <- matrix(M_k3, 200, 200) # Converting the Vectors 40000x1 into
200x200 Matrices:
Matrix_k3 <- Matrix_k3[ ,ncol(Matrix_k3):1] #Reversing the image to be in the
appropriate format.

image(Matrix_k3, col=grey(c(0, 1, .67, .33)), main="Clustered Image w/ k=3, t=.01")
#Showing the Clustered Image with k=3, and t = .01:
image(Matrix_k3, col=grey(c(0, 1, .67, .33)), main="Clustered Image w/ k=3, t=.001")
#Showing the Clustered Image with k=3, and t=.001:

image(Matrix_k3, col=grey(c(0, 1, .67, .33)), main="Clustered Image w/ k=3, t=.0001")
#Showing the Clustered Image with k =3, and t=.0001:
#For k=4
Matrix_k4 <- matrix(M_k4, 200, 200) # Converting the Vectors 40000x1 into
200x200 Matrices:
Matrix_k4 <- Matrix_k4[ ,ncol(Matrix_k4):1] #Reversing the image to be in the
appropriate format.
image(Matrix_k4, col=grey(c(0, 1, .67, .33)), main="Clustered Image w/ k=4, t=.01")
#Showing the Clustered Image with k=4, and t= .01:

image(Matrix_k4, col=grey(c(0, 1, .67, .33)), main="Clustered Image w/ k=4, t=.001")
#Showing the Clustered Image with k=4, and t=.001:
image(Matrix_k4, col=grey(c(0, 1, .67, .33)), main="Clustered Image w/ k=4, t=.0001")
#Showing the Clustered Image with k=4, and t=.0001:

#For k=5
Matrix_k5 <- matrix(M_k5, 200, 200) # Converting the Vectors 40000x1 into
200x200 Matrices:
Matrix_k5 <- Matrix_k5[ ,ncol(Matrix_k5):1] #Reversing the image to be in the
appropriate format.
image(Matrix_k5, col=grey(c(0, 1, .67, .33)), main="Clustered Image w/ k=5, t=.01")
#Showing the Clustered Image with k=5, and t=.01:
image(Matrix_k5, col=grey(c(0, 1, .67, .33)), main="Clustered Image w/ k=5, t=.001")
#Showing the Clustered Image with k=5, and t=.001:

image(Matrix_k5, col=grey(c(0, 1, .67, .33)), main="Clustered Image w/ k=5, t=.0001")
#Showing the Clustered Image with k=5, and t=.0001:

#Please note that the M_k3, M_k4, and M_k5 were 40000x1 vectors, and a portion of
#their entries were as follows with t=.0001:
#M_k3 =
#[1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 3 3 3 3 3 1 1 1 1 1 3 3
#[32] 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 3 3 3 1 1 3 3 3 3 3 2 2 2 2 3 3
#[63] 1 1 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 1 3
#[94] 3 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 3 3
#[125] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#……………………………………….
#[40000] 1
#M_k4 =

#[1] ……………………..
#[373] 4 2 3 3 3 3 3 3 3 3 3 2 4 4 4 4 4 4 1 1 1 1 4 4 4 2 2 4 2 2 2
#[404] 2 2 2 2 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
#[435] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4
#[466] 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 4 4 4 4 4
#[497] 4 4 4 4 4 1 4 4 4 4 4 4 4 4 4 4 4 4 4 2 2 2 2 2 2 2 2 2 2 2 2
#[528] 2 2 2 2 4 2 2 2 2 2 2 4 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#……………………………………
# [40000] 1
#M_k5 =
# [1]………………………..
#[9859] 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1
#[9890] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
#[9921] 5 5 5 5 5 2 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
#[9952] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 2 2 2 2 2 2 5 5 5 2 2 1
#[9983] 3 1 4 4 4 4 4 4 4 4 4 4 4 4 4 5 2 2
#…………………………………………
#[40000] 1
#Results: As can be seen by the images on the previous pages, the number of clusters
#determines the number of classifying sections of the image that have been computed
#using the Multinomial EM algorithm. With k=4 and k=5 clusters with low threshold
#parameters (ie t=.001 or t=.0001), the distinctions within the image look rather similar to
#the original 200x200 image. As the threshold parameter decreased from .01 to .0001, the
#clustered images also changed quite remarkably, such that certain features of the image
#were distinguished. For k=5 and t=.001, the separation of the land plots have a different
#color representing a cluster that is distinguishing those features of the image. With a low
#cluster (ie k=3), the image is characteristic of the original image; however, many features
#aren’t as pronounced as in the images which are outputted using a larger number of
#clusters in the Multinomial EM algorithm.