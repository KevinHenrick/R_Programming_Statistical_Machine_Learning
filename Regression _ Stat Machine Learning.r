
#Problem 1) - R Coding - Regression Problem with Basis Functions
#Homework 3 - Kevin Henrick - kph2115@columbia.edu
#Question 1-
#Part 1: Generating testing data x, and values for a sinc function on x:
n<-25
x<- matrix(runif(n, 0, 1), n, 1)
epsilon<- matrix(rnorm(n, 0, .1), n, 1)
f_x <- (sin(3*x))/(3*x)
y <- f_x + epsilon
#Part 2: Programming the Regression Estimator:
regress_gauss <- function(x, y, means, var, Lambda)
{
 n <- length(x)
 d <- length(means)
 h_x <- matrix(1,n,(d+1))
 H_x <- Compute_h_x(n,d,x,means,var)

 Beta <- solve(t(H_x)%*%H_x + Lambda*(diag(1,(d+1)))) %*% t(H_x) %*% y

 return(Beta)
}
#Computing the transformation of the x vector to the matrix H_x.
Compute_h_x <- function (n, d, x, means, var)
{
 h_x<- matrix(1, n, (d+1))
 for (j in 1:n)
 {
 h_x[j, 2:(d+1)] <- exp(-((x[j] - means)^2)/ (2*var^2))
 }
 return (h_x)
}

#Part 3: Applying the estimator on the generated data:
d<-21
var <- .04*c(rep(1,d))
means <- c(0:(d-1))/(d-1)
#2

Lambda <- c(.001, .05, 5)
number_of_experiments <- 100
Beta_1 <- matrix(0, d+1, number_of_experiments)
Beta_2 <- matrix(0, d+1, number_of_experiments)
Beta_3 <- matrix(0, d+1, number_of_experiments)
Regression_Runs <- function(number_of_experiments, n, d, means, var, Lambda)

{
 for(j in 1:number_of_experiments){
 x <- runif(n, 0, 1)
 epsilon <- rnorm(n, mean=0, sd=0.1)
 f_x <- (sin(3*x))/(3*x)
 y <- f_x + epsilon


 Beta_1[ ,j] <- regress_gauss(x, y, means, var, Lambda[1])
 Beta_2[ ,j] <- regress_gauss(x, y, means, var, Lambda[2])
 Beta_3[ ,j] <- regress_gauss(x, y, means, var, Lambda[3])

 if(j==1){
 x_1<-x
 y_1<-y
 }

 }
 Beta <- list(Beta_1, Beta_2, Beta_3, x_1, y_1)
 return(Beta)

}
Output_of_Betas <- Regression_Runs(number_of_experiments, n, d, means, var, Lambda)
Beta_1 <- matrix(unlist(Output_of_Betas[1]), ncol=number_of_experiments)
Beta_2 <- matrix(unlist(Output_of_Betas[2]), ncol=number_of_experiments)
Beta_3 <- matrix(unlist(Output_of_Betas[3]), ncol=number_of_experiments)
x_1 <- matrix(unlist(Output_of_Betas[4]))
y_1 <- matrix(unlist(Output_of_Betas[5]))
Average_Beta_1 <- rowMeans(Beta_1)
Average_Beta_2 <- rowMeans(Beta_2)
Average_Beta_3 <- rowMeans(Beta_3)



#3
## Plotting the result of the regression experiment:
x_plot <- c(0:(number_of_experiments*.5))/(number_of_experiments*.5)
h_x <- Compute_h_x(length(x_plot), d, x_plot, means, var)
#Computing the f_x_hat values:
f_x_hat_1 <- h_x %*% Beta_1[,1]
f_x_hat_2 <- h_x %*% Beta_2[,1]
f_x_hat_3 <- h_x %*% Beta_3[,1]
#Plotting the x_1 vs. y_1 values with various Lambdas for the Regression.
plot(x_1, y_1, col="blue", type="p", main="Regression Results with Various Lambdas")
points(x_plot, f_x_hat_1, col="red", type= "l", lty=1)
points(x_plot, f_x_hat_2, col="green", type= "l", lty=1)
points(x_plot, f_x_hat_3, col="black", type= "l", lty=1)
legend(.6, 1.1, c("Training Data", bquote(Lambda_1),bquote(Lambda_2),
bquote(Lambda_3)), col=c("blue","red", "green", "black"), lty=c(1,1,1,1), cex=.6);


#Please find the outputted plots for the various Lambda's as follows:
#Regression Results (red line) with Lambda = .001, the f_x_hat values can be found in
#Blue.
#Regression Results (green line) with Lambda = .05, the f_x_hat values can be found in
#Blue.
#4
#Regression Results (black line) with Lambda = 5, the f_x_hat values can be found in
#Blue.
#5
#The following graph shows the overall Regression Results for all of the different
#Lambdas (.001, .05, .5). Please note that the legend distinguishes each:
#As can be seen in the plot above, the largest Lambda = 5 (represented by the black
#regression line), gave a far-off estimate for the Training Data (x_1 vs. y_1) initially, but
#gave a smoother line, while a smaller Lambda (represented by the green regression line)
#fits more of the data points. The lowest Lambda = .001 (represented by the red regression
#line) seems to have overfit the data, such that there are large swings. In the higher x_1
#values, the lower Lambda regression lines do tend to become smoother as the data tends
#to crunch together from the sinc function becoming smaller and smaller.
#6
#Now plotting the Average Beta Regression Functions for each Lambda as follows:
#Regression Results (red line) with Lambda = .001, and Averaging the Betas. The actual
#sinc function (f_x_plot) is represented by the smooth blue line.
#Regression Results (green line) with Lambda = .05, and Averaging the Betas. The actual
#sinc function (f_x_plot) is represented by the smooth blue line.
#7
#Regression Results (black line) with Lambda = 5, and Averaging the Betas. The actual

#sinc function (f_x_plot) is represented by the smooth blue line.
#Combining the graphs, we obtain the following output:
#As can be seen by the resulting plot, the best estimator using average Betas appears to be
#the Lambda = .05 (represented by the green line), since it follows the actual function
#(represented by the blue line) so closely. The worst regression estimator using average
#Betas appears to be the line with the largest Lambda=5 (represented by the black line).
#The smallest Lambda=.001 (represented by the regression line in red) oscillates about the 
#8
#actual function quite a bit. Please also note that at the two ends of the plot, the regression
#estimators all perform poorly. This occurs because the error function increases at the tails
#of the graph.
#The code that generated the Average Betas as well as their plots can be found below:


#Now plotting the regression using the Average Betas:

f_x_hat_1new = h_x %*% Average_Beta_1
f_x_hat_2new = h_x %*% Average_Beta_2
f_x_hat_3new = h_x %*% Average_Beta_3
x_plot <- c(1:(number_of_experiments))/(number_of_experiments)
f_x_plot <- sin(3*x_plot)/(3*x_plot)
plot(x_plot, f_x_plot,col="blue",type="l", main="Regression Estimated by Averaging the
Beta")
points(x_plot, f_x_hat_1new, col= "red", type="l",lty=1)
points(x_plot, f_x_hat_2new, col= "green", type="l",lty=1)
points(x_plot, f_x_hat_3new, col= "black", type="l",lty=1)









#Part 4 - Computing the estimation error.
Lambda <- exp(seq(-5, 2, 0.3))
z <- seq(0.01, 1.01, (1/number_of_experiments))
f_z <- sin(3*z)/(3*z)
H_x <- Compute_h_x(length(z), d, z, means, var)
Estimation_Error <- c()
for(i in 1: length(Lambda)){

 Output_of_Beta <- Regression_Runs(number_of_experiments, n, d, means, var,
Lambda[i])

 Beta_1 <- matrix(unlist(Output_of_Beta[1]), ncol=number_of_experiments)

 Beta_Average <- rowMeans(Beta_1)

 f_hat <- H_x %*% Beta_Average


 Estimation_Error[i] = (1/number_of_experiments)*sum((f_z - f_hat)^2)

}
plot(Lambda, Estimation_Error, main="Estimation Error Vs. Lambda")


#9
# Please note that the above Regression_Runs function was modified and then reran as
#follows, before being called in the for loop to calculate the Estimation Error:

Regression_Runs <- function(number_of_experiments, n, d, means, var, Lambda)

{
 for(j in 1:number_of_experiments){
 x <- runif(n, 0, 1)
 epsilon <- rnorm(n, mean=0, sd=0.1)
 f_x <- (sin(3*x))/(3*x)
 y <- f_x + epsilon


 Beta_1[ ,j] <- regress_gauss(x, y, means, var, Lambda[1])
 Beta <- list(Beta_1)
 }
 return(Beta)

}
# The plot of the Estimation Error with respect to Lambda can be found below:
#10
#For the graph on the previous page, please note that there is a cluster of lower Error
#Estimates for lower values of Lambda, while for higher values of Lambda, the Error
#Estimates tend to become much larger. Therefore, at either end of the graph, on the left
#there exists a cluster of data points representing small values of Lambda with small
#corresponding Error Estimates. On the right side of the graph, there are a few scattered
#data points that become increasingly larger in the Error Estimate axis as Lambda
#increases. Therefore, very large Lambda corresponds to a higher Error Estimate, while
#smaller Lambdas correspond to a lower Error Estimate.









