library(ggplot2)
install.packages("patchwork")
library(patchwork)

# Prac2: Creating a custom lowess function 
##### 1. Generate Simulated Data #####
set.seed(1) #Set your seed to 1

# Create X as a sequence of numbers from 1 to 100 
x_vec <- numeric(100) #allocate space
for (i in 1:100){
  x_vec[i] <- i
}

e_vec <- rnorm(100, mean = 0, sd = 0.2) #Generates 100 error terms

# Generate Y as a noisy sine wave according to equation
y_vec <- numeric(100) #allocate space
for (i in 1:100){
  
  y_vec[i]<- sin(x_vec[i]/10) + e_vec[i] #equation given
}

##### 2. Implement the LOWESS algorithm #####
customLowess <- function(x, y, f){
  # Data (will attempt just using indexing)
  n <- length(x)
  # Span
  k <- f*n
  
  # Smoothed weights
  returnable <- numeric(n)
  for(i in 1:n){
    # Compute weights
    distances <- abs(x-x[i]) #get distances of neighbours
    neighbours_ordered <- order(distances)[1:k] #stores k closest neighbours (as indexes)
    d_max <- distances[neighbours_ordered[k]] #furthest = last in ordered vector
    weights <- (1-(distances[neighbours_ordered]/d_max)^3)^3 #Tricube
    
    #Weighted Regression
    neighbour_x <- x[neighbours_ordered]
    neighbour_y <- y[neighbours_ordered]
    weight_diag <- diag(weights) #makes a diagonal matrix of the weights
    design_matrix <- cbind(1, neighbour_x)
    beta <- solve(t(design_matrix)%*%weight_diag%*%design_matrix)%*%t(design_matrix)%*%weight_diag%*%neighbour_y
    
    #Smoothed value
    smoothed_y <- beta[1]+beta[2]*x[i]
    returnable[i] <- smoothed_y
  }
  
  return(returnable)
}

smoothed <- numeric(100)
smoothed <- customLowess(x_vec, y_vec, 0.1)

built_in_smooth <- lowess(x_vec, y_vec, 0.1, 0)$y #makes it the smoothed y-values as it returns both x and y by default

#Plotting
plot(x_vec, y_vec, col = "gray", main = "LOWESS Custom Function Smoothing") + lines(x_vec, smoothed, col = "red", lwd = 2)
lines(x_vec, smoothed, col = "red", lwd = 2)
lines(lowess(x_vec, y_vec, 0.1, 0), col ="green", lwd = 2)


##### Plotting for the QMD #####
#Use ggplot2 because we need to learn
?ggplot()
#Plot CUSTOM
custom_plot <- ggplot(data.frame(x_vec = x_vec, y_vec = y_vec, smoothed = smoothed), aes(x = x_vec, y = y_vec)) + geom_point(color = "gray") + geom_line(aes(y = smoothed), color = "red", lwd = 1) + ggtitle("Custom Function")

#Plot BUILT-IN
built_in_plot <- ggplot(data.frame(x_vec = x_vec, y_vec = y_vec, built_in_smooth = built_in_smooth), aes(x = x_vec, y = y_vec)) + geom_point(color = "gray") + geom_line(aes(y = built_in_smooth), color = "green", lwd = 1) + ggtitle("Built-in Function")

# custom_plot
# built_in_plot

custom_plot + built_in_plot

##### Testing work #####
#test_dist <- abs(x_vec - x_vec[2]) #you can make a new vector by subtracting scalar from vector
#ordering <- order(test_dist)[1:5] #orders then only stores the first n values
#dm <- ordering[5]
#weightsof <- (1-(test_dist[ordering]/dm)^3)^3
#print(weightsof)


