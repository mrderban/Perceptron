
######################################################################
####    Chapter 4 - Introduction to Neural Networks - using R    #####
### Simple Perceptron implementation function in R - iris dataset  ###
######################################################################

######################################################################
##########                      STEPS                       ##########
######################################################################

# 1) Provide the perceptron with inputs for which there is a known answer (training)
# 2) Ask the perceptron to guess an answer.
# 3) Compute the error (did it get the answer right or wrong ?)
# 4) Adjust all the weights according to the error
# 5) Return to step 1 and repeat

######################################################################
##########                      CODE                        ##########
######################################################################

# Libraries ---------------------------------------------------------------
require(ggplot2)
require(readr)


# Data loading & preview ---------------------------------------------------------------

data(iris)
View(iris)
head(iris, n=100)

# Data Selection ---------------------------------------------------------------

# only pick rows w/ setosa and versicolor species and only cols 1, 3 and 5
iris_sub=iris[1:100, c(1, 3, 5)]

# define names for the sub data set
names(iris_sub)=c("sepal", "petal", "species")
#View(names(iris_sub))
head(iris_sub)

# Visualize Data ----------------------------------------------------------

# scatter plot
ggplot(iris_sub, aes(x = sepal, y = petal)) +
  geom_point(aes(colour=species, shape=species), size = 3) +
  xlab("Sepal length") +
  ylab("Petal length") +
  ggtitle("Species vs Sepal and Petal lengths")


# Prep training data (Randomize) --------------------------------------------------------
index = sample(1:nrow(iris_sub),round(0.70*nrow(iris_sub)))
train_data <- as.data.frame(iris_sub[index,])

# Prep Test data ----------------------------------------------------------
test_data <- as.data.frame(iris_sub[-index,])


# Training data inputs -----------------------------------------------------------
# inputs
x = cbind(train_data$sepal, train_data$petal)
View(x)

# labels. The function 'ifelse' returns a vector of the same length and attributes as iris_sub$species
y = ifelse(train_data$species == "setosa", +1, -1)
View(train_data$species)
View(y)

# Visualize Training Data ----------------------------------------------------------
# scatter plot
ggplot(train_data, aes(x = sepal, y = petal)) +
  geom_point(aes(colour=species, shape=species), size = 3) +
  xlab("Sepal length") +
  ylab("Petal length") +
  ggtitle("Species vs Sepal and Petal lengths")

# Helper functions --------------------------------------------------------

# compute absolute value
abs = function(x) {sqrt(sum(x * x))}

# guess species given output
guess = function(x,w,b){
  # 1) compute output given inputs, weights and bias & normalize result
  output = sum(x*w) + b/abs(sum(x*w) + b)
  return(ifelse(output < 0, -1, +1)) # apply activation function (-1 if output<0; 1 if output >0)
}

# Perceptron function -----------------------------------------------------

# the objective is to converge towards an optimal weight vector, in order to get the best prediction possible

perceptron = function(x, y, learning.rate=0.05) {
  w = runif(ncol(x)) # initialize weights vector (vector in R^(number of inputs)) w/ random values
  b = 1 # initialize bias
  k = 0 # initialize iteration counter
  
  #declare data frame to append durinf for loop
  weight_iter <- data.frame(weight_1=numeric(), weight_2=numeric(),iteration=numeric()) 
  
    for (i in 1:nrow(x)) {
      # debug:
      print (w)
      print (b)
      print (k)
      
      # compute output and guess
      current_guess = guess(x[i,],w,b)
      
      # if guess is wrong
      if (y[i] != current_guess) {
        error=(y[i]-current_guess)  # compute error (NOT loss function)
        #can only be 0, -2, or 2
        #ex1: if current_guess = -1 & y[i] = 1 then weights values are too low => positive error
        #ex2: if current_guess = 1 & y[i] = -1 then weights values are too high => negative error
        w = w + learning.rate * error * x[i,] # adjust weight vector w with given error
        b = b + learning.rate * error # adjust bias b given error
      }
      k = k+1 # increment iteration counter
      
      # vector to be added
      vec <- c(w[1],w[2],k)
      
      # change current wd
      setwd("C:/Users/sbenard/Desktop/Perceptron/data")
      
      # write vector to rds file 
      saveRDS(vec, paste0("weights_iter", as.numeric(Sys.time())*1000,".rds"))
      
    }

  # return updated weights (supposed to be optimals), updated bias and updated number of iterations
  return(list(w,b,k))
}

# Perceptron p Training -------------------------------------------------------

p <- perceptron(x,y,0.5)


# Extract Trained perceptron parameters -----------------------------------

p$w=(p[[1]])
print(p$w)
print(p$w[1])
print(p$w[2])
p$b=(p[[2]])
print(p$b)

#  Plotting the linear separation line -----------------------------------------------

plot(x,cex=0.2)
points(subset(x,y==1),col="black",pch="+",cex=2)
points(subset(x,y==-1),col="red",pch="*",cex=2)
intercept <- -p$b[1] / p$w[2]
slope <- -p$w[1] / p$w[2]
abline(intercept,slope,col="green")

