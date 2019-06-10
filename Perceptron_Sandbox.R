# Declare vector in R² ----------------------------------------------------
h = vector(length = 2)
View(h)

# Generate random vector --------------------------------------------------
z=runif(4)
View(z)

# Test for-loop -----------------------------------------------------------
View(iris_sub)
View(iris_sub)
for (i in 1:nrow(iris_sub)) { 
  print(i)
  # print all col for each row i
  print(c(iris_sub[i,1],iris_sub[i,2],iris_sub[i,3]))
  #i++
  i<-i+1
}

# test apply function over rows (1)
z=cbind(8,-10,4)
dim(z)
print(z)
sum_z <- apply(z,1,sum)
print(sum_z)

# test apply function over rows (1)
# Rq: a matrix is just a vector with a dim attribute
vec_h = c(8,-10,4,12,8,14)


# Vector to Matrix --------------------------------------------------------
#transform vector to matrix of dimension 2 by 3, filling = top to bottom & left to right
dim(vec_h) <- c(2, 3)
dim(vec_h)
print(vec_h)


# Sum over cols -----------------------------------------------------------
sum_col_h <- apply(vec_h,2,sum)
print(sum_col_h)

# Sum over rows -----------------------------------------------------------
sum_row_h <- apply(vec_h,1,sum)
print(sum_row_h)

# Extract maximum row sum -------------------------------------------------
sum_row_max=max(apply(vec_h, 1, sum))
print(sum_row_max)


# Test abs function on matrix ---------------------------------------------

#normalize every cell of given matrix
#apply abs to each entry
abs = function(x) {sqrt(sum(x * x))}

my.matrx <- matrix(c(1:5, 6:10, -5:-1), nrow = 5, ncol = 3)
my.matrx
norm.my.matrx <- sapply(my.matrx, abs)
norm.my.matrx
#get back correct dimension
dim(norm.my.matrx) <- dim(my.matrx)
norm.my.matrx


# Test guess function -----------------------------------------------------
guess = function(x,w,b){
  # 1) compute output given inputs, weights and bias & normalize result
  output = sum(x*w) + b/abs(sum(x*w) + b)
  return(ifelse(output < 0, -1, +1)) # apply activation function (-1 if output<0; 1 if output >0)
}

x = cbind(iris_sub$sepal, iris_sub$petal)
w = runif(ncol(x))
b=1
print(x)
print(w)
print(x[1,])
print(x[1,]*w)
print(sum(x[1,]*w)+b)

current_guess=guess(x[1,],w,b)
print(current_guess)


# Test weight adjusting ---------------------------------------------------
w_vec = runif(ncol(x))
print (w_vec)
print(x[1,])
w_vec = w_vec + x[1,]
print (w_vec)
