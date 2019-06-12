# change current working directory
setwd("C:/Users/sbenard/Desktop/Perceptron/data")

# merge all rds files in current working directory
mergedata <- do.call('rbind', lapply(list.files(full.names = TRUE), readRDS))
mergedata
class(mergedata)

# convert mergedata matrix to dataframe
df<-data.frame(mergedata)
df
class(df)

# set names to data frame
df <- setNames(df,c('weight_1', 'weight_2', 'iteration_number'))
View(df)

#  Plot data (-w1/w2) as a function of iteration number -------------------
# extract all iterations (x axis)
iterationNumber <- mergedata[,3]

# compute -w1 / w2 (slope of linear separation line)
slope <- apply(mergedata, 1, function(x) -x[1]/x[2])
slope

# we will make y the response variable and x the predictor
# the response variable is usually on the y-axis

# curve fitting with first order
plot(iterationNumber,slope,pch=19)
fit  <- lm(slope~iterationNumber)
summary(fit)
abline(fit,col="green")
lines(iterationNumber,slope,col="red")

# polynomial approximations
fit3 <- lm(slope ~ poly(iterationNumber,degree=3))
summary(fit3)
fit4 <- lm(slope ~ poly(iterationNumber,degree=4))
summary(fit4)
fit5 <- lm(slope ~ poly(iterationNumber,degree=5))
summary(fit5)

#plot test
df.car_spec_data <- mergedata
df.car_spec_data %>% group_by(iterationNumber) %>%
  summarise(maxiter=max(weight_1, na.rm=T)) %>%  
  ggplot(aes(x=iterationNumber, y=maxiter, group=1)) + 
  geom_point(color='red',   alpha=0.3,size=3)+
  stat_smooth(method='lm', formula = y~poly(x,2))
