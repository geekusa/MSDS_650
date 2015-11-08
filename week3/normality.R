setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS650/week3/")
library(ggplot2)
library(reshape2)
#create function
#' @name assign_vector
#' @param data A vector of data to perform the t-test on.
#' @param n An integer indicating the number of t-tests to perform. Default is 1000
#' @return A data frame in "tall" format
assign_vector <- function(data, n = 1000) {
  # replicate the call to shapiro.test n times to build up a vector of p-values
  p.5 <- replicate(n=n, expr=shapiro.test(sample(my.data, 5, replace=TRUE))$p.value)
  p.10 <- replicate(n=n, expr=shapiro.test(sample(my.data, 10, replace=TRUE))$p.value)
  p.1000 <- replicate(n=n, expr=shapiro.test(sample(my.data, 1000, replace=TRUE))$p.value)
  #' Combine the data into a data frame, 
  #' one column for each number of samples tested.
  p.df <- cbind(p.5, p.10, p.1000)
  p.df <- as.data.frame(p.df)
  colnames(p.df) <- c("5 samples","10 samples","1000 samples")
  #' Put the data in "tall" format, one column for number of samples
  #' and one column for the p-value.
  p.df.m <- melt(p.df)
  #' Make sure the levels are sorted correctly.
  p.df.m <- transform(p.df.m, variable = factor(variable, levels = c("5 samples","10 samples","1000 samples")))
  return(p.df.m)  
}

#corrected function
#create function
#' @name assign_vector
#' @param data A vector of data to perform the t-test on.
#' @param n An integer indicating the number of t-tests to perform. Default is 1000
#' @return A data frame in "tall" format
assign_vector <- function(data, n = 1000) {
  p.5 <- replicate(n=n, expr=shapiro.test(sample(data, 5, replace=TRUE))$p.value)
  p.10 <- replicate(n=n, expr=shapiro.test(sample(data, 10, replace=TRUE))$p.value)
  p.1000 <- replicate(n=n, expr=shapiro.test(sample(data, 1000, replace=TRUE))$p.value)
  p.df <- cbind(p.5, p.10, p.1000)
  p.df <- as.data.frame(p.df)
  colnames(p.df) <- c("5 samples","10 samples","1000 samples")
  p.df.m <- melt(p.df)
  p.df.m <- transform(p.df.m, variable = factor(variable, levels = c("5 samples","10 samples","1000 samples")))
  return(p.df.m)  
}


#run test
n.rand <- 100000
n.test <- 10000
my.data <- rnorm(n.rand)
p.df.m <- assign_vector(my.data, n = n.test)

#plot normal distribution
ggplot(p.df.m, aes(x = value)) + 
  geom_histogram(binwidth = 1/10) + 
  facet_grid(facets=variable ~ ., scales="free_y") + 
  xlim(0,1) +
  ylab("Count of p-values") +
  xlab("p-values") +
  theme(text = element_text(size = 16))

#plot t-distribution
ggplot(NULL, aes(x=x, colour = distribution)) + 
  stat_function(fun=dnorm, data = data.frame(x = c(-6,6), distribution = factor(1))) + 
  stat_function(fun=dt, args = list( df = 20), data = data.frame(x = c(-6,6), distribution = factor(2)), linetype = "dashed") + 
  scale_colour_manual(values = c("blue","red"), labels = c("Normal","T-Distribution"))

my.data <- rt(n.rand, df = 20)
p.df.m <- assign_vector(my.data, n = n.test)
ggplot(p.df.m, aes(x = value)) + 
  +   geom_histogram(binwidth = 1/10) + 
  +   facet_grid(facets=variable ~ ., scales="free_y") + 
  +   xlim(0,1) +
  +   ylab("Count of p-values") +
  +   xlab("p-values") +
  +   theme(text = element_text(size = 16))

pAbove05 <- p.df.m[p.df.m$variable=="1000 samples" & p.df.m$value<0.05,]
sum(pAbove05$value)
length(p.df.m[p.df.m$variable=="1000 samples" & p.df.m$value>0.05,"value"])

#test the tails
my.data <- rt(n.rand, df = 20)
my.data.2 <- rnorm(n.rand)
# Trim off the tails
my.data <- my.data[which(my.data < 3 & my.data > -3)]
# Add in tails from the other distribution
my.data <- c(my.data, my.data.2[which(my.data.2 < -3 | my.data.2 > 3)])
p.df.m <- assign_vector(my.data, n = n.test)
ggplot(p.df.m, aes(x = value)) + 
  +   geom_histogram(binwidth = 1/10) + 
  +   facet_grid(facets=variable ~ ., scales="free_y") + 
  +   xlim(0,1) +
  +   ylab("Count of p-values") +
  +   xlab("p-values") +
  +   theme(text = element_text(size = 16))
