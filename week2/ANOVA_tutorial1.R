setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS650/week2/")
df1 = read.table("fastfood-1.txt", header=TRUE); df1
r = c(t(df1)); r
f = c("Item1", "Item2", "Item3") 
k = 3
n = 6
tm = gl(k, 1, n*k, factor(f)); tm
av = aov(r ~ tm)
summary(av)
#vertical
rAsC = cbind(c(t(df1))); rAsC
f = c("Item1", "Item2", "Item3") 
k = 3
n = 6
tm = gl(k, 1, n*k, factor(f)); tm
av = aov(rAsC ~ tm)
summary(av)

#Randomized
df2 = read.table("fastfood-2.txt", header=TRUE); df2
r = c(t(df2)); r
f = c("Item1", "Item2", "Item3")
k = 3        
n = 6
tm = gl(k, 1, n*k, factor(f)); tm
blk = gl(n, k, k*n); blk
av = aov(r ~ tm + blk)
summary(av)
#vertical
rAsC = cbind(c(t(df2))); rAsC
f = c("Item1", "Item2", "Item3")
k = 3        
n = 6
tm = gl(k, 1, n*k, factor(f)); tm
blk = gl(n, k, k*n); blk
av = aov(rAsC ~ tm + blk)
summary(av)

#Factorial Design
df3 = read.csv("fastfood-3.csv")
r = c(t(df3))
r
f1 = c("Item1", "Item2", "Item3")
f2 = c("East", "West")  
k1 = length(f1)
k2 = length(f2)
n = 4
tm1 = gl(k1, 1, n*k1*k2, factor(f1)) 
tm1
tm2 = gl(k2, n*k1, n*k1*k2, factor(f2)) 
tm2
av = aov(r ~ tm1 * tm2)
summary(av)
#column excercise
rAsC = cbind(c(t(df3)))
rAsC
f1 = c("Item1", "Item2", "Item3")
f2 = c("East", "West")  
k1 = length(f1)
k2 = length(f2)
n = 4
tm1 = gl(k1, 1, n*k1*k2, factor(f1)) 
tm1
tm2 = gl(k2, n*k1, n*k1*k2, factor(f2)) 
tm2
av = aov(rAsC ~ tm1 * tm2)
summary(av)