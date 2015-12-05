setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS650/week7/")
passing <- read.csv("nflpassing_names.csv",sep = '\t')
playernames <- passing[,1]
passing <- passing[,-1]
rownames(passing) <- playernames
#remove team and postition
passing <- passing[,-2]
passing <- passing[,-2]
head(passing)
km <-kmeans(passing[,1:17], 3)
plot(passing[,17], passing[,6], col=km$cluster, xlab = 'rating',ylab = 'yards')
plot(passing[,17], passing[,4], col=km$cluster, xlab = 'rating',ylab = 'percentage')
plot(passing[,17], passing[,1], col=km$cluster, xlab = 'rating',ylab = 'ranking')
plot(passing[,17], passing[,3], col=km$cluster, xlab = 'rating',ylab = 'attempts')
plot(passing[,17], passing[,2], col=km$cluster, xlab = 'rating',ylab = 'completions')
plot(passing[,17], passing[,8], col=km$cluster, xlab = 'rating',ylab = 'yards per game')
plot(passing[,17], passing[,9], col=km$cluster, xlab = 'rating',ylab = 'touchdowns')
plot(passing[,17], passing[,10], col=km$cluster, xlab = 'rating',ylab = 'int')
plot(passing[,17], passing[,16], col=km$cluster, xlab = 'rating',ylab = 'sacks')
