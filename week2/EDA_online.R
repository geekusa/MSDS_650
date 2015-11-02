setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS650/week2/")
online= read.csv("online0623-1028.csv")

#anonymize function - pulled from http://www.r-bloggers.com/anonymising-data/
anonymize <- function(data, cols_to_anon, algo = "sha256")
{
  if(!require(digest)) stop("digest package is required")
  to_anon <- subset(data, select = cols_to_anon)
  unname(apply(to_anon, 1, digest, algo = algo))
}
online$login <- anonymize(online, "login")
summary(online)

#total events
length(online$status)
#total successful and failed logins
table(online$status)
#total count of unique login ids that have logged in successfully
length(unique(online$login))
#summary hours and days successful logins
cbind(summary(online$date_hour))
cbind(summary(online$date_wday))

#hours
hours <- data.frame(table(online$date_hour))
names(hours) <- c("Hour", "Freq")
hoursPlot <- ggplot(hours, aes(x=Hour, y=Freq)) + geom_bar(stat="identity", fill="#33CC33")
hoursPlot + ylab('Frequency') + 
  xlab('HourOfDay')
