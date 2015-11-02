setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS650/week2/")
agents_online= read.csv("agents0622-1026.csv")

#count by 2 groups
counts <- ddply(agents_online, .(agents_online$email, agents_online$status), nrow)
names(counts) <- c("email", "status", "Freq")
head(counts)

#anonymize function - pulled from http://www.r-bloggers.com/anonymising-data/
anonymize <- function(data, cols_to_anon, algo = "sha256")
{
  if(!require(digest)) stop("digest package is required")
  to_anon <- subset(data, select = cols_to_anon)
  unname(apply(to_anon, 1, digest, algo = algo))
}
agents_online$email <- anonymize(agents_online, "email")
summary(agents_online)

barplot(table(agents_online$date_hour), col="blue")
table(agents_online$date_hour)

daysDF <- data.frame(rbind(table(agents_online$date_wday)))
reorder_daysDF <- daysDF[,c(2,6,7,5,1,3,4)]
library(reshape2)
reorder_daysDF.molten <- melt(reorder_daysDF, value.name="Count", variable.name="DayOfWeek", na.rm=TRUE)
library(ggplot2)
ggplot(reorder_daysDF.molten, aes(x=DayOfWeek, y=Count)) + geom_bar(stat="identity", fill="#33CC33")

#total events
length(agents_online$status)
#total successful and failed logins
table(agents_online$status)
#total count of unique login ids that have logged in successfully
length(unique(agents_online[agents_online$status==200,"email"]))
#summary hours and days successful logins
agents_online_success <- agents_online[agents_online$status==200,]
summary(agents_online_success$date_hour)
summary(agents_online_success$date_wday)
#high and low count of successful logins by user
tail(sort(table(agents_online_success$email)))
successCounts <- as.data.frame(table(agents_online_success$email))
table(successCounts$Freq)
#topuser's failed login count
topuser <- agents_online_fail[which(agents_online_fail$email==
  "eba108585da5161006a186ca04755736de71ea84b6382b8cb764d5b88b3d3bbb"),]
length(topuser$status)

#summary hours failed logins
agents_online_fail <- agents_online[agents_online$status==401,]
summary(agents_online_fail$date_hour)
summary(agents_online_fail$date_wday)

#hoursVsStatus
hoursVsStatus <- table(agents_online$date_hour,agents_online$status, dnn=c("Success"))
hoursVsStatus
hoursVsStatusDF <- data.frame(hoursVsStatus)
names(hoursVsStatusDF) <- c("Hour", "Status", "Freq")
#hoursVsStatusPlot <- qplot(factor(hoursVsStatusDF$status), x=hoursVsStatusDF$Hour, y=hoursVsStatusDF$Freq, 
#      data=hoursVsStatusDF, geom="bar", fill=factor(hoursVsStatusDF$Status), 
#      stat="identity") 
hoursVsStatusPlot <- ggplot(hoursVsStatusDF, aes(x=Hour, y=Freq, 
      fill=factor(hoursVsStatusDF$Status))) + geom_bar(stat="identity")
hoursVsStatusPlot + scale_fill_manual(values = c("200" = "#83CAFF", "401" = "#FF90B5"), 
      name = 'Status', labels=c("Success","Fail")) + ylab('Frequency') + 
      xlab('HourOfDay') + theme(legend.position = 'top')
#grid
hoursVsStatusPlot + scale_fill_manual(values = c("200" = "#83CAFF", "401" = "#FF90B5"), 
  name = 'Status', labels=c("Success","Fail")) + ylab('Frequency') + 
  xlab('HourOfDay') + theme(legend.position = 'top')

#daysVsStatus
daysVsStatus <- table(agents_online$date_wday,agents_online$status, dnn=c("Success"))
daysVsStatus
daysVsStatusDF <- data.frame(daysVsStatus)
names(daysVsStatusDF) <- c("Day", "Status", "Freq")
daysVsStatusDF$Day2 <- factor(daysVsStatusDF$Day, levels = c(2,9,6,13,7,14,5,12,1,8,3,10,4,11))
daysVsStatusPlot <- ggplot(daysVsStatusDF, aes(daysVsStatusDF$Day, y=Freq, 
  fill=factor(daysVsStatusDF$Status))) + geom_bar(stat="identity")
daysVsStatusPlot + scale_fill_manual(values = c("200" = "#83CAFF", "401" = "#FF90B5"), 
  name = 'Status', labels=c("Success","Fail")) + ylab('Frequency') +   xlab('HourOfDay') +
  scale_x_discrete(limits=c("monday","tuesday","wednesday","thursday","friday",
  "saturday","sunday")) + theme(legend.position = 'top')

#grid
daysHoursVsStatus <- table(agents_online$date_wday,agents_online$date_hour,
                                 agents_online$status)
daysHoursVsStatusDF <- data.frame(daysHoursVsStatus)
daysHoursVsStatusPlot <- ggplot(daysHoursVsStatusDF, aes(x=date_hour, y=Freq, 
    fill=factor(daysHoursVsStatusDF$Status))) + geom_bar(stat="identity") + 
  facet_grid(daysHoursVsStatusDF$date_wday)

#map latitude and longitude
require(ggplot2)
require(ggmap)
sumLon <- cbind(summary(agents_online_success$geoip_longitude))
sumLat <- cbind(summary(agents_online_success$geoip_latitude))
mapUS <- get_map(location = c(lon = sumLon[4], lat = sumLat[4]), zoom = 4, 
                 maptype = "terrain", scale = 2)
ggmap(mapUS) +
  geom_point(data = agents_online_success, aes(x = geoip_longitude, y = geoip_latitude, fill = "red", alpha = 0.8), size = 3, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

#Find most common
agents_online_success_LonLat <- data.frame(table(agents_online_success$geoip_latitude, 
   agents_online_success$geoip_longitude))
agents_online_success$latLon <- paste(agents_online_success$geoip_latitude,
  agents_online_success$geoip_longitude, sep = ' ')
successLonLat <- data.frame(sort(table(agents_online_success$latLon)))

library(dplyr)
agents_online_success_LonLat <- agents_online_success %>% 
    group_by(agents_online_success$geoip_latitude, agents_online_success$geoip_longitude) %>% 
    tally()
