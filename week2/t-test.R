setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS650/week2/")

#before and after results
before = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
after = c(12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1)
#run the test
t.test(before,after, paired=TRUE)
#t-tabulated value
qt(0.975, 9)
#new coach
before = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
after = c(12.0, 12.2, 11.2, 13.0, 15.0, 15.8, 12.2, 13.4, 12.9, 11.0)
#test alternative hypothesis
t.test(before,after, paired=TRUE, alt="l")
#show alternative hypoth before is greater that after
t.test(before,after, paired=TRUE, alt="g")
