# ISAT Lab Exercise 4

myfile <-  file.choose()
myfile
mms.bags <- read.csv(myfile,header=TRUE)
summary(mms.bags)

## sample size as the row number in the dimension
n <- dim(mms.bags)[1]
n
head(mms.bags)
x <- c(51,51,50,49,51.92,51,51,48,51,48,49,50,50,50,49,49,48,50.3,50,50,50,48,49,50,51,49,50,50,52,48,51,50,49,48,52,51,52,48)
mean.result <- mean(x)
mean.result
variance.result <- var(x)
variance.result
sd.result <- sqrt(var(x))
sd.result
hist(mms.bags$weight, main = "PDF of ISAT 251 Student's M&M's Weight", xlab = "M&M's Weight (gm)", ylab = "Probability", freq = F, col = "purple")
se <- sd(mms.bags$weight,na.rm=TRUE) / sqrt(length(mms.bags$weight)) 
se
mms.df <- n-1
t_90 <- qt(p=.95, df = mms.df)
t_95 <- qt(p=.975, df = mms.df)
t_99 <- qt(p=.995, df = mms.df)
t_90
t_95
t_99
mms.df

ME90 <- (t_90 * se)
ME90
ME95 <- (t_95 * se)
ME95
ME99 <- (t_99 * se)
ME99
install.packages("Rmisc")
library(Rmisc)
CI(mms.bags$weight, ci=.90)
CI(mms.bags$weight, ci=.95)
CI(mms.bags$weight, ci=.99)
