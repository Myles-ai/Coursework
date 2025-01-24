myfile <- file.choose()
height.data <- read.csv(myfile,header=TRUE)
summary(height.data)
height.female <- height.data[which(height.data$gender=='F'),]
summary(height.female)
height.male <- height.data[which(height.data$gender=='M'),]
summary(height.male)
sd(height.male$height)
sd(height.female$height)
hist(height.female$height, main = "PDF of ISAT 251 Female Student's Height", xlab = "Females Height (in)", ylab = "Probability", freq = F, col = "gold")
hist(height.male$height, main = "PDF of ISAT 251 Male Student's Height", xlab = "Males Height (in)", ylab = "Probability", freq = F, col = "purple")
var.test(height.female$height,height.male$height)
sef <- sd(height.female$height,na.rm=TRUE) / sqrt(length(height.female$height)) 
sem <- sd(height.male$height,na.rm=TRUE) / sqrt(length(height.male$height)) 
sef
sem
sdm <- sd(height.male$height) * sd(height.male$height)
sdm
sdf <- sd(height.female$height) * sd(height.female$height)
sdf
se <- sqrt((sdm/119) + (sdf/53))
se
height.df <- 172-2
t_95 <- qt(p=.95, df = height.df)
t_95
height.df
ME95 <- (t_95 * se)
ME95
install.packages("Rmisc")
library(Rmisc)
s1 <- sd(height.female$height)
s1
s2 <- sd(height.male$height)
s2
n1 <- length(height.female$height)
n1
n2 <- length(height.male$height)
n2
spooled <- sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
spooled

se <- spooled * sqrt(1/n1+1/n2)
se
t.star <- qt(0.975,height.df)
t.star

ME <- t.star * se
ME

female.h.bar <- mean(height.female$height)
female.h.bar
male.h.bar <- mean(height.male$height)
male.h.bar
x.diff <- female.h.bar - male.h.bar
x.diff

CI_95 <- x.diff + c(- ME, ME)
CI_95
