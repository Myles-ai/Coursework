## ISAT 251 Week 4 Lab
## Correlation, Covariance, Linear Regression

myfile <- file.choose()
myfile

hand_height.data <- read.csv(myfile, header = T)
summary(hand_height.data)
head(hand_height.data1)

## Remove those rows with NA's
hand_height.data1 <- na.omit(hand_height.data)
summary(hand_height.data1)

# Correlation coefficient
# cov + sd

install.packages("descr")
library(descr)
descr(hand_height.data1)

fivenum(hand_height.data1$height)

hist(breaks =8,hand_height.data1$height, main = "Distribution of ISAT 251 Student's Height", xlab = "Student's Height (cm)", ylab = "Number of Students", axes = F, xlim = c(156,194), ylim = c(0,30), col = "blue")

axis(1, pos= 0, at = seq(100, 195, by =1))
axis(2, pos= 155, at = seq(0, 30, by =1))

boxplot(hand_height.data1$height, main = "Box Plot of ISAT 251 Student's Height", xlab = "Height (cm)", horizontal = T, ylim=c(150, 200))

hand_height.cov <- cov(hand_height.data1$height,hand_height.data1$right.hand.span)
hand_height.cov

height.sd <- sd(hand_height.data1$height)
height.sd

hand.sd <- sd(hand_height.data1$right.hand.span)
hand.sd

r <- hand_height.cov/(height.sd*hand.sd)
r

# correlation in R
r1 <- cor(hand_height.data1$height, hand_height.data1$right.hand.span)
r1

# linear regression model
fit_rhs_ht <- lm(hand_height.data1$right.hand.span ~ hand_height.data1$height)
summary(fit_rhs_ht)

summary(fit_rhs_ht)$r.squared
r2 <- (r*r)
r2

## Scatterplot
plot(hand_height.data1$height, hand_height.data1$right.hand.span, main = "Scatter Plot of Right Hand Span vs. Height", xlab = "Right Hand Span (cm)", ylab = "Height (cm)")

# add line for the linear model
abline(fit_rhs_ht, col ='red', lwd = 2)

##PDF - dnorm function
sd(hand_height.data1$height)
dnorm(x= 0, mean =175.9, sd =9.555055)

rds1 <- rnorm(x= 99, mean =176, sd =9.6)
summary(rds1)
par(mfrow=c(1,2))
hist(rds1, col = 'grey', main="Histogram of 99 Students from a Normal Distribution", xlab= "Height",xlim = c(4,16), ylim = c(0.00,0.30), ylab = "Probability", freq = F)

## QQplot
qqnorm(rds1)
qqline(rds1, col = 'red', lwd =2)

hand_height.male <- hand_height.data1[which(hand_height.data1$gender=='M'),]
summary(hand_height.male)
sd(hand_height.male$height)

rds2 <- rnorm(72, mean = 179.7, sd = 7.8)
summary(rds2)

par(mfrow =c(1,2))
hist(rds2, col = 'light blue', main="Histogram of 72 Male Students from a Normal Distribution", xlab= "Height",xlim = c(150,200),ylim = c(0.00,0.06), ylab = "Probability", freq = F)

qqnorm(rds2)
qqline(rds2, col = 'red', lwd =2)

## Finding z-scores
height_sd <- sd(hand_height.data1$height)*sqrt((length(hand_height.data1$height)-1)/(length(hand_height.data1$height)))
height_mean <- mean(hand_height.data1$height)
z <- (180 - 175.5) / 7.4
z

dnorm(165,161.8,6.9)
pnorm(q= 178.5, mean = 175.5, sd =7.4, lower.tail = F)
pnorm(q= 165.7, mean = 175.5, sd =7.4, lower.tail = T)

pnorm(q= 170, mean = 175.5, sd =7.4) - pnorm(q= 155, mean = 175.5, sd =7.4)
pnorm(q= 165.1, mean = 161.8, sd =6.9, lower.tail = F)
qnorm(p =0.90, mean = 175.5, sd =7.4, lower.tail = F)
qnorm(p =0.90, mean = 161.8, sd =6.9, lower.tail = T)
