dice.value.table <- c(2,2,5,2,0,1)
names(dice.value.table) <- c("1","2","3","4","5","6")
barplot(dice.value.table, main = "Distribution of the Outcome of Rolling Dice", xlab = "Number Face of Dice", ylab = "Frequency of Number", col = 'gold')

dice.means <- c(4.667,3,2.33,3.667,2.667,5,4.33,4.33,4,4.33,3.667,4,3,2.33,3.667,4.33,4.33,4.33,3.667,4.33)
hist(dice.means, main = "Histogram of the Sampling Distribution of the Mean of Rolling Dice", xlab = "Dice mean", ylab = "Frequency", col = 'light blue')

sample_size <- 100
tries <- 1000

rolls <- replicate(tries, sample(c(1:6), sample_size, replace=T))

if (sample_size > 1) {
  roll.means <- apply(rolls,2,mean)
} else {
  roll.means <- rolls
}

summary(roll.means)

hist(roll.means, main = "Sampling Distribution of Mean: Rolling Dice", xlab = "Mean of Die Outcomes", ylab = "Frequency", col = 'green', ylim=c(0, 250))

