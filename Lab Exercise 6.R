myfile <- file.choose()
parking.data <- read.csv(myfile,header=TRUE)
summary(parking.data)
n <- length(parking.data$satisfaction)
n_successes <- length(which(parking.data$satisfaction=="N"))
n_failures <- n - n_successes
n
n_successes
n_failures
z <- qnorm(p= 0.995, mean = 0,sd = 1)
z
diff <- z * 0.04640351
lower <- (n_successes - diff)
higher <- (n_successes + diff)
lower
higher
diff
install.packages("PropCIs")
library(PropCIs)
add4ci(66,89,conf.level = 0.995)
scoreci(66,89,conf.level = 0.995)
exactci(66,89,conf.level = 0.995)
