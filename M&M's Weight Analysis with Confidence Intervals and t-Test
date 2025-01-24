myfile <- file.choose()
myfile
mms.bags <- read.csv(myfile,header=TRUE)
summary(mms.bags)
n <- dim(mms.bags)[1]
n
fivenum(mms.bags$weight)
weight.sd <- sd(mms.bags$weight)
weight.sd
hist(breaks =8,mms.bags$weight, main = "Distribution of ISAT 251 Student's M&M's Weight", xlab = "M&M's Weight (gm)", ylab = "Number of Students", ylim = c(0,12), col = "blue")
shadenorm(between=c(9.68479145,Inf), color="black")
1-pt(1.253914,df=37)
t_star <- qt(0.995,df=37)
t_star
CI_99 <- 49.87 + (t_star * weight.sd/n)
CI_99
CI_99 <- 49.87 - (t_star * weight.sd/n)
CI_99
t.test(mms.bags$weight,alternative = "greater", mu = 47.9, conf.level = 0.995)
