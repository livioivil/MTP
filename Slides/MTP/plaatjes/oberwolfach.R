# R code with the talk: motivating ridge regression
setwd("M:\\praatjes\\2010.01.26 Oberwolfach\\plaatjes")

set.seed(123456)
X <- matrix(rnorm(400),20,20)
colnames(X) <- LETTERS[1:20]
Y <- rowMeans(X) + .1*rnorm(20)

library(penalized)
ft <- profL2(Y, X, minsteps=100, fold=1, maxlambda2=1000, minlambda2=.01)
par(mai=c(1,1,.1,1))
plotpath(ft$full, log="x")
opt <- optL2(Y, X)

sum(coef(opt$full)^2)

# lambda versus t
cfs <- sapply(ft$full, coef)
par(mai=c(1,1,.1,.1))
plot(ft$lambda,colSums(cfs[-1,]^2), log="x", type="l",xlim = rev(range(ft$lambda)), xlab="lambda",ylab="t")

# plaatjes MSE
plot.new()
plot(0:1, 4*0:1,col=0, xlab="c", ylab="MSE contribution")
c <- 0:100/100
variance <- c^2
lines(c, variance)
for (mu in c(0,1,2)) {
  bias2 <- mu^2*(1-c)^2
  lines(c, bias2, col=mu+2)
}
legend("topright", fill=1:4, legend=c("variance",
"sq. bias, mu=0", "sq. bias, mu=1", "sq. bias, mu=2"))

plot.new()
plot(0:1, 4*0:1,col=0, xlab="c", ylab="MSE")
c <- 0:100/100
variance <- c^2
for (mu in c(0,1,2)) {
  mse <- mu^2*(1-c)^2 +variance
  lines(c, mse, col=mu+2)
}
legend("topright", fill=2:4, legend=c(
"MSE, mu=0", "MSE, mu=1", "MSE, mu=2"))

plot.new()
plot(c(-5,5), c(0,4),col=0, xlab=expression(mu), ylab="MSE")
mu <- -500:500/100
variance <- c^2
cc <- c(1,.8,.5,.2)
for (i in 1:4) {
  mse <- mu^2*(1-cc[i])^2 + cc[i]^2
  lines(mu, mse, col=i)
}
legend("topright", fill=1:4, legend=c(
"MSE, c=1", "MSE, c=0.8", "MSE, c=0.5", "MSE, c=0.2"))

# plaatjes MSE mu=5
plot.new()
plot(0:1, 4*0:1,col=0, xlab="c", ylab="MSE contribution")
c <- 0:100/100
variance <- c^2
lines(c, variance)
for (mu in c(0,1,2)) {
  bias2 <- mu^2*(1-c)^2
  lines(c, bias2, col=mu+2)
}
legend("topright", fill=1:4, legend=c("variance",
"sq. bias, mu=5", "sq. bias, mu=4", "sq. bias, mu=3"))

plot.new()
plot(0:1, 4*0:1,col=0, xlab="c", ylab="MSE")
c <- 0:100/100
variance <- c^2
for (mu in c(0,1,2)) {
  mse <- mu^2*(1-c)^2 +variance
  lines(c, mse, col=mu+2)
}
legend("topright", fill=2:4, legend=c(
"MSE, mu=5", "MSE, mu=4", "MSE, mu=3"))

plot.new()
plot(5+c(-5,5), c(0,4),col=0, xlab=expression(mu), ylab="MSE")
mu <- 5+-500:500/100
variance <- c^2
cc <- c(1,.8,.5,.2)
for (i in 1:4) {
  mse <- (mu-5)^2*(1-cc[i])^2 + cc[i]^2
  lines(mu, mse, col=i)
}
legend("topright", fill=1:4, legend=c(
"MSE, c=1", "MSE, c=0.8", "MSE, c=0.5", "MSE, c=0.2"))