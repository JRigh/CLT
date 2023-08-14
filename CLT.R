#---------------------------------------
# Central Limit Theorem and applications
# in R
#---------------------------------------

# 1. central limit theorem (CLT) (empirical mean converges in Law to N(0,1))
set.seed(2023)
par(mfrow=c(1,2))
lambda = 5
n = 1000
x = rexp(n, lambda)  # random exponential variates
hist(x, breaks=30, main="Distribution of Exponential random variates", col="gray80",
     xlab="Y", ylab="density", prob=TRUE )  # obviously the distribution is very skewed (to the right)
curve(dexp(x, rate=lambda), 
      col="red", lwd=2, add=TRUE, yaxt="n")

text(x=0.75, y=3, expression("E[Y] = 1/5"), cex=1)
text(x=0.75, y=2.5, expression("Var[Y] = 1/25"), cex=1)

mean(x) #[1] 0.2023026
var(x) # [1] 0.03969075

# Illustration of the CLT
nsim = 100000; x1 = rep(0,nsim)
for(i in 1:nsim){
  x1[i] <- mean(rexp(n, 5))
}

hist(x1, breaks=20, main="Distribution of the empirical mean", col="gray80",
     xlab=expression(bar(Y)==W), ylab="density", prob=TRUE )
curve(dnorm(x, mean=1/lambda, sd=1/lambda/sqrt(n)), 
      col="red", lwd=2, add=TRUE, yaxt="n")

#plot(density(x1))  # the distribution of the empirical mean tends to a nice N(0,1) distr.
mean(x1) # [1] 0.1999819
var(x1) # [1] 3.994372e-05

text(x=0.185, y=50, expression("E[W] = 1/5") , cex=1)
text(x=0.22, y=50, expression("Var[W] = 1/25000"), cex=1)


# example 2

# 1/3
pnorm(q=86, mean=83, sd=7/5, lower.tail = FALSE)

# 2/3
c((83 - 1.96*(7/5)), (83 + 1.96*(7/5)))
# [1] 80.256 85.744

#----
# end
#----

