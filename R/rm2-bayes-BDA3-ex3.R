## rm2-bayes-BDA3-ex3.R
##
## 05/27/2015 YY

library("MCMCpack")
library("plyr")
library("ggplot2")

#####################################################
## No. 2
size <- 10000
alpha.1 <- rbeta(size, 295, 308)
alpha.2 <- rbeta(size, 288, 332)
dif <- alpha.2 - alpha.1
hist(dif, yaxt = "n", col = "darkgray",
     xlab = expression(alpha[2] - alpha[1]),
     ylab = "", main = "")
abline(v = 0, col = "tomato", lwd = 2)
## probability of shift toward Bush
mean(dif > 0)
#####################################################

#####################################################
## No. 3
size <- 10000
mu.c <- rt(size, df = 31) * (0.24 / sqrt(32)) + 1.013
mu.t <- rt(size, df = 35) * (0.2 / sqrt(36)) + 1.173
dif <- mu.t - mu.c
hist(dif, yaxt = "n", col = "darkgray", nclass = 16,
     xlab = expression(mu[t] - mu[c]), ylab = "", main = "")
abline(v = quantile(dif, c(.025, .975)), col = "tomato", lwd = 2)
#####################################################

#####################################################
## No. 4
size <- 10000
p.0 <- rbeta(size, 40, 636)
p.1 <- rbeta(size, 23, 659)
odds.ratio <- (p.1 / (1 - p.1)) / (p.0 / (1 - p.0))
hist(odds.ratio, yaxt = "n", col = "darkgray", xlim = c(0, 2),
     xlab = "odds ratio: p_1 vs p_0", ylab = "", main = "")
#####################################################

#####################################################
## No. 5
y <- c(10, 10, 12, 11, 9)
n <- length(y)
(y.bar <- mean(y))
(s.sq <- var(y))
size <- 10000

## Ignoring rounding
sigma.1.sq <- rinvgamma(size, shape = (n - 1)/2, scale = ((n - 1)/2) * s.sq)
sigma.1 <- sqrt(sigma.1.sq)
mu.1 <- rnorm(size, mean = y.bar, sd = sigma.1 / sqrt(n))

## Accounting for rounding using grid approximation on (mu, log-sigma)
mu <- seq(0, 22, by = 0.1)
log.sigma <- seq(-2, 4, by = 0.1)
Grid <- expand.grid(mu, log.sigma)
names(Grid) <- c("mu", "log.sigma")

## liklihood of mu and sigma given a single obs.
lik <- function(y, mu, sigma) {
    pnorm((y + .5 - mu) / sigma) - pnorm((y - .5 - mu) / sigma)
}
## joint posterior of mu and sigma
post.2 <- function(grid, data) {
    mu <- grid$mu
    sigma <- exp(grid$log.sigma)
    nrow <- dim(Grid)[1]
    post <- rep(NA, nrow)
    for(i in 1:nrow) {
        post[i] <- prod(lik(y = data, mu = mu[i], sigma = sigma[i]))
    }
    return(post / max(post))
}
Grid$density <- post.2(Grid, y)

## contour plot
cont.levels <- c(.00001, .0001, .001, .01, seq(.05, .95, by = .05))
cont.p <- ggplot(Grid, aes(x = mu, y = log.sigma, z = density)) +
    stat_contour(breaks = cont.levels) + xlim(0, 22) + ylim(-2, 4)
print(cont.p)

## Posterior marginals
Grid$dens.norm <- with(Grid, density / sum(density)) 
post.mu.mgnl <- ddply(Grid, .(mu), summarize, density = sum(dens.norm))
mu.2 <- sample(post.mu.mgnl$mu, size, prob = post.mu.mgnl$density,
               replace = TRUE) 
post.log.sigma.mgnl <- ddply(Grid, .(log.sigma),
                             summarize, density = sum(dens.norm))
log.sigma.2 <- sample(post.log.sigma.mgnl$log.sigma, size,
                      prob = post.log.sigma.mgnl$density,
                      replace = TRUE) 
sigma.2 <- exp(log.sigma.2)

## compare summary stats
sum_stat <- function(x) {
    c(mean(x), sd(x),
      quantile(x, c(.025, .25, .5, .75, .975)))
}
sum.1.2 <- rbind(sum_stat(mu.1), sum_stat(mu.2),
                 sum_stat(sigma.1), sum_stat(sigma.2))
colnames(sum.1.2)[1:2] <- c("mean", "sd")
rownames(sum.1.2) <- c("mu1", "mu2", "sigma1", "sigma2")
sum.1.2

## posterior of z
z <- matrix(NA, size, n)
for (i in 1:n) {
    lb <- pnorm(y[i] - .5, mu.2, sigma.2)
    ub <- pnorm(y[i] + .5, mu.2, sigma.2)
    z[, i] <- qnorm(lb + runif(size) * (ub - lb), mu.2, sigma.2)
}
mean((z[, 1] - z[, 2])^2)
#####################################################

#####################################################
