## frequentistProb.R
######################################################
## Purpose: explain frequentist definition of
##     probability with examples
##
## created: 03/20/2015 Yuki Yanai
## last modified: 04/14/2015 YY
#######################################################

####
## prob of getting head (H) by flipping a coin
set.seed(2015-03-20)
n.flip = 1000
coin.flip <- rbinom(n.flip, 1, prob = .5)
prob.h <- rep(NA, n.flip)
for (i in seq_along(coin.flip)) {
    prob.h[i] <- sum(coin.flip[1:i]) / i
}
## plot the simulation result
plot(1:n.flip, prob.h, type = "l",
     xlab = "試行回数",
     ylab = "表が出た割合",
     main = "長期的相対頻度としての確率：コイン投げ")
abline(h = 1/2, lty = "dashed", col = "tomato", lwd = 2)


######################################################
## prob of getting 6 by rolling a 6-face (cube) die
set.seed(2015-04-10)
dice <- 1:6
n <- 1000
results <- sample(dice, n, replace = TRUE)
is.six <- results == 6
prob <- rep(NA, n)
for (i in seq_along(results)) {
    prob[i] <- sum(is.six[1:i]) / i
}
## plot the simulation result
plot(1:n, prob, type = "l",
     xlab = "試行回数",
     ylab = "6が出た割合",
     main = "長期的な相対頻度としての確率：サイコロ")
abline(h = 1/6, lty = "dashed", col = "tomato", lwd = 2)

## prob of getting 3 or 4 by rolling a 6-face (cube) die
set.seed(1919)
results <- sample(dice, n, replace = TRUE)
is.3or4 <- results == 3 | results == 4
prob.3or4 <- rep(NA, n)
for (i in seq_along(results)) {
    prob.3or4[i] <- sum(is.3or4[1:i]) / i
}
## plot the simulation result
plot(1:n, prob.3or4, type = "l",
     xlab = "試行回数",
     ylab = "3または4が出た割合",
     main = "長期的相対頻度としての確率：サイコロ")
abline(h = 1/3, lty = "dashed", col = "tomato", lwd = 2)
