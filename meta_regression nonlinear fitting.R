### ref:https://stats.stackexchange.com/questions/122196/nonlinear-meta-regression
library(metafor)
set.seed(12326)

### number of studies
n <- 40

### simulate explanatory variable
xi <- round(runif(n, 1, 10), 1)

### simulate sampling variances
vi <- rgamma(n, 2, 2)/20


### simulate estimates (quadratic relationship + residual heterogeneity + sampling error)
yi <- 1 + -0.3 * xi +0.03 * xi^2 + rnorm(n, 0, 0.1) + rnorm(n, 0, sqrt(vi))



res <- rma(yi, vi, mods = ~ xi + I(xi^2))
res

plot(xi, yi, pch=19, cex=.2/sqrt(vi), xlim=c(1,10), ylim=c(0,2))
xi.new <- seq(1,10,length=100)
pred <- predict(res, newmods = cbind(xi.new, xi.new^2))
lines(xi.new, pred$pred)
lines(xi.new, pred$ci.lb, lty="dashed", col="gray")
lines(xi.new, pred$ci.ub, lty="dashed", col="gray")
points(xi, yi, pch=19, cex=.2/sqrt(vi))



