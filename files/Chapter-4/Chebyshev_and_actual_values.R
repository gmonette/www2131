#
# Chebyshev's inequality compared with actual values for some distributions
# 
# Normal distribution
# 
library(lattice)
if(!require('latticeExtra')) install.packages('latticeExtra')
library(latticeExtra)


plot_chebyshev <- function(data, low, high, ylab = 'density') {
  xyplot(y ~ x, data, type = 'l',  lwd = 3, ylim = c(0, 1.1*max(data$y)),
         ylab = ylab) +
#       scales = list(x = list(at= seq(-5,5,1)))) +
  layer(panel.polygon(x=c(min(x),x), y=c(0,y), col = 'red', alpha = .5), data = subset(data, x >= high))+
  layer(panel.polygon(x=c(max(x),x), y=c(0,y), col = 'red', alpha = .5), data = subset(data, x <= low))+
  layer_(panel.grid(v=-1,h=-1))
}

norm <- 
  within(
    data.frame(x=seq(-4,4,.01)),
    {
      y <- dnorm(x)
    }
  )

plot_chebyshev(norm, -1, 1)
plot_chebyshev(norm, -2, 2)

exponential <- 
  within(
    data.frame(x=seq(0,5,.1)),
    {
      y <- dexp(x)
    }
  )
plot_chebyshev(exponential, 1-1, 1+1)
plot_chebyshev(exponential, 1-2, 1+2)


options(scipen = 20)
k <- 2:4
pnorm(0 - k * 1) + pnorm(0 + k * 1, lower = FALSE)
pexp(1 - k * 1) + pexp(1 + k * 1, lower = FALSE)
ppois(1 - k * 1,1) + ppois(1 + k * 1, 1, lower = FALSE) + dpois(1 + k * 1,1)
# t with df =3   ( 2 has no variance) 
# mean 0
# var = df/(df-2) = 3
pt(0 - k * sqrt(3), 3) + pt(0 + k * sqrt(3), 3, lower = FALSE)


