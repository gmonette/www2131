#' ---
#' title: Multivariate Normal
#' ---
#' 
library(p3d)
library(misc3d)

Init3d()
dd <- data.frame(x1=c(-4,4), x2 = c(-4,4), f = c(0,2))
Plot3d(f ~ x1 + x2, dd)
Pop3d()
Axes3d()
