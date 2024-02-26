
sum((1:6)^2 )/6
3.5^2


df <- expand.grid(n = 1:100, p = c(.0001,.001, .01, .05, .1))
head(df)
dim(df)
df <- within(df,
             {
               Etests <- 1* (1-p)^n + (n + 1) * (1 - (1 - p)^n)
               Cost_per_sample <- Etests/n
             }
             )

library(latticeExtra)
trellis.par.set(superpose.line = list(lwd=3, lty = 1:3))
xyplot(Cost_per_sample ~ n, df, 
       groups = p, type = 'l',
       ylab = "Cost per sample",
       auto.key = list(reverse.rows = T)) +
  layer_(panel.grid(h=-1, v = -1))
