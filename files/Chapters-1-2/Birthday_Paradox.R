# 
# Birthday problem: Example E p. 10
# 
pNoMatch <- function(groupSize) {
  # 
  # probability of no matching birthday in 
  # a group of size groupSize
  # ignoring anyone born on Feb. 29
  # 
  if(groupSize > 365) return(0)
  prod((365 - seq(0:(groupSize-1)))/365) # why not use factorials
  prod(seq(from = 365, to = 365 - (groupSize - 1), by = -1)/365)
} 

pNoMatch(1)
pNoMatch(2)
pNoMatch(10)
pNoMatch(100)

pMatch <- function(groupSize) {
  1 - pNoMatch(groupSize)
}

# In our class

pMatch(103)
pNoMatch(103)

# 
# Vectorizing a function that takes a single value
# 
#+ error=TRUE
pMatch(c(1,2)) 
pMatch <- Vectorize(pMatch)

plot(1:60, pMatch(1:60))

plot(1:60, pMatch(1:60), xlab = 'group size', ylab = 'probability of matching birthdays', pch = 16)
plot(1:60, pMatch(1:60), xlab = 'group size', ylab = 'probability of matching birthdays', type = 'l')
grid(ny=10)

# Let's try some values in the text:

groups <- c(4, 16, 23, 32, 40, 56, 103)
cbind(groups = groups, 'prob. of a match' = pMatch(groups))

