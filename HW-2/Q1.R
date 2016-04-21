## Download and install the package
#install.packages("igraph")

## Load package
library(igraph)
library(netrw)

# Part a
num_nodes = 1000
prob = 0.01
rg1 <- random.graph.game(num_nodes, prob, directed = FALSE)
diameter(rg1)

# Part b
myRWfunc <- function(g,num_walkers,txt)
{
avg = numeric()
std = numeric()
for (i in 1:100) {
  all_dist = numeric()
  rw <- netrw(g, walker.num = num_walkers, damping = 1, T = i, output.walk.path = TRUE)
  paths = rw$walk.path
  for (j in 1:num_walkers) {
    dist = shortest.paths(g, v = paths[1, j], to = paths[i, j])
    if (dist == Inf) 
      dist = 0
    all_dist = c(all_dist, dist)
  }
  avg = c(avg, mean(all_dist))
  std = c(std, sd(all_dist))
}
print(avg)
print(std)
par(mfrow=c(1,2))
plot(avg, type='l',col="red", main = paste0("avg ",txt," nodes"), xlab = "step size", ylab = "avg")
plot(std, type='l',col="blue", main = paste0("std ",txt," nodes"), xlab = "step size", ylab = "std")
dev.copy(png,paste0(txt,'avgstd.png'))
dev.off()
}

myRWfunc(rg1,100,"1000")

# Part d
n2=100
rg2 <- random.graph.game(n2, prob, directed = FALSE)
d2 <- diameter(rg2)
print(d2)
myRWfunc(rg2,100,"100")

n3=10000
rg3 <- random.graph.game(n3, prob, directed = FALSE)
d3 <- diameter(rg3)
print(d3)
myRWfunc(rg3,100,"10000")

#Part e
degrees = numeric()
num_walkers = 100
stepSize = 100
deg <- degree(rg1)
rw <- netrw(rg1, walker.num = num_walkers, damping = 1, T = 100, output.walk.path = TRUE)
paths = rw$walk.path
  for (j in 1:num_walkers) {
    degrees = c(degrees, deg[paths[stepSize, j]])
  }
par(mfrow=c(1,2))
hist(degrees, breaks = seq(-0.5, by = 1, length.out = max(deg) + 2), freq = F, main = "deg dist. of nodes-random walk",col="purple")
hist(deg, breaks = seq(-0.5, by = 1, length.out = max(deg) + 2), freq = F, main = "degree distribution of the graph",col="palegreen")
dev.copy(png,'Q1parte.png')
dev.off()

