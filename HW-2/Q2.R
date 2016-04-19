library(igraph)
library(netrw)

# Part a
num_nodes=1000
bg=barabasi.game(num_nodes, directed=FALSE)
bgd=diameter(bg)

# Part b
# Part 
RandomWalker = function(g)
{
 avg = numeric()
  std = numeric()
  walkNum = 100 #100, 500
  for (i in 1:100) {
    dists = numeric()
    rw <- netrw(g, walker.num = walkNum, damping = 1, T = i, output.walk.path = TRUE)
    #r=netrw(g, T=i,damping=DF, output.walk.path=TRUE)
    #r1<- netrw(graph1, walker.num=1000, start.node=1:1000, damping=1, T=i, output.walk.path=TRUE)
    paths = rw$walk.path
    #print(paths) matrix of steps X walkers
    for (j in 1:walkNum) {
      #print(paths[1, j])
      #print(paths[i, j])
      dist = shortest.paths(g, v = paths[1, j], to = paths[i, j])
      if (dist == Inf) 
        dist = 0
      dists = c(dists, dist)
    }
    avg = c(avg, mean(dists))
    std = c(std, sd(dists))
  }
}


#layout(matrix(c(1,2),1,2,byrow=T))
par(mfrow=c(1,2))#Part e
par(mfrow=c(1,2))
#hist(degree(g),main ="Degree Distribution of the Graph",xlab="degree",col="blue")
plot(degree(rg1), type = "l", main = "Degree Distribution with p=0.05 ",xlab="degree",ylab="relative frequency")
deg2=numeric()
deg2=rw$walk.path[100,]
#hist(degree(g,deg2),main ="Degree Distribution at end of walk",xlab="degree",col="blue")
plot(degree(rg1,deg2), type="l", main = "Degree Distribution with p=0.05 ",xlab="degree",ylab="relative frequency")
#hist(deg, breaks = seq(-0.5, by = 1, length.out = max(deg) + 2), freq = F, main = "Histogram of degree distribution")

degrees = numeric()
walkNum = 1000
stepSize = 100
g <- random.graph.game(1000, p = 0.01, direct = FALSE)
deg = degree(g)
for (i in 1:100) {
  rw <- netrw(g, walker.num = walkNum, damping = 1, T = stepSize, output.walk.path = TRUE)
  paths = rw$walk.path
  for (j in 1:walkNum) {
    #og
    deg = c(deg, deg[paths[stepSize, j]])
    degrees = c(degrees, deg[paths[stepSize, j]])
  }
}

hist(degrees, breaks = seq(-0.5, by = 1, length.out = max(deg) + 2), freq = F, main = "Histogram of degree distribution")
hist(deg, breaks = seq(-0.5, by = 1, length.out = max(deg) + 2), freq = F, main = "Histogram of degree distribution")
#h = hist(degree(g), breaks = seq(-0.5, by = 1, length.out = max(deg) + 2), freq = F, main = "Histogram of degree distribution")



# Part d
n2=100
rg2 <- random.graph.game(n2, prob)
d2=diameter(rg2)
RandomWalker(rg2)
n3=10000
rg3 <- random.graph.game(n3, prob)
d3=diameter(rg3)
RandomWalker(rg3)
plot(avg, main = "average distance_1000 nodes", xlab = "step size", ylab = "avg")
plot(std, main = "standard deviation_1000 nodes", xlab = "step size", ylab = "std")

RandomWalker(bg)

# Part c



