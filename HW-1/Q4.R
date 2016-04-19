## Load package
library(igraph)

# Q4
# Part a
ff <- sample_forestfire(1000, fw.prob=0.37, bw.factor=0.32/0.37)
din <- degree_distribution(ff, mode="in")
dout <- degree_distribution(ff, mode="out")
plot(din, main = "In Degree Distribution",xlab="degree",ylab="relative frequency")
dev.copy(png,'FFin.png')
dev.off()
plot(dout, main = "Out Degree Distribution",xlab="degree",ylab="relative frequency")
dev.copy(png,'FFout.png')
dev.off()

# Part b and c
dia = numeric()
mod = numeric()
for (i in 1:50) {
  f <- sample_forestfire(1000, fw.prob=0.37, bw.factor=0.32/0.37)
  dia = c(dia, diameter(f))
  community <- cluster_walktrap(f)
  mod = c(mod, modularity(community))
}
mean(dia)
mean(mod)
plot(community,f)
dev.copy(png,'FFCommunityStructure.png')
dev.off()
