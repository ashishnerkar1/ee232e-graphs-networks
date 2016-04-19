## Download and install the package
#install.packages("igraph")

## Load package
library(igraph)

# Q1
# Part a
num_nodes = 1000
prob = c(0.01, 0.05, 0.1)
g1 <- sample_gnp(num_nodes, prob[1])
d1 <- degree_distribution(g1)
g2 <- sample_gnp(num_nodes, prob[2])
d2 <- degree_distribution(g2)
g3 <- sample_gnp(num_nodes, prob[3])
d3 <- degree_distribution(g3)
plot(d1, main = "Degree Distribution with p=0.01 ",xlab="degree",ylab="relative frequency")
dev.copy(png,'DDP=0.01.png')
dev.off()
plot(d2, main = "Degree Distribution with p=0.05 ",xlab="degree",ylab="relative frequency")
dev.copy(png,'DDP=0.05.png')
dev.off()
plot(d3, main = "Degree Distribution with p=0.1 ",xlab="degree",ylab="relative frequency")
dev.copy(png,'DDP=0.1.png')
dev.off()

# Part b
con1 = numeric()
con2 = numeric()
con3 = numeric()
dia1 = numeric()
dia2 = numeric()
dia3 = numeric()
for (i in 1:50) {
  g1 <- sample_gnp(num_nodes, prob[1])
  g2 <- sample_gnp(num_nodes, prob[2])
  g3 <- sample_gnp(num_nodes, prob[3])
  con1 = c(con1, is_connected(g1))
  con2 = c(con2, is_connected(g2))
  con3 = c(con3, is_connected(g3))
  dia1 = c(dia1, diameter(g1))
  dia2 = c(dia2, diameter(g2))
  dia3 = c(dia3, diameter(g3))
}
mean(con1)
mean(con2)
mean(con3)
mean(dia1)
mean(dia2)
mean(dia3)

# Part c
  pc = numeric()
  for (j in 1:50) {
    for (p in seq(from = 0, to = prob[1], by = 1e-03)) {
      g = sample_gnp(num_nodes, p)
      if (is.connected(g)) 
        break
    }
    pc = c(pc, p)
  }
  print(mean(pc))