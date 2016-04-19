# Que 3

library(igraph)
library(netrw)

# Part a

num_nodes = 1000
prob = 0.01
network <- random.graph.game(num_nodes, walker.num=100, prob)

rand_walk_sim = netrw(network)

# prob that walker will visit each node
# rand_walk_sim$ave.visit.prob 
# plot
deg = degree(network)
#head(deg)
correlation = cor(deg, rand_walk_sim$ave.visit.prob)
plot (rand_walk_sim$ave.visit.prob, main = "Probability", xlab = "Nodes",ylab="Probability of visiting nodes", col="red" )
correlation
plot (deg, rand_walk_sim$ave.visit.prob, main = "Relation between Degree of nodes & Probability of visiting them", xlab = "Degree of Nodes",ylab="Probability of visiting nodes", col="blue" )

# Part b

network <- random.graph.game(num_nodes, walker.num=100, prob,  directed = TRUE)

rand_walk_sim = netrw(network)

#rand_walk_sim$ave.visit.prob

total_deg = degree(network)
in_deg = degree(network,mode = "in")
out_deg = degree(network,mode = "out")

total_corr = cor(total_deg, rand_walk_sim$ave.visit.prob)
in_deg_corr = cor(in_deg, rand_walk_sim$ave.visit.prob)
out_deg_corr = cor(out_deg, rand_walk_sim$ave.visit.prob)

plot (rand_walk_sim$ave.visit.prob, main = "Probability", xlab = "Nodes",ylab="Probability of visiting nodes", col="red" )
total_corr
in_deg_corr
out_deg_corr
plot (in_deg, rand_walk_sim$ave.visit.prob, main = "Relation between Degree of nodes & Probability of visiting them", xlab = "Degree of Nodes",ylab="Probability of visiting nodes", col="blue" )

# Part c

network <- random.graph.game(num_nodes, walker.num=100, prob)

rand_walk_sim = netrw(network, damping = 0.85)

#rand_walk_sim$ave.visit.prob
deg = degree(network)
correlation = cor(deg, rand_walk_sim$ave.visit.prob)
plot (rand_walk_sim$ave.visit.prob, main = "Probability", xlab = "Nodes",ylab="Probability of visiting nodes", col="red" )
correlation
plot (deg, rand_walk_sim$ave.visit.prob, main = "Relation between Degree of nodes & Probability of visiting them", xlab = "Degree of Nodes",ylab="Probability of visiting nodes", col="blue" )