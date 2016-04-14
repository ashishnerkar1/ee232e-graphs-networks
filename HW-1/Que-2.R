library(igraph)

#g <- sample_fitness_pl(1000, 4000, 3, -1)

g <- barabasi.game(n = 1000, directed = FALSE)

# # graph
# plot(degree_distribution(g), type = "l" )
# 
# # diameter of graph
# diameter(g, directed= FALSE)
# 
# # part b
# cl <- clusters(g)
# gccIndex = which.max(cl$csize)
# nonGccNodes <- (1:vcount(g))[cl$membership != gccIndex]
# gcc <- delete.vertices(g, nonGccNodes)
# 
# E(gcc)
# V(gcc)
# 
# 
# # find communities
# fg <- fastgreedy.community(gcc)
# 
# # modularity
# modularity(fg)
# 
# cmsize <- sizes(fg)
# 
# cmsize <- as.vector(sizes(fg))
# cmsize
# gccNodes <- (1:vcount(g))[cl$membership == gccIndex]
# gccNodes
# cm1Nodes <- gccNodes[fg$membership == 1]
# #cm1Nodes
# 

# part d

random_node = sample(1:1000, 1)
random_node
neighbors_list = neighbors(g, random_node)
neighbors_list
random_neighbor = sample(neighbors_list,1)
random_neighbor
degree(random_neighbor)
