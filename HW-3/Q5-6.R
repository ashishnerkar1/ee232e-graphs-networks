library("igraph")
####### Q1
filename="./sorted_directed_net.txt"
g = read_graph(file = filename ,format = "ncol",directed = T)

#clusters = components(g)

#clusters
