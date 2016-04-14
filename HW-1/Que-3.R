g <- sample_pa(1000,1, directed = FALSE)
plot(degree_distribution(g), type = "l")


# find the giant connected component
cl <- clusters(g)
gccIndex = which.max(cl$csize)
nonGccNodes <- (1:vcount(g))[cl$membership != gccIndex]
gcc <- delete.vertices(g, nonGccNodes)


# find communities
fg <- fastgreedy.community(gcc)
cmsize <- sizes(fg)

cmsize <- as.vector(sizes(fg))

gccNodes <- (1:vcount(g))[cl$membership == gccIndex]
cm1Nodes <- gccNodes[fg$membership == 1]