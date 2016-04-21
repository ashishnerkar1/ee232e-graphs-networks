# Question 4

# Part a

num_nodes = 1000
prob = 0.01
T = 1000

network1 <- random.graph.game(num_nodes, prob,  directed = TRUE)

rand_walk_sim1= netrw(network1, walker.num=1000, damping = 0.85,
                      T, output.walk.path=TRUE, start.node=1:vcount(network1))
plot (rand_walk_sim1$ave.visit.prob, main = "Simulated Page Rank with random walk on Directed Graph", xlab = "Nodes",ylab="Probability of visiting nodes", col="blue" )

# Part b

pg <- page_rank(network1, directed = TRUE, damping = 0.85)

rand_walk_sim2 = netrw(network1, walker.num=1000, damping = 0.85, 
                      teleport.prob = pg$vector, 
                      T, output.walk.path=TRUE, start.node=1:vcount(network1))

#relation= cor(pagerank, new_pagerank)
#relation
#plot (new_pagerank, main = "Personalized Page Rank", xlab = "Nodes",ylab="Probability of visiting nodes", col="blue" )

plot (rand_walk_sim2$ave.visit.prob, main = "Personalized Page Rank with Random Walk", xlab = "Nodes",ylab="Probability of visiting nodes", col="red" )