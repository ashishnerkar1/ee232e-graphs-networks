# Que 3

library(igraph)
library(netrw)

# Part a

num_nodes = 1000
prob = 0.01
stepsize = 1000

network1 <- random.graph.game(num_nodes, prob, direct = FALSE)
                             
rand_walk_sim1 = netrw(network1, walker.num=1000, damping = 1,
                      T = stepsize, output.visit.prob = TRUE,
                      output.nodes = 1:num_nodes,output.walk.path=TRUE,
                      start.node=1:1000)

plot (rand_walk_sim1$ave.visit.prob, main = "Random Walk on Undirected graph", xlab = "Nodes",ylab="Probability of visiting nodes", col="red" )

deg_list <- c()
visit_prob_list <- c()

for(k in rand_walk_sim1$walk.path[,1])
{
  deg_list<-append(deg_list,degree(network1,k))
  visit_prob_list<-append(visit_prob_list , rand_walk_sim1$ave.visit.prob[k])
}

relation<- rbind(deg_list, visit_prob_list)
#order by degree
relation<- relation[,order(relation[1,])]  
plot(relation[1,],relation[2,],xlab="Degree",ylab="Probability of visiting nodes", main="Relation Between Node Degree and Visit Probability for Undirected Network", col= "blue")


# Part b

network2 <- random.graph.game(num_nodes, prob,  directed = TRUE)

rand_walk_sim2 = netrw(network2, walker.num=1000, damping = 1,
                      T = stepsize, output.visit.prob = TRUE,
                      output.walk.path=TRUE,
                      start.node=1:1000)

total_deg = degree(network2)
in_deg = degree(network2,mode = "in")
out_deg = degree(network2,mode = "out")

total_corr = cor(total_deg, rand_walk_sim2$ave.visit.prob)
in_deg_corr = cor(in_deg, rand_walk_sim2$ave.visit.prob)
out_deg_corr = cor(out_deg, rand_walk_sim2$ave.visit.prob)

plot (rand_walk_sim2$ave.visit.prob, main = "Random Walk on directed graph", xlab = "Nodes",ylab="Probability of visiting nodes", col="blue" )
total_corr
in_deg_corr
out_deg_corr

deg_list <- c()
visit_prob_list <- c()

for(k in rand_walk_sim2$walk.path[,1])
{
  deg_list<-append(deg_list,degree(network2,k))
  visit_prob_list<-append(visit_prob_list , rand_walk_sim2$ave.visit.prob[k])
}

relation<- rbind(deg_list, visit_prob_list)
#order by degree
relation<- relation[,order(relation[1,])]  
plot(relation[1,],relation[2,],xlab="Degree",ylab="Probability of visiting nodes", main="Relation Between Node Degree and Visit Probability for Directed Network", col= "red")


#plot (in_deg, rand_walk_sim$ave.visit.prob, main = "Relation between Degree of nodes & Probability of visiting them", xlab = "Degree of Nodes",ylab="Probability of visiting nodes", col="blue" )

# Part c

network3 <- random.graph.game(num_nodes, prob, direct = FALSE)

rand_walk_sim3 = netrw(network3 ,walker.num=1000, damping = 0.85,
                      T = stepsize, output.visit.prob = TRUE,
                      output.walk.path=TRUE,
                      start.node=1:1000)

#rand_walk_sim$ave.visit.prob
deg = degree(network3)
correlation = cor(deg, rand_walk_sim3$ave.visit.prob)
correlation

plot (rand_walk_sim3$ave.visit.prob, main = "Random Walk on graph", xlab = "Nodes",ylab="Probability of visiting nodes", col="green" )

deg_list <- c()
visit_prob_list <- c()

for(k in rand_walk_sim3$walk.path[,1])
{
  deg_list<-append(deg_list,degree(network3,k))
  visit_prob_list<-append(visit_prob_list , rand_walk_sim3$ave.visit.prob[k])
}

relation<- rbind(deg_list, visit_prob_list)
#order by degree
relation<- relation[,order(relation[1,])]  
plot(relation[1,],relation[2,],xlab="Degree",ylab="Probability of visiting nodes", main="Relation Between Node Degree and Visit Probability for Undirected Network with damping 0.85", col= "purple")

