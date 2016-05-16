library("igraph")
library("MASS")

filePath = "C:\\E-232E-Graphs\\ee232e-graphs-networks\\Project-1\\facebook_combined.txt"

g = read.graph(file = filePath,format = "ncol", directed = FALSE)

 # Q 2
  node_subg = induced.subgraph(g, c(1, neighbors(g,1)))
 
  node_vec = rep(3,vcount(node_subg))
  node_vec[1]=5
  node_color = rep("green",vcount(node_subg))
  node_color[1] ="brown"
  plot.igraph(node_subg,vertex.size=node_vec,vertex.label =NA,vertex.color=node_color)
  num_node = vcount(node_subg)
  num_edges = ecount(node_subg)
  num_node
  num_edges
 
  # Q 3
 
  core_index = numeric(0)
  core_degree = numeric(0)
  for(i in 1: length(degree(g))){
    if(length(neighbors(g,i))>200){
      core_index = c(core_index, i)
      core_degree = c(core_degree, length(neighbors(g,i)))
    }
  }
  length(core_index)
  core_ave_degree = mean(core_degree)
  cat("\n Core avg degree")
  core_ave_degree

 # community structure by fast greedy method
  fg = fastgreedy.community(node_subg)
  color_vec = fg$membership+1
  plot(node_subg,vertex.color=color_vec,vertex.label=NA,vertex.size=4, main = "Community structure using Fast-Greedy")
  modularity(fg)
  #no of communities
  length(sizes(fg))
 
  # community structure by edge between-ness method
  eb = edge.betweenness.community(node_subg)
  color_vec = eb$membership+1
  plot(node_subg,vertex.color=color_vec,vertex.label=NA,vertex.size=4, main = "Community structure using Edge-Betweenness")
  modularity(eb)
  length(sizes(eb))
 
  # community structure by Info map method
  ic = infomap.community(node_subg)
  color_vec = ic$membership+1
  plot(node_subg,vertex.color=color_vec,vertex.label=NA,vertex.size=4, main = "Community structure using Infomap method")
  modularity(ic)
  length(sizes(ic))
 
  # Q 4
 
 node_subg_remove = induced.subgraph(g,neighbors(g,1))
 
  # community structure by fast greedy method
 fg2 = fastgreedy.community(node_subg_remove)
  color_vec = fg2$membership+1
  plot(node_subg_remove, vertex.color=color_vec, vertex.label=NA, vertex.size=3, main = "Community structure using Fast-Greedy")
  #modularity
 
  modularity(fg2)
  #no of communities
  length(sizes(fg2))
 
  # #no of communities
 
  # # community structure by edge between-ness method
  eb2 = edge.betweenness.community(node_subg_remove)
  color_vec = eb2$membership+1
  plot(node_subg_remove, vertex.color=color_vec,vertex.label=NA, vertex.size=3,main = "Community structure using Edge Betweenness method")
  modularity(eb2)
  #no of communities
  length(sizes(eb2))
 
 
 # # community structure by Info map method
  ic_r = infomap.community(node_subg_remove)
  color_vec = ic_r$membership+1
  plot(node_subg_remove, vertex.color=color_vec,vertex.label=NA, vertex.size=3, main = "Community structure by Info map method")
  modularity(ic_r)
 #no of communities
  length(sizes(ic_r))

# Que 6
# hist of all values of diff parameters
# write results to excel file & put snapshots of it

core_index = numeric(0)
core_degree = numeric(0)

for(i in 1: length(degree(g))){

    if(length(neighbors(g,i))>200){
    core_index = c(core_index, i)
    core_degree = c(core_degree, length(neighbors(g,i)))
  }
}

g_node1 = induced.subgraph(g, c(1, neighbors(g,1)))
ver_vector = rep(3,vcount(g_node1))
ver_vector[1]=5
ver_col = rep("magenta",vcount(g_node1))
ver_col[1] ="black"
plot.igraph(g_node1,vertex.size=ver_vector,vertex.label =NA,vertex.color=ver_col)
n_node = vcount(g_node1)
n_edge = ecount(g_node1)
V(g)$name = V(g)

#core no

max_avg_deg = numeric(0)
max_clust_coeff = numeric(0)
max_density = numeric(0)
min_average_degree = numeric(0)
min_clust_coeff = numeric(0)
min_density = numeric(0)
max_index_average_degree = numeric(0)
max_index_clustering_coefficient = numeric(0)
max_index_density = numeric(0)
min_index_average_degree = numeric(0)
min_index_clustering_coefficient = numeric(0)
min_index_density = numeric(0)

out <- matrix(data = NA, nrow = 40, ncol = 7)

for(i in 1:length(core_index))
{
  core_neighbors =neighbors(g,core_index[i])
  core_personal = induced.subgraph(g,c(core_index[i],core_neighbors))
  core_community = walktrap.community(core_personal)
  community_num_10up = numeric(0)
  for(j in 1:length(core_community))
  {
    community_number=V(core_personal)[which(core_community$membership==j)]
    if(length(community_number)>10)
    {
      community_num_10up = c(community_num_10up,j)
    }
  }
  average_degree = numeric(0)
  global_clustering_coefficient = numeric(0)
  density = numeric(0)
  for(k in 1:length(community_num_10up))
  {
    community_graph = induced.subgraph(core_personal,V(core_personal)[which(core_community$membership==community_num_10up[k])])
    average_degree = c(average_degree,mean(degree(community_graph))/vcount(community_graph))
    global_clustering_coefficient = c(global_clustering_coefficient,transitivity(community_graph,type="global"))
    density = c(density,graph.density(community_graph))
  }
  max_index_average_degree=c(max_index_average_degree,community_num_10up[which.max(average_degree)])
  max_index_clustering_coefficient = c(max_index_clustering_coefficient,community_num_10up[which.max(global_clustering_coefficient)])
  max_index_density=c(max_index_density,community_num_10up[which.max(density)])
  min_index_average_degree=c(min_index_average_degree,community_num_10up[which.min(average_degree)])
  min_index_clustering_coefficient = c(min_index_clustering_coefficient,community_num_10up[which.min(global_clustering_coefficient)])
  min_index_density=c(min_index_density,community_num_10up[which.min(density)])
  
  
 cat( c(core_index[i],
      community_num_10up[which.max(average_degree)],
      community_num_10up[which.min(average_degree)],
      community_num_10up[which.max(global_clustering_coefficient)],
      community_num_10up[which.min(global_clustering_coefficient)],
      community_num_10up[which.max(density)],
      community_num_10up[which.min(density)]),
      "\n"
      )

  max_avg_deg = c(max_avg_deg,max(average_degree))
  max_clust_coeff = c(max_clust_coeff,max(global_clustering_coefficient))
  max_density = c(max_density,max(density))
  min_average_degree = c(min_average_degree,min(average_degree))
  min_clust_coeff = c(min_clust_coeff,min(global_clustering_coefficient))
  min_density = c(min_density,min(density))
  
}


# maximum values
max_index_average_degree = t(data.matrix(max_index_average_degree))
max_index_clustering_coefficient = t(data.matrix(max_index_clustering_coefficient))
max_index_density = t(data.matrix(max_index_density))

# min values
min_index_average_degree = t(data.matrix(min_index_average_degree))
min_index_clustering_coefficient = t(data.matrix(min_index_clustering_coefficient))
min_index_density = t(data.matrix(min_index_density))

max_avg_deg = t(data.matrix(max_avg_deg))
max_clust_coeff = t(data.matrix(max_clust_coeff))
max_density = t(data.matrix(max_density))

min_average_degree = t(data.matrix(min_average_degree))
min_clust_coeff = t(data.matrix(min_clust_coeff))
min_density = t(data.matrix(min_density))

# Plot graphs / histograms

hist(max_avg_deg ,main="Histogram of Max Average Degree", xlab="Max Average Degree",border="blue",col="orange", ylab = "Frequency" )
hist(max_clust_coeff ,main="Histogram of Max Clustering Coefficient", xlab="Maximum Clustering Coefficients",border="blue",col="magenta", ylab = "Frequency" )
hist(max_density ,main="Histogram of Max Density", xlab="Maximum Density",border="blue",col="purple", ylab = "Frequency" )

hist(min_average_degree ,main="Histogram of Min Average Degree", xlab="Min Average Degree",border="blue",col="orange", ylab = "Frequency" )
hist(min_clust_coeff ,main="Histogram of Min Clustering Coefficient", xlab="Clustering Coefficients",border="blue",col="magenta", ylab = "Frequency" )
hist(min_density ,main="Histogram of Min Density", xlab="Minimum Density",border="blue",col="purple", ylab = "Frequency" )
