library("igraph")
####### Q1
filename="./sorted_directed_net.txt"
g = read_graph(file = filename ,format = "ncol",directed = T)
con = is.connected(g)
clusters = components(g)
gccInd = which.max(clusters$csize)
nonGccNodes = (1:gorder(g))[clusters$membership != gccInd]
gcc = delete.vertices(g,nonGccNodes)

####### Q2

in_degree = degree_distribution(gcc,mode="in")
out_degree = degree_distribution(gcc,mode="out")
#hist(in_degree, breaks=100,main ="In-Degree Distribution of GCC", xlab="degree",ylab="density")
#hist(out_degree, breaks=100,main ="Out-Degree Distribution of GCC", xlab="degree",ylab="density")
plot(in_degree, type='l', col='red', main = "In - Degree Distribution of gcc ",xlab="degree",ylab="relative frequency")
plot(out_degree, type='l',col='blue', main = "Out - Degree Distribution of gcc ",xlab="degree",ylab="relative frequency")

in_degree = degree(gcc,mode="in")
out_degree = degree(gcc,mode="out")
#hist(in_degree, breaks=100,main ="In-Degree Distribution of GCC", xlab="degree",ylab="density")
#hist(out_degree, breaks=100,main ="Out-Degree Distribution of GCC", xlab="degree",ylab="density")
plot(in_degree, type='l', col='red', main = "In - Degree Distribution of gcc ",xlab="degree",ylab="relative frequency")
plot(out_degree, type='l',col='blue', main = "Out - Degree Distribution of gcc ",xlab="degree",ylab="relative frequency")

###### Q3
#option #1
undirected_g1 = as.undirected(gcc,mode = "each")
lpc_g1 = label.propagation.community(undirected_g1)
commsizes_lpc_g1=sizes(lpc_g1)
mod_lpc_g1 = lpc_g1$modularity
#plot(lpc_g1,undirected_g1)

#option #2
sqrtweight<-function(weight){
  #new_weight = sqrt(weight[1]*weight[2])
  new_weight = sqrt(prod(weight))
  new_weight
}
undirected_g2 = as.undirected(gcc,mode = "collapse",edge.attr.comb = sqrtweight)
fg_g2 = fastgreedy.community(undirected_g2)
commsizes_fg_g2=sizes(fg_g2)
mod_fg_g2 = fg_g2$modularity
#plot(fg_g2,undirected_g2)

lpc_g2 = label.propagation.community(undirected_g2)
commsizes_lpc_g2=sizes(lpc_g2)
mod_lpc_g2 = lpc_g2$modularity
#plot(lpc_g2,undirected_g2)

#problem 4
maxSizeCommIndex = which.max(commsizes_fg_g2)
non_subcmp_nodes = (1:gorder(undirected_g2))[fg_g2$membership != maxSizeCommIndex]
sub_community = delete.vertices(undirected_g2,non_subcmp_nodes)

fg_sub_community = fastgreedy.community(sub_community)
hist(fg_sub_community$modularity)
print(sizes(fg_sub_community))
#plot(fg_sub_community,sub_community)

lp_sub_community = label.propagation.community(sub_community)
hist(lp_sub_community$modularity)
print(sizes(lp_sub_community))
#plot(lp_sub_community,sub_community)

