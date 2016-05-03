library("igraph")
## Q1
filename=cat(getwd(),"sorted_directed_net.txt")
g = read_graph(file = filename ,format = "ncol",directed = T)
con = is.connected(g)
clusters = components(g)
gccInd = which.max(clusters$csize)
nonGccNodes = (1:gorder(g))[clusters$membership != gccInd]
gcc = delete.vertices(g,nonGccNodes)


## Q2
in_degree = degree_distribution(gcc,mode="in")
out_degree = degree_distribution(gcc,mode="out")
par(mfrow=c(1,2))
plot(in_degree, type='l', col='red', main = "In - Degree Distribution of gcc ",xlab="degree",ylab="density")
plot(out_degree, type='l',col='blue', main = "Out - Degree Distribution of gcc ",xlab="degree",ylab="density")


## Q3
#option 1
undirected_g1 = as.undirected(gcc,mode = "each")
lpc_g1 = label.propagation.community(undirected_g1)
commsizes_lpc_g1=sizes(lpc_g1)
mod_lpc_g1 = lpc_g1$modularity
plot(lpc_g1,undirected_g1,main="Comm Structure using Label Prop")

#option 2
sqrtwght<-function(weight){
  new_weight = sqrt(prod(weight))
  new_weight
}
undirected_g2 = as.undirected(gcc,mode = "collapse",edge.attr.comb = sqrtwght)
fg_g2 = fastgreedy.community(undirected_g2)
commsizes_fg_g2=sizes(fg_g2)
mod_fg_g2 = fg_g2$modularity
plot(fg_g2,undirected_g2,main="Comm Structure using Fast Greedy")

lpc_g2 = label.propagation.community(undirected_g2)
commsizes_lpc_g2=sizes(lpc_g2)
mod_lpc_g2 = lpc_g2$modularity
plot(lpc_g2,undirected_g2,main="Comm Structure using Label Prop")


## Q4
maxSizeCommIndex = which.max(commsizes_fg_g2)
non_subcmp_nodes = (1:gorder(undirected_g2))[fg_g2$membership != maxSizeCommIndex]
sub_community = delete.vertices(undirected_g2,non_subcmp_nodes)

fg_sub_community = fastgreedy.community(sub_community)
commsizes_fg_subcomm=sizes(fg_sub_community)
print(commsizes_fg_subcomm)
plot(fg_sub_community,sub_community,main="Sub Community Structure using Fast Greedy")

lp_sub_community = label.propagation.community(sub_community)
commsizes_lp_subcomm=sizes(lp_sub_community)
print(commsizes_lp_subcomm)
plot(lp_sub_community,sub_community,main="Sub Community Structure using Label Prop")


## Q5
gcc_undirected2 = as.undirected(gcc,mode = "collapse",edge.attr.comb = sqrtwght)
com_fg = fastgreedy.community(gcc_undirected2)

com_index = which(sizes(com_fg)>100)
for(i in 1:length(com_index))
{
  cat("\nSub-community index",com_index[[i]],":\n")
  non_com = V(gcc_undirected2)[which(com_fg$membership != com_index[i])]
  sub_com_100up = delete.vertices(gcc_undirected2,non_com)
  sub_com_100up_fg = fastgreedy.community(sub_com_100up)
  cat("sub-community structure using fastgreedy:\n")
  print(sizes(sub_com_100up_fg))
  print(modularity(sub_com_100up_fg))
  sub_com_100up_lp = label.propagation.community(sub_com_100up)
  cat("sub-community structure using labelpropagation:\n")
  print(sizes(sub_com_100up_lp))
  print(modularity(sub_com_100up_lp))
}
