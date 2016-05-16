library("igraph")
getwd()
filename="facebook_combined.txt"
g = read.graph(file = filename, format="ncol", directed=FALSE)

#core node
core_nodes = numeric(0)
for(i in 1: length(degree(g))){
  if(length(neighbors(g,i))>200){
    core_nodes = c(core_nodes, i)
  }
}

total_embed=numeric(0)
total_disp=numeric(0)
for(i in 1:length(core_nodes))
{
  print(i)
  embeddedness=numeric(0)
  dispersion = numeric(0)
  
  core_neighbors = neighbors(g,core_nodes[i])
  core_personal = induced.subgraph(g,c(core_nodes[i],core_neighbors))
  
  # embded and dispersion
  for(j in 1:length(core_neighbors))
  {
    #embedness
    embeddedness =c(embeddedness,length(intersect(neighbors(g,core_nodes[i]),neighbors(g,core_neighbors[j]))))
    #dispersion
    mutual_friends = intersect(neighbors(g,core_nodes[i]),neighbors(g,core_neighbors[j]))
    subgraph = delete.vertices(core_personal,c(which(V(core_personal)$name==core_nodes[i]),which(V(core_personal)$name==core_neighbors[j])))
    shortestpath=numeric(0)
    for(m in 1:length(mutual_friends))
    { for(n in (m+1): length(mutual_friends))
      {shortestpath = c(shortestpath,shortest.paths(subgraph,which(V(subgraph)$name==mutual_friends[m]),which(V(subgraph)$name==mutual_friends[n])))
      }
    }
    dispersion = c(dispersion, sum(shortestpath))
  }
  
  total_embed=c(total_embed,embeddedness)
  total_disp=c(total_disp,dispersion)
  
  max_disp = which.max(dispersion)
  cat("max disp",max_disp,dispersion[max_disp],"\n")
  max_embed = which.max(embeddedness)
  cat("maxemd",max_embed,embeddedness[max_embed],"\n")
  max_disp_embed = which.max(dispersion/embeddedness)
  cat("max_disp_embed",max_disp_embed,(dispersion/embeddedness)[max_disp_embed],"\n")
  
  wtc = walktrap.community(core_personal)
  v_color = membership(wtc)
  dn = which(V(core_personal)$name==core_neighbors[max_disp]$name)
  dname = core_neighbors[max_disp]$name
  en = which(V(core_personal)$name==core_neighbors[max_embed]$name)
  ename = core_neighbors[max_embed]$name
  den = which(V(core_personal)$name==core_neighbors[max_disp_embed]$name)
  dename = core_neighbors[max_disp_embed]$name
  
  v_size = rep(3, length(V(core_personal)))
  v_size[dn] = 8
  v_size[en] = 8
  v_size[den] = 8
  v_size[which(V(core_personal)$name==core_nodes[i])] = 7
  
  e_col = rep(8, length(E(core_personal)))
  e_col[which(get.edgelist(core_personal)[,1] == dname | get.edgelist(core_personal)[,2] == dname)] = 5
  e_col[which(get.edgelist(core_personal)[,1] == ename | get.edgelist(core_personal)[,2] == ename)] = 6
  e_col[which(get.edgelist(core_personal)[,1] == dename | get.edgelist(core_personal)[,2] == dename)] = 7
  E(core_personal)$color = e_col
  
  e_width = rep(1, length(E(core_personal)))
  e_width[which(get.edgelist(core_personal)[,1] == dname | get.edgelist(core_personal)[,2] == dname)] = 10
  e_width[which(get.edgelist(core_personal)[,1] == ename | get.edgelist(core_personal)[,2] == ename)] = 10
  e_width[which(get.edgelist(core_personal)[,1] == dename | get.edgelist(core_personal)[,2] == dename)] = 10
  plot(core_personal, vertex.label= NA, vertex.color=v_color,vertex.size=v_size, edge.width = e_width)
}

hist(total_embed,breaks=50,main = "Embeddedness Distribution",xlab = "Embeddedness", col="blue")
dev.copy(png,paste('embed.png'))
dev.off()
hist(total_disp[which(total_disp!=Inf)],breaks = 100,main = "Dispersion Distribution",xlab = "Dispersion",col="red")
dev.copy(png,paste('disp.png'))
dev.off()