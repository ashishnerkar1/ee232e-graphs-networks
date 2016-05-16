library("igraph")

folder = "gplus/"
allfiles = list.files(folder)
egoNodeIds = dir(folder,pattern = "circles")

for(i in 1:length(egoNodeIds)){
  egoNodeId = strsplit(egoNodeIds[i],".circles")[[1]]
  cat("Node id",egoNodeId,"\n")
  edgelistFile = paste(folder , egoNodeId  , ".edges" , sep="")
  circlesFile = paste(folder , egoNodeId , ".circles" , sep="")

  circlesFile1 = file(circlesFile , open="r")
  lines = readLines(circlesFile1)
  if(length(lines)<=2)
  {
    close(circlesFile1)
    next
  }
  
  circles = list()
  for (i in 1:length(lines)) {
    linesplit = strsplit(lines[i],"\t")
    circles[[i]] = linesplit[[1]][-1]
  }
  close(circlesFile1)
  
  g2pre = read.graph(edgelistFile , format = "ncol" , directed=TRUE)
  g2 = add.vertices(g2pre,1,name=egoNodeId)
  addEgoEdges = c()
  for (nodeIndex in 1:(vcount(g2)-1)) {
    addEgoEdges = c(addEgoEdges , c(vcount(g2),nodeIndex))
  }
  g2 = add.edges(g2,addEgoEdges)
  
  
  #comm = walktrap.community(g2)
  comm = infomap.community(g2)
  write(paste(egoNodeId," ","Number of communties",":",max(comm$membership)," ","Number of circles",":",length(circles),sep=""), file="q7IMC.txt", append=TRUE)
  for(j in 1:max(comm$membership)){ # no of comm of a node
    members = c()
    for(i in 1:length(comm$membership)){ 
      if(comm$membership[i]==j){
        members = c(members,(comm$name[i]))
      }
    }
    percentage_comm = c()
    percentage_circle = c()
    for(k in 1:length(circles)){ # no of circles of a node given
      ints_members = intersect(members,circles[[k]])
      t1 = length(ints_members)/length(members)
      t2 = length(ints_members)/length(circles[[k]])
      percentage_comm = c(percentage_comm, t1)
      percentage_circle= c(percentage_circle, t2)
     
    }
    write(paste('Community: ',j,sep=""), file="q7IMC.txt", append=TRUE)
    write(paste("Overlap Ratio (Communites):", percentage_comm, ",Overlap Ratio (Circles):", percentage_circle,sep=""), file="q7IMC.txt", append=TRUE)
  }
  write('End of information for this user', file="q7IMC.txt", append=TRUE)
  
}
