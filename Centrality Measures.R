#  Centrality Metrics
#Impoert Adjacency Matrix of considered dataset

library(ggnet); library(network); library(sna); library(ggplot2)

BC=betweenness(AdjacencyMatrix) # Betweenness Centrality

library(igraph)
graphh= graph.adjacency(AdjacencyMatrix, mode="undirected", weighted=NULL)
plot(graphh)
plot.igraph(graphh, edge.arrow.size=0.2)

library(centiserve)
EVC=eigen_centrality(graphh)  # EigenVector Centrality

KC= katzcent(graph,alpha=0.02)   # Katz Centrality

CC= closeness(graph)  # Closeness Centrality

DC = degree(graphh) # Degree Centrality


# Dangling Centrality Computation
#--------------------------------
N= sqrt(length(AdjacencyMatrix))

SP=geodist(AdjacencyMatrix)   # Shortest Path Distance Matrix
sp=SP$gdist   
E=rep(0,N)
Dangling=rep(0,N)
ii=diag(N)
i2=diag(N-1)
spu=sp+ii
eij=1/spu
eij=eij-ii
ss=sum(eij)/2
DanglingG=ss
DanglingG
for (i in 1:N){
  x=AdjacencyMatrix[-c(i),-c(i) ]
  SP2=geodist(x)
  SP2=SP2$gdist
  
  
  spp=SP2+i2
  ei=1/spp
  ei=ei-i2
  s2=sum(ei)/2
  E[i]=s2
  Dangling[i]=(DanglingG-s2)/DanglingG
}
Dangling

