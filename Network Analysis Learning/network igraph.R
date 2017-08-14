####################### Network in igraph ##############################
rm(list=ls())
gc()

library(igraph)

####### create network #########
# 2.1 create network
g1 <- graph(edges = c(1,2,2,3,3,1),n=3,directed = F)
plot(g1)
class(g1)

# with 10 vertices, and directed by default
g2 <- graph(edges=c(1,2,2,3,3,1),n=10)
plot(g2)

# when edge list has vertex names, the number of nodes is not needed
g3 <- graph(c("John","Jim","Jim","Jill","Jill","John"))
plot(g3)

g4 <- graph(c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"), isolates=c("Jesse", "Janis", "Jennifer", "Justin")) 
plot(g4,edge.arrow.size=.5,vertex.color="gold",vertex.size=15,vertex.frame.color="gray",vertex.label.color="black",
     vertex.label.cex=0.8,vertex.label.dist=2,edge.curved=0.2)

# - for undirected tie, +- or -+ for directed ties pointing left & right, ++ for symmetric tie, and . for sets of vertices
plot(graph_from_literal(a--b,b--c))
plot(graph_from_literal(a--+b, b+--c))
plot(graph_from_literal(a+-+b, b+-+c)) 
plot(graph_from_literal(a:b:c---c:d:e))

gl <- graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j)
plot(gl)

# 2.2 edge, vertex, and network attributes
plot(g4)
E(g4) # the edge of the object
V(g4) # vertices of the object
g4[]

V(g4)$name
V(g4)$gender <- c("male", "male", "male", "male", "female", "female", "male")
E(g4)$type <- "email"
E(g4)$weight <- 10

# examine attributes
edge_attr(g4)
vertex_attr(g4)
graph_attr(g4)

plot(g4,edge.arrow.size=.5,vertex.label.color="black", vertex.label.dist=1.5,
     vertex.color=c("pink","skyblue")[1+(V(g4)$gender=="male")])

g4s <- simplify(g4, remove.multiple = T, remove.loops = F, edge.attr.comb=c(weight="sum", type="ignore") )
plot(g4s,vertex.label.dist=1.5)

# The description of an igraph object starts with up to four letters:
# D or U, for a directed or undirected graph
# N for a named graph (where nodes have a name attribute)
# W for a weighted graph (where edges have a weight attribute)
# B for a bipartite (two-mode) graph (where nodes have a type attribute)

# 2.3 Specific graph and graph models
# empty graph
eg <- make_empty_graph(40)
plot(eg, vertex.size=10, vertex.label=NA)

# full graph
fg <- make_full_graph(40)
plot(fg, vertex.size=10, vertex.label=NA)

# simple star graph
st <- make_star(40)
plot(st, vertex.size=10, vertex.label=NA) 

# tree graph
tr <- make_tree(40, children = 3, mode = "undirected")
plot(tr, vertex.size=10, vertex.label=NA) 

# ring graph
rn <- make_ring(40)
plot(rn, vertex.size=10, vertex.label=NA)

# Erdos-Renyi random graph model
er <- sample_gnm(n=100, m=40) 
plot(er, vertex.size=6, vertex.label=NA)

# Watts-Strogatz small-world model
sw <- sample_smallworld(dim=2, size=10, nei=1, p=0.1)
plot(sw, vertex.size=6, vertex.label=NA, layout=layout_in_circle)

# Barabasi-Albert preferential attachment model for scale-free graphs
ba <-  sample_pa(n=100, power=1, m=1,  directed=F)
plot(ba, vertex.size=6, vertex.label=NA)

zach <- graph("Zachary") # the Zachary carate club
plot(zach, vertex.size=10, vertex.label=NA)


# rewiring a graph
rn.rewired <- rewire(rn, each_edge(prob=0.1))
plot(rn.rewired, vertex.size=10, vertex.label=NA)

rn.neigh = connect.neighborhood(rn, 5)
plot(rn.neigh, vertex.size=8, vertex.label=NA) 

plot(rn, vertex.size=10, vertex.label=NA) 

plot(tr, vertex.size=10, vertex.label=NA) 

plot(rn %du% tr, vertex.size=10, vertex.label=NA) 


################ Reading network data from files #####################
nodes <- read.csv("~/../Desktop/Network Analysis Learning/Dataset1-Media-Example-NODES.csv")
links <- read.csv("~/../Desktop/Network Analysis Learning/Dataset1-Media-Example-EDGES.csv")

head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

nodes2 <- read.csv("~/../Desktop/Network Analysis Learning/Dataset2-Media-User-Example-NODES.csv")
links2 <- read.csv("~/../Desktop/Network Analysis Learning/Dataset2-Media-User-Example-EDGES.csv",header=T, row.names=1)


############## Turning networks into igraph objects ####################
net <- graph_from_data_frame(d=links,vertices=nodes,directed=T)
class(net)
net

E(net)
V(net)
E(net)$type
V(net)$media

plot(net,edge.arrow.size=.4,vertex.label=V(net)$media)

# simplify the graph
net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net,edge.arrow.size=.4,vertex.label=V(net)$media)

as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="weight")

# Dataset 2
net2 <- graph_from_incidence_matrix(links2)
table(V(net2)$type)

net2.bp <- bipartite.projection(net2)
as_incidence_matrix(net2)  %*% t(as_incidence_matrix(net2)) 
t(as_incidence_matrix(net2)) %*%   as_incidence_matrix(net2)
plot(net2.bp$proj1, vertex.label.color="black", vertex.label.dist=1,
     vertex.size=7, vertex.label=nodes2$media[!is.na(nodes2$media.type)])

plot(net2.bp$proj2, vertex.label.color="black", vertex.label.dist=1,
     vertex.size=7, vertex.label=nodes2$media[ is.na(nodes2$media.type)])

##################### Plotting networks with igraph ##########################
# Plot with curved edges (edge.curved=.1) and reduce arrow size:
plot(net, edge.arrow.size=.4, edge.curved=.1)

# Set edge color to gray, and the node color to orange. 
# Replace the vertex label with the node names stored in "media"
plot(net, edge.arrow.size=.2, edge.curved=0,vertex.color="orange", 
     vertex.frame.color="#555555",
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7) 

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Set node size based on audience size:
V(net)$size <- V(net)$audience.size*0.7

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label.color <- "black"
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12

plot(net) 
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

plot(net, vertex.shape="none", vertex.label=V(net)$media, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")

# color igraph based on source node code
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]
plot(net, edge.color=edge.col, edge.curved=.1) 

# Network Layouts
net.bg <- sample_pa(80) 
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0
plot(net.bg)

plot(net.bg, layout=layout_randomly)

# calculate the vertex coordinates in advance
l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)


# Look at all layouts
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]
par(mfrow=c(3,3), mar=c(1,1,1,1))

for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }

hist(links$weight)
mean(links$weight)
sd(links$weight)

# delete the edges where the weight is less than the cutoff
cut.off <- mean(links$weight) 
net.sp <- delete_edges(net, E(net)[weight<cut.off])
par(mfrow=c(1,1), mar=c(1,1,1,1))
plot(net.sp) 

# separate tie types (hyperlin & mention)
E(net)$width <- 1.5
plot(net, edge.color=c("dark red", "slategrey")[(E(net)$type=="hyperlink")+1],
     vertex.color="gray40", layout=layout.circle)

net.m <- net - E(net)[E(net)$type=="hyperlink"] # another way to delete edges
net.h <- net - E(net)[E(net)$type=="mention"]

# Plot the two links separately:
par(mfrow=c(1,2))
plot(net.h, vertex.color="orange", main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", main="Tie: Mention")

# Interactive plotting with tkplot
tkid <- tkplot(net) # tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
tk_close(tkid, window.close = T)
plot(net, layout=l)

# Other way to represent a network



################## Network and node descriptives ########################
# Density: proportion of present edges from all possible edges in the network
edge_density(net,loops=F)
ecount(net) / (vcount(net)*(vcount(net)-1))

# Reciprocity: the proportion of reciprocated ties (for a directed network)
reciprocity(net)
dyad_census(net) # Mutual, asymmetric, and nyll node pairs
2*dyad_census(net)$mut/ecount(net) # Calculating reciprocity

# Transitivity:
# global - ratio of triangles to connected triples
# local - ratio of triangles to connected triples each vertex is part of
transitivity(net, type="global")  # net is treated as an undirected network
transitivity(as.undirected(net, mode="collapse")) # same as above
transitivity(net, type="local")
triad_census(net) # for directed networks 

# Diameter: longest geodesic distance (length of the shortest path between two nodes)
diameter(net, directed=F, weights=NA)
diameter(net, directed=F)
diam <- get_diameter(net, directed=T)
diam

vcol <- rep("gray40", vcount(net))
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(net))
ecol[E(net, path=diam)] <- "orange" 
# E(net, path=diam) finds edges along a path, here 'diam'
plot(net, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)

# Node degrees: in-degree, out-degree, all or total degress
deg <- degree(net, mode="all")
dev.off()
plot(net, vertex.size=deg*3)
hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")

# Degree distribution
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

# Centrality and centralization
# degree (number ofties)
degree(net, mode="in")
centr_degree(net, mode="in", normalized=T)

# Closeness(centrlity based on distance to others in the graph)
closeness(net, mode="all", weights=NA) 
centr_clo(net, mode="all", normalized=T) 

# Eigenvector (centrality proportional to the sum of connection centralities)
eigen_centrality(net, directed=T, weights=NA)
centr_eigen(net, directed=T, normalized=T) 

# Betweeness (centrality based on a broker position connecting others)
betweenness(net, directed=T, weights=NA)
edge_betweenness(net, directed=T, weights=NA)
centr_betw(net, directed=T, normalized=T)

# hubs and authorities
hs <- hub_score(net, weights=NA)$vector
as <- authority_score(net, weights=NA)$vector
par(mfrow=c(1,2))
plot(net, vertex.size=hs*50, main="Hubs")
plot(net, vertex.size=as*30, main="Authorities")
dev.off()

######################### Distance and Paths #################################
# average path length between each pair of nodes in the network
mean_distance(net, directed=F)
mean_distance(net, directed=T)

# length of all shortest path
distances(net) # with edge weights
distances(net, weights=NA) # ignore weights






##################### Subgroups and communities #########################
net.sym <- as.undirected(net, mode= "collapse",
                         edge.attr.comb=list(weight="sum", "ignore"))

# Cliques
cliques(net.sym) # list of cliques       
sapply(cliques(net.sym), length) # clique sizes
largest_cliques(net.sym) # cliques with max number of nodes

vcol <- rep("grey80", vcount(net.sym))
vcol[unlist(largest_cliques(net.sym))] <- "gold"
plot(as.undirected(net.sym), vertex.label=V(net.sym)$name, vertex.color=vcol)

# Community detection
# community detection based on edge betweeness
ceb <- cluster_edge_betweenness(net)
dendPlot(ceb, mode="hclust")
plot(ceb, net)

class(ceb)
length(ceb)
membership(ceb) # community membership for each node
modularity(ceb) # how modular the graph partitioning is
crossing(ceb,net) # boolean vector: T for edges across community

# Community detection based on based on propagating labels
clp <- cluster_label_prop(net)
plot(clp, net)

# Community detection based on greedy optimization of modularity
cfg <- cluster_fast_greedy(as.undirected(net))
plot(cfg, as.undirected(net))

# plot with built-in plot
V(net)$community <- cfg$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])


# K-core decomposition
# The k-core is the maximal subgraph in which every node has degree of at least k. 
# The result here gives the coreness of each vertex in the network. 
# A node has coreness D if it belongs to a D-core but not to (D+1)-core.
kc <- coreness(net, mode="all")
plot(net, vertex.size=kc*6, vertex.label=kc, vertex.color=colrs[kc])



#################### Assortativity and Homophily ##########################
# Homophily: the tendency of nodes to connect to others who are similar on some variable.



