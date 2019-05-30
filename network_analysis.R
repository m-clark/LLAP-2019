
# Network analysis --------------------------------------------------------


library(igraph) # If you haven't installed this already, then run install.packages("igraph")

# GENERATE 2-MODE NETWORK FOR ALL INVESTIGATORS, 2016-2018
# Create 2-mode edgelist, then reshape it to a matrix
alledges <- read.csv("data/all_2mode.csv", header=T, as.is=T)
adj <- reshape(alledges[,-1], v.names = "tiepresent", idvar = "awdid", 
               timevar = "personid", direction = "wide")
adj[is.na(adj)] <- 0

# Create node list with attributes
allnodes <- read.csv("data/all_2mode nodes.csv", header=T, as.is=T)

# Generate 2-mode network from the matrix above
net <- graph_from_incidence_matrix(adj)

# Decompose the 2-mode award-person network to two one-mode networks; award-award (proj1) and person-person (proj2)
net.bp <- bipartite.projection(net)

# Save the two one-mode networks for analysis and plots
awdnet <- net.bp$proj1
pplenet <- net.bp$proj2

#Basic plot of one-mode person-person network
plot(pplenet, vertex.size=4, vertex.label=NA, layout=layout_with_fr)


# GENERATE 2-MODE NETWORK FOR ONLY INVESTIGATORS THAT USED THE LIBRARY, 2016-2018
# Create 2-mode edgelist, then reshape it to a matrix
libuseredges <- read.csv("data/libuser_2mode.csv", header=T, as.is=T)
adj2 <- reshape(libuseredges[,-1], v.names = "tiepresent", idvar = "awdid", 
                timevar = "personid", direction = "wide")
adj2[is.na(adj2)] <- 0

# Create node list with attributes
libusernodes <- read.csv("data/libuser_2mode nodes.csv", header=T, as.is=T)

# Generate 2-mode network from the matrix above
net2 <- graph_from_incidence_matrix(adj2)

# Decompose the 2-mode award-person network to two one-mode networks
net2.bp <- bipartite.projection(net2)

# Save the two one-mode networks
awdnet2 <- net2.bp$proj1
pplenet2 <- net2.bp$proj2

#Basic plot of one-mode person-person network
plot(pplenet2, vertex.size=4, vertex.label=NA, layout=layout_with_fr)

# COMPARE THE TWO NETWORKS
# Generate edge density...this runs quickly
edge_density(pplenet, loops=F) # 0.0004684331
edge_density(pplenet2, loops=F) # 0.0007458001
0.0007458001/0.0004684331 # 1.592117

# EXPLORATORY COMMUNITY DETECTION IN THE TWO NETWORKS
# Let's try an algorithm that runs relatively quickly...like greedy optimization of modularity
# all investigators
clfastgr <- cluster_fast_greedy(as.undirected(pplenet))
plot(clfastgr, vertex.size=4, vertex.label=NA, layout=layout_with_fr, as.undirected(pplenet))

# library user investigators
clfastgr2 <- cluster_fast_greedy(as.undirected(pplenet2))
plot(clfastgr2, vertex.size=4, vertex.label=NA, layout=layout_with_fr, as.undirected(pplenet2))
