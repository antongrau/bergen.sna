#######################################################
## 1. Packages and data

# Setting working directory
setwd("~/bergen.sna/")

setwd("~/My Dropbox/R/bergen.sna/") # For anton

# Loading packages
library(igraph)
library(Matrix)
library(ggplot2)

# Loading functions
source("soc.sna.R")

# Reading data
rel.gov      <- read.csv(file = "Government_edgelist_english.csv", sep = ",", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# Removing blank lines
rel.gov      <- rel.gov[is.na(rel.gov$ORG_ID) == FALSE, ]

# Having a quick look at our dataset
str(rel.gov)

########################################################
## 2. Creating an adjacency matrix

# Creating an edge affiliation list
edge.affialiation.list    <- data.frame(name = rel.gov$NAME, org = rel.gov$ORG)
str(edge.affialiation.list)

# Creating a incidence matrix - sometimes called a case - affiliation matrix
incidence.matrix        <- table(edge.affialiation.list)
View(incidence.matrix)

# Creating an individual * individual adjacency matrix
adjacency.ind           <- incidence.matrix %*% t(incidence.matrix)
View(adjacency.ind)

# Viewing the diagonal which represents the amount of memberships each individual holds
diag(adjacency.ind)

# Creating an organisation * organisation adjacency matrix
adjacency.org           <- t(incidence.matrix) %*% incidence.matrix
View(adjacency.org)

# Viewing the diagonal which represents the number of members in each organisation
diag(adjacency.org)

##########################################################
## 3. Creating and plotting network objects in igraph

# Find help on graph adjacency
?graph.adjacency

# Creating an igraph network object for individuals
graph.ind               <- graph.adjacency(adjacency.ind, mode = "undirected", weighted = TRUE)
graph.ind

# A list of vertexes 
V(graph.ind)

# A list of edges
E(graph.ind)

# Edge weights
E(graph.ind)$weight

# A basic plot
plot(graph.ind)

# Removing loops
graph.ind              <- graph.adjacency(adjacency.ind, mode = "undirected", weighted = TRUE, diag = FALSE)
plot(graph.ind)

# Density
graph.density(graph.ind)

##############################
# Exercise:
# Create an igraph network object with the organisations as vertexes
# Plot the network, access the weights and find the density.

##########################################################
# 4. Analysis and centrality

graph.plot(graph.ind, text = TRUE, edge.alpha = E(graph.ind)$weight, text.size = 6, vertex.size = diag(adjacency.ind))

# Degree centrality
deg                   <- degree(graph.ind)
plot(sort(deg))
sort(deg)

# Betweenness centrality
between               <- betweenness(graph.ind)
plot(sort(between))
sort(between)
plot(deg, between)

##############################
# Exercise:
# Find the closeness centrality of the ministers
# Find the betweeness and degree of the organizations
?igraph

############################################################
# 5. Two-mode networks

graph.two.mode        <- graph.incidence(incidence.matrix, directed=FALSE, weighted = TRUE)
graph.two.mode

degree(graph.two.mode)

# A plot of the two-mode network
graph.plot(graph.two.mode, text=TRUE, vertex.fill=V(graph.two.mode)$type, vertex.size=degree(graph.two.mode))

