#####################################################
##  Corporate example                              ##
#####################################################


#####################################################
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
rel.corp     <- read.csv(file = "Corporations_edgelist.csv", sep = "|", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# Having a quick look at our dataset
str(rel.corp)

#####################################################
## 2. Creating a igraph network object

edge.affialiation.list    <- data.frame(name = rel.corp$NAME, org = rel.corp$ORG_NAME)
str(edge.affialiation.list)

# Creating a incidence matrix - sometimes called a case - affiliation matrix
incidence.matrix        <- table(edge.affialiation.list)

# Converting the incidence.matrix to a sparse matrix
incidence.matrix        <- Matrix(incidence.matrix)

# Creating an individual * individual adjacency matrix
adjacency.ind           <- incidence.matrix %*% t(incidence.matrix)
str(adjacency.ind)

# Creating an igraph network
graph.ind              <- graph.adjacency(adjacency.ind, mode = "undirected", weighted = TRUE, diag = FALSE)
graph.ind

###################
# Exercise:
# Find and plot degree and betweenness for this network


#####################################################
## 3. Components

# Making a layout for plotting a graph
layout                 <- layout.fruchterman.reingold(graph.ind)

# Plotting the entire network
graph.plot(graph.ind, layout = layout, edge.alpha = E(graph.ind)$weight, vertex.size = degree(graph.ind))

# Finding the components
components             <- clusters(graph.ind)
str(components)

# Identifying the largest component
sort(components$csize)
max.com                <- which.max(components$csize)
max.com

# Who are NOT members of the largest component
components$membership
member.component       <- which(components$membership != max.com)

# Removing the unconnected
graph.com              <- graph.ind - member.component
graph.com

#####################################################
## 4. Linkers

# Finding the number of board memberships for the largest component members
memberships     <- diag(adjacency.ind)
memberships     <- memberships[-which(components$membership != max.com)]
table(memberships)

###################
# Exercise
# Remove all members with less than 2 memberships from graph.com

graph.linkers <- graph.com - which(memberships == 1)

#####################################################
## 5. Fancy plots

# Creating a layout
layout         <- layout.fruchterman.reingold(graph.linkers) 

# Setting the edge attributes
graph.plot(graph.linkers, layout = layout, edge.size = E(graph.linkers)$weight, edge.alpha = E(graph.linkers)$weight, edge.color = "purple")

# The thick lines are awful lets make them thinner by adjusting the scale
graph.plot(graph.linkers, layout = layout, edge.size = E(graph.linkers)$weight,
           edge.alpha = E(graph.linkers)$weight, edge.color = "purple") + scale_size_continuous(range = c(0.5, 2))

# Changing the size and fill of the vertex
graph.plot(graph.linkers, layout = layout, edge.alpha = E(graph.linkers)$weight, edge.color = "purple",
           vertex.fill = betweenness(graph.linkers), vertex.size = degree(graph.linkers))

# The colors are not helpful lets make them a "stylish" purple
graph.plot(graph.linkers, layout = layout, edge.alpha = E(graph.linkers)$weight, edge.color = "purple",
           vertex.fill = betweenness(graph.linkers), vertex.size = degree(graph.linkers)) + scale_fill_continuous(high = "darkblue", low = "papayawhip")

# How about some labels?
graph.plot(graph.linkers, layout = layout, edge.color = "purple",
           vertex.fill = betweenness(graph.linkers), vertex.size = betweenness(graph.linkers),
           text = TRUE, text.alpha = betweenness(graph.linkers), text.size = betweenness(graph.linkers),
           ) + scale_fill_continuous(high = "darkblue", low = "papayawhip") + scale_size_continuous(range = c(0.2, 6))

# Exporting the last plot using ggsave
ggsave(filename = "fancy_plot.pdf", height = 10, width = 12)
ggsave(filename = "fancy_plot.png", height = 10, width = 12)

