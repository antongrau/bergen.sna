# Functions for Social Network Analysis


#########################################################################
### Create adjacency

adj.ind <- function(rel){
  netmat           <- data.frame(rel$NAME, rel$ORG)
  colnames(netmat) <- c("name", "org")
  
  ### Nu laves netværksobjekterne
  tabnet           <- table(netmat)
  tabnet           <- Matrix(tabnet)
  adj              <- tabnet%*%t(tabnet) # Individ*individ
  return(adj)
}

adj.org <- function(rel){
  netmat <- data.frame(rel$NAME, rel$ORG)
  colnames(netmat) <- c("name", "org")
  
  ### Nu laves netværksobjekterne
  tabnet          <- table(netmat)
  tabnet          <- Matrix(tabnet)
  adj             <- t(tabnet)%*%tabnet # Org*Org
  return(adj)
}

## Two-mode network
two.mode <- function(rel, weighted=TRUE, directed=TRUE, ... ){
  
  netmat           <- droplevels(data.frame(rel$NAVN_MATCH, rel$ORG_NAVN))
  colnames(netmat) <- c("navn", "org")
  
  ### Nu laves netværksobjekterne
  tabnet          <- table(netmat)
  tabnet          <- Matrix(tabnet)
  
  graph.incidence(tabnet, weighted=weighted, directed=directed, ...)
}

##################################################
###  Graph plot

graph.plot <- function(graph, layout=layout.fruchterman.reingold(graph),
                       vertex.color="black", vertex.fill="grey60", vertex.shape=21, vertex.size=3, vertex.alpha=1,
                       edge.color="black", edge.alpha=0.2, edge.size=1, edge.line="solid",
                       text=FALSE, text.size=3, text.colour="black", text.alpha=1, legend="side", text.vjust=1.5){
  
  edge.coords             <- edge.coord(graph, layout)
  vertex.coords           <- as.data.frame(vertex.coord(graph, layout))
  
  vertex.l                <- list(color=vertex.color, fill=vertex.fill, shape=vertex.shape, size=vertex.size, alpha=vertex.alpha)
  v.i                     <- unlist(lapply(vertex.l, length)) == 1
  vertex.attributes       <- vertex.l[v.i]
  vertex.aes              <- vertex.l[v.i==FALSE]
  vertex.aes$x            <- vertex.coords$x
  vertex.aes$y            <- vertex.coords$y
  
  edge.l                  <- list(color=edge.color, alpha=edge.alpha, size=edge.size, linetype=edge.line)
  e.i                     <- unlist(lapply(edge.l, length)) == 1
  edge.attributes         <- edge.l[e.i]
  edge.attributes$lineend <- "butt"
  edge.aes                <- edge.l[e.i==FALSE]
  edge.aes$x              <- edge.coords$start.x
  edge.aes$y              <- edge.coords$start.y
  edge.aes$xend           <- edge.coords$slut.x
  edge.aes$yend           <- edge.coords$slut.y
  
  text.l                  <- list(size=text.size, color=text.colour, alpha=text.alpha, vjust=text.vjust)
  t.i                     <- unlist(lapply(text.l, length)) == 1
  text.attributes         <- text.l[t.i]
  text.aes                <- text.l[t.i==FALSE]
  text.aes$x              <- vertex.coords$x
  text.aes$y              <- vertex.coords$y
  text.aes$label          <- rownames(vertex.coords)
  
  # Plot edges
  p <- ggplot()
  
  edge.attributes$mapping     <- do.call("aes", edge.aes)
  p <- p + do.call("geom_segment", edge.attributes, quote=TRUE)
  
  # Plot vertices
  vertex.attributes$mapping     <- do.call("aes", vertex.aes)
  p <- p + do.call("geom_point", vertex.attributes, quote=TRUE)
  
  # Plot text
  if(text==TRUE){
    text.attributes$mapping     <- do.call("aes", text.aes)
    p <- p + do.call("geom_text", text.attributes, quote=TRUE)
  }
  
  # Formatting
  p <- p + theme_bw()
  p <- p + labs(alpha="Alpha", shape="Shape", color="Color", linetype="Linetype", size="Size", fill="Fill")
  
  if(legend=="bottom")  p <- p + theme(legend.position='bottom', legend.direction="horizontal", legend.box="horizontal")
  
  p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
  
  
  
}




##################################
# Data object for plotting

edge.coord <- function(graph, layout){
  
  rownames(layout)  <- V(graph)$name
  vertex.coord            <- as.data.frame(layout, rownames(layout))
  colnames(layout)  <- c("x", "y")
  
  e.l <- data.frame(get.edgelist(graph))
  e.l <- cbind(e.l, E(graph)$weight)
  
  mat <- as.data.frame(matrix(ncol=5, nrow=nrow(e.l)))
  colnames(mat) <- c("start.x", "start.y", "slut.x", "slut.y", "weight")
  for(i in (1:nrow(e.l))){
    start       <- as.character(e.l[i,1])
    start.coord <- vertex.coord[rownames(vertex.coord)==start,]
    slut        <- as.character(e.l[i,2])
    slut.coord  <- vertex.coord[rownames(vertex.coord)==slut,]
    mat[i,1:2]  <- start.coord
    mat[i,3:4]  <- slut.coord
    mat[i,5]    <- e.l[i,3]
  }
  mat  
}

vertex.coord <- function(graph, layout=layout.fruchterman.reingold(graph)){
  rownames(layout)  <- V(graph)$name
  layout            <- as.data.frame(layout, rownames(layout))
  colnames(layout)  <- c("x", "y")
  layout
}

###################################################
# Network by variable

network.by.variable <- function(net, variabel){
  variabel <- as.factor(variabel)
  dele <- levels(variabel)
  output <- matrix(nrow=20, ncol=length(dele)) # Output matrix
  for ( i in 1:length(dele)){
    del <- dele[i]
    del.ind <- which(variabel==del)
    del.not <- which(variabel!=del)
    net.del             <- net - del.not
    
    # Antal Vertices
    Number.of.vertices  <- length(del.ind)
    # Antal edges
    Number.of.edges     <- sum(degree(net)[del.ind])
    # Average degree
    Average.degree      <- round(Number.of.edges/Number.of.vertices, 1)
    # Part density i 1000
    Part.density        <- round(Number.of.edges/((Number.of.vertices*(vcount(net)-1)/2))*1000, 1)
    # Clusters in part
    Number.of.clusters.in.del  <- clusters(net.del)$no
    
    # Average path length total network
    sp                  <- shortest.paths(net)
    ind.av.sp           <- rowSums(sp)[del.ind]/ncol(sp)
    Average.path.length <- round(sum(ind.av.sp)/length(del.ind),1)
    # Average path length within group
    sp.del                  <- shortest.paths(net)[del.ind,del.ind]
    ind.av.sp.del           <- rowSums(sp.del)/length(del.ind)
    Average.path.length.del <- round(sum(ind.av.sp.del)/length(del.ind),1)
    
    # Longest path within group
    Longest.path.del    <- max(sp.del)
    
    # Largest number of degrees
    Largest.degree <- max(degree(net)[del.ind])
    # Largest degree in part
    Largest.degree.del <-max(degree(net.del))
    # Largest 2 neighborhoods
    Largest.2.neighborhood <- max(neighborhood.size(net, 2)[del.ind])
    # Largest 3 neighborhoods
    Largest.3.neighborhood <- max(neighborhood.size(net, 3)[del.ind])
    
    # Average closeness whole network * 10000
    Average.closeness.network    <- round(sum(closeness(net)[del.ind])/length(del.ind) * 10000, 1)
    # Average closeness part
    Average.closeness.part       <- round(sum(closeness(net.del))/length(del.ind) * 10000, 1)
    # Average betweenness whole network
    Average.betweenness.network  <- round(sum(betweenness(net)[del.ind])/length(del.ind))
    # Average betweeness part
    Average.betweenness.part     <- round(sum(betweenness(net.del))/length(del.ind))
    # Maximum betweeness whole network
    Maximum.betweenness          <- max(betweenness(net)[del.ind])
    # Maximum closeness whole network * 10000
    Maximum.closeness            <- round(max(closeness(net)[del.ind]) * 10000, 1)
    # Average eigenvector centrality * 1000
    Average.eigen.network        <- round(sum(evcent(net)$vector[del.ind])/length(del.ind) * 1000, 1)
    # Maximum eigenvector centrality
    Maximum.eigen                <- round(max(evcent(net)$vector[del.ind])* 1000, 1)
    
    del.stat <- c(Number.of.vertices, Number.of.edges, Average.degree, Part.density, Number.of.clusters.in.del,
                  Average.path.length, Average.path.length.del, Longest.path.del, Largest.degree, Largest.degree.del,
                  Largest.2.neighborhood, Largest.3.neighborhood,
                  Average.closeness.network, Average.closeness.part, Maximum.closeness,
                  Average.betweenness.network, Average.betweenness.part, Maximum.betweenness,
                  Average.eigen.network, Maximum.eigen)
    
    
    output[,i] <- round(del.stat, 1)
  }
  colnames(output) <- dele
  rownames(output) <- c("Number of vertices", "Number of edges", "Average degree", "Part density (o/oo)", "Number of clusters in part",
                        "Average path length", "Average path length in part", "Longest path in part", "Highest degree", "Highest degree in part",
                        "Largest 2. neighborhood", "Largest 3. neighborhood",
                        "Average closeness", "Average closeness in part", "Maximum closeness",
                        "Average betweeness", "Average betweenness in part", "Maximum betweenness",
                        "Average eigencentrality", "Maximum eigencentrality")
  return(output)
  # Net er et igraph object
  # Variabel er en factor i samme længde og orden som den adjacency matrice net er lavet fra
}