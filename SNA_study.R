library(igraph)
library(statnet)

# install these packages and restart R studio. (Otherwise it might cause errors)
#install.packages("igraph", "statnet")

# Set Working Directory
#setwd("~/Documents/Study")


# 1

#fradj = read.delim("Krack-High-Tec-ADVICE.tab", row.names = 1)
fradj = as.matrix(read.delim("http://stanford.edu/~messing/Krack-High-Tec-ADVICE.tab", header = TRUE, row.names = 1))

colnames(fradj) = 1:21

frnet = graph.adjacency(fradj)
frnet
plot.igraph(frnet)
plot.igraph(frnet, layout=layout.fruchterman.reingold)

V(frnet)$name
V(frnet)$label = letters[as.numeric(V(frnet)$name)]
plot.igraph(frnet, layout=layout.fruchterman.reingold)


# 2
advadj = as.matrix(read.delim("Data/Krack-High-Tec-ADVICE.tab", row.names = 1))
colnames(advadj) = 1:21
rptadj = as.matrix(read.delim("Data/Krack-High-Tec-REPORTS_TO.tab", row.names = 1))
colnames(rptadj) = 1:21

advnet = graph.adjacency(advadj)
rptnet = graph.adjacency(rptadj)

length(E(advnet))
length(E(rptnet))

la = layout.fruchterman.reingold( advnet )  
plot.igraph( advnet, layout = la )

E(advnet)$weight <- count.multiple(advnet)
advnet <- simplify(advnet)
la = layout.fruchterman.reingold( advnet, weights = E( advnet )$weight )   
plot.igraph( advnet, layout = la )
# Check how to show edge weight


# 3
df <- data.frame( person = c('Sam','Sam','Sam','Greg','Tom','Tom','Tom','Mary','Mary'), group = c('a','b','c','a','b','c','d','b','d'), stringsAsFactors = F)
M = as.matrix( table(df) )
Mrow = M %*% t(M)
Mcol = t(M) %*% M

magact96 = read.delim("http://stanford.edu/~messing/mag_act96.txt", na.strings = "na")
magact97 = read.delim("http://stanford.edu/~messing/mag_act97.txt", na.strings = "na")
magact98 = read.delim("http://stanford.edu/~messing/mag_act98.txt", na.strings = "na")

magattrib = magact96[,1:4]

g96 = as.matrix(magact96[,-(1:4)])
row.names(g96) = magact96$ID
g97 = as.matrix(magact97[,-(1:4)])
row.names(g97) = magact97$ID
g98 = as.matrix(magact98[,-(1:4)])
row.names(g98) = magact98$ID

i96 <- graph.incidence(g96, mode=c("all") )
i97 <- graph.incidence(g97, mode=c("all") )
i98 <- graph.incidence(g98, mode=c("all") )

V(i96)$color[1:1295] = rgb(1,0,0,.5)
V(i96)$color[1296:1386] = rgb(0,1,0,.5)

V(i96)$label = V(i96)$name
V(i96)$label.color = rgb(0,0,.2,.5)
V(i96)$label.cex = .4
V(i96)$size = 6
V(i96)$frame.color = NA

E(i96)$color = rgb(.5,.5,0,.2)

pdf("i96.pdf")
plot(i96, layout=layout.fruchterman.reingold)
dev.off()


#i96 = delete.vertices(i96, V(i96)[ degree(i96)==0 ]) # got an error
V(i96)$label[1:857] = NA
V(i96)$color[1:857] =  rgb(1,0,0,.1)
V(i96)$size[1:857] = 2 

E(i96)$width = .3
E(i96)$color = rgb(.5,.5,0,.1)

pdf("i96_2.pdf")
plot(i96, layout=layout.kamada.kawai)
dev.off()

pdf("i96_3.pdf")
plot(i96, layout=layout.fruchterman.reingold.grid)
# looks grid fruchterman is removed from the pkg.
dev.off()

pdf("i96_4.pdf")
plot(i96, layout=layout.fruchterman.reingold)
dev.off()





g96e = t(g96) %*% g96
g97e = t(g97) %*% g97
g98e = t(g98) %*% g98

i96e = graph.adjacency(g96e, mode = "undirected")

E(i96e)$weight <- count.multiple(i96e)
i96e <- simplify(i96e)

# Set vertex attributes
V(i96e)$label = V(i96e)$name
V(i96e)$label.color = rgb(0,0,.2,.8)
V(i96e)$label.cex = .6
V(i96e)$size = 6
V(i96e)$frame.color = NA
V(i96e)$color = rgb(0,0,1,.5)

# Set edge gamma according to edge weight
egam = (log(E(i96e)$weight)+.3)/max(log(E(i96e)$weight)+.3)
E(i96e)$color = rgb(.5,.5,0,egam)

pdf("i96e.pdf")
plot(i96e, main = "layout.kamada.kawai", layout=layout.kamada.kawai)
plot(i96e, main = "layout.fruchterman.reingold", layout=layout.fruchterman.reingold) 
dev.off()





ol96 = g96e/diag(g96e)
ol97 = g97e/diag(g97e)
ol98 = g98e/diag(g98e)

magall = ol96 + ol97 + ol98
magall[is.na(magall)] = 0

magdiag = apply(cbind(diag(g96e), diag(g97e), diag(g98e)), 1, mean ) 

magallg = graph.adjacency(magall, weighted=T)

# Degree 
#V(magallg)$degree = degree(magallg) #got an error

# Betweenness centrality
#V(magallg)$btwcnt = betweenness(magallg) #got an error

plot(density(magall))

magallgt1 = magall
magallgt1[magallgt1<1] = 0
magallggt1 = graph.adjacency(magallgt1, weighted=T)

# Removes loops:
magallggt1 <- simplify(magallggt1, remove.multiple=FALSE, remove.loops=TRUE)

magallggt1$layout = layout.fruchterman.reingold(magallggt1)
V(magallggt1)$label = V(magallggt1)$name
tkplot(magallggt1)



magallggt1$layout = tkplot.getcoords(1)

# Set vertex attributes
V(magallggt1)$label = V(magallggt1)$name
V(magallggt1)$label.color = rgb(0,0,.2,.6)
V(magallggt1)$size = 6
V(magallggt1)$frame.color = NA
V(magallggt1)$color = rgb(0,0,1,.5)

# Set edge attributes
E(magallggt1)$arrow.size = .3

# Set edge gamma according to edge weight
egam = (E(magallggt1)$weight+.1)/max(E(magallggt1)$weight+.1)
E(magallggt1)$color = rgb(.5,.5,0,egam)

V(magallggt1)$label.cex = V(magallggt1)$degree/(max(V(magallggt1)$degree)/2)+ .3
#note, unfortunately one must play with the formula above to get the 
#ratio just right

pdf("magallggt1customlayout.pdf")
plot(magallggt1)
dev.off()
