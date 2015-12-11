library(RCurl)
EBtext<-"A community detection algorithm is run on the biggest cluster of our (disconnected) graph.
On this particular instance the communities are seperated according to <b>edge betweeness</b>, a graph metric that 
creates communities of nodes which are densely connected themselves but sparsely connected to other communities of nodes.
It is one of the metrics that we want to explore if they are appropriate for cutting spurious links (False Positives)"


LEtext<-"A community detection algorithm is run on the biggest cluster of our (disconnected) graph.
On this particular instance the communities are seperated according to <b>leading  eigenvectors</b> metric, 
With this method we try to find densely connected subgraphs in a graph by calculating the leading non-negative eigenvector of the modularity matrix of the graph. 
We want to explore the suitability of this metric for cutting spurious links (False Positives)"


Insttext <-"In the Visualization tab :</p>
- You can zoom in / out (middle wheel on mouse) </p>
- By clicking and moving you can go left/right up/down too. </p>
- Click on nodes and get info. </p>
- Change the threshold at which two nodes are linked with the slider. </p>
- Real matches have a thicker line connecting the nodes. Possible links have a thin line</p>

In the Clustering tab :</p>

- Change the threshold at which two nodes are linked with the slider. </p>
- Press Download button to save a bigger resolution / less cramped image of the clustering of the largest cluster.</p>
- Real matches have a solid line connecting nodes. Possible links have a dotted line.Normalised weights from the EM 
  algorithm can be seen above lines.</p>


"





nfile =  "https://gist.githubusercontent.com/mamonu/597c86b9d2a4ebc7da16/raw/09b473cb05ddfe21f6b12bee3342ffa215e257a5/nodedf.csv"
efile =  "https://gist.githubusercontent.com/mamonu/14fec81fdd68d1d9f7be/raw/64f7538cfa8b3b7f5ffdfa8c2ba5bff2b6f216c9/edgedf.csv"

n <- getURL(nfile)
e <- getURL(efile)

nodeDF <- read.csv(textConnection(n), header = T)



edgeDF <-  read.csv(textConnection(e), header = T)
edgeDF$from <- as.numeric(edgeDF$from) 
edgeDF$to <- as.numeric(edgeDF$to) 
edgeDF$weight <- as.numeric(edgeDF$weight) 


row.names(nodeDF)<-nodeDF$nodeid
edgeDF$match <-ifelse(nodeDF[as.character(edgeDF$from),'personID'] == nodeDF[as.character(edgeDF$to),'personID'],1.0,0.1)






#nodeDF <- read.table( file= nurl ,sep=",", header = TRUE)
#edgeDF <- read.table( file= eurl ,sep=",", header = TRUE)


