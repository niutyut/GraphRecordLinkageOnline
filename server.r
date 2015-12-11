library(shiny)
library(igraph)
library(RNeo4j)
library(networkD3)
library(htmlwidgets)
library(shinythemes)
library(dplyr)
library(ggplot2)



rm(list=ls())

# graph = startGraph("http://localhost:7474/db/data/")
# #query = "MATCH (n)-[r]-(b) RETURN id(n), n.person_ID,r.prob,id(b)"
# 
# 
# 
# edgequery = "
# MATCH (n)-[r]->(m)
# 
# RETURN ID(n) as from , ID(m) as to ,r.prob as weight 
# "
# 
# nodequery = "
# MATCH (n)
# RETURN DISTINCT ID(n) as nodeid , n.person_ID as personID , n.source as set_AB , 
# n.forename as forename , n.surname as surname,n.pcode_1,n.pcode_2,n.sex_1,n.sex_2
# "
# 
# 
# edgelist<-cypher(graph, edgequery)
# nodelist<-cypher(graph, nodequery)
# 
# #numofconns<- cypher(graph, numofconsquery)
# 
# 
# edgeDF <-data.frame(edgelist)
# 
# edgeDF$from <- (edgeDF$from - 1319)
# edgeDF$to <- (edgeDF$to - 1319)
# 
# 
# nodeDF <-data.frame(nodelist)
# nodeDF$nodeid <- (nodeDF$nodeid - 1319)
# 
# nodeDF$set_AB <-   factor(nodeDF$set_AB)
# nodeDF$nodesize=6
# 
# 
# nsz<-10


##ROC_DF<-data.frame(thres=c(0:10)/10, t=c(0,1:9,1),f= c(0,1:9,1) )
##plot (ROC_DF$f,ROC_DF$t)



#write.csv(nodeDF,row.names = FALSE,  sep = ",",
#            col.names = TRUE,
#            file = "nodedf.csv", append = FALSE)

#write.csv(edgeDF,row.names = FALSE,  sep = ",",
#            col.names = TRUE,
#            file = "edgedf.csv", append = FALSE)




#nodeDF <- read.csv(file = "nodedf.csv" ,sep=",", header = TRUE)
#edgeDF <- read.csv(file = "edgedf.csv" ,sep=",", header = TRUE)


#server init stuff above this line
#--------------------------------------------------------------------------------------------------  




shinyServer(function(input, output) {
  
  output$text <-  renderUI({ 
    
#     HTML(paste(EBtext))
#     ig = graph_from_data_frame(EdgedataInput(),vertices=nodeDF, directed=F)
#     #ig = graph_from_data_frame(edgeDF,vertices=nodeDF, directed=F)
#     modules <- decompose.graph(ig)
#     largest <- which.max(sapply(modules, vcount))
#     #subgrs <- data.frame(sapply(modules, vcount))
#     
#     
#     
#     
#     
#     adjmatr <- as.matrix(get.adjacency(modules[[largest]],attr="weight"))
#     g2 <- graph.adjacency(adjmatr, weighted=TRUE, mode = "undirected")
#     clustdens <-graph.density(g2) #g2
#     clustdiam <- diameter(g2,directed = FALSE, unconnected = TRUE , weights=NA)  
#     clustcount<- vcount(g2)
#     
#  
#     wc <- cluster_edge_betweenness(g2,weights = E(g2)$weight
#                                      ,directed = FALSE,bridges = TRUE,modularity = TRUE)
#     
#     re<-data.frame(crossing(wc, g2))
#     
#     
#     d_fr <-  gsub(pattern = "\\|[0-9]+", replacement = "", rownames(re))  
#     d_to <-  gsub(pattern = "[0-9]+\\|", replacement = "", rownames(re))   
#     brcut <- re$crossing.wc..g2. 
   
    #brmatch <- filter( EdgedataInput() ,edgeDF$from =! d_fr && edgeDF$to =! d_to)
    
    
    
    #final<-paste0(d_fr,"-",d_to,"=",brcut)
    #HTML(paste(final))
    HTML(paste(EBtext))
    
    #HTML(paste(EBtext,". This cluster is made of ",clustcount," nodes. <b> Its clust.diameter is " , 
    #            clustdiam," & its clust.density is ",clustdens))
    
    })
  
  
  
  output$instrtext <- renderUI({ HTML(paste(Insttext))   })
  
  MyClickScript <- 
    '      d3.select(this).select("circle").transition()
  .duration(450)
  .attr("r", 90);
  Shiny.onInputChange("nodeclicked", number=d.index );
  '
  #--------------------------------------------------------------------------------------------------  
  
  EdgedataInput <- reactive({ 
    
    filter(edgeDF, weight<=1, weight>=input$LT)  

    })
  
  #--------------------------------------------------------------------------------------------------  

  #
  
  #--------------------------------------------------------------------------------------------------  
  
  output$force <- renderForceNetwork({
  
    
  forceNetwork ( Links = EdgedataInput() , Nodes = nodeDF,
                 Source = "from", Target = "to" ,
                 Value = "match", NodeID = "personID",Group = "set_AB" ,
                 
                 
                 linkDistance = JS('function(d) {', 'return Math.sqrt(d.value*20) ;', '}'),
                 
                 linkWidth = JS("function(d) { return Math.sqrt(d.value*3.5); }"),  charge= -500,
                 
                 legend = T,
                 opacity = 0.95 ,opacityNoHover = 0.66, zoom = T, bounded = F, clickAction = MyClickScript)
  })

  #--------------------------------------------------------------------------------------------------  
  output$nodeinfo <-DT::renderDataTable ({
    
    
    DT::datatable(nodeDF[ which(nodeDF$nodeid== input$nodeclicked) , ], rownames = FALSE, 
                  caption = "node info", filter = c("none"))
    
    
  })
  
  #--------------------------------------------------------------------------------------------------   
  
  
  #--------------------------------------------------------------------------------------------------  
  
  output$mytable <- DT::renderDataTable({DT::datatable(nodeDF, rownames = FALSE)  })
  #--------------------------------------------------------------------------------------------------  
  output$mytableE <- DT::renderDataTable({DT::datatable(EdgedataInput(), rownames = FALSE) })
    
  #Create Confusion Matrix of Predictions
  #--------------------------------------------------------------------------------------------------  
  output$confusionmatrix <- renderText({
    thr<- input$LT
#     
#     A.     N.     Y.          
#     P
#     N.     1.     2.
#     
#     Y.     3.     4.
#     
#     
#     
#     MxN = all
#     
#     1. All - count (relationships where id1=id2)
#     2. (Id1=id2) but no relationship 
#     3. (Id1<>id2) but relationship
#     4. (Id1=id2) and relationship 

    
    TPquery = paste0("
                     MATCH yy=(a)-[r]->(b)
                     WHERE a.source= 'a' AND b.source='b' AND a.person_ID=b.person_ID AND r.prob >= ",thr," 
                     RETURN count(yy)
                     ")

    TP<-cypher(graph, TPquery)
    
    
    FNquery = "
    MATCH YN=(a),(b) WHERE a.source= 'a' AND b.source='b' AND a.person_ID=b.person_ID AND (not(a-[:MATCHPROB]->(b))) 
    RETURN count(YN) 
    "
    
    FN<-cypher(graph, FNquery)
    
    FPquery = paste0("
    MATCH NY=(a)-[r]->(b)
    WHERE a.source= 'a' AND b.source='b' AND a.person_ID<>b.person_ID AND r.prob >= ",thr," 
    RETURN count(NY)
    ")
    
    FP<-cypher(graph, FPquery)
    
    allquery = "
    MATCH all=(a),(b)
    WHERE a.source= 'a' AND b.source='b' 
    RETURN count(all)
    "
    all<-cypher(graph, allquery)
    
    TNquery = "
    MATCH TN=(a),(b) WHERE a.source= 'a' AND b.source='b' AND a.person_ID<>b.person_ID AND (not(a-[:MATCHPROB]->(b))) 
    RETURN count(TN) 
    "
    
    TN<- cypher(graph, TNquery)
    
    #---------------------------now the calculation of metrics---------------------------------------
    
    #accuracy  <- (TP+TN)/all      " TN:" , TN
    #misclass  <- 1 - accuracy
    TPR       <- TP/(FN+TP)    # Recall or Sensitivity
    FPR       <- TN/(FP+TN)    # False Positive rate
    precision <- TP/(FP+TP)    # precision
    f1        <- 2*TP /  ((2*TP)+FP+FN) 
    
    
    
    
    resultstring1 = paste0(" TP:",TP," FN:",FN," FP:" , FP)
    #resultstring1 = ""
    resultstring2 = paste0( "\n Precision:",precision,"\n Sensitivity/Recall(TPR):" , TPR, 
                           "\n F1 (harmonic mean of precision and sensitivity):",f1
                           ,"\n Specificity:" , 1-FPR)
    
    resultstring=paste0(resultstring1,resultstring2)                   
    resultstring
  
    
    
    })
  
  #--------------------------------------------------------------------------------------------------  
  output$distanceplot <- renderPlot({
   
    ig = graph_from_data_frame(EdgedataInput(), directed=F)
    lengthhist<-path.length.hist (ig)
    
    tab <- as.table(path.length.hist(ig)$res)
    names(tab) <- 1:length(tab)
    barplot(tab,ylab="Nodes", xlab="distances in hops",main= paste0("Distance Histogram"))
    
  })
  
  #--------------------------------------------------------------------------------------------------  
  
  output$EMscoreplot <- renderPlot({
    
    ie = graph_from_data_frame(EdgedataInput(), directed=F)
    hist(E(ie)$weight,ylab="Frequency",breaks=12, xlab="EM normalized score (0-1)",main= paste0("EM Score histogram"))
    

    
    
  })
  
  
  
  
  
  
  output$degreedist <- renderPlot({
    
    ig = graph_from_data_frame(EdgedataInput(), directed=F)
    dd<-degree.distribution(ig) 
    
    
    plot(dd,ylab="Degree Distribution", xlab="node degree",main= paste0("Degree Distribution"))
    lines(degree.distribution(ig)) 
    
  })
  
  #--------------------------------------------------------------------------------------------------  

  output$cliques <- renderPlot   ({
    
    ig = graph_from_data_frame(EdgedataInput(), directed=F)

    comps <- decompose.graph(ig)
    t<-table(sapply(comps, vcount))
    t
    
    plot(t, type = "h", ylim = c(0, max(t)), lwd = 2,
         ylab="frequency", xlab="# of nodes in cluster",main= paste0("cluster size histogram"))
    
  })
  
  output$showsomething <- renderText   ({
    
#   lala<-EdgedataInput()
#   lala
  
  })
  
  output$gmetrics <- renderText   ({ 

    udiameter <- function(gr)
    {
    ud <-   diameter(gr,directed = FALSE, unconnected = TRUE , weights=NA)
      return (ud)
    }
    
    
    ig = graph_from_data_frame(EdgedataInput(), directed=F)
    # (if graph is weighted then with weights=NA function is not taking them into account)    
    uwdiam <- diameter(ig,directed = FALSE, unconnected = TRUE , weights=NA)   
    wtdiam <- diameter(ig,directed = FALSE, unconnected = TRUE )   
    avgpl <- average.path.length(ig)
    numofE<-length(E(ig))
    dens <-graph.density(ig)
    numofV<-length(V(ig))
    
    
    #L=graph.laplacian(ig)
    #computes Laplacian L of graph
    #evals=eigen(L)
    
    # computes eigenvectors and eigenvalues of L
    
    #ev=evals$vec
    #y=ev[,numofE-1]>0
    
      
    #ev
    #y
    
    
    #@\n EV:",y
    
    
    #sumofweights<-sum(E(ig$weight))
    #,"\n sumofW:",sumofweights
    
    #gc <- graph.closeness<-closeness(ig,vids=V(ig))
    
  
#     modules <- decompose.graph(ig)
#     sapply(modules, udiameter)
#     sizes(modules)
    
    resultstring = paste0(" #E:",numofE," #V:",numofV,"\n unw.diameter:" , uwdiam," w.diameter:" , 
                       wtdiam,"\n avg path length:" ,avgpl,"\n density:",dens)
    resultstring
    
    })
  
  #--------------------------------------------------------------------------------------------------  
  
  plotBiggestCluster = function() {
    
    ig = graph_from_data_frame(EdgedataInput(), directed=F)
    #ig = graph_from_data_frame(edgeDF, directed=F)
    modules <- decompose.graph(ig)
    largest <- which.max(sapply(modules, vcount))
    adjmatr <- as.matrix(get.adjacency(modules[[largest]],attr="weight"))
    g2 <- graph.adjacency(adjmatr, weighted=TRUE, mode = "undirected")
    
    
    V(g2)$label <- paste0(nodeDF[as.character(V(g2)$name),'set_AB'],":\n",nodeDF[as.character(V(g2)$name),'personID'],":\n",as.character(V(g2)$name) )                   
    V(g2)$shape <- ifelse ( nodeDF[as.character(V(g2)$name),'set_AB']=="a","rectangle","sphere") 
    #V(g2)$type <- ifelse ( nodeDF[as.character(V(g2)$name),'set_AB']=="a",FALSE,TRUE) 
    
    biggest<- ends(g2,E(g2))
    E(g2)$lty   <- ifelse (nodeDF[as.character(biggest[,1]),'personID'] == nodeDF[as.character(biggest[,2]) ,'personID'] ,1,2)
    thickness   <- ifelse (nodeDF[as.character(biggest[,1]),'personID'] == nodeDF[as.character(biggest[,2]) ,'personID'] ,5.5,1.5)
    
    
    #weights = E(g2)$weight
    
    
    wc <- edge.betweenness.community(g2,weights = NULL
                                     ,directed = FALSE,bridges = TRUE)

    L = layout_with_fr(g2)
    plot(wc,g2,layout=L,vertex.size=10, edge.width = thickness,
         edge.label = E(g2)$weight,vertex.label.cex = 2.2) 
    
    
  }
  
  
  #--------------------------------------------------------------------------------------------------  
  
  
  output$dlb <- downloadHandler  ( 

    filename = 'test.png',
    content = function(file) {
      png(file,width = 2*2048,height = 2*1536 )
      plotBiggestCluster()
      dev.off()
    
    
      })
  
  
  
  
  output$clusters <-renderPlot   ({
    
    
    ig = graph_from_data_frame(EdgedataInput(), directed=F)
   
    #ig = graph_from_data_frame(edgeDF, directed=F)
    modules <- decompose.graph(ig)
    
    
    largest <- which.max(sapply(modules, vcount))
    adjmatr <- as.matrix(get.adjacency(modules[[largest]],attr="weight"))
    g2 <- graph.adjacency(adjmatr, weighted=TRUE, mode = "undirected")
    
    
     
  
    
     #V(g2)$label <- paste0(as.character(V(g2)$name),":",nodeDF[as.character(V(g2)$name),'personID'] )    
     V(g2)$label <- paste0(nodeDF[as.character(V(g2)$name),'set_AB'],":\n",nodeDF[as.character(V(g2)$name),'personID'],
                           ":\n",as.character(V(g2)$name) )  
     V(g2)$shape <- ifelse ( nodeDF[as.character(V(g2)$name),'set_AB']=="a","rectangle","sphere") 
     V(g2)$type <- ifelse ( nodeDF[as.character(V(g2)$name),'set_AB']=="a",FALSE,TRUE) 
    
     biggest<- ends(g2,E(g2))
     E(g2)$lty   <- ifelse (nodeDF[as.character(biggest[,1]),'personID'] == nodeDF[as.character(biggest[,2]) ,'personID'] ,1,2)
     thickness   <- ifelse (nodeDF[as.character(biggest[,1]),'personID'] == nodeDF[as.character(biggest[,2]) ,'personID'] ,5.5,1.5)
     
     
     
     
     
    clustdens <-graph.density(g2) #g2
    clustdiam <- diameter(g2,directed = FALSE, unconnected = TRUE , weights=NA)  
    
    
   # E(g2)$weight
    
    wc <- edge.betweenness.community(g2,weights = NULL
                                       ,directed = FALSE,bridges = TRUE)
    
    #siz<-sizes(wc)
    
    #br<-wc$bridges  
    
    
    
    #dendPlot(wc, mode="hclust", use.modularity= TRUE,rect = 5)
    #plot(rev(wc$bridges), xlab = 'Number of nodes', ylab = 'Modularity value')
    
    #as.dendrogram(wc, hang = -1,
    #              use.modularity = FALSE)
    

    #L=layout_as_tree (g2)
    L = layout_with_fr(g2)
    
    #rs = paste0("clust.diameter:" , clustdiam," clust.density:",clustdens)
    plot(wc,g2,layout=L,edge.label.cex=0.80,vertex.size= 17,edge.width = thickness*E(g2)$weight,
         edge.label = E(g2)$weight,vertex.label.cex = 0.8) 
    
   
    
    
    #wc$bridges
    
  })
  
 
#   
#   
 
})
