library(shiny)
library(shinythemes)
library(igraph)
library(RNeo4j)
library(networkD3)
library(htmlwidgets)



#nodeDF <- read.table(file = "./data/nodedf.csv" ,sep=",", header = TRUE)
#edgeDF <- read.table(file = "./data/edgedf.csv" ,sep=",", header = TRUE)

# selectInput("ddm", "Community detection method :", 
#             choices = c('fastgreedy'='fg','label.propagation'='lp',
#                         'leading.eigenvector.community'='le','optimal.community'='oc',
#                         'spinglass.community'='sc','walktrap.community'='wc',
#                         'fastgreedy'='fg','edge.betweeness'='eb',
#                         'infomap.community'='ic' )


shinyUI(
  
  

  
  fluidPage( theme = shinytheme("cerulean"),
                           titlePanel("Graph Databases Data Linkage Demo :: v.1.7 (2.12.15)"  ),
  sidebarLayout(
    sidebarPanel(width = 3,
      #img(src = "ons.gif", height = 47, width = 239),
      sliderInput("LT", "LowerThreshold", 0.6, min = 0.0,
                       max = 1, step = .01),

     
     plotOutput("distanceplot",width="auto",height=180),plotOutput("EMscoreplot",width="auto",height=180),
     plotOutput("cliques",width="auto",height=180) ,verbatimTextOutput("gmetrics")
    ),
 
    mainPanel(
      tabsetPanel(
        #tabPanel("weights", verbatimTextOutput("showsomething")),
        tabPanel("Graph Visualization", forceNetworkOutput("force",width="auto",height=690)
                 , DT::dataTableOutput("nodeinfo")),
        tabPanel("EdgeBetweenessClustering",downloadButton('dlb'), htmlOutput("text"),
                 plotOutput("clusters",width="auto",height="600"))  ,
  
        tabPanel("Instructions", htmlOutput("instrtext"))
        )
      
       
     

      
    )
  )
))