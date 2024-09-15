#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(factoextra)
library(Rtsne)
library(class)
library(stats)
library(caret)
library(cluster)
library(clusterCrit)
library(dbscan)
library(caTools)
library(MASS)

# Define UI for application that draws a histogram

options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)


DBval <-NULL
ui <- page_fluid( 

  #Style section--------------------------------------------------------------------
  tags$head(
    tags$style(HTML("
      
      body{background-color:#ffd}
      
     .row{
     margin-left:20px;
     margin-right:20px; }
   
    .nav{background-color:#DBDBDB }
   
   h2{font-size:40px;
   margin-top:15px;
   margin-bottom:15px}
   
   .tab-content{font-size:20px}
   
      "
    ))
  ),
  
  
  
  #File Upload Panel---------------------------------------
    
  navset_tab( 
    nav_panel("File Upload",
              
              titlePanel("Import Files"),
              
              sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File", accept = ".csv")
        
      ),
      mainPanel(
        DTOutput("contents", width = "auto", height = "auto")
       
      )
      
    )
    
            ),
      
  
    #Data Analysis Panel---------------------------------------

    nav_panel("Data Analysis", 
            
                mainPanel(
                  
                  titlePanel("PCA"),
                  checkboxInput("center", "PCA Center", TRUE),
                  checkboxInput("scale", "PCA Scale", TRUE),
                  plotOutput("pcaPlot", width = "60vw", height = "50vh"),
                  
                  titlePanel("TSNE"),
                  plotOutput("tsnePlot", width = "60vw", height = "50vh"),
                  
                  titlePanel("Data summary"),
                  DTOutput("EDsummary"),
                  
                  titlePanel("Data Visualization"),
                  varSelectInput("VisualX", "Select X",DBval),
                  varSelectInput("VisualY", "Select Y",DBval),
                  plotOutput("EDVisualPlot", width = "auto"),
                  varSelectInput("VisualHist", "Select",DBval),
                  plotOutput("EDVisualHist", width = "auto")
                )
                
                ),
            
              
              
              
              
              
              
    #Clustering Panel---------------------------------------          
               
    nav_panel("MM Clustering",
              
              # Application title
            
    
                mainPanel(
                  
                  titlePanel("K-means Clustering"),
                  numericInput("k", "K Value:",10,min = 1),
                  selectInput("intk1","IntCriteria type",c("Ball Hall" = "Ball_Hall",
                                                           "Banfeld Raftery" ="Banfeld_Raftery",
                                                           "C index" = "C_index",
                                                           "Calinski Harabasz"="Calinski_Harabasz",
                                                           "Davies Bouldin"= "Davies_Bouldin",
                                                           "Silhouette" = "Silhouette")),
                 plotOutput("Plotkmeans",height = "800px"),
                 
                 
                 titlePanel("DBscan Clustering"),
                  sliderInput("eps","eps Value",step =0.1, value = 0.5, min = 0,max = 4),
                 numericInput("minpts", "Min points:",10),
                 selectInput("intk2","IntCriteria type",c("Ball Hall" = "Ball_Hall",
                                                          "Banfeld Raftery" ="Banfeld_Raftery",
                                                          "C index" = "C_index",
                                                          "Calinski Harabasz"="Calinski_Harabasz",
                                                          "Davies Bouldin"= "Davies_Bouldin",
                                                          "Silhouette" = "Silhouette")),
                 plotOutput("PlotDbscan",height = "800px"),
                 plotOutput("KmeansSilPlot",height = "800px"),
                 plotOutput("DBscanSilPlot",height = "800px"),
                 
                 
                 
                )
              ),
 #Classification Panel--------             
    nav_panel("MM Classification",
              
              # Application title
              titlePanel("Old Faithful Geyser Data"),
              
              mainPanel(
                titlePanel("KNN method"),
                numericInput("k", "K Value:",10,min = 1),
                plotOutput("PlotKNN"),
               
              
                numericInput("rftree", "Tree Value:",10,min = 1),
                titlePanel("Random Forest Method"),  
                plotOutput("RedForest"),
                titlePanel("Random Forest stats"),
                verbatimTextOutput("precisionRF")
              )
    ), 
 
 
 #Info Panel------------------------------------------------------------
 
 nav_panel("Info", 
           
           
           titlePanel("File input tab"),
           
           span("This tab allows the input of data for use in the application and gives an overview of 
           the data through a data table."),
        
           titlePanel("Data Analysis tab"),
           
           span("This tab focuses on different types of visualizations of the input data through different functions."),
           br(),
           span("It contains functions for PCA and TSNE while also allowing x.y axis comparison between different data lists
           and a histogram visualization."),
           
           titlePanel("MM Clustering tab"),
           
           span("This tab runs the data through a K-means and a DBscan algorhythm and shows different stats for each."),
           
           titlePanel("MM Classification tab"),
           
           span("This tab runs the data through a K-NN and a Random Forest alghorhythm and shows different stats for each")
           
           
           
           
           
           
           
           
           
           ),        
    
),
)
  id = "tab"
  

# Define server logic required to draw a histogram
server <- function(input, output) {


  
  
  
  output$contents <- renderDT({
    
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
  

    
    
   
      
     DBval <-read.csv(file$datapath,header = TRUE)
     
     col <-ncol(DBval)
  
    updateVarSelectInput(session = getDefaultReactiveDomain(), "VisualX",data = DBval)
    updateVarSelectInput(session = getDefaultReactiveDomain(), "VisualY",data = DBval)
    updateVarSelectInput(session = getDefaultReactiveDomain(), "VisualHist",data = DBval)
    
    
    #Data Visualization Tab--------------------
    
         output$pcaPlot <- renderPlot({
            
           
           pca <-  prcomp(x =sapply(DBval,as.numeric),center = input$center, scale = input$scale)
             fviz_pca(pca)})
         
         output$tsnePlot <-renderPlot({
           
           tsne <- Rtsne(DBval,check_duplicates = F)
           tsne_plot <- data.frame(x = tsne$Y[,1], y = tsne$Y[,2])
           plot(tsne_plot)
         })
         
         output$EDsummary <- renderDT(data.frame(summary(DBval)))
         
         output$EDVisualPlot <- renderPlot({
           
           plot(DBval[,c(
             str_glue(
               {input$VisualX}))],
             
             DBval[,c(
               str_glue(
                 {input$VisualY}))],
             
             xlab = input$VisualX,
             ylab = input$VisualY
             
             )
                 
                 })
  
         output$EDVisualHist <- renderPlot({
           
           hist(DBval[,c(
             str_glue(
               {input$VisualHist}))],
             main = NULL,
         
             xlab = input$VisualHist)
             
           
         })
         
  #Clustering Tab------------------------------
         
         output$Plotkmeans <- renderPlot({
           
           
           
           criter <- sapply(DBval,as.numeric)
          
           cl <- kmeans(criter,input$k,nstart = 1)
           
           
           
           vals = NULL
           vals <- c(vals,as.numeric(intCriteria(criter,cl$cluster,input$intk1)))
           
         
           
              output$KmeansSilPlot <- renderPlot({
                
                  si <- silhouette(cl$cluster,(dist(criter)))
                  plot(si,col = c("red", "green", "blue", "purple"),
                       do.col.sort = length(col) > 1,
                       do.n.k = TRUE,main = "Kmeans Silhouette")
                })
          
          
           fviz_cluster(cl,criter,main =str_glue("Best index value is {vals}"))
          
         })
         
         output$PlotDbscan <- renderPlot({
           
           criter <- sapply(DBval,as.numeric)
           
           DBscan <- dbscan(criter,eps = input$eps,minPts = input$minpts)
           
           
           output$DBscanSilPlot <- renderPlot({
             
             si <- silhouette(DBscan$cluster,(dist(criter)))
             
             plot(si,col = c("red", "green", "blue", "purple"),
                  main = "DBscan Silhouette",ylim = c(-10000,10000))
           })
           
           
           vals = NULL
           vals <- c(vals,as.numeric(intCriteria(criter,DBscan$cluster,input$intk2)))
           
           fviz_cluster(DBscan,criter,main =str_glue("Best index value is {vals}"))
           
         })
         
  #Classification Tab--------------------------
         
         output$PlotKNN <- renderPlot({
         
           splt <- sample.split(DBval, SplitRatio = 0.7) 
          traindata <- subset(DBval, splt =="TRUE")
          testdata <- subset(DBval, splt == "FALSE")
          
         
           train_scale <- scale(traindata[, 1:4]) 
           test_scale <- scale(testdata[, 1:4])
           
           nn <- knn(train = train_scale, test = test_scale,cl = traindata[,c(col)] , k = input$k)
           
           
           plot(nn)
         })
         
         
         
         
         
         
         
         
         output$RedForest <-renderPlot({
         
           train <-createDataPartition(DBval[,"Species"],p = 0.8,list = FALSE)
           data.trn <- DBval[train,]
           data.tst <-DBval[-train,]
           
           names(data.trn)[col] <- c("Test")
           names(data.tst)[col] <- c("Test")
           ctrl <- trainControl(method = "cv",number = 10)
           
         rfdata <- train(Test ~ .,data = data.trn ,method="rf",
                         trcontrol = ctrl ,tuneLength=input$rftree)
         
         
         
         
         
         output$precisionRF <-renderPrint({
           
           pred <- predict(rfdata,data.tst)
           
           print(confusionMatrix(table(data.tst[,"Test"],pred)))
          
          
         })
         
         
         
         plot(rfdata)
         })
         
         
         read.csv(file$datapath,header = TRUE)}, )}

# Run the application 
shinyApp(ui = ui, server = server)
