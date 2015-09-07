library(shiny)
library(dplyr)
library(psych)
require(shiny);
require(caret);
require(e1071);
require(randomForest);
require(nnet);
require(glmnet);
library(reshape2);
library(DT)
library(corrplot);
library(ggplot2)
library(rlist)
library(ROCR)
library(easyGgplot2)#,lib.loc="/home/madhu/R/x86_64-pc-linux-gnu-library/3.2/"


options(shiny.maxRequestSize = 1000*1024^2)

shinyServer(function(input, output,session) {
  
  
  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)){
      return(NULL)      
    }
    read.csv(infile$datapath)
  })
  
  
  output$ui.action <- renderUI({
    if (is.null(filedata())) return()
    actionButton("action", "Run regression")
  })
  
  
  output$dependent <- renderUI({
    df <- filedata()
  
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("dependent","Response Variable(DEPENDENT):",items,multiple = T)
  })
  
  
  
  output$independents <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("independents","Exploratory Variable(INDEPENDENT):",items,multiple=TRUE)
  })

  
  
  
  output$rawDataView =  DT::renderDataTable({
    newData = filedata()
    if(is.null(newData))
      return()
    DT::datatable(newData, options = list(scrollX = TRUE));
 
  })
  
  
  
  output$describe<- DT::renderDataTable({
    dataset<-filedata()
    if(is.null(filedata())){return()}
   DT::datatable(describeBy(dataset), options = list(scrollX = TRUE))
  })
  
  
  
  
  #This will print the number of rows and columns in the data frame
  output$dim <- renderPrint({
   data_stats <- paste("There are", nrow(filedata()), "rows and", ncol(filedata()), "columns for Dataset.")
    print(data_stats)
  })
  
  #This will print the structure of the data frame, fields and its data type
  output$structure <- renderPrint({
    str(filedata())
  
    
  })
  
  
  
  cordata<-reactive({
    data<-filedata()
    drops <- dependentsel()
    head(cor(data[,!(names(data) %in% drops)]))
  })
  
  
  output$corr <- DT::renderDataTable({
    DT::datatable(cordata(), options = list(scrollX = TRUE))
  })
  
  
  output$corplot <- renderPlot({
    M <- cordata() 
    corrplot(M, type="lower", order="hclust", tl.col="black", tl.srt=25)
    
  })
  
   dependentsel <- reactive(input$dependent)
   
   
   sumr<- reactive({
     
     df <- filedata()
     drops <- dependentsel()
     res <- lapply(df[,!(names(df) %in% drops)], function(x) rbind(
       Mean = mean(x) ,
       StandardDeviation = sd(x) ,
       Median = median(x) ,
       Minimum = min(x) ,
       Maximum = max(x) 
     ) )
     
     ss<-data.frame(res)
     t(ss)
   })
   
   
   
   output$summary <- DT::renderDataTable({
  summr<-sumr()
 DT::datatable(summr, options = list(scrollX = TRUE))
     # head(t(ss))
   })
  
   
   ##########################################################Skew and kurtosis###################################
   
   plotskk<-reactive({
     dataset<-filedata()
     aa<- describeBy(dataset)
     # aa1 <- as.data.frame(aa)
     # print(aa)
     myplot<-ggplot(aa, aes(x=skew)) + labs(title="Skewness")+
       geom_histogram(aes(y=..density..), 
                      binwidth=.2,
                      colour="black", fill="lightblue2") +
       geom_density(alpha=.2, fill="#FF6666")
     myplot + theme(panel.background = element_blank())
   })
   
   
   plotkk<-reactive({
     
     dataset<-filedata()
     aa<- describeBy(dataset)
     # aa1 <- as.data.frame(aa)
     # print(aa)
     myplot<-ggplot(aa, aes(x=kurtosis)) + labs(title='Kurtosis')+
       geom_histogram(aes(y=..density..), 
                      binwidth=.2,
                      colour="black", fill="lightblue2") +
       geom_density(alpha=.2, fill="#FF6666")
     myplot + theme(panel.background = element_blank())
   })
   
   
   
   
   output$plotsk <-renderPlot({
     
     ggplot2.multiplot(plotskk(),plotkk(), cols=2,rows=1)
   })

   
   # independent<- reactive(input$independents)
   
   
 ################################################################  Ramdata Summary Ploting section ###############################  
   col<-reactive({
     data<-filedata()
     drops<- dependentsel()
     c(names(data[,!names(data) %in% drops]))
   
   })
   
   
   Meanplot<-reactive({
     mm<-data.frame(sumr())
     mm$variable<-col()
ggplot(mm, aes(x=variable ,y=Mean,fill=factor(Mean))) + geom_bar(stat="identity",width=0.25)+xlab(NULL)+ylab("Mean")+scale_fill_discrete(name="Mean")+theme_bw()+labs(title='Mean') 
     
   })
   
   
   Medianplot<-reactive({
     
     mm<-data.frame(sumr())
     mm$variable<-col()
 ggplot(mm, aes(x=variable ,y=Median,fill=factor(Median))) + geom_bar(stat="identity",width=0.25)+xlab(NULL)+ylab("Median")+scale_fill_discrete(name="Median")+theme_bw()+labs(title='Median')
   
   })
   
   
   Sdplot<-reactive({
     mm<-data.frame(sumr())
     mm$variable<-col()
   ggplot(mm, aes(x=variable ,y=StandardDeviation,fill=factor(StandardDeviation))) + geom_bar(stat="identity",width=0.5)+xlab(NULL)+ylab("StandardDeviation")+scale_fill_discrete(name="StandardDeviation")+theme_bw()+labs(title='StandardDeviation')
 
     
   })
   
   
   Minplot<-reactive({
     
     mm<-data.frame(sumr())
     mm$variable<-col()
     ggplot(mm, aes(x=variable ,y=Minimum,color=factor(Minimum))) + geom_point( aes(size =factor(Minimum)))+xlab(NULL)+ylab("Minimum") +scale_color_discrete(name="Minimum")+scale_size_discrete(name="Size")+theme_bw()+labs(title='Min')

   })
   
   Maxplot<-reactive({
     
     mm<-data.frame(sumr())
     mm$variable<-col()
  ggplot(mm, aes(x=variable ,y=Maximum,color=factor(Maximum))) + geom_point( aes(size =factor(Maximum)))+xlab(NULL)+ylab("Maximum") +scale_color_discrete(name="Maximum")+scale_size_discrete(name="Size")+theme_bw()+labs(title='Max')

     
   })
   
   output$sumplot<- renderPlot({
     ggplot2.multiplot(Meanplot(),Medianplot(),Sdplot(),Minplot(),Maxplot(), cols=2,rows=3)
     
   
  })
   

   
   
    
   
   
   
###################training Section#############################################3
   
   dependentsel <- reactive(input$dependent)
   
   traineddata<-reactive({
     
     
     df <- filedata()
     # split in training testing datasets
     set.seed(100)
     trainIndex <- sample(nrow(df), size = nrow(df)*input$slidertrainsplit)
     training = df[trainIndex,]
   
#      drops <- dependentsel()
#      head(training[,!(names(training) %in% drops)])
     
     
     
   })
   
   output$traindataview = DT::renderDataTable({
     
     DT::datatable(traineddata(), options = list(scrollX = TRUE))
     
     # head(traineddata())
             
         })
     
 
   output$traindim <- renderPrint({
     df <- filedata()
     trainIndex <- sample(nrow(df), size = nrow(df)*input$slidertrainsplit)
     training = df[trainIndex,]
     
     ss<-data.frame(training)
     
     
     data_stats <- paste("There are", nrow(ss), "rows and", ncol(ss), "columns for Dataset.")
     print(data_stats)
   })
   
   
   output$trainstructure <- renderPrint({
     df <- filedata()
     trainIndex <- sample(nrow(df), size = nrow(df)*input$slidertrainsplit)
     training = df[trainIndex,]
     str(training)
   })
   
   
   
   output$traindescribe<- DT::renderDataTable({
     df <- filedata()
     trainIndex <- sample(nrow(df), size = nrow(df)*input$slidertrainsplit)
     training = df[trainIndex,]
     
    
   DT::datatable(describeBy(training), options = list(scrollX = TRUE))
   })
   
   
 
   
   
   traincor <- reactive({
     
     df <- filedata()
     trainIndex <- sample(nrow(df), size = nrow(df)*input$slidertrainsplit)
     training = df[trainIndex,]
     drops <- dependentsel()
     
     cor(training[,!(names(training) %in% drops)])
     
     # head(cor(training[,!(names(training) %in% drops)]))
   })
   
   
   
   output$traincorr <- DT::renderDataTable({
     
        trcorr<- traincor()
     DT::datatable(trcorr, options = list(scrollX = TRUE))
     # head(cor(training[,!(names(training) %in% drops)]))
   })
   
   
   output$trcorplot <- renderPlot({
     M <- traincor() 
     corrplot(M, type="lower", order="hclust", tl.col="black", tl.srt=25)
     
   })
   
   
   
   trainsum<-reactive({
     df <- filedata()
     trainIndex <- sample(nrow(df), size = nrow(df)*input$slidertrainsplit)
     training = df[trainIndex,]
     
     drops <- dependentsel()
     res <- lapply(training[,!(names(training) %in% drops)], function(x) rbind(
       Mean = mean(x) ,
       StandardDeviation = sd(x) ,
       Median = median(x) ,
       Minimum = min(x) ,
       Maximum = max(x) 
     ) )
     
     ss<-data.frame(res)
     t(ss)
     
   })
   
   
   output$trainsummary <- DT::renderDataTable({
     
     trainss<- trainsum()
     
     DT::datatable(trainss, options = list(scrollX = TRUE))
     # head(t(ss))
   })
   
   
   
   
   trplotskk<-reactive({
     dataset<- traineddata()
     aa<- describeBy(dataset)
     # aa1 <- as.data.frame(aa)
     # print(aa)
     myplot<-ggplot(aa, aes(x=skew)) + labs(title="Skewness")+
       geom_histogram(aes(y=..density..), 
                      binwidth=.2,
                      colour="black", fill="lightblue2") +
       geom_density(alpha=.2, fill="#FF6666")
     myplot + theme(panel.background = element_blank())
   })
   
   
   trplotkk<-reactive({
     
     dataset<- traineddata()
     aa<- describeBy(dataset)
     # aa1 <- as.data.frame(aa)
     # print(aa)
     myplot<-ggplot(aa, aes(x=kurtosis)) + labs(title='Kurtosis')+
       geom_histogram(aes(y=..density..), 
                      binwidth=.2,
                      colour="black", fill="lightblue2") +
       geom_density(alpha=.2, fill="#FF6666")
     myplot + theme(panel.background = element_blank())
   })
   
   
   
   
   output$trplotsk <-renderPlot({
     
     ggplot2.multiplot(trplotskk(),trplotkk(), cols=2,rows=1)
   })
   
   
   
   
   
   ################################################################  Train Summary Ploting section ###############################  
   col1<-reactive({
     data<-traineddata()
     drops<- dependentsel()
     c(names(data[,!names(data) %in% drops]))
     
   })
   
   
   trMeanplot<-reactive({
     mm1<-data.frame(trainsum())
     mm1$variable1<-col1()
     ggplot(mm1, aes(x=variable1 ,y=Mean,fill=factor(Mean))) + geom_bar(stat="identity",width=0.25)+xlab(NULL)+ylab("Mean")+scale_fill_discrete(name="Mean")+theme_bw()+labs(title='Mean')
     
     
   })
   
   
   trMedianplot<-reactive({
     
     mm1<-data.frame(trainsum())
     mm1$variable1<-col1()
     ggplot(mm1, aes(x=variable1 ,y=Median,fill=factor(Median))) + geom_bar(stat="identity",width=0.25)+xlab(NULL)+ylab("Median")+scale_fill_discrete(name="Median")+theme_bw()+labs(title='Median')
     
   })
   
   
   trSdplot<-reactive({
     mm1<-data.frame(trainsum())
     mm1$variable1<-col1()
     ggplot(mm1, aes(x=variable1 ,y=StandardDeviation,fill=factor(StandardDeviation))) + geom_bar(stat="identity",width=0.5)+xlab(NULL)+ylab("StandardDeviation")+scale_fill_discrete(name="StandardDeviation")+theme_bw()+labs(title='StandardDeviation')
     
   })
   
   
   trMinplot<-reactive({
     
     mm1<-data.frame(trainsum())
     mm1$variable1<-col1()
     ggplot(mm1, aes(x=variable1 ,y=Minimum,color=factor(Minimum))) + geom_point( aes(size =factor(Minimum)))+xlab(NULL)+ylab("Minimum") +scale_color_discrete(name="Minimum")+scale_size_discrete(name="Size")+theme_bw()+labs(title='Min')
     
   })
   
   trMaxplot<-reactive({
     
     mm1<-data.frame(trainsum())
     mm1$variable1<-col1()
     ggplot(mm1, aes(x=variable1 ,y=Maximum,color=factor(Maximum))) + geom_point( aes(size =factor(Maximum)))+xlab(NULL)+ylab("Maximum") +scale_color_discrete(name="Maximum")+scale_size_discrete(name="Size")+theme_bw()+labs(title='Max')
     
     
   })
   
   output$trainsumplot<- renderPlot({
     ggplot2.multiplot(trMeanplot(),trMedianplot(),trSdplot(),trMinplot(),trMaxplot(), cols=2,rows=3)
     
     
   })
   
   
   
   
   
   
   ################### Test Section#############################################3
   
   dependentsel <- reactive(input$dependent)
   
   
   
   testeddata<-reactive({
     
     df <- filedata()
     # split in training testing datasets
     set.seed(100)
     trainIndex <- sample(nrow(df), size = nrow(df)*input$slidertrainsplit)
     testing = df[-trainIndex,]
    
#      drops <- dependentsel()
#      head(testing[,!(names(testing) %in% drops)])
     
     
   })
   
   
   
   
   output$testdataview = DT::renderDataTable({
     
     DT::datatable(testeddata(), options = list(scrollX = TRUE))
        # head(testeddata())
     
   })
   
   
   output$testdim <- renderPrint({
     df <- filedata()
     trainIndex <- sample(nrow(df), size = nrow(df)*input$slidertrainsplit)
     testing= df[-trainIndex,]
     
     ss<-data.frame(testing)
     
     
     data_stats <- paste("There are", nrow(ss), "rows and", ncol(ss), "columns for Dataset.")
     print(data_stats)
   })
   
   
   output$teststructure <- renderPrint({
     df <- filedata()
     trainIndex <- sample(nrow(df), size = nrow(df)*input$slidertrainsplit)
     testing = df[-trainIndex,]
     str(testing)
   })
   
   
   
   output$testdescribe<- DT::renderDataTable({
     df <- filedata()
     trainIndex <- sample(nrow(df), size = nrow(df)*input$slidertrainsplit)
     testing = df[-trainIndex,]
     
     DT::datatable(testeddata(), options = list(scrollX = TRUE))
     
   DT::datatable(describeBy(testing), options = list(scrollX = TRUE))
   })
   
   
   
  
   
   testcor <- reactive({
     df <- filedata()
     trainIndex <- sample(nrow(df), size = nrow(df)*input$slidertrainsplit)
     testing = df[-trainIndex,]
     drops <- dependentsel()
     cor(testing[,!(names(testing) %in% drops)])

   })
   
   
   output$testcorr <- DT::renderDataTable({
     testcr<- testcor()
     DT::datatable(testcr, options = list(scrollX = TRUE))
    
   })
   
   
   output$ttcorplot <- renderPlot({
     M <- testcor()
     corrplot(M, type="lower", order="hclust", tl.col="black", tl.srt=25)
     
   })
   
   
   testsum<-reactive({
     df <- filedata()
     trainIndex <- sample(nrow(df), size = nrow(df)*input$slidertrainsplit)
     testing = df[-trainIndex,]
     
     drops <- dependentsel()
     res <- lapply(testing[,!(names(testing) %in% drops)], function(x) rbind(
       Mean = mean(x) ,
       StandardDeviation = sd(x) ,
       Median = median(x) ,
       Minimum = min(x) ,
       Maximum = max(x) 
     ) )
     
     ss<-data.frame(res)
     t(ss)
   })
   
   
   output$testsummary <- DT::renderDataTable({
    
     tests<-testsum()
     DT::datatable(tests, options = list(scrollX = TRUE))
     
     # head(t(ss))
   })
   
   
   ttplotskk<-reactive({
     dataset<- testeddata()
     aa<- describeBy(dataset)
     # aa1 <- as.data.frame(aa)
     # print(aa)
     myplot<-ggplot(aa, aes(x=skew)) + labs(title="Skewness")+
       geom_histogram(aes(y=..density..), 
                      binwidth=.2,
                      colour="black", fill="lightblue2") +
       geom_density(alpha=.2, fill="#FF6666")
     myplot + theme(panel.background = element_blank())
   })
   
   
   ttplotkk<-reactive({
     
     dataset<- testeddata()
     aa<- describeBy(dataset)
     # aa1 <- as.data.frame(aa)
     # print(aa)
     myplot<-ggplot(aa, aes(x=kurtosis)) + labs(title='Kurtosis')+
       geom_histogram(aes(y=..density..), 
                      binwidth=.2,
                      colour="black", fill="lightblue2") +
       geom_density(alpha=.2, fill="#FF6666")
     myplot + theme(panel.background = element_blank())
   })
   
   
   
   
   output$ttplotsk <-renderPlot({
     
     ggplot2.multiplot(ttplotskk(),ttplotkk(), cols=2,rows=1)
   })
   
   
   
   
   ################################################################  Test Summary Ploting section ###############################  
  
   
   
    col2<-reactive({
     data<- testeddata()
     drops<- dependentsel()
     c(names(data[,!names(data) %in% drops]))
     
   })
   
   
   trMeanplot<-reactive({
     mm1<-data.frame(testsum())
     mm1$variable1<-col2()
     ggplot(mm1, aes(x=variable1 ,y=Mean,fill=factor(Mean))) + geom_bar(stat="identity",width=0.25)+xlab(NULL)+ylab("Mean")+scale_fill_discrete(name="Mean")+theme_bw()+labs(title='Mean')
     
     
   })
   
   
   trMedianplot<-reactive({
     
     mm1<-data.frame(testsum())
     mm1$variable1<-col2()
     ggplot(mm1, aes(x=variable1 ,y=Median,fill=factor(Median))) + geom_bar(stat="identity",width=0.25)+xlab(NULL)+ylab("Median")+scale_fill_discrete(name="Median")+theme_bw()+labs(title='Median')
     
   })
   
   
   trSdplot<-reactive({
     mm1<-data.frame(testsum())
     mm1$variable1<-col2()
     ggplot(mm1, aes(x=variable1 ,y=StandardDeviation,fill=factor(StandardDeviation))) + geom_bar(stat="identity",width=0.5)+xlab(NULL)+ylab("StandardDeviation")+scale_fill_discrete(name="StandardDeviation")+theme_bw()+labs(title='StandardDeviation')
     
     
   })
   
   
   trMinplot<-reactive({
     
     mm1<-data.frame(testsum())
     mm1$variable1<-col2()
     ggplot(mm1, aes(x=variable1 ,y=Minimum,color=factor(Minimum))) + geom_point( aes(size =factor(Minimum)))+xlab(NULL)+ylab("Minimum") +scale_color_discrete(name="Minimum")+scale_size_discrete(name="Size")+theme_bw()+labs(title='Min')
     
   })
   
   trMaxplot<-reactive({
     mm1<-data.frame(testsum())
     mm1$variable1<-col2()
     ggplot(mm1, aes(x=variable1 ,y=Maximum,color=factor(Maximum))) + geom_point( aes(size =factor(Maximum)))+xlab(NULL)+ylab("Maximum") +scale_color_discrete(name="Maximum")+scale_size_discrete(name="Size")+theme_bw()+labs(title='Max')
     
     
   })
   
   output$testsumplot<- renderPlot({
     ggplot2.multiplot(trMeanplot(),trMedianplot(),trSdplot(),trMinplot(),trMaxplot(), cols=2,rows=3)
     
   })
   
  ########################################################Preprocessing and  ModelSelections#############################################################33 
   
   
   #responsible for building the model, responds to the button
   #REQUIRED, as the panel that holds the result is hidden and trainResults will not react to it, this one will  
   output$dummyTagUI = renderUI({
     dataInput = trainResults()
     if(is.null(dataInput))
       return();
     activeTab = updateTabsetPanel(session,"mainTabUI",selected="Model Results View");
     return();
   })
   
   #this is the function that responds to the clicking of the button
   trainResults = reactive({
     #respond to the button
     input$runAnalysisUI;
     
     #the model we are interested in
     modelTag = isolate(input$modelSelectionUI);
     
     #make sure the data are loaded
     newData = isolate(traineddata());
     if(is.null(newData))
       return();
     
     #grab the column
     column = isolate(input$dependent);
     
     columnElement = which(colnames(newData) == column);
     
     foldsType = isolate(input$crossFoldTypeUI);
     
     folds = isolate(input$foldsUI);
     
     control = trainControl(method=foldsType,number=folds)
     
     if(foldsType == "repeatedcv")
     {
       numberOfRepeats = isolate(input$repeatUI);
       control = trainControl(method=foldsType,number=folds,repeats=numberOfRepeats);
     }
     
     preprocessType = isolate(input$preprocessingUI);
     # indep <- reactive(input$independents)
     column2 = isolate(input$independents);
     
     #build the equation
     #form = as.formula(paste(column," ~ .",sep=""));
     
     form = as.formula(paste(column," ~ ",paste(column2,collapse="+"),sep=""));
     
# as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+")
     
     kFolds = isolate(input$foldsUI);
     
     foldType = isolate(input$crossFoldTypeUI);
     
     if(preprocessType == "")
       preprocessType = NULL;
     
     results = NULL;
     
     results = withProgress(session, min=1, max=2, {
       setProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...')
       
       setProgress(value = 1)
       
       
       
       #choose the view based on the model
       if(modelTag == "en")
       {
         
         alphaStart = isolate(input$enAlphaStartUI);
         alphaEnd = isolate(input$enAlphaEndUI);      
         alphaRange = isolate(input$enAlphaRangeUI);
         lambdaStart = isolate(input$enLambdaStartUI);
         lambdaEnd = isolate(input$enLambdaEndUI);      
         lambdaRange = isolate(input$enLambdaRangeUI);
         familyData = isolate(input$enModelTypeUI);
         gridding = expand.grid(.alpha=seq(alphaStart,alphaEnd,length.out=alphaRange),.lambda=seq(lambdaStart,lambdaEnd,length.out=lambdaRange));
         
         #create the equation
         
         
         
         results = train(form,data=newData,tuneGrid=gridding,method="glmnet",family=familyData,trControl=control,preProcess=preprocessType);
         
         return(results);
         
       } else if(modelTag == "rf") {
         
         mTryStartEnd = isolate(input$mTryRangeUI)
         
         nMtry = isolate(input$rmTryUI);
         
         familyData = isolate(input$rfModelTypeUI);
         
         gridding = expand.grid(.mtry=seq(mTryStartEnd[1],mTryStartEnd[2],by=nMtry));
         
         
         
         if(familyData != "Gaussian") {
           newData[,columnElement] = as.factor(newData[,columnElement]);
         } else {
           newData[,columnElement] = as.numeric(newData[,columnElement]);
         }
         
         
         results = train(form,data=newData,tuneGrid=gridding,method="rf",trControl=control,preProcess=preprocessType);
         return(results);
         
         
       } else if (modelTag == "nn") {
         
         familyData = isolate(input$nnModelTypeUI);
         nnRange = isolate(input$nnSizeUI);
         numNN = isolate(input$nnSizeRangeUI);
         nnDecayRange = isolate(input$nnDecayUI);
         numnnDecayRange = isolate(input$nnDecayRangeUI);
         
         gridding = expand.grid(.size=seq(nnRange[1],nnRange[2],length.out=numNN),.decay=seq(nnDecayRange[1],nnDecayRange[2],length.out=numnnDecayRange));
         
         
         if(familyData != "Gaussian") {
           newData[,columnElement] = as.factor(newData[,columnElement]);
         } else {
           newData[,columnElement] = as.numeric(newData[,columnElement]);
         }
         
         results = train(form,data=newData,tuneGrid=gridding,method="nnet",trControl=control,preProcess=preprocessType);
         return(results);
       }
       setProgress(value = 2);
     });
     
     return(results);
     
     
     
   })
   
   #responsible for displaying the full results
   output$trainResultsUI = renderTable({
     data = trainResults();
     if(is.null(data))
       return();
     head(data$results,15)
     
     # DT::datatable(data$results, options = list(scrollX = TRUE))
     #data$results[as.numeric(!rownames(data$bestTune)[1]),]
     
   })
   
   #the one that matches the best
   output$bestResultsUI = renderTable({
     data = trainResults();
     if(is.null(data))
       return();
  data$results[as.numeric(rownames(data$bestTune)[1]),];
#      DT::datatable(data$results[as.numeric(rownames(data$bestTune)[1]),], options = list(scrollX = TRUE))
#      
   })
   

   
   #the results graph of the caret output
   output$finalPlotUI = renderPlot({
     data = trainResults();
     if(is.null(data)){
       return();
     } else {
       
       #the model we are interested in
       modelTag = isolate(input$modelSelectionUI);
       
       
       #grab the column
       column = isolate(input$dependent);
       
       #build the equation
       form = as.formula(paste(column," ~ .",sep=""));
       par(mfrow=c(2,1));
       # p = plot(data);
       p=ggplot(data)
       print(p);
     
       
     }
   })
   
 
   #a dynamic table responsible for building the input types to the model
   output$modelParametersUI = renderUI({
     
     modelTag = input$modelSelectionUI;
     
     if(modelTag == "en")
     {
       tagList(selectInput("enModelTypeUI","Model Type",c('Binomial'="binomial",'Gaussian'="gaussian",'Multinomial'="multinomial"),"Binomial"),
               numericInput("enAlphaStartUI","Alpha Start",0.1),
               numericInput("enAlphaEndUI","Alpha End",1.0),
               numericInput("enAlphaRangeUI","# Alpha",5),
               numericInput("enLambdaStartUI","Lambda Start",0.1),
               numericInput("enLambdaEndUI","Lambda End",1),
               numericInput("enLambdaRangeUI","# Lambda",5))
     } else if(modelTag == "rf") {
       data = traineddata();
       if(is.null(data)){
         dataRange = 2;
       } else {
         dataRange = ncol(data)-1;
       }
       tagList(selectInput("rfModelTypeUI","Model Type",c("Binomial","Gaussian","Multinomial"),"Binomial"),
               sliderInput("mTryRangeUI","mTry Range",min=1,max=dataRange,value=c(1,dataRange),step=1),
               numericInput("rmTryUI","mTry Skip",1)
       )
     } else if (modelTag == "nn") {
       tagList(selectInput("nnModelTypeUI","Model Type",c('Binomial'="binomial",'Gaussian'="gaussian",'Multinomial'="multinomial"),"Binomial"),
               sliderInput("nnSizeUI","NN Size",min=1,max=25,value=c(1,5)),
               numericInput("nnSizeRangeUI","NN Size Range",5),
               sliderInput("nnDecayUI","NN Decay",min=0.0,max=1.0,value=c(0,0.1),step=0.001),
               numericInput("nnDecayRangeUI","NN Decay Range",5))      
     }
     
   })
   
   
    
   output$caretPlotUI = renderPlot({
     
     data <- filedata();
     
     column <- input$dependent;
     
     
     #check if the data is loaded first
     if(is.null(data)){
       return()
     } else {
       
       columnElement <- which(colnames(data) == column);  
       
       p <- featurePlot(x=data[,-columnElement],y=data[,columnElement],plot="pairs",auto.key=T);
       print(p);
     }
   })
   
   
   
   output$trainResultsUI1 = renderTable({
     data = trainResults();
     if(is.null(data))
       return();
     # data
    
#      print(paste("ModelType" ,":", data$modelType[][1])) 
#      print(paste("Method",":",data$method[[1]])) 
# 
#      print(paste("CrossValidationType",":",data$control[[1]]))
#      print(paste("Number",":",data$control[[2]]))
#      print(paste("Repeat",":",data$control[[3]]))
#      print(paste(paste("P-value",":",data$control[[4]])))
   
        datab<-data.frame(ModelType=c(data$modelType[][1]),Method=c(data$method[[1]]),CrossValidationType=c(data$control[[1]]),Number=c(data$control[[2]]),
                          Pvalue=c(data$control[[4]]))
              datab            
   })
   
   
   
   ##############Predicted Results###########################################
   
   prediction<-reactive({
     
     tml<- trainResults();
     ttl<-testeddata();
     drops<- dependentsel()
     tdrops<-ttl[,!(names(ttl) %in% drops)]
     predict(tml,tdrops)

   })
   ########################## For Confusion matric Actual Testing data################################
   testeddata1<-reactive({
     df <- filedata()
     # split in training testing datasets
     set.seed(100)
     trainIndex <- sample(nrow(df), size = nrow(df)*input$slidertrainsplit)
     testing = df[-trainIndex, dependentsel()]
     
   })
   
   ################Confusion Matrix#############################
   
   conf<- reactive({
     table(prediction(),testeddata1())
     
   })
   
output$pr = renderTable({
  conf()
   })
   
# errorrate<-reactive({
#    1-sum(diag(conf()))/sum(conf())
# })
#    

#################Overall accuracy and other performance measures#####################################

preff<- reactive({
  confusionMatrix(prediction(),testeddata1())
  
})


output$perf<- renderTable({
  
 pf<- preff()
 
 prf<-data.frame(pf[[4]])
 
 prf
})


ppf<-reactive({
  
  pf<- preff()
  
  prf<-data.frame(pf[[3]])
  
  
  names(prf)[1]<-paste("Measures")
  
  prf
})



output$ac<- renderTable({
  
ppf()

  
})

output$ts<-renderPrint({
  testcase<-prediction()
  # print(paste("No of Test cases",":",sum(as.factor(testcase)),sep=""))
  print(testcase)
})


output$esr<- renderPrint({
  
  mm<-ppf()
  
  # print(mm[1,c(1)])
  oose <- 1 - mm[1,c(1)]
  print(oose) 
})



output$confplot<- renderPlot({
  dd<-as.data.frame(conf())
  plot <- ggplot(dd)
  plot + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) + scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) + labs(fill="Normalized\nFrequency")
  
  
  
})




  
}) 








    

















