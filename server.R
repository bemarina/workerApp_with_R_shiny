
library(opencpu)
library(httr)
library(jsonlite)
library(distcomp)
library(shiny)
library(digest)


shinyServer(function(input, output, session) {
  
  #verify whether file has extension "RDS"
  checkfileISrds<-function(fileName){
    checkfileName = tryCatch({
      fileName <- readRDS(fileName)
    },  
    warning=function(fileName) {  
      return(fileName)
    },
    error=function(fileName) {  
      return(fileName)
    })
  }
  
  #Read the project file 
  readProject <- reactive({      
    if(input$uploadPro == 0) return()     
    isolate({      
      shiny::validate(
        need(input$projectfile != "", "Please select a project definition file.")
      )              
      inFile <- input$projectfile
      if (is.null(inFile))
        return(NULL)           
      #check file is indeed RDS    
      checkRDS <<- checkfileISrds(inFile$datapath) 
      #if there is an error reading the file
      if ('message' %in% names(checkRDS)){
        proObj <- checkRDS
      }else{
        #there's no error and the file is indeed an RDS
        proObj <- readRDS(inFile$datapath)
      }
      return(proObj)
    })
  })
  
  #print the contents of object project (summary)
  contentInput <- reactive({   
    if(input$uploadPro == 0) return()  
    isolate({
      proObj1<<-readProject() 
      if (!('message' %in% names(checkRDS))){
        printProSumm(proObj1)
      }else{            
        cat('Your data file does not seem to be a \'.RDS\' file.\n')
        if (class(checkRDS)[2]=="error"){
          cat('Error:\n',checkRDS$message)            
        }else{
          cat('Warning:\n',checkRDS$message)
        }   
        print(checkRDS)
      }      
    })    
  })  
  
  # prints the contents of the RDS file (when it's an actual project)
  printProSumm <- function(proObj){
    cat("Project Unique ID:", proObj$defnId, "\n")
    cat("Project Name: ", proObj$name,"\n")
    cat("Project Description: ", proObj$description,"\n")
    cat("Type of computation: ", proObj$compType,"\n")
    cat("Formula: ", proObj$formula,"\n")
    cat ('Variables needed are: ')
    numvar = length(proObj$variables)    
    for (i in 1: numvar){
      cat (proObj$variables[[i]]$Name,' ')
    }  
  }
  
  output$contentPro <- renderPrint({
    contentInput()
  })
  
  output$fileRDS <- reactive({
    fileisRds <- 0
    proObj2<-readProject()
    if (exists("checkRDS")){
      if (!('message' %in% names(checkRDS))){
        fileisRds <- 1
      }
    }
    return(fileisRds)
  })    
  outputOptions(output, "fileRDS", suspendWhenHidden=FALSE) 
  
  output$contentData <- renderPrint({
    contentInputData()
  })
  
  #Data Upload/print summary  
  #verify whether file has extension "CSV"
  checkfileIScsv<-function(fileName){
    checkfileName = tryCatch({
      fileName <- read.csv(fileName)
    },  
    warning=function(fileName) {  
      return(fileName)
    },
    error=function(fileName) {  
      return(fileName)
    })
  }
  
  
  #Create data frame
  createDF <- reactive({      
    if(input$uploadData == 0) return()     
    isolate({      
      shiny::validate(
        need(input$datafile != "", "Please select a data set")
      )              
      inFile2 <- input$datafile
      if (is.null(inFile2))
        return(NULL)           
      nasNocommas=gsub("\\s","", input$naIn)
      navals<-strsplit(nasNocommas,",")[[1]]
      navector<-c(navals[1:length(navals)])
      #check file is indeed CSV    
      checkCSV <<- checkfileIScsv(inFile2$datapath) 
      if (class(checkCSV)[[1]]=="data.frame"){
        df <- read.csv(inFile2$datapath, na.strings = navector)          
      }else{
        df <- checkCSV
      }            
      return(df)
    })    
  })
  
  
  #print the data frame (summary)
  contentInputData <- reactive({   
    if(input$uploadData == 0) return()  
    isolate({
      dfuser<<-createDF() 
      if (is.data.frame(dfuser)){
        writeLines(paste(str(dfuser), collapse= "\n"))  
      }else{            
        cat('Your data file does not seem to be a \'csv\' file.\n')
        if (class(checkCSV)[2]=="error"){
          cat('Error:\n',checkCSV$message)            
        }else{
          cat('Warning:\n',checkCSV$message)
        }   
        print(checkCSV)
      }      
    })    
  })
  
  
  output$fileCSV <- reactive({
    fileisCSV <- 0
    df22<-createDF()
    if (exists("checkCSV")){
      if (class(checkCSV)[[1]]=="data.frame"){
        fileisCSV <- 1
      }
    }
    return(fileisCSV)
  })    
  outputOptions(output, "fileCSV", suspendWhenHidden=FALSE) 
  
  output$remindFormula <- renderPrint({
    proO<-readProject() 
    cat("The project formula is: ", proO$formula) 
    })
  
  
  output$uiMatch <- renderUI({
    proAgain<-readProject()   
    numVar1=length(proAgain$variables)
    names2Match=numeric(0)
    for (k in 1: numVar1){
      names2Match=c(names2Match,proAgain$variables[[k]]$Name)
    }
    dfuserVarChoices<-createDF() ## this allows for changes on the fly
    obj2 <- names2Match ## obj2 is a shorter name
    lapply(1:length(obj2), function(i) {
      selectInput(noquote(obj2[i]), strong(paste0('Select ', noquote(obj2[i]))),
                                  choices = names(dfuserVarChoices))      
      })     
  })
  
  
  output$varMap <- renderPrint({
    
    proAgain<-readProject()  
    
    varsInForm = all.vars(as.formula(proAgain$formula))
    numVar1=length(proAgain$variables)
    names2Match=numeric(0)
    for (k in 1: numVar1){
      names2Match=c(names2Match,proAgain$variables[[k]]$Name)
    }
    obj2 <- names2Match
    for (j in 1:numVar1){
      cat(varsInForm[j],":", input[[ obj2[j] ]],"\n") ## works but with a lot of spaces
    }
    
    })
  
  "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y  
  #count how many 'valid' rows are in the data frame
  checkEmptySet <- function(newDF){      
    indx2rm=numeric(0)
    allRows=c(1:nrow(newDF))  
    for (i in 1:nrow(newDF)){  
      if (any(is.na(newDF[i,]))){
        ## save index of row to remove
        indx2rm=c(indx2rm,i)
      }    
    }  
    ## remove rows with NA from newDF (data frame for the specific formula)
    validRows=allRows %w/o% indx2rm  
    validRowsLen = length(validRows)   
    ## add different messages for different values of the validRowslen
    return(validRowsLen)  
  }
  
  
  ## create a function that receives as input the project object (.RDS)
  ## and return a vector with the NAMES in the formula
  getProjectFormulaNames <- function(projectObject){
    varsInFormula <- all.vars(as.formula(projectObject$formula))
    numVars <-length(projectObject$variables)
    names2Match <- numeric(0)
    for (k in 1: numVars){
        names2Match=c(names2Match,projectObject$variables[[k]]$Name)
    }     
    namesInProFormula <- names2Match ## this vector contains the NAMES in the PROJECT FORMULA
  
    return(namesInProFormula)
  }
  
  
  ## create a function that receives as input the vector of NAMES in PROJECT
  ## return CHOICES of USER
  
  getUserChoices<-function(namesInFormula){
    namesChosen<-numeric(0)
    for (k in 1:length(namesInFormula)){
        namesChosen<-c(namesChosen, input[[ namesInFormula[k] ]])        
    }
    return(namesChosen)
  }
  
  
  output$validRows <- renderPrint({
    if(input$checkEmpty == 0) return()   
    isolate({   
        dfuserValRows<-createDF() ## create it again to allow for changes on the fly
        proAgain<-readProject()                  
        ## this vector contains the NAMES in the PROJECT FORMULA
        obj2<-getProjectFormulaNames(proAgain)
        ## this vector contains the USER CHOICES for the NAMES in the formula
        chosenColumns<-getUserChoices(obj2)    
        dfDataUser1 <- dfuserValRows[,chosenColumns]
        valrows=checkEmptySet(dfDataUser1)         
        if(valrows==0){
                cat(sprintf('The data frame for your formula is empty!\n')) 
            }else if (valrows==1){
                cat(sprintf('The data frame for your formula has only %i valid row.\n', valrows))        
            }else{      
                cat(sprintf('The data frame for your formula has %i valid rows.\n', valrows))
      }      
    })    
  })
  
  
  checkNumRowsAgain<-reactive({   
    if(input$checkEmpty == 0) return()  
    isolate({  
        valRows<<-0
        dfNonEmptyCheck <- createDF()
        proObjRowsCheck<-readProject() 
        objFormNames<-getProjectFormulaNames(proObjRowsCheck)
        userChoices<-getUserChoices(objFormNames)         
        dfNonEmptySet <- dfNonEmptyCheck[,userChoices]   
        validRowsNewDF <- checkEmptySet(dfNonEmptySet)       
        if (validRowsNewDF>1){
                valRows<<-1    
            }
        })        
  })
  
  
  
  output$validRowsCheck <- reactive({
        vr1<-0
        checkNumRowsAgain()        
        if (exists("valRows")){
            if (valRows==1){
                vr1=1    
            } 
        }
        ### have to make sure the names Chosen are different --- Issue a warning!             
        return(vr1)
  })    
  outputOptions(output, "validRowsCheck", suspendWhenHidden=FALSE) 
  
  checkDirExists<-function(dirName){
    checkDirIs = tryCatch({
      expr <- sprintf('ls %s',dirName)
      isDir <- system(expr, intern = TRUE, ignore.stderr = TRUE)
    },  
    warning=function(isDir) {  
      return(isDir)
    },
    error=function(isDir) {  
      return(isDir)
    })
  }
   
  checkUploadComp<-function(url,defn,data){
    answer = tryCatch({
      upload <- uploadNewComputation(url,defn,data)
    },  
    warning=function(upload) {  
      return(upload)
    },
    error=function(upload) {  
      return(upload)
    })
  }
    
  output$serializeMsg<-renderPrint({
    if(input$runOnVM == 0) return()
    isolate({ 
      userDF11 <-createDF()
      proVMrun <- readProject() 
        
      #vmResult <-uploadNewComputation(input$slaveURL,proVMrun,userDF11)      
      vmResult<- checkUploadComp(input$slaveURL,proVMrun,userDF11)
      
      if (length(class(vmResult))==1){
        cat('Success! Instance created.\n')      
      }else{                
        cat('The message cannot be serialized.\n')
          if (class(vmResult)[2]=="error"){
            cat('Error:\n',vmResult$message)            
          }else{
            cat('Warning:\n',vmResult$message)
          }        
      }      
      })
  })
      
  
  
  
  
  
  #### ----- functions to make tabs active sequentially ----- 
  ## 1: Move from "Project/Data Summary" to "Variable Matching / Data Check" when user clicks on "Proceed to.."
  observe({
    if (input$matchVarsCheckData > 0){ 
        session$sendCustomMessage('activeNavs', 'Variable Matching')
        updateTabsetPanel(session, inputId="mynavlist", selected="varMatch")
    }
  })
  
  observe({
    if (input$varMatchDone > 0){ 
      session$sendCustomMessage('activeNavs', 'Data Check')
      updateTabsetPanel(session, inputId="mynavlist", selected="dataCheck")
    }
  })
  
  
  observe({
     if (input$dataCheckDone > 0){ 
      session$sendCustomMessage('activeNavs', 'Dry Run')
      updateTabsetPanel(session, inputId="mynavlist", selected="dryRun")
    }
  })
  
  
  
  
})
