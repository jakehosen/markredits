markserve <- function(input, output, session) {
######## DATA CONTROLS
  flags = reactiveValues(d=NULL,f=NULL)  # placeholder for model flagged data
  # d is the data with a flag column
  # f is the flag list
  dat = reactiveValues(up=NULL)  # placeholder for data
  # Load new data
  dataup = reactive({
    inFile = input$file1
    print(input$file1)
    if (is.null(inFile))
      return(NULL)
    readr::read_csv(inFile$datapath)
  })
  # If data is loaded, update variable list
  observe({
    if(!is.null(dataup())){
      dat$up = dataup()
      cvars = colnames(dat$up)[!colnames(dat$up)%in%c("DateTime","flags")]
      updateSelectizeInput(session, "vari", choices=cvars, server=TRUE)
      output$fitbutton = renderUI(actionButton("fitButton", "Fit it!"))
    }
  })

  if(datain){
    if(colnames(data)[1]!="DateTime"){
      ccn = colnames(data)
      data = data %>% select_(.dots=c("DateTime",ccn[-grep("DateTimeUTC",ccn)]))
    }
    dat$up = data
    cvars = colnames(dat$up)[!colnames(dat$up)%in%c("DateTime","flags")]
    updateSelectizeInput(session, "vari", choices=cvars, server=TRUE)
    output$fitbutton = renderUI(actionButton("fitButton", "Fit it!"))
  }else{ # data don't exist or are incorrect
    # display the file upload interface
    output$fileup = renderUI(HTML(paste0(
      "<i>Data requirements:</i> One column named 'DateTime' with an R-readable date or time format and at least one data column.",
      fileInput('file1', '1. Upload data to test for anomalies.', accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))
    )))
  }
######## MODEL CONTROLS
  # If the model directory exists, update model list
  if(dir.exists("previously_flagged_data/")){
    fdfiles = grep(".Rda", list.files("previously_flagged_data/"), value=TRUE)
    if(length(fdfiles) > 0){
      emods = strsplit(fdfiles, ".Rda")
      updateSelectizeInput(session, "predat", choices=c("Choose a dataset...",emods), server=FALSE)
    }
  }
  # Fit the model
  observeEvent(input$fitButton,{
    if( (input$traindat=="old" & input$predat%in%c("No data found...","Choose a dataset...")) |
      (input$traindat=="new" & is.null(input$vari)) ){
      # If nothing is selected in the open tab, throw an error
      output$fiterr = renderUI(HTML("<font color='red'><i>Please choose training variables or a previously-trained dataset.</i></font>"))
    }else{
      output$fiterr = renderUI(HTML("<font color='green'><i>Model fitting...</i></font>"))
      if(input$traindat=="old"){ # If a pre-existing model is selected, load it
        load(paste0("previously_flagged_data/",input$predat,".Rda")) # load existing model objects
        # needs to have "mod" and "mcols"
        traindat = flagged
        mcols = colnames(traindat)
        updateSelectizeInput(session, "flag_name", choices=flagids, server=FALSE)
      }else{ # Else, fit a new model
        mcols = input$vari
        if(input$filt==""){
          traindat = dat$up %>% select_(.dots=mcols)
        }else{
          traindat = dat$up %>% filter_(.dots=input$filt) %>% select_(.dots=mcols)
        }
        if(input$delt) traindat = traindat %>% mutate_each(funs( delta=c(0,diff(.)) ))
      }
      # check if all data columns in model are in data
      if(all(mcols%in%colnames(dat$up))){
        mod = e1071::svm(traindat, nu=input$nu, scale=TRUE, type='one-classification', kernel='radial')
        # predict flags for uploaded data
        dat$up$f = 0
        preddat = dat$up %>% select_(.dots=mcols)
        if(input$delt) preddat = preddat %>% mutate_each(funs( delta=c(0,diff(.)) ))
        dat$up$f[!predict(mod, preddat)] = 1 # flagged
        dat$f = factor(dat$f,levels=0:2)
        d = dat$up %>% select_(.dots=c("DateTime",mcols,"f")) %>% gather(variable, value, -DateTime, -f)
        d$variable = factor(d$variable, levels=unique(d$variable))
        flags$d = d
        output$fiterr = renderUI(HTML(""))
      }else{
        output$fiterr = renderUI(HTML(paste("<font color='red'>Model columns <i>",mcols[which(!mcols%in%colnames(dat$up))],"</i> not in loaded data.<br>Please check your data and reupload.</font>")))
      }
    }
  })
######## PLOT CONTROLS
  # Function for getting rows that are in brush
  brushedLogic = function(df,pb) c(df[pb$mapping$panelvar1]==pb$panelvar1 & df[pb$mapping$x]>pb$xmin & df[pb$mapping$x]<pb$xmax)
  ranges = reactiveValues(x=NULL)
  observeEvent(input$plot_dblclick,{
    brush = input$plot_brush
    if(!is.null(brush)){
      ranges$x = as.POSIXct(c(brush$xmin, brush$xmax),origin='1970-01-01')
    }else{
      ranges$x = NULL
    }
  })
  observeEvent(input$flag_reset,{ ranges$x = NULL })
  observe({ # draw plot
    if(!is.null(flags$d)){
      output$flagplot = renderPlot({
        ptsz = rep(1,nrow(flags$d))
        ptsz[flags$d$f > 0] = 4
        ggplot(flags$d, aes(DateTime, value, col=f)) +
          geom_point(shape=20,size=ptsz) +
          facet_grid(variable~.,scales='free_y') +
          theme(legend.position='none') +
          scale_colour_manual(values=cbPalette) +
          coord_cartesian(xlim=ranges$x)
      }, height=200*length(unique(flags$d$variable)))
    }
  })
######## FLAG CONTROLS
  # # REMOVE NA VALUES
  observeEvent(input$na_rm,{
    navals = brushedLogic(flags$d, input$plot_brush)
    if(any(navals)){
      var = input$plot_brush$panelvar1
      nas = unique(flags$d$value[navals]) #the unique values in the NA brush
      flags$d$value[which(flags$d$value %in% nas & flags$d$variable == var)] = NA # assign all those matching values as NA
    }
  })
  # # ADD A NEW FLAG
  observeEvent(input$flag_new,{
    newflags = brushedLogic(flags$d, input$plot_brush)
    if(any(newflags)) flags$d$f[newflags] = 1
  })
  # # ERASE A FLAG
  observeEvent(input$flag_erase,{
    eraseflags = brushedLogic(flags$d, input$plot_brush)
    if(any(eraseflags)) flags$d$f[eraseflags] = 0
  })
  # # CLEAR ALL UNSTORED FLAGS
  observeEvent(input$flag_clear,{
    flags$d$f[flags$d$f==1] = 0
    ranges$x = NULL
  })
  # # STORE FLAGS
  observeEvent(input$flag_store,{
    storeflags = brushedLogic(flags$d, input$plot_brush)
    if(any(storeflags) & !is.null(input$flag_name)){
      flags$d$f[which(storeflags&flags$d$f==1)] = 2 # stored!
      flaglist = list(ID=input$flag_name, comment=input$flag_comment, flags=flags$d$DateTime[storeflags])
      if(is.null(flags$f)){
        flags$f = list(flaglist)
      }else{
        flags$f = c(flags$f, list(flaglist))
      }
      flagids = unique(unlist(lapply(flags$f, function(x) x$ID))) # unique flags
      updateSelectizeInput(session, "flag_name", choices=flagids, server=FALSE)
    }else{
      output$loadtext = renderText("Please select flagged points and/or enter a flag name/ID")
    }
  })
  # # SAVE FLAGS
  observeEvent(input$flag_save,{
    # need DF with model fit columns and rows that are not in flags
    # and a list of the unique flag names
    #remove rows with stored flags
    flagged = flags$d %>% filter(f!=2) %>% spread(variable,value) %>% select_(.dots=input$vari) # not flagged data
    flagids = unique(unlist(lapply(flags$f, function(x) x$ID))) # unique flags
    if(!dir.exists("previously_flagged_data/")) dir.create("previously_flagged_data/")
    savefn = function(){ paste0("previously_flagged_data/",sub("(.*)\\..*", "\\1", input$file1$name),"-",Sys.Date(), ".Rda") }
    save(flagged,flagids,file=savefn())
    output$loadtext = renderText(paste0("Saved ",length(flagids)," flags."))
    updateTextInput(session, "flag_comments", value="")
    updateSelectizeInput(session, "flag_name", choices=flagids, server=FALSE)
  })
  # DOWNLOAD FLAGGED DATA
  observeEvent(input$flag_download,{
    filename1 = function(){ paste0(sub("(.*)\\..*", "\\1", input$file1$name),"-FLAGGED-",Sys.Date(),".csv") }
    flagIDs = do.call("rbind", lapply(flags$f, function(x) data_frame(DateTime=x$flags,Flag=x$ID)))
    flaggeddat = left_join(dataup(),flagIDs, by="DateTime")
    filename2 = function(){ paste0(sub("(.*)\\..*", "\\1", input$file1$name),"-FLAGMETA-",Sys.Date(),".csv") }
    flaggedmeta = do.call("rbind", lapply(flags$f, function(x) data_frame(Flag=x$ID,MinDate=min(x$flags),MaxDate=max(x$flags),Comments=x$comment)))
    readr::write_csv(flaggeddat, filename1)
    readr::write_csv(flaggedmeta, filename2)
    output$loadtext = renderText(paste0("Saved ",filename1," and ",filename2," to current working directory."))
  })
######### EXAMPLE CODE
  observeEvent(input$egdata,{ # generate new random eg data
    y = runif(200,-3,2)
    x = rnorm(200,y*y,2)
    ytest = rnorm(100,mean(y),sd(y))
    xtest = rnorm(100,mean(x),sd(x))
    mod = e1071::svm(cbind(x,y), nu=0.01, scale=TRUE, type='one-classification', kernel='radial')
    pred = predict(mod,cbind(xtest,ytest))
    pcol = rep("#E69F00",100)
    pcol[pred] = "#009E73"
    output$egplot = renderPlot({
      par(mar=c(4,4,0,0))
      plot(x,y,pch=1,xlim=range(c(x,xtest)),ylim=range(c(y,ytest)),bty="n",las=1,xlab="V1",ylab="V2")
      if(input$egshow){
        points(xtest,ytest,pch=21,bg=pcol,cex=1.1)
      }
      legend("topright",c("Training data","Testing - Good","Testing - Anomaly"),pch=21,pt.bg=c("#ffffff","#009E73","#E69F00"),pt.cex=c(1,1.1,1.1))
    })
  })
}
