library(shiny)
library(shinyBS)
library(ggplot2)
library(png)
library(grid)
library(gtable)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(shinyjs)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  observeEvent(input$useAge, {
    if(input$useAge == FALSE)
    {
      input$age ==0;
      updateNumericInput(session,"age", value= 0);
    }
    
  })
  
  observeEvent(input$tabselected,{
    
               if(input$tabselected=="MMRM")
               {
                  shinyjs::hide(id= "mysidebar")
               }
    else shinyjs::show(id="mysidebar")
  }
  )
  
  output$pspPlot <- renderPlot({
    g<- ggplot(psp[psp$x==input$n & psp$sex ==input$sex & psp$age== input$age & psp$inputStudyIdList == paste(input$study,collapse=","),], aes(x = Var, y = COL1*100, fill=X_NAME_)) +
      geom_bar(stat='identity', color="black") +
      labs(title = "", y = "Percent of patients\n", x = "", fill = "impairment level") +
      scale_fill_manual(values=c("#ff6961","#ffb347", "#77dd77")) +
      guides(fill = guide_legend(override.aes = list(colour = NULL), title=NULL)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.text.x = element_text(size = 18, colour = 'black', angle=45, hjust = 1),
            axis.text.y = element_text(size = 18, colour = 'black'),
            axis.title.x = element_text(size = 18, colour = 'black'),
            axis.title.y = element_text(size = 18, colour = 'black'),
            legend.text = element_text(size = 16, colour = 'black'),
            legend.key = element_rect(colour = "black"),
            legend.position="none")+
      theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5)) +
      scale_y_continuous(breaks=seq(0,100,10))
    g
    
    
  })
  
  
  
  output$hover_info <- 
    renderUI({
      if(!is.null(input$plot_hover))
      {
        hover=input$plot_hover
        position = hover$x[1]
        
        addTooltip(session, "distPlot", title = fib(position), placement = "right", trigger = "click",
                   options = list(container = "body"))
      }
      
    })
  
  output$disease_severity <- 
    renderUI({
      SeverityText = " ";
      if (input$n < 25) SeverityText = "Mild Disease"
      if (input$n < 45 & input$n >=25) SeverityText = "Moderate Disease"
      if (input$n >= 45) SeverityText ="Severe Disease"
      print(SeverityText)
    })
  
  #####################
  
  output$cbdPlot <- renderPlot({
    
    g<- ggplot(cbd[cbd$x==input$n & cbd$sex ==input$sex & cbd$age== input$age & cbd$inputStudyIdList == paste(input$study,collapse=","),], aes(x = Var, y = COL1*100, fill=X_NAME_)) +
      geom_bar(stat='identity', color="black") +
      labs(title = "", y = "Percent of patients\n", x = "", fill = "impairment level") +
      scale_fill_manual(values=c("#ff6961","#ffb347", "#77dd77")) +
      guides(fill = guide_legend(override.aes = list(colour = NULL), title=NULL)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.text.x = element_text(size = 18, colour = 'black', angle = 45, hjust = 1),
            axis.text.y = element_text(size = 18, colour = 'black'),
            axis.title.x = element_text(size = 18, colour = 'black'),
            axis.title.y = element_text(size = 18, colour = 'black'),
            legend.text = element_text(size = 16, colour = 'black'),
            legend.key = element_rect(colour = "black"),
            legend.position="none")+
      theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5)) +
      scale_y_continuous(breaks=seq(0,100,10))
    g
    
  })
  output$hover_cbd <- 
    renderUI({
      if(!is.null(input$cbdplot_hover))
      {
        hover=input$cbdplot_hover
        position = hover$x[1]
        
        addTooltip(session, "cbdPlot", title = fib(position), placement = "right", trigger = "click",
                   options = list(container = "body"))
      }
      
    })
  
  
  ###########
  
  
  output$cgiPlot <- renderPlot({
    
    g<- ggplot(cgi[cgi$x==input$n2 & cgi$sex ==input$sex & cgi$age== input$age & cgi$inputStudyIdList == paste(input$study,collapse=","),], aes(x = Var, y = COL1*100, fill=X_NAME_)) +
      geom_bar(stat='identity', color="black") +
      labs(title = "", y = "Percent of patients\n", x = "", fill = "impairment level") +
      scale_fill_manual(values=c("#ff6961","#ffb347", "#77dd77")) +
      guides(fill = guide_legend(override.aes = list(colour = NULL), title=NULL)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.text.x = element_text(size = 18, colour = 'black', angle = 45, hjust = 1),
            axis.text.y = element_text(size = 18, colour = 'black'),
            axis.title.x = element_text(size = 18, colour = 'black'),
            axis.title.y = element_text(size = 18, colour = 'black'),
            legend.text = element_text(size = 16, colour = 'black'),
            legend.key = element_rect(colour = "black"),
            legend.position="none")+
      theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5)) +
      scale_y_continuous(breaks=seq(0,100,10))
    g
    
  })
  
  output$hover_cgi <- 
    renderUI({
      if(!is.null(input$cgiplot_hover))
      {
        hover=input$cgiplot_hover
        position = hover$x[1]
        
        addTooltip(session, "cgiPlot", title = fib(position), placement = "right", trigger = "click",
                   options = list(container = "body"))
      }
      
    })
  
  output$cgi_severity <- 
    renderUI({
      CgiSeverityText = " ";
      if (input$n2 < 3)  CgiSeverityText = "Mild Disease"
      if (input$n2 < 4 & input$n2 >=3)  CgiSeverityText = "Moderate Disease"
      if (input$n2 >= 4) CgiSeverityText ="Severe Disease"
      print(CgiSeverityText)
    })
  
  
  ###########
  output$seadlPlot <- renderPlot({
 
    g<- ggplot(seadl[seadl$x== (input$n3) & seadl$sex ==input$sex & seadl$age== input$age & seadl$inputStudyIdList == paste(input$study,collapse=","),], aes(x = Var, y = COL1*100, fill=X_NAME_)) +
      geom_bar(stat='identity', color="black") +
      labs(title = "", y = "Percent of patients\n", x = "", fill = "impairment level") +
      scale_fill_manual(values=c("#ff6961","#ffb347", "#77dd77")) +
      guides(fill = guide_legend(override.aes = list(colour = NULL), title=NULL)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.text.x = element_text(size = 18, colour = 'black', angle = 45, hjust = 1),
            axis.text.y = element_text(size = 18, colour = 'black'),
            axis.title.x = element_text(size = 18, colour = 'black'),
            axis.title.y = element_text(size = 18, colour = 'black'),
            legend.text = element_text(size = 16, colour = 'black'),
            legend.key = element_rect(colour = "black"),
            legend.position="none")+
      theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5)) +
      scale_y_continuous(breaks=seq(0,100,10))
    g
    
  })
  
  output$hover_seadl <- 
    renderUI({
      if(!is.null(input$seadlplot_hover))
      {
        hover=input$seadlplot_hover
        position = hover$x[1]
        
        addTooltip(session, "seadlPlot", title = fib(position), placement = "right", trigger = "click",
                   options = list(container = "body"))
      }
      
    })
  
  output$seadl_severity <- 
    renderUI({
      SeadlSeverityText = " ";
      if (input$n3 < 25)  SeadlSeverityText = "Severe Disease"
      if (input$n3 < 45 & input$n3 >=25)  SeadlSeverityText = "Moderate Disease"
      if (input$n3 >= 45) SeadlSeverityText = "Mild Disease" 
      print(SeadlSeverityText)
    })
  
  
  
  
  fib <- function(position) {
    
    if( 0.5 < position & position < 1.5)
    {
      text_output =  "Ocular Motor questions identify difficulties in eye movement. 
      There are 5 questions resulting in scores ranging from 0-16."
    }
    if( 1.5 < position & position <2.5)
    {
      text_output =  "Gait and Midline questions test for difficulties in balance,
      movement, stability and sitting or rising. There are 5 questions with a maximum score of 20."
    }
    if( 2.5 < position & position <3.5)
    {
      text_output =  "History is measured in 7 questions on daily living, falls, incontinence and sleep.
      A maximum score is 24."
    }
    if( 3.5 < position & position <4.5)
    {
      text_output =  "Limb Motor function is measured over 6 questions on Movement, tapping and tremor.
      A maximum score is 16."
    }
    if( 4.5 < position & position <5.5)
    {
      text_output =  "The mentation category focuses on whether patients suffer disorientation or have cognitive difficulties. 
      There are 4 questions and a maximum score is 16."
    }
    if( 5.5 < position & position <6.5)
    {
      text_output =  "The questions on Bulbar focus on difficulties in speech and swallowing. 
      Two questions are asked with a maximum score of 8."
    }
    return(text_output)
  }
  
  
  output$mmrmPlot <- renderPlotly({
    # use the key aesthetic/argument to help uniquely identify selected observations
    subscore_enum <- c('PSPRS_Total', 'PSPRS_Sub_Mentation', 'PSPRS_Sub_Bulbar', 'PSPRS_Sub_GaitMidline', 
                       'PSPRS_Sub_LimbMotor', 'PSPRS_Sub_OcularMotor', 'PSPRS_Sub_History');
    
    subscore_legend <- c('PSP-RS Total', 'Mentation', 'Bulbar', 'Gait', 
                       'Limb', 'Ocular', 'History');
    
    colours <-c('Grey', 'Green',  'Blue', 'Purple', 'Black', 'orange','red')
    linestyle <-c('dashdot', 'solid',  'solid', 'solid', 'solid', 'solid','solid')
    markerstyle <-c('star-dot', 'circle',  'circle', 'circle', 'circle', 'circle','circle')
    groups <- length(subscore_enum)
    
    jitter <- (sample(1:10, 7, replace=F)-5)/10
    
    x <- list(
      title = "Weeks",
      
      tickvals= c(6, 13, 26, 39, 52),
      ticktext=c(6, 13, 26, 39, 52)
    )
    y <- list(
      title = "Effect Size relative to baseline",
      showline=TRUE
    )
    MMRMdataset$outputText <- paste("x = ", MMRMdataset$week, ", y =", round(MMRMdataset$effectsize, 2), " +/- ", round(MMRMdataset$StdErr, 2)) 
    
    
    p<- plot_ly(text = ~outputText, hoverinfo = 'text') %>%
      layout(xaxis = x, yaxis = y )
    
    for(i in 1:length(subscore_enum)){
      
      plotdata = MMRMdataset[MMRMdataset$qstvar== subscore_enum[i] & MMRMdataset$study == "all" & MMRMdataset$disease == "PSP",]
      plotdata$weekj <- plotdata$week + jitter[i]
      p<- add_trace(p, data = plotdata,
                    x = ~weekj, y = ~effectsize, type="scatter", mode="lines+markers",  name = subscore_legend[i], line = list(color = colours[i], width= 2, dash = linestyle[i]),
                    marker=list(symbol=markerstyle[i], size= 10),
                    error_y = list(type = "data",color= colours[i], symmetric='False', array =c(plotdata$upperci - plotdata$effectsize), 
                                   arrayminus = c( plotdata$effectsize -plotdata$lowerCI)))
    }
    p
  })
  
  
  output$proportionaloddsPlot <- renderPlotly({
    # use the key aesthetic/argument to help uniquely identify selected observations
    x <- list(
      title = "PSP Rating Scale ",
      titlefont=list(
        size=16),
      tickfont = list(
        size= 14)
    )
    y <- list(
      title = "Proportion of patients with impairment",
      autotick=TRUE,
      ticks='outside',
      titlefont=list(
        size=16),
      tickfont = list(
        size= 14)
    )
    subscore_enum2 <- c('Mentation', 'Bulbar', 'Gait', 'Limb',
                       'Ocular', 'History');
    
   
    colours <-c('Green',  'Blue', 'Purple', 'Black', 'orange','red')
    
    groups <- length(subscore_enum2)
    
    pspsubset <- psp[   psp$sex == input$sex & 
                        psp$age== input$age & 
                        psp$X_NAME_ == input$POMSeverity &
                        psp$inputStudyIdList == paste(input$study,collapse=",")
                      ,]
    
    pmom<- plot_ly() %>%
      layout(xaxis = x, yaxis = y)
    for(i in 1:length(subscore_enum2)){
      plotdata = pspsubset[pspsubset$Var == subscore_enum2[i],]
      if(input$POMSeverity == "Severe impairment")
      {
        plotdata$COL2 <- plotdata$COL1
      }
      else plotdata$COL2 <- 1-plotdata$COL1
      pmom<- add_trace(pmom, data = plotdata,
                    x = ~x, y = ~COL2, type="scatter", mode="lines", name= subscore_enum2[i], line = list(color = colours[i], width= 2))
    }
    pmom
  })
  
 
  

    })


