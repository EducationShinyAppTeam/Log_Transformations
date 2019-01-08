#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(DAAG)
library(shinyjs)
library(shinyWidgets)

disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Pick a data set and view the effect of the log transform on the X and or Y variables.",
      type = "info"
    )
  })
  #Go to overview Button
  observeEvent(input$goover, {
    updateTabItems(session, "tabs", "overview")
  })
  #Explore Button
  observeEvent(input$explore, {
    updateTabItems(session, "tabs", "transformations")
  })
  # Adding in Data
  worlddata = read.csv('world2.csv')
  worlddata = worlddata[,-c(1,2,7)]
  worlddata = worlddata[-c(36),]
  worlddata$income = as.numeric(worlddata$income)
  quakedata = datasets::attenu
  animaldata = MASS::Animals
  
  output$animalDownload <- downloadHandler(
      filename = function() {
        paste('animal-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(animaldata, con)
      }
    )
  
  output$quakeDownload <- downloadHandler(
    filename = function() {
      paste('quake-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(quakedata, con)
    }
  )
  
  output$worldDownload <- downloadHandler(
    filename = function() {
      paste('world-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(worlddata, con)
    }
  )
  # Animal Plots
  output$animalPlot = 
    renderPlot({
      
      #If they don't checkbox anything
      if(length(input$transforms) == 0)
      {
        plot(animaldata[,input$Xanimal], animaldata[,input$Yanimal], xlab = input$Xanimal, ylab = input$Yanimal, main = "Brain(g) vs Body(kg)")
        # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]), col = 'red')
        # legend("topright", bty = "n", legend = paste("R2 is", 
        #                                              format(summary(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]))$adj.r.squared, digits = 4)))
      }
      
      # If they checkbox one of them
      else if(length(input$transforms) == 1)
      {
        # If they only checkbox the Transform Y option
        if(input$transforms == 'Transform Y')
        {
          plot(animaldata[,input$Xanimal], log(animaldata[,input$Yanimal]), xlab = input$Xanimal, ylab = paste("Log:", input$Yanimal), main = "Brain(g) vs Body(kg)")
          # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]), col = 'red')
          # legend("topright", bty = "n", legend = paste("R2 is", 
          #                                              format(summary(lm(animaldata[,input$Xanimal]~log(animaldata[,input$Yanimal])))$adj.r.squared, digits = 4)))
        }
        # If they only checkbox the Transform X option
        else if(input$transforms == 'Transform X')
        {
          plot(log(animaldata[,input$Xanimal]), animaldata[,input$Yanimal], xlab = paste("Log:", input$Xanimal), ylab = input$Yanimal, main = "Brain(g) vs Body(kg)")
          # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]), col = 'red')
          # legend("topright", bty = "n", legend = paste("R2 is", 
          #                                              format(summary(lm(log(animaldata[,input$Xanimal])~animaldata[,input$Yanimal]))$adj.r.squared, digits = 4)))
        }      
        
      }
      #If they check both boxes
      else #Doesn't plot line, but plots R-squared value
      {
        plot(log(animaldata[,input$Xanimal]), log(animaldata[,input$Yanimal]), xlab = paste("Log:", input$Xanimal), ylab = paste("Log:", input$Yanimal), main = "Brain(g) vs Body(kg)")
        # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]), col = 'red')
        # legend("topright", bty = "n", legend = paste("R2 is", 
        #                                              format(summary(lm(log(animaldata[,input$Xanimal])~log(animaldata[,input$Yanimal])))$adj.r.squared, digits = 4)))
      }
      
    })
  
  
  #World Plots(bestfit line doesn't work for Transform y) Maybe Can use ggplot
  output$worldPlot = 
    renderPlot({
      
      if(length(input$transforms) == 0)
      {
        plot(worlddata[,input$Xworld], worlddata[,input$Yworld], xlab = input$Xworld, ylab = input$Yworld, main = paste(input$Xworld, "vs", input$Yworld))
        #abline(lm(worlddata[,input$Xworld]~worlddata[,input$Yworld]), col = 'red')
        #legend("topright", bty = "n", legend = paste("R2 is", 
                                                     #format(summary(lm(worlddata[,input$Xworld]~worlddata[,input$Yworld]))$adj.r.squared, digits = 4)))
      }
      
      else if(length(input$transforms) == 1)
      {
        if(input$transforms == 'Transform Y')
        {
          plot(worlddata[,input$Xworld], log(worlddata[,input$Yworld]), xlab = input$Xworld, ylab = paste("Log:", input$Yworld), main = paste(input$Xworld, "vs", "Log:", input$Yworld))
          #abline(lm(worlddata[,input$Xworld]~worlddata[,input$Yworld]), col = 'red')
        }
        else if(input$transforms == 'Transform X')
        {
          plot(log(worlddata[,input$Xworld]), worlddata[,input$Yworld], xlab = paste("Log:", input$Xworld), ylab = input$Yworld, main = paste("Log:", input$Xworld, "vs", input$Yworld))
          #abline(lm(worlddata[,input$Xworld]~worlddata[,input$Yworld]), col = 'red')
        }
      }
      
      else
      {
        plot(log(worlddata[,input$Xworld]), log(worlddata[,input$Yworld]), xlab = paste("Log:", input$Xworld), ylab = paste("Log:", input$Yworld), main = paste("Log:", input$Xworld, "vs", "Log:", input$Yworld))
        #abline(lm(worlddata[,input$Xworld]~worlddata[,input$Yworld]), col = 'red')
      }
      
    })
  
  output$quakePlot = 
    renderPlot({
      
      #If they don't checkbox anything
      if(length(input$transforms) == 0)
      {
        plot(quakedata[,input$Xquake], quakedata[,input$Yquake], xlab = input$Xquake, ylab = input$Yquake, main = paste(input$Xquake, "vs", input$Yquake))
        # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]), col = 'red')
        # legend("topright", bty = "n", legend = paste("R2 is", 
        #                                              format(summary(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]))$adj.r.squared, digits = 4)))
      }
      
      # If they checkbox one of them
      else if(length(input$transforms) == 1)
      {
        # If they only checkbox the Transform Y option
        if(input$transforms == 'Transform Y')
        {
          plot(quakedata[,input$Xquake], log(quakedata[,input$Yquake]), xlab = input$Xquake, ylab = paste("Log:", input$Yquake), main = paste(input$Xquake, "vs Log:", input$Yquake))
          # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]), col = 'red')
          # legend("topright", bty = "n", legend = paste("R2 is", 
          #                                              format(summary(lm(animaldata[,input$Xanimal]~log(animaldata[,input$Yanimal])))$adj.r.squared, digits = 4)))
        }
        # If they only checkbox the Transform X option
        else if(input$transforms == 'Transform X')
        {
          plot(log(quakedata[,input$Xquake]), quakedata[,input$Yquake], xlab = paste("Log:", input$Xquake), ylab = input$Yquake, main = paste("Log:", input$Xquake, "vs", input$Yquake))
          # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]), col = 'red')
          # legend("topright", bty = "n", legend = paste("R2 is", 
          #                                              format(summary(lm(log(animaldata[,input$Xanimal])~animaldata[,input$Yanimal]))$adj.r.squared, digits = 4)))
        }      
        
      }
      #If they check both boxes
      else #Doesn't plot line, but plots R-squared value
      {
        plot(log(quakedata[,input$Xquake]), log(quakedata[,input$Yquake]), xlab = paste("Log:", input$Xquake), ylab = paste("Log:", input$Yquake), main = paste("Log:", input$Xquake, "vs Log:", input$Yquake))
        # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]), col = 'red')
        # legend("topright", bty = "n", legend = paste("R2 is", 
        #                                              format(summary(lm(log(animaldata[,input$Xanimal])~log(animaldata[,input$Yanimal])))$adj.r.squared, digits = 4)))
      }
    })
  
  output$filePlot = 
    
    renderPlot({
      inFile = input$file
      req(inFile)
      f <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
      #If they don't checkbox anything
      if(length(input$transforms) == 0)
      {
        plot(f[,input$columns], f[,input$columns2], xlab = input$columns, ylab = input$columns2, main = paste(input$columns, "vs", input$columns2))
        # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]), col = 'red')
        # legend("topright", bty = "n", legend = paste("R2 is", 
        #                                              format(summary(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]))$adj.r.squared, digits = 4)))
      }
      
      # If they checkbox one of them
      else if(length(input$transforms) == 1)
      {
        # If they only checkbox the Transform Y option
        if(input$transforms == 'Transform Y')
        {
          plot(f[,input$columns], log(f[,input$columns2]), xlab = input$columns, ylab = paste("Log:", input$columns2), main = paste(input$columns, "vs Log:", input$columns2))
          # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]), col = 'red')
          # legend("topright", bty = "n", legend = paste("R2 is", 
          #                                              format(summary(lm(animaldata[,input$Xanimal]~log(animaldata[,input$Yanimal])))$adj.r.squared, digits = 4)))
        }
        # If they only checkbox the Transform X option
        else if(input$transforms == 'Transform X')
        {
          plot(log(f[,input$columns]), f[,input$columns2], xlab = paste("Log:", input$columns), ylab = input$columns2, main = paste("Log:", input$columns, "vs", input$columns2))
          # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]), col = 'red')
          # legend("topright", bty = "n", legend = paste("R2 is", 
          #                                              format(summary(lm(log(animaldata[,input$Xanimal])~animaldata[,input$Yanimal]))$adj.r.squared, digits = 4)))
        }      
        
      }
      #If they check both boxes
      else #Doesn't plot line, but plots R-squared value
      {
        plot(log(f[,input$columns]), log(f[,input$columns2]), xlab = paste("Log:", input$columns), ylab = paste("Log:", input$columns2), main = paste("Log:", input$columns, "vs Log:", input$columns2))
        # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]), col = 'red')
        # legend("topright", bty = "n", legend = paste("R2 is", 
        #                                              format(summary(lm(log(animaldata[,input$Xanimal])~log(animaldata[,input$Yanimal])))$adj.r.squared, digits = 4)))
      }
      
    })
  
  #Histograms of both variables
  observeEvent(input$hist, {
    output$animalBars =
      renderPlot({
        if(input$loghist1 == TRUE)
        {
          hist(log(animaldata[,input$Xanimal]),
               main = paste("Histogram of Log:", input$Xanimal, sep = ''),
               xlab = paste("Log:", input$Xanimal))
        }
        else{
          hist(animaldata[,input$Xanimal],
               main = paste("Histogram of", input$Xanimal),
               xlab = input$Xanimal)
        }
      })
    output$animalBars2 =
      renderPlot({
        if(input$loghist2 == TRUE)
        {
          hist(log(animaldata[,input$Yanimal]),
               main = paste("Histogram of Log:", input$Yanimal, sep = ''),
               xlab = paste("Log:", input$Yanimal))
        }
        else{
          hist(animaldata[,input$Yanimal],
               main = paste("Histogram of", input$Yanimal),
               xlab = input$Yanimal)
        }
      })
    output$worldBars =
      renderPlot({
        if(input$loghist1 == TRUE)
        {
          hist(log(worlddata[,input$Xworld]),
               main = paste("Histogram of Log:", input$Xworld, sep = ''),
               xlab = paste("Log:", input$Xworld))
        }
        else{
          hist(worlddata[,input$Xworld],
               main = paste("Histogram of", input$Xworld),
               xlab = input$Xworld)
        }
      })
    output$worldBars2 =
      renderPlot({
        if(input$loghist2 == TRUE)
        {
          hist(log(worlddata[,input$Yworld]), 
               main = paste("Histogram of Log:", input$Yworld, sep = ''),
               xlab = paste("Log:", input$Yworld))
        }
        else{
          hist(worlddata[,input$Yworld],
               main = paste("Histogram of", input$Yworld),
               xlab = input$Yworld)
        }
      })
    output$quakeBar2 =
      renderPlot({
        if(input$loghist2 == TRUE)
        {
          hist(log(quakedata[,input$Yquake]),
               main = paste("Histogram of Log:", input$Yquake, sep = ''),
               xlab = paste("Log:", input$Yquake))
        }
        else{
          hist(quakedata[,input$Yquake],
               main = paste("Histogram of", input$Yquake),
               xlab = input$Yquake)
        }
      })
    output$quakeBar =
      renderPlot({
        if(input$loghist1 == TRUE)
        {
          hist(log(quakedata[,input$Xquake]),
               main = paste("Histogram of Log:", input$Xquake, sep = ''),
               xlab = paste("Log:", input$Xquake))
        }
        else{
          hist(quakedata[,input$Xquake],
               main = paste("Histogram of", input$Xquake),
               xlab = input$Xquake)
        }
      })
    
    output$fileBars2 =
      renderPlot({
        inFile = input$file
        req(inFile)
        f <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
        if(input$loghist2 == TRUE)
        {
          hist(log(f[,input$columns2]),
               main = paste("Histogram of Log:", input$columns2, sep = ''),
               xlab = paste("Log:", input$columns2))
        }
        else{
          hist(f[,input$columns2],
               main = paste("Histogram of", input$columns2),
               xlab = input$columns2)
        }
      })
    output$fileBars =
      renderPlot({
        inFile = input$file
        req(inFile)
        f <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
        if(input$loghist1 == TRUE)
        {
          hist(log(f[,input$columns]),
               main = paste("Histogram of Log:", input$columns, sep = ''),
               xlab = paste("Log:", input$columns))
        }
        else{
          hist(f[,input$columns],
               main = paste("Histogram of", input$columns),
               xlab = input$columns)
        }
      })
  })
  
  output$fileinput = renderTable({
    inFile = input$file
    req(inFile)
    f <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars = names(f)
    updateSelectInput(session, "columns", "Select Your X-Axis", choices = vars)
    updateSelectInput(session, "columns2", "Select Your Y-Axis", choices = vars)
    head(f, 5)
  })
  
  output$dataAnimal = renderText({
    "Data Description"
  })
  
  output$dataQuake = renderText({
    "Data Description"
  })
  
  output$dataWorld = renderText({
    "Data Description"
  })
  
output$animalQ = renderText({
  if(input$Xanimal == "body" && input$Yanimal == "brain" && length(input$transforms) == 2){
    "Correct"
  }
})

output$quakeQ = renderText({
  if(input$Xquake == "dist" && input$Yquake == "accel" && length(input$transforms) == 2){
    "Correct"
  }
})

output$worldQ = renderText({
  if(input$Xworld == "gdp" && input$Yworld == "military" && length(input$transforms) == 2){
    "Correct"
  }
})
  
values = reactiveValues(
  count = 0
)
  
    output$q1 = renderText({
      "Submit when finished"
      
      input$q1A
      
      isolate(if(input$Xanimal == 'body' && 
         input$Yanimal == 'brain' && 
         length(input$transforms) == 2)
      {
        values$count = values$count + 5
        
        text = "Correct"
        updateButton(session, "q1A", disabled = TRUE)
        # disableActionButton("q1A", session)
        

      }
      else if(input$Xanimal == "body" && input$Yanimal == "body"){
        text = "Submit when finished!"
      }
      
      else{
        values$count = values$count - 2
        text = "Wrong!"
        
      })
      text
      # observe({
      #   if(output$q1 == "Correct"){
      #     disableActionButton("q1A", session)
      #   }
      # })
    })

  
  output$q2 = renderText({
    "Submit when finished!"
    
    input$q2A
    
    isolate(
    if(input$Xquake == 'dist' &&
       input$Yquake == 'accel'&&
       length(input$transforms) == 2){
      values$count = values$count + 5
      text = "Correct!"
      updateButton(session, "q2A", disabled = TRUE)
      
    }
    else if(input$Xquake == "mag" && input$Yquake == "mag"){
      text = "Submit when finished!"
    }
    
    else{
      values$count = values$count - 2
      text = "Wrong!"
      
    })
    text
    # if(text == "Correct!"){
    #   disableActionButton("q2A", session)
    # }
  })

output$q3 = renderText({
  "Submit when finished!"
  
  input$q3A
  
  isolate(
    if(input$Xworld == 'gdp' &&
       input$Yworld == 'military'&&
       length(input$transforms) == 2){
      values$count = values$count + 5
      "Correct!"
      updateButton(session, "q3A", disabled = TRUE)
      
    }
    else if(input$Xworld == "gdp" && input$Yworld == "gdp"){
      "Submit when finished!"
    }
    
    else{
      values$count = values$count - 2
      "Wrong!"
    })

})
  
output$Total = renderText({
  
  h3(paste("Your Total Score is:", values$count))
  
  
  # else{
  #   values$count = 0
  #   "It's okay, we won't let you go below 0"
  # }
})

###########################################################################
#FOR MATCHING GAME PURPOSES#



time<-reactiveValues(inc=0, timer=reactiveTimer(1000), started=FALSE)

observeEvent(input$go, {time$started<-TRUE})
observeEvent(input$submitA, {time$started <- FALSE})
observeEvent(input$goo, {time$started <- TRUE})
observeEvent(input$submitB, {time$started <- FALSE})
observeEvent(input$gooo, {time$started <- TRUE})
observeEvent(input$submitC, {time$started <- FALSE})

observe({
  time$timer()
  if(isolate(time$started))
    time$inc<-isolate(time$inc)+1
})

output$timer1 <- renderPrint({
  cat("You have used:", time$inc, "secs")})
output$timer2 <- renderPrint({
  cat("You have used:", time$inc, "secs")})
output$timer3 <- renderPrint({
  cat("You have used:", time$inc, "secs")})

output$answer1 <- renderUI({
  if (!is.null(input$drp1)){
    if ((input$drp1 == "body" || input$drp1 == "brain") && input$Log1x == TRUE){
      img(src = "check.png",width = 30)
      #values$count = values$count + 5
    }
    # else if(input$drp1 == "body" && input$Log1x == FALSE){
    #   img(src = "check.png", width = 30)
    #   img(src = "cross.png", width = 30)
    # }
    else{
      img(src = "cross.png",width = 30)
      #values$count = values$count - 2
    }
  }
})

output$answer2 <- renderUI({
  if (!is.null(input$drp2)){
    if ((input$drp2 == "body" || input$drp2 == "brain") && input$Log1y == TRUE){
      img(src = "check.png",width = 30)
      #values$count = values$count + 5
    }else{
      img(src = "cross.png",width = 30)
      #values$count = values$count - 2
    }
  }
})

# output$answer3 <- renderUI({
#   if (!is.null(input$drp3)){
#     if (input$drp3 == "Log Transform"){
#       img(src = "check.png",width = 30)
#       #values$count = values$count + 5
#     }else{
#       img(src = "cross.png",width = 30)
#       #values$count = values$count - 2
#     }
#   }
# })

observeEvent(input$submitA,{
  if (!is.null(input$drp1)){
    if ((input$drp1 == "body" || input$drp1 == "brain") && input$Log1x == TRUE){
      values$count = values$count + 10
    }
    else{
      values$count = values$count - 4
    }
  }
  
  if (!is.null(input$drp2)){
    if ((input$drp2 == "body" || input$drp2 == "brain") && input$Log1y == TRUE){
      values$count = values$count + 10
    }
    else{
      values$count = values$count - 4
    }
  }
  if((input$drp1 == "body" || input$drp1 == "brain") && input$Log1x == TRUE && (input$drp2 == "brain" || input$drp2 == "body") && input$Log1y == TRUE){
    updateButton(session = session, "submitA", disabled = TRUE)
  }
  
  # if (!is.null(input$drp3)){
  #   if (input$drp3 == "Log Transform"){
  #     values$count = values$count + 5
  #   }
  #   else{
  #     values$count = values$count - 2
  #   }
  # }
})

output$answer4 <- renderUI({
  if (!is.null(input$drp4)){
    if (input$drp4 == "distance" && input$Log2x == TRUE){
      img(src = "check.png",width = 30)
      #values$count = values$count + 5
    }else{
      img(src = "cross.png",width = 30)
      #values$count = values$count - 2
    }
  }
})

output$answer5 <- renderUI({
  if (!is.null(input$drp5)){
    if (input$drp5 == "Peak Acceleration" && input$Log2y == TRUE){
      img(src = "check.png",width = 30)
      #values$count = values$count + 5
    }else{
      img(src = "cross.png",width = 30)
      #values$count = values$count - 2
    }
  }
})

# output$answer6 <- renderUI({
#   if (!is.null(input$drp6)){
#     if (input$drp6 == "Log Transform"){
#       img(src = "check.png",width = 30)
#       #values$count = values$count + 5
#     }else{
#       img(src = "cross.png",width = 30)
#       #values$count = values$count - 2
#     }
#   }
# })

observeEvent(input$submitB,{
  if (!is.null(input$drp4)){
    if ((input$drp4 == "distance" || input$drp4 == "Peak Acceleration")  && input$Log2x == TRUE){
      values$count = values$count + 15
    }
    else{
      values$count = values$count - 6
    }
  }
  
  if (!is.null(input$drp5)){
    if ((input$drp5 == "Peak Acceleration" || input$drp5 == "distance") && input$Log2y == TRUE){
      values$count = values$count + 15
    }
    else{
      values$count = values$count - 6
    }
  }
  
  if((input$drp4 == "distance" || input$drp4 == "Peak Acceleration")  && input$Log2x == TRUE && (input$drp5 == "Peak Acceleration" || input$drp5 == "distance") && input$Log2y == TRUE){
    updateButton(session = session, "submitB", disabled = TRUE)
  }
  
  # if (!is.null(input$drp6)){
  #   if (input$drp6 == "Log Transform"){
  #     values$count = values$count + 5
  #   }
  #   else{
  #     values$count = values$count - 2
  #   }
  # }
})

output$answer7 <- renderUI({
  if (!is.null(input$drp7)){
    if ((input$drp7 == "gdp" || input$drp7 == "military") && input$Log3x == TRUE){
      img(src = "check.png",width = 30)
      #values$count = values$count + 5
    }else{
      img(src = "cross.png",width = 30)
      #values$count = values$count - 2
    }
  }
})

output$answer8 <- renderUI({
  if (!is.null(input$drp8)){
    if ((input$drp8 == "military" || input$drp8 == "gdp") && input$Log3y == TRUE){
      img(src = "check.png",width = 30)
      #values$count = values$count + 5
    }else{
      img(src = "cross.png",width = 30)
      #values$count = values$count - 2
    }
  }
})

# output$answer9 <- renderUI({
#   if (!is.null(input$drp9)){
#     if (input$drp9 == "Log Transform"){
#       img(src = "check.png",width = 30)
#       #values$count = values$count + 5
#     }else{
#       img(src = "cross.png",width = 30)
#       #values$count = values$count - 2
#     }
#   }
# })

observeEvent(input$submitC,{
  if (!is.null(input$drp7)){
    if ((input$drp7 == "gdp" || input$drp7 == "military")&& input$Log3x == T){
      values$count = values$count + 25
    }
    else{
      values$count = values$count - 10
    }
  }
  
  if (!is.null(input$drp8)){
    if ((input$drp8 == "military" || input$drp8 == "gdp") && input$Log3y == T){
      values$count = values$count + 25
    }
    else{
      values$count = values$count - 10
    }
  }
  
  if((input$drp7 == "gdp" || input$drp7 == "military") && input$Log3x == T && (input$drp8 == "military" || input$drp8 == "gdp") && input$Log3y == T){
    updateButton(session = session, "submitC", disabled = TRUE)
  }
  
  # if (!is.null(input$drp9)){
  #   if (input$drp9 == "Log Transform"){
  #     values$count = values$count + 5
  #   }
  #   else{
  #     values$count = values$count - 2
  #   }
  # }
})

output$score = renderText({
  values$count
})

output$score1 = renderText({
  values$count
})

output$score2 = renderText({
  values$count
})

value = reactiveValues()

update = reactive({
  value = data.frame("Name" = as.character(input$initials),
                     "Score" = as.numeric(values$count),
                     "Time Taken" = as.numeric(time$inc),
                     "Date" = Sys.Date())
})
value$df = data.frame()

outputDir = "scores"
saveQuestions <- function(data) {
  # data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.Date()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}

x = as.character(as.numeric(Sys.Date()))
y = as.character(as.numeric(Sys.Date() - 7))
if(substring(x, 5, 5) >= 7){
  pattern = paste0(substring(x, 1, 4), "[", substring(y, 5, 5), "-", substring(x, 5, 5), "]", sep = '')
}
else{
  pattern = paste0(substring(y, 1, 4), "[",substring(y, 5, 5), "-9]|", substring(x,1,4), "[0-", substring(x, 5, 5), "]")
}

loadDataWeek <- function() {
  files = list.files(outputDir, pattern = pattern, full.names = TRUE)
  data = lapply(files, read.csv, stringsAsFactors = FALSE)
  data = do.call(rbind, data)
  data
}


data = reactive({
  data = loadData()
  data = data[order(-data[,"Score"], data[,"Time.Taken"]),]
})

data2 = reactive({
  data = loadDataWeek()
  data = data[order(-data[,"Score"], data[,"Time.Taken"]),]
})

observeEvent(input$finish, {
  scores = update()
  value$df = rbind(value$df, scores)
  saveQuestions(value$df)
})


observeEvent(input$showhigh, {
output$highscore = renderTable({
  if(is.null(loadData()) == TRUE){
    data.frame()
  }
  else{
    head(data(), 3)
  }
})
})

observeEvent(input$weekshowhigh, {
  output$weekhighscore = renderTable({
    if(is.null(loadDataWeek()) == TRUE){
      data.frame()
    }
    else{
      head(data2(), 3)
    }
  })
})


#Update to Next button on Instructions Page
observeEvent(input$next1, {
  updateTabItems(session = session, "main", selected = "1level")
})

observeEvent(input$next2, {
  updateTabItems(session = session, "main", selected = "2level")
})

observeEvent(input$next3, {
  updateTabItems(session = session, "main", selected = "3level")
})

# Adding variable descriptions
output$Animal1 = renderText({
  "body"
})

output$Animal2 = renderText({
  "brain"
})

output$Quake1 = renderText({
  "magnitude"
})

output$Quake2 = renderText({
  "distance"
})

output$Quake3 = renderText({
  "peak acceleration"
})

output$World1 = renderText({
  "gdp"
})

output$World2 = renderText({
  "income"
})

output$World3 = renderText({
  "literacy"
})

output$World4 = renderText({
  "military"
})

#Make Next Button undisabled when full score
observeEvent(input$submitA, {
  # if(input$drp1 == "body" && input$Log1x == TRUE && input$drp2 == "brain" && input$Log1y == TRUE)
  # {
    updateButton(session, "next2", disabled = F)
    updateButton(session, "submitA", disabled = T)
  #}
})

observeEvent(input$submitB, {
  #if(input$drp4 == "distance"  && input$Log2x == TRUE && input$drp5 == "Peak Acceleration" && input$Log2y == TRUE){
    updateButton(session, "next3", disabled = F)
    updateButton(session, "submitB", disabled = T)
  #}
})

#Top 3 finish give button to submit score
observeEvent(input$submitC,{
  #if(input$drp7 == "gdp" && input$Log3x == T && input$drp8 == "military" && input$Log3y == T){
    updateButton(session, "next4", disabled = F)
    updateButton(session, "submitC", disabled = T)
  # }
  output$finish = renderUI({
  # if(nrow(data2()) >= 3)
  # {
  # if(values$count > data2()[3, 2])
  # {
  #   textInput("initials", "Input your name")
  #   bsButton("finish", "Finished!", style = "primary")
  # }
  # else if(values$count == data2()[3,2]){
  #   if(time$inc < data2()[3,3]){
  #     textInput("initials", "Input your name")
  #     bsButton("finish", "Finished!", style = "primary")
  #   }
  # }
  # }
  # else{
      textInput("initials", "Input your name")
      bsButton("finish", "Finished!", style = "primary")
    # }
})})

observeEvent(input$submitC,{
  output$initials = renderUI({
    # if(nrow(data2()) == 0){
    #   textInput("initials", "Input your nickname")
    # }
    # if(nrow(data2()) >= 3)
    # {
    #   if(values$count > data2()[3, 2])
    #   {
    #     textInput("initials", "Input your name", value = "")
    #     
    #   }
    #   else if(values$count == data2()[3,2]){
    #     if(time$inc < data2()[3,3]){
    #       textInput("initials", "Input your name", value = "")
    #       
    #     }
    #   }
    # }
    # else{
      textInput("initials", "Input your name", value = "")
     
    #}
  })})
#observeEvent(input$finish, {
output$names = renderText({
  # for(i in 1:length(data[,1]))
  df = data.frame(data())
  df2 = df[,1]
  # for(i in 1:length(df2)){
  if(input$initials %in% df2){
    updateButton(session, "finish", disabled = T)
    "Nickname unusable"
  }
  else{
    updateButton(session, "finish", disabled = F)
    "Nickname usable"
  }
})
#})

observeEvent(input$finish, {
output$badge = renderUI({
  if(is.null(input$initials) == T){
    place = 0
  }
  else{
  # for(i in 1:3)
  #   {
  #   if(data2()[i, 1] == input$initials){
  #     place = i
  #   }
  # }
   if(values$count > data2()[1,2]){
     place = 1
   }
    
    else if(values$count == data2()[1,2]){
      if(time$inc <= data2()[1,3]){
        place = 1
      }
      else{
        if(time$inc <= data2()[2,3])
        {
        place = 2
        }
        else{
          place = 3
        }
      }
    }
    else{
      if(values$count > data2()[2,2]){
        place = 2
      }
      else if(values$count == data2()[2,2])
        {
        if(time$inc <= data2()[2,3]){
          place = 2
        }
        else{
          if(time$inc <= data2()[3,3])
          {
            place = 3
          }
        }
      }
      else{
        place = 3
      }
    }
  }
  if(place == 1){
    # 1st place image
    img(src = "1stplace.png")
  }

  else if(place == 2){
    # 2nd place
    img(src = "2ndplace.png")
  }

  else if(place == 3){
    # 3rd place
    img(src = "3rdplace.png")
  }
  else{
    ""
  }
  #else if(values$count == data2()[1,2] && time$inc <= )
})
})



# output$summary = renderPrint({
  #   input$transforms
  # })
  # 
  # FOR IMPORT YOUR OWN DATA ##################################################################################################
  # info <- eventReactive(input$choice, {
  #   inFile <- input$file
  #   # Instead # if (is.null(inFile)) ... use "req"
  #   req(inFile)
  #   
  #   # Changes in read.table 
  #   f <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
  #   vars <- names(f)
  #   # Update select input immediately after clicking on the action button. 
  #   updateSelectInput(session, "columns","Select Columns", choices = vars)
  #   
  #   f
  # })
  # 
  # output$table_display <- renderTable({
  #   f <- info()
  #   f <- subset(f, select = input$columns) #subsetting takes place here
  #   head(f)
  # })
  
  
})
