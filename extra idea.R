# Extra Idea for the App ----

#ui ----
# input your own file ----
conditionalPanel(
  condition = "input.inputs == 'Input your own'",
  "Be careful when entering a dataset with non-positive values",
  fileInput('file', 'Choose info-file to upload',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              'text/tab-separated-values',
              'text/plain',
              '.csv',
              '.tsv'
            )
  ),
  bsPopover("file", "File Upload", "If you have your own dataset you would like to enter, upload it through this"),
  checkboxInput('header', 'Header', TRUE),
  bsPopover("header", "Header", "Please check whether there is a header for your data or not"),
  radioButtons('sep', 'Separator',
               c(Comma = ',',
                 Semicolon = ';',
                 tab = '\t'),
               ','),
  bsPopover("sep", "Separator", "Choose which separation your file is using(If you do not know, it is most likely separating with commas"),
  radioButtons('quote', 'Quote',
               c(None = '',
                 'Double Quote' = '"',
               'Single Quote' = "'"),
              '"'),
  bsPopover("quote", "Quotes", "Check what kind of data you have and whether you need to use quotes or not"),
  selectInput("columns", "Select Your X-Axis", choices = NULL),
  #bsPopover("columns", "Choose your X-Variable", "Choose which variable you want on the X Axis in the Dataset you inputted"),
  selectInput("columns2", "Select Your Y-Axis", choices = NULL)
  #bsPopover("columns", "Choose your Y-Variable", "Choose which variable you want on the Y Axis in the Dataset you inputted")
  #tableOutput("fileinput")
),

conditionalPanel(
  condition = "input.inputs == 'Input your own'",
  tableOutput("fileinput"),
  bsPopover("fileinput", "Inputted Dataset Sample Table", "Sample table for the Inputted Dataset"),
  plotOutput("filePlot"),
  bsPopover("filePlot", "Inputted Dataset Plot", "Plot for the Inputted Dataset given X and Y variables chosen by the user"),
  plotOutput("fileBars"),
  bsPopover("fileBars", "Inputted X-Variable Histogram", "Histogram of the seleted X-Variable"),
  plotOutput("fileBars2"),
  bsPopover("fileBars2", "Inputted Y-Variable Histogram", "Histogram of the selected Y-Variable")
)
# conditionalPanel(
  condition = "input.Xanimal == 'none'",
  plotOutput("EmptyPlot")
),
conditionalPanel(
  condition = "input.Yanimal == 'none'",
  plotOutput("EmptyPlot")
)
verbatimTextOutput("summary")
# Add in option to make it a histogram


# task & game ----
tabItem(tabName = "task",
        h3("Tasks range from Easy to Hard."),
          h3("One Question from each data set"),
        h3("After reading each question, go back to the App
           screen and attempt to make the transformation and
           check back at this screen to see if you're correct"),
        sidebarLayout(
          sidebarPanel(
            "Use Log Transformations to make the data linear in
             the Animal Data Set",
            br(),
            br(),
            textOutput("q1"),

            shinyBS::bsButton("q1A", "Submit"),
            br(),
            br(),
            br(),
            "Use Log Transformations to find the variables
            needed to make the data linear in the earthquake data
            World Dataset",
            br(),
            br(),
            textOutput("q2"),

            shinyBS::bsButton("q2A", "Submit"),
            br(),
            br(),
            br(),
            "Use Log Transformations to find the variables needed to make the data
            linear in the world data",
            br(),
            br(),
            textOutput("q3"),
            shinyBS::bsButton("q3A", "Submit")
            ),
          mainPanel(
            h3(textOutput("Total"))

          )
          )

),

# Matching Game
tabItem(tabName = "1level",
        fluidPage(

          theme = "styles.css",
          titlePanel("Level 1"),
          h3("Total Score"),
          h3(textOutput("score")),
          bsPopover("score", "Current Score", "The score you have obtained throughout the game"),
          fluidRow(column(3, offset = 9, h3(textOutput("timer1")))),
          br(),
          br(),
          fluidRow(column(1, offset = 5,
                        bsButton("go", "G O !", style = "warning", size = "large"))),
          br(),
          br(),
          br(),
          conditionalPanel("input.go != 0",
                           fluidRow(
                             column(6, dragUI("var1","brain", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
                             #bsPopover("var1", "Matching Game", "Place a tile here"),
                             column(6, dragUI("var2","body", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))

                             #column(3, dragUI("var3","Log Transform", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
                             #column(3, dragUI("var4","No Log Transform", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))
                           )
                           ),

        br(),
        br(),
        br(),
        # fluidRow(
        #   column(6, h3("X-Axis")),
        #   column(6, h3("Y-Axis"))
        #   #column(4, h3("Log Transform"))
        # ),
        fluidRow(

          wellPanel(
            fluidRow(
              column(6, h3("X-Axis"))
            ),
            fluidRow(
              column(2, dropUI("drp1", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
              bsPopover("drp1", "Matching Game", "Place a tile here"),
              column(1, conditionalPanel("input.submitA != 0", htmlOutput("answer1")))
            ),
            fluidRow(
              column(6, checkboxInput("Log1x", h4("Log Transform X"))),
              bsPopover("Log1x", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
            )
          ,class = "col-lg-6 col-md-6 col-sm-12 wellPanel1"),

          wellPanel(
            fluidRow(
              column(6, h3("Y-Axis"))
            ),
            fluidRow(
              column(2, dropUI("drp2", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
              bsPopover("drp2", "Matching Game", "Place a tile here"),
              column(1,conditionalPanel("input.submitA != 0",htmlOutput("answer2")))
            ),
            fluidRow(
              column(6, checkboxInput("Log1y", h4("Log Transform Y"))),
              bsPopover("Log1y", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
            )
          ,class = "col-lg-6 col-md-6 col-sm-12 wellPanel1")
        ),



        # fluidRow(
        #
        #
        #
        #
        #
        #
        #   column(2, offset = 3, dropUI("drp2", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
        #   bsPopover("drp2", "Matching Game", "Place a tile here"),
        #   column(1,conditionalPanel("input.submitA != 0",htmlOutput("answer2")))
        #   #column(3,dropUI("drp3", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
        #   #column(1,conditionalPanel("input.submitA != 0",htmlOutput("answer3")))
        # ),
        # fluidRow(
        #   column(6, checkboxInput("Log1x", h4("Log Transform X"))),
        #   bsPopover("Log1x", "Log Transformation", "Check this box if you believe the variable needs Log Transformed"),
        #   column(6, checkboxInput("Log1y", h4("Log Transform Y"))),
        #   bsPopover("Log1y", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
        # ),
        br(),
        br(),
        br(),
        fluidRow(
          column(2, offset = 8, bsButton("next2", "Next>>", style = "primary", disabled = T)),
          column(2, bsButton("submitA", "Submit Answer", style = "primary")),
          bsPopover("submitA", "Submission", "Click this button once you are done filling in each tile to see the correct answer")
        )


        )),
tabItem(tabName = "2level",
        fluidPage(
          theme = "bootstrap.css",
          titlePanel("Level 2"),
          h3("Total Score"),
          h3(textOutput("score1")),
          bsPopover("score1", "Current Score", "The score you have obtained throughout the game"),
          fluidRow(column(3, offset = 9, h3(textOutput("timer2")))),
          br(),
          br(),
          br(),
          conditionalPanel("input.next2 != 0",
          fluidRow(column(1, offset = 5,
                  # conditionalPanel("input.go != 0",
                           bsButton("goo", "G O !", style = "warning", size = "large")
                           ),
                           bsPopover("goo", "Start Button", "Press this button in order to start the game and the timer"),
                           br(),
                           br(),
                           br(),
          conditionalPanel("input.goo != 0",
                           fluidRow(
                             column(4,dragUI("var5","magnitude", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
                             column(4,dragUI("var6","distance", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
                             column(4,dragUI("var7","Peak Acceleration", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))
                             #column(3,dragUI("var8","Log Transform", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))
                           )
                           # br(),
                           # br(),,
                           # fluidRow(
                           #   column(3, dragUI("var9","No Log Transform", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))
                           # )

          ))),

          br(),
          br(),
          br(),
          fluidRow(

            wellPanel(
              fluidRow(
                column(6, h3("X-Axis"))
              ),
              fluidRow(
                column(2, dropUI("drp4", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
                bsPopover("drp4", "Matching Game", "Place a tile here"),
                column(1, conditionalPanel("input.submitB != 0", htmlOutput("answer4")))
              ),
              fluidRow(
                column(6, checkboxInput("Log2x", h4("Log Transform X"))),
                bsPopover("Log2x", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
              )
              ,class = "col-lg-6 col-md-6 col-sm-12 wellPanel1"),

            wellPanel(
              fluidRow(
                column(6, h3("Y-Axis"))
              ),
              fluidRow(
                column(2, dropUI("drp5", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
                bsPopover("drp5", "Matching Game", "Place a tile here"),
                column(1,conditionalPanel("input.submitB != 0",htmlOutput("answer5")))
              ),
              fluidRow(
                column(6, checkboxInput("Log2y", h4("Log Transform Y"))),
                bsPopover("Log2y", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
              )
              ,class = "col-lg-6 col-md-6 col-sm-12 wellPanel1")
          ),

          br(),
          br(),
          br(),
          fluidRow(
            column(2, offset = 8, bsButton("next3", "Next>>", style = "primary", disabled = T)),
            column(1, bsButton("submitB", "Submit Answer", style = "primary")),
            bsPopover("submitB", "Submission", "Click this button once you are done filling in each tile to see the correct answer")
          )

        )
        ),
tabItem(tabName = "3level",
        fluidPage(
          theme = "bootstrap.css",
          titlePanel("Level 3"),
          h3("Total Score"),
          h3(textOutput("score2")),
          bsPopover("score2", "Current Score", "The score you have obtained throughout the game"),
          fluidRow(column(3, offset = 9, h3(textOutput("timer3")))),
          br(),
          br(),
          br(),
          conditionalPanel("input.next3 != 0",
          fluidRow(column(1, offset = 5,
                          conditionalPanel("input.go != 0",
                                           bsButton("gooo", "G O !", style = "warning", size = "large")))),
          bsPopover("gooo", "Start Button", "Press this button in order to start the game and the timer")),
          br(),
          br(),
          br(),
          conditionalPanel("input.gooo != 0",
                           fluidRow(
                             column(3,dragUI("var10","gdp", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
                             column(3,dragUI("var11","income", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
                             column(3,dragUI("var12","literacy", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
                             column(3, dragUI("var13","military", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))
                           )
                           # br(),
                           # br(),
                           # fluidRow(
                           # column(3, dragUI("var14","Log Transform", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
                           # column(3, dragUI("var15","No Log Transform", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))
                           # )

          ),

          br(),
          br(),
          br(),
          fluidRow(

            wellPanel(
              fluidRow(
                column(6, h3("X-Axis"))
              ),
              fluidRow(
                column(2, dropUI("drp7", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
                bsPopover("drp7", "Matching Game", "Place a tile here"),
                column(1, conditionalPanel("input.submitC != 0", htmlOutput("answer7")))
              ),
              fluidRow(
                column(6, checkboxInput("Log3x", h4("Log Transform X"))),
                bsPopover("Log3x", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
              )
              ,class = "col-lg-6 col-md-6 col-sm-12 wellPanel1"),

            wellPanel(
              fluidRow(
                column(6, h3("Y-Axis"))
              ),
              fluidRow(
                column(2, dropUI("drp8", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
                bsPopover("drp8", "Matching Game", "Place a tile here"),
                column(1,conditionalPanel("input.submitC != 0",htmlOutput("answer8")))
              ),
              fluidRow(
                column(6, checkboxInput("Log3y", h4("Log Transform Y"))),
                bsPopover("Log3y", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
              )
              ,class = "col-lg-6 col-md-6 col-sm-12 wellPanel1")
          ),
          br(),
          br(),
          fluidRow(
            htmlOutput("badge")
          ),
          br(),
          fluidRow(
            #column(2, offset = 8, bsButton("next3", "Next>>", style = "primary", disabled = T)),
            column(1, offset = 10, bsButton("submitC", "Submit Answer", style = "primary")),
            bsPopover("submitC", "Submission", "Click this button once you are done filling in each tile to see the correct answer"),
            br(),
            br(),
          fluidRow(
            column(4, uiOutput("initials"))),
          fluidRow(
            column(2, uiOutput("finish")),
            bsPopover("finish", "Insert your highscore", "")),
          fluidRow(
            column(6, bsButton("showhigh", "Show Highscores", style = "primary")),
            column(6, offset = 0, bsButton("weekshowhigh", "Show Weekly Highscores", style = "primary"))
          ),
          fluidRow(
            column(6, tableOutput("highscore")),
            column(6, tableOutput("weekhighscore")),
            bsPopover("showhigh", "Scoreboard", "All Time Highscores"),
            bsPopover("weekshowhigh", "Weekly Scoreboard", "Highscores for the week")
          ),
          fluidRow(

            textOutput("names"))
          )

        )
        ),
tabItem(tabName = "instr",
        titlePanel("Instructions"),
        tags$ul(
        tags$li(h5("There are three levels, each level gets consecutively harder.")),
        br(),
        tags$li(h5("Each level is based on the data sets you interacted with in the previous task.")),
        br(),
        tags$li(h5("In each level, after you hit the GO! button, a number of boxes will
           show up on your screen.")),
        br(),
        tags$li(h5("Drag each variable to the designated boxes and check the box for Log Transformation(s) if needed to linearize the data")),
        br(),
        tags$li(h5("After you have filled both areas with boxes, hit the submit button to check if you are correct.")),
        br(),
        tags$li(h5("Clicking Next will bring you to the first level.")),
        br(),
        tags$li(h5("Hit Go! to start each level!"))),
        br(),
        br(),
        fluidRow(column(1, offset = 11, bsButton("next1", "Next>>", style = "primary")))
)

#server ----
output$filePlot = 
  renderPlot({
    inFile = input$file
    req(inFile)
    f <- read.table(inFile$datapath, header = input$header, 
                    sep = input$sep, quote = input$quote)
    #If they don't checkbox anything
    if(length(input$transforms) == 0)
    {
      plot(f[,input$columns], f[,input$columns2],
           xlab = input$columns, ylab = input$columns2,
           main = paste(input$columns, "vs", input$columns2))
      # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]),
      # col = 'red')
      # legend("topright", bty = "n", legend = paste("R2 is", 
      #  format(summary(lm(animaldata[,input$Xanimal]~
      # animaldata[,input$Yanimal]))$adj.r.squared, digits = 4)))
    }
    # If they checkbox one of them
    else if(length(input$transforms) == 1)
    {
      # If they only checkbox the Transform Y option
      if(input$transforms == 'Transform Y')
      {
        plot(f[,input$columns], log(f[,input$columns2]),
             xlab = input$columns, ylab = paste("Log:", input$columns2),
             main = paste(input$columns, "vs Log:", input$columns2))
        # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]),
        # col = 'red')
        # legend("topright", bty = "n", legend = paste("R2 is", 
        #  format(summary(lm(animaldata[,input$Xanimal]~
        # log(animaldata[,input$Yanimal])))$adj.r.squared, digits = 4)))
      }
      # If they only checkbox the Transform X option
      else if(input$transforms == 'Transform X')
      {
        plot(log(f[,input$columns]), f[,input$columns2],
             xlab = paste("Log:", input$columns), ylab = input$columns2,
             main = paste("Log:", input$columns, "vs", input$columns2))
        # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]),
        # col = 'red')
        # legend("topright", bty = "n", legend = paste("R2 is", 
        #  format(summary(lm(log(animaldata[,input$Xanimal])~
        # animaldata[,input$Yanimal]))$adj.r.squared, digits = 4)))
      }
    }
    #If they check both boxes
    else #Doesn't plot line, but plots R-squared value
    {
      plot(log(f[,input$columns]), log(f[,input$columns2]),
           xlab = paste("Log:", input$columns),
           ylab = paste("Log:", input$columns2),
           main = paste("Log:", input$columns, "vs Log:", input$columns2))
      # abline(lm(animaldata[,input$Xanimal]~animaldata[,input$Yanimal]),
      # col = 'red')
      # legend("topright", bty = "n", legend = paste("R2 is", 
      #  format(summary(lm(log(animaldata[,input$Xanimal])~
      # log(animaldata[,input$Yanimal])))$adj.r.squared, digits = 4)))
    }
  })

output$summary = renderPrint({
  input$transforms
})

FOR IMPORT YOUR OWN DATA ##################################################################################################
info <- eventReactive(input$choice, {
  inFile <- input$file
  # Instead # if (is.null(inFile)) ... use "req"
  req(inFile)

  # Changes in read.table
  f <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
  vars <- names(f)
  # Update select input immediately after clicking on the action button.
  updateSelectInput(session, "columns","Select Columns", choices = vars)

  f
})

output$table_display <- renderTable({
  f <- info()
  f <- subset(f, select = input$columns) #subsetting takes place here
  head(f)
})




