#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

# Stuff to do for lata
# Put everything into dashboard form
# Think more about the True false problem.  Kinda working now

library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyDND)
library(shinyjs)
library(shinyWidgets)

#Drag n Drop with correct variables and each data set
# Put things on PSU shiny server


# Define UI for application that draws a histogram
header = dashboardHeader(title = "LogTransformations"
                         )

sidebar = dashboardSidebar(
  
  sidebarMenu(id = "tabs",
              menuItem("Prerequisites",tabName = "pre",icon = icon("book")),
              menuItem("Overview",tabName = "overview",icon = icon("dashboard")),
              menuItem("Transform",icon = icon('wpexplorer'),tabName = "transformations")
  )
)

body = dashboardBody(
  tags$head( 
    tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
 ),
  # tags$head( 
  #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  # ),
  tabItems(
    tabItem(tabName = "pre",
            
            withMathJax(),
              h3(strong("Background: Some reasons for using a Log Transformation")),br(),
            h4(tags$li("Make a distribution more symmetric.")),
            h4(tags$li("Make the relationship between distributions more linear.")),
            h4(tags$li("Make a relationship more interpretable.")),
            
            br(),
            div(style = "text-align: center",bsButton("goover", "Go to the overview", icon("bolt"), size = "median"))
    ),
    tabItem(tabName = "overview",
            tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
            br(),br(),br(),
            h3(tags$b("About:")),
            h4(p("The goals of this app are to know when to use log transformations to linearize data, 
                 how to analyze scenarios where a Log Transform is needed and when it is not.")),
            h4(p("There are three different datasets, which are about Animals, Earthquakes, and Countries.")),    br(),
            h3(tags$b("Instructions:")),
            h4(tags$li("In the application, you will see a number of different data sets you can go through.")),
            h4(tags$li("Go through each data set and look through the different variables to check whether a log transformation would help.")),
            h4(tags$li("Use the 'Show Histogram' button to see each individual variable more clearly.")),
            h4(tags$li("You can also examine log transforms on variables in your own data set!")),
            
            div(style = "text-align: center",bsButton("explore", "Explore", icon("bolt"), size = "large")),
            br(),
            h3(tags$b("Acknowledgements:")),
          h4(p("This app was coded and developed by Alex Chen. The 'Countries' data set was extracted from https://www.stat.berkeley.edu/~s133/resources.html on June 15, 2017."))
  ),
    tabItem(tabName = "transformations",
            div(style="display: inline-block;vertical-align:top;",
                tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
            ),
            div(style="display: inline-block;vertical-align:top;",
                circleButton("info",icon = icon("info"), status = "myClass",size = "xs")
            ),
            h2("Log Transformation Task"),
            sidebarLayout(
              sidebarPanel(
                selectInput("inputs", "Select Data Set", choices = c('None', 'Animals', 'Earthquakes', "Countries", 'Input your own')),
                #bsPopover("inputs", "Dataset Selection", "Choose which data set you would like to observe"),
                conditionalPanel(
                  condition = "input.inputs == 'Countries'",
                  selectInput("Xworld", 
                              "Select Your X-Axis", 
                              c('gdp', 'income', 'literacy', 'military')),
                  #bsPopover("Xworld", "Choose your X-Variable", "Choose which variable you want on the X Axis in the World Dataset", placement = "top"),
                  selectInput("Yworld",
                              "Select Your Y-Axis",
                              c('gdp', 'income', 'literacy', 'military'))
                  #bsPopover("Yworld", "Choose your Y-Variable", "Choose which variable you want on the Y Axis in the World Dataset", placement = "top")
                ),
                conditionalPanel(
                  condition = "input.inputs == 'Earthquakes'",
                  selectInput("Xquake",
                              "Select Your X-Axis",
                              c("magnitude"="mag", "distance" = "dist", "Peak Acceleration" = "accel")
                  ),
                  #bsPopover("Xquake", "Choose your X-Variable", "Choose which variable you want on the X Axis in the Earthquake Dataset", placement = "top"),
                  selectInput("Yquake",
                              "Select Your Y-Axis",
                             c("magnitude" = "mag", "distance" = "dist", "Peak Acceleration" = "accel"))
                  #bsPopover("Yquake", "Choose your Y-Variable", "Choose which variable you want on the Y Axis in the Earthquake Dataset", placement = "top")
                ),
                conditionalPanel(
                  condition = "input.inputs == 'Animals'",
                  selectInput("Xanimal",
                              "Select Your X-Axis",
                              c('body','brain')),
                  #bsPopover("Xanimal", "Choose your X-Variable", "Choose which variable you want on the X Axis in the Animal Dataset", placement = "top"),
                  selectInput("Yanimal",
                              "Select Your Y-Axis",
                              c('body', 'brain')
                  )
                  #bsPopover("Yanimal", "Choose your Y-variable", "Choose which variable you want on the Y Axis in the Animal Dataset", placement = "top")
                ),
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
                checkboxGroupInput("transforms", "Transform X or Y", c("Transform X", "Transform Y"), selected = NULL),
                bsPopover("transforms", "Transformation", "Decide whether you want to log transform the X, Y, or both axes"),
                
                actionButton('hist', "Show histograms"),
                bsPopover("hist", "Histogram", "Click me if you want to see the histograms of both the X and Y axes you have selected"),
                checkboxInput('loghist1', 'Show Log: XValue Hist'),
                bsPopover("loghist1", "Log Transform of Histogram(X-Variable)", "Check this box if you want to see the log transformation of the X-axis in the Histogram"),
                checkboxInput('loghist2', 'show Log: YValue Hist'),
                bsPopover("loghist2", "Log Transform of Histogram(Y-Variable)", "Check this box if you want to see the log transformation of the Y-axis in the Histogram"),
                conditionalPanel(
                  "input.inputs == 'Animals'",
                  h4(textOutput("dataAnimal")),
                  bsPopover("dataAnimal", "Animals Data Set", "This data set describes the correlation between the brain weight(g) and body weight(kg) of different animals"),
                  downloadButton("animalDownload", "Download"),
                  bsPopover("animalDownload", "", "Download this data set!")
                ),
                conditionalPanel(
                  "input.inputs == 'Earthquakes'",
                  h4(textOutput("dataQuake")),
                  bsPopover("dataQuake", "Earthquake Data Set", "This data set describes different earthquakes that occurred in the US using the magnitude, distance from where it was recorded and the ground acceleration of the earthquake"),
                  downloadButton("quakeDownload", "Download"),
                  bsPopover("quakeDownload", "", "Download this data set!")
                ),
                conditionalPanel(
                  "input.inputs == 'Countries'",
                  h4(textOutput("dataWorld")),
                  bsPopover("dataWorld", "Countries Data Set", "This data set contains 154 countries and data about them such as GDP, income per capita, literacy rate, and money spent on military"),
                  downloadButton("worldDownload", "Download"),
                  bsPopover("worldDownload", "", "Download this data set!")
                  
                ),
                h4("Variable Descriptions"),
                conditionalPanel(
                  condition = "input.inputs == 'Animals'",
                  textOutput("Animal1"),
                  textOutput("Animal2"),
                  bsPopover("Animal1", "Body", "Body Weight in kg"),
                  bsPopover("Animal2", "Brain", "Brain weight in g"),
                  h3("Challenge Question"),
                  h3("Does it help to make a log transform on both variables?"),
                  textOutput("animalQ")
                ),
                
                conditionalPanel(
                  condition = "input.inputs == 'Earthquakes'",
                  textOutput("Quake1"),
                  textOutput("Quake2"),
                  textOutput("Quake3"),
                  bsPopover("Quake1", "Magnitude", "Magnitude of earthquake"),
                  bsPopover("Quake2", "Distance", "Distance in km from station recorded"),
                  bsPopover("Quake3", "Peak Acceleration", "Ground Acceleration in units of gravity"),
                  h3("Challenge Question"),
                  h3("Which two variables would benefit most from a log transformation?  Why?"),
                  textOutput("quakeQ")
                ),
                
                conditionalPanel(
                  condition = "input.inputs == 'Countries'",
                  textOutput("World1"),
                  textOutput("World2"),
                  textOutput("World3"),
                  textOutput("World4"),
                  bsPopover("World1", "GDP", "Gross Domestic Product per capita"),
                  bsPopover("World2", "Income", "Income per capita"),
                  bsPopover("World3", "Literacy", "% of population literate"),
                  bsPopover("World4", "Military", "Dollars(USD) spent on military"),
                  h3("Challenge Question"),
                  h3("Which two variables would benefit most from a log transformation?  Why?"),
                  textOutput("worldQ")
                )
                #selectInput("plottype", "Plot Type", choices = c("Dot Plot", "Histogram"))
              ),
                
              
              # Show a plot of the generated distribution
              mainPanel(
                conditionalPanel(
                  condition = "input.inputs == 'Countries'",
                  plotOutput("worldPlot"),
                  bsPopover("worldPlot", "World Dataset Plot", "Plot for the World Dataset given X and Y variables chosen by the user"),
                  plotOutput("worldBars"),
                  bsPopover("worldBars", "World X-Variable Histogram", "Histogram of the selected X-Variable"),
                  plotOutput("worldBars2"),
                  bsPopover("WorldBars2", "World Y-Variable Histogram", "Histogram of the selected Y-Variable")
                  
                  # plotOutput("worldBars3"),
                  # plotOutput("worldBars4")
                ),
                conditionalPanel(
                  condition = "input.inputs == 'Animals'",
                  plotOutput("animalPlot"),
                  bsPopover("animalPlot", "Animal Dataset Plot", "Plot for the Animal Dataset given X and Y variables chosen by the user"),
                  plotOutput("animalBars"),
                  bsPopover("animalBars", "Animal X-Variable Histogram", "Histogram of the selected X-Variable"),
                  plotOutput("animalBars2"),
                  bsPopover("animalBars2", "Animal Y-Variable Histogram", "Histogram of the selected Y-Variable")
                ),
                conditionalPanel(
                  condition = "input.inputs == 'Earthquakes'",
                  plotOutput("quakePlot"),
                  bsPopover("quakePlot", "Earthquake Dataset Plot", "Plot for the Earthquake Dataset given X and Y variables chosen by the user"),
                  plotOutput("quakeBar"),
                  bsPopover("quakeBar", "Earthquake X-Variable Histogram", "Histogram of the selected X-Variable"),
                  plotOutput("quakeBar2"),
                  bsPopover("quakeBar2", "EarthQuake Y-Variable Histogram", "Histogram of the selected Y-Variable")
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
                #   condition = "input.Xanimal == 'none'",
                #   plotOutput("EmptyPlot")
                # ),
                # conditionalPanel(
                #   condition = "input.Yanimal == 'none'",
                #   plotOutput("EmptyPlot")
                # )
                # verbatimTextOutput("summary")
                # Add in option to make it a histogram
                
              )))
    # tabItem(tabName = "task",
    #         h3("Tasks range from Easy to Hard."),
    #           h3("One Question from each data set"),
    #         h3("After reading each question, go back to the App
    #            screen and attempt to make the transformation and
    #            check back at this screen to see if you're correct"),
    #         sidebarLayout(
    #           sidebarPanel(
    #             "Use Log Transformations to make the data linear in
    #              the Animal Data Set",
    #             br(),
    #             br(),
    #             textOutput("q1"),
    # 
    #             shinyBS::bsButton("q1A", "Submit"),
    #             br(),
    #             br(),
    #             br(),
    #             "Use Log Transformations to find the variables
    #             needed to make the data linear in the earthquake data
    #             World Dataset",
    #             br(),
    #             br(),
    #             textOutput("q2"),
    # 
    #             shinyBS::bsButton("q2A", "Submit"),
    #             br(),
    #             br(),
    #             br(),
    #             "Use Log Transformations to find the variables needed to make the data
    #             linear in the world data",
    #             br(),
    #             br(),
    #             textOutput("q3"),
    #             shinyBS::bsButton("q3A", "Submit")
    #             ),
    #           mainPanel(
    #             h3(textOutput("Total"))
    # 
    #           )
    #           )
    # 
    #         ),
    # 
    # # Matching Game
    # tabItem(tabName = "1level",
    #         fluidPage(
    # 
    #           theme = "styles.css",
    #           titlePanel("Level 1"),
    #           h3("Total Score"),
    #           h3(textOutput("score")),
    #           bsPopover("score", "Current Score", "The score you have obtained throughout the game"),
    #           fluidRow(column(3, offset = 9, h3(textOutput("timer1")))),
    #           br(),
    #           br(),
    #           fluidRow(column(1, offset = 5,
    #                         bsButton("go", "G O !", style = "warning", size = "large"))),
    #           br(),
    #           br(),
    #           br(),
    #           conditionalPanel("input.go != 0",
    #                            fluidRow(
    #                              column(6, dragUI("var1","brain", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
    #                              #bsPopover("var1", "Matching Game", "Place a tile here"),
    #                              column(6, dragUI("var2","body", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))
    # 
    #                              #column(3, dragUI("var3","Log Transform", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
    #                              #column(3, dragUI("var4","No Log Transform", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))
    #                            )
    #                            ),
    # 
    #         br(),
    #         br(),
    #         br(),
    #         # fluidRow(
    #         #   column(6, h3("X-Axis")),
    #         #   column(6, h3("Y-Axis"))
    #         #   #column(4, h3("Log Transform"))
    #         # ),
    #         fluidRow(
    # 
    #           wellPanel(
    #             fluidRow(
    #               column(6, h3("X-Axis"))
    #             ),
    #             fluidRow(
    #               column(2, dropUI("drp1", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
    #               bsPopover("drp1", "Matching Game", "Place a tile here"),
    #               column(1, conditionalPanel("input.submitA != 0", htmlOutput("answer1")))
    #             ),
    #             fluidRow(
    #               column(6, checkboxInput("Log1x", h4("Log Transform X"))),
    #               bsPopover("Log1x", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
    #             )
    #           ,class = "col-lg-6 col-md-6 col-sm-12 wellPanel1"),
    # 
    #           wellPanel(
    #             fluidRow(
    #               column(6, h3("Y-Axis"))
    #             ),
    #             fluidRow(
    #               column(2, dropUI("drp2", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
    #               bsPopover("drp2", "Matching Game", "Place a tile here"),
    #               column(1,conditionalPanel("input.submitA != 0",htmlOutput("answer2")))
    #             ),
    #             fluidRow(
    #               column(6, checkboxInput("Log1y", h4("Log Transform Y"))),
    #               bsPopover("Log1y", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
    #             )
    #           ,class = "col-lg-6 col-md-6 col-sm-12 wellPanel1")
    #         ),
    # 
    # 
    # 
    #         # fluidRow(
    #         #
    #         #
    #         #
    #         #
    #         #
    #         #
    #         #   column(2, offset = 3, dropUI("drp2", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
    #         #   bsPopover("drp2", "Matching Game", "Place a tile here"),
    #         #   column(1,conditionalPanel("input.submitA != 0",htmlOutput("answer2")))
    #         #   #column(3,dropUI("drp3", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
    #         #   #column(1,conditionalPanel("input.submitA != 0",htmlOutput("answer3")))
    #         # ),
    #         # fluidRow(
    #         #   column(6, checkboxInput("Log1x", h4("Log Transform X"))),
    #         #   bsPopover("Log1x", "Log Transformation", "Check this box if you believe the variable needs Log Transformed"),
    #         #   column(6, checkboxInput("Log1y", h4("Log Transform Y"))),
    #         #   bsPopover("Log1y", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
    #         # ),
    #         br(),
    #         br(),
    #         br(),
    #         fluidRow(
    #           column(2, offset = 8, bsButton("next2", "Next>>", style = "primary", disabled = T)),
    #           column(2, bsButton("submitA", "Submit Answer", style = "primary")),
    #           bsPopover("submitA", "Submission", "Click this button once you are done filling in each tile to see the correct answer")
    #         )
    # 
    # 
    #         )),
    # tabItem(tabName = "2level",
    #         fluidPage(
    #           theme = "bootstrap.css",
    #           titlePanel("Level 2"),
    #           h3("Total Score"),
    #           h3(textOutput("score1")),
    #           bsPopover("score1", "Current Score", "The score you have obtained throughout the game"),
    #           fluidRow(column(3, offset = 9, h3(textOutput("timer2")))),
    #           br(),
    #           br(),
    #           br(),
    #           conditionalPanel("input.next2 != 0",
    #           fluidRow(column(1, offset = 5,
    #                   # conditionalPanel("input.go != 0",
    #                            bsButton("goo", "G O !", style = "warning", size = "large")
    #                            ),
    #                            bsPopover("goo", "Start Button", "Press this button in order to start the game and the timer"),
    #                            br(),
    #                            br(),
    #                            br(),
    #           conditionalPanel("input.goo != 0",
    #                            fluidRow(
    #                              column(4,dragUI("var5","magnitude", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
    #                              column(4,dragUI("var6","distance", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
    #                              column(4,dragUI("var7","Peak Acceleration", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))
    #                              #column(3,dragUI("var8","Log Transform", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))
    #                            )
    #                            # br(),
    #                            # br(),,
    #                            # fluidRow(
    #                            #   column(3, dragUI("var9","No Log Transform", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))
    #                            # )
    # 
    #           ))),
    # 
    #           br(),
    #           br(),
    #           br(),
    #           fluidRow(
    # 
    #             wellPanel(
    #               fluidRow(
    #                 column(6, h3("X-Axis"))
    #               ),
    #               fluidRow(
    #                 column(2, dropUI("drp4", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
    #                 bsPopover("drp4", "Matching Game", "Place a tile here"),
    #                 column(1, conditionalPanel("input.submitB != 0", htmlOutput("answer4")))
    #               ),
    #               fluidRow(
    #                 column(6, checkboxInput("Log2x", h4("Log Transform X"))),
    #                 bsPopover("Log2x", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
    #               )
    #               ,class = "col-lg-6 col-md-6 col-sm-12 wellPanel1"),
    # 
    #             wellPanel(
    #               fluidRow(
    #                 column(6, h3("Y-Axis"))
    #               ),
    #               fluidRow(
    #                 column(2, dropUI("drp5", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
    #                 bsPopover("drp5", "Matching Game", "Place a tile here"),
    #                 column(1,conditionalPanel("input.submitB != 0",htmlOutput("answer5")))
    #               ),
    #               fluidRow(
    #                 column(6, checkboxInput("Log2y", h4("Log Transform Y"))),
    #                 bsPopover("Log2y", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
    #               )
    #               ,class = "col-lg-6 col-md-6 col-sm-12 wellPanel1")
    #           ),
    # 
    #           br(),
    #           br(),
    #           br(),
    #           fluidRow(
    #             column(2, offset = 8, bsButton("next3", "Next>>", style = "primary", disabled = T)),
    #             column(1, bsButton("submitB", "Submit Answer", style = "primary")),
    #             bsPopover("submitB", "Submission", "Click this button once you are done filling in each tile to see the correct answer")
    #           )
    # 
    #         )
    #         ),
    # tabItem(tabName = "3level",
    #         fluidPage(
    #           theme = "bootstrap.css",
    #           titlePanel("Level 3"),
    #           h3("Total Score"),
    #           h3(textOutput("score2")),
    #           bsPopover("score2", "Current Score", "The score you have obtained throughout the game"),
    #           fluidRow(column(3, offset = 9, h3(textOutput("timer3")))),
    #           br(),
    #           br(),
    #           br(),
    #           conditionalPanel("input.next3 != 0",
    #           fluidRow(column(1, offset = 5,
    #                           conditionalPanel("input.go != 0",
    #                                            bsButton("gooo", "G O !", style = "warning", size = "large")))),
    #           bsPopover("gooo", "Start Button", "Press this button in order to start the game and the timer")),
    #           br(),
    #           br(),
    #           br(),
    #           conditionalPanel("input.gooo != 0",
    #                            fluidRow(
    #                              column(3,dragUI("var10","gdp", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
    #                              column(3,dragUI("var11","income", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
    #                              column(3,dragUI("var12","literacy", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
    #                              column(3, dragUI("var13","military", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))
    #                            )
    #                            # br(),
    #                            # br(),
    #                            # fluidRow(
    #                            # column(3, dragUI("var14","Log Transform", style = "background-color:orange; width: 250px; font-size:20px; height: 30px")),
    #                            # column(3, dragUI("var15","No Log Transform", style = "background-color:orange; width: 250px; font-size:20px; height: 30px"))
    #                            # )
    # 
    #           ),
    # 
    #           br(),
    #           br(),
    #           br(),
    #           fluidRow(
    # 
    #             wellPanel(
    #               fluidRow(
    #                 column(6, h3("X-Axis"))
    #               ),
    #               fluidRow(
    #                 column(2, dropUI("drp7", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
    #                 bsPopover("drp7", "Matching Game", "Place a tile here"),
    #                 column(1, conditionalPanel("input.submitC != 0", htmlOutput("answer7")))
    #               ),
    #               fluidRow(
    #                 column(6, checkboxInput("Log3x", h4("Log Transform X"))),
    #                 bsPopover("Log3x", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
    #               )
    #               ,class = "col-lg-6 col-md-6 col-sm-12 wellPanel1"),
    # 
    #             wellPanel(
    #               fluidRow(
    #                 column(6, h3("Y-Axis"))
    #               ),
    #               fluidRow(
    #                 column(2, dropUI("drp8", style = "border-left: 6px solid orange; background-color: lightgrey; height:50px; width:300px")),
    #                 bsPopover("drp8", "Matching Game", "Place a tile here"),
    #                 column(1,conditionalPanel("input.submitC != 0",htmlOutput("answer8")))
    #               ),
    #               fluidRow(
    #                 column(6, checkboxInput("Log3y", h4("Log Transform Y"))),
    #                 bsPopover("Log3y", "Log Transformation", "Check this box if you believe the variable needs Log Transformed")
    #               )
    #               ,class = "col-lg-6 col-md-6 col-sm-12 wellPanel1")
    #           ),
    #           br(),
    #           br(),
    #           fluidRow(
    #             htmlOutput("badge")
    #           ),
    #           br(),
    #           fluidRow(
    #             #column(2, offset = 8, bsButton("next3", "Next>>", style = "primary", disabled = T)),
    #             column(1, offset = 10, bsButton("submitC", "Submit Answer", style = "primary")),
    #             bsPopover("submitC", "Submission", "Click this button once you are done filling in each tile to see the correct answer"),
    #             br(),
    #             br(),
    #           fluidRow(
    #             column(4, uiOutput("initials"))),
    #           fluidRow(
    #             column(2, uiOutput("finish")),
    #             bsPopover("finish", "Insert your highscore", "")),
    #           fluidRow(
    #             column(6, bsButton("showhigh", "Show Highscores", style = "primary")),
    #             column(6, offset = 0, bsButton("weekshowhigh", "Show Weekly Highscores", style = "primary"))
    #           ),
    #           fluidRow(
    #             column(6, tableOutput("highscore")),
    #             column(6, tableOutput("weekhighscore")),
    #             bsPopover("showhigh", "Scoreboard", "All Time Highscores"),
    #             bsPopover("weekshowhigh", "Weekly Scoreboard", "Highscores for the week")
    #           ),
    #           fluidRow(
    #           
    #             textOutput("names"))
    #           )
    # 
    #         )
    #         ),
    # tabItem(tabName = "instr",
    #         titlePanel("Instructions"),
    #         tags$ul(
    #         tags$li(h5("There are three levels, each level gets consecutively harder.")),
    #         br(),
    #         tags$li(h5("Each level is based on the data sets you interacted with in the previous task.")),
    #         br(),
    #         tags$li(h5("In each level, after you hit the GO! button, a number of boxes will
    #            show up on your screen.")),
    #         br(),
    #         tags$li(h5("Drag each variable to the designated boxes and check the box for Log Transformation(s) if needed to linearize the data")),
    #         br(),
    #         tags$li(h5("After you have filled both areas with boxes, hit the submit button to check if you are correct.")),
    #         br(),
    #         tags$li(h5("Clicking Next will bring you to the first level.")),
    #         br(),
    #         tags$li(h5("Hit Go! to start each level!"))),
    #         br(),
    #         br(),
    #         fluidRow(column(1, offset = 11, bsButton("next1", "Next>>", style = "primary")))
    # )
  
            
  )
)

shinyUI(dashboardPage(skin = "green", header, sidebar, body))
