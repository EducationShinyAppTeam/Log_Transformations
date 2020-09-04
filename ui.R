library(shiny)
library(shinydashboard)
library(shinyBS)
#library(shinyjs) #for drag and drop
#library(shinyDND) #for drag and drop game if we use that tab
library(shinyWidgets)
library(boastUtils)
library(MASS)
library(DAAG)

#Drag n Drop with correct variables and each data set
# Put things on PSU shiny server
# Define UI for application that draws a histogram
ui <- list(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  ),
  dashboardPage(
    skin = "green", 
    ### Create the app header
    dashboardHeader(
      titleWidth = 250, 
      title = "Log Transformations", 
      tags$li(class = "dropdown", 
              actionLink("info",icon("info",class = "myClass"))), 
      tags$li(class = "dropdown", 
              tags$a(href='https://shinyapps.science.psu.edu/', 
                     icon("home")))
    ), 
    dashboardSidebar(
      width = 250, 
      sidebarMenu(
        id = "tabs", 
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")), 
        menuItem("Prerequisites", tabName = "prerequisite", icon = icon("book")), 
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")), 
        #menuItem("Task", tabName = "task", icon = icon("leanpub")), 
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ), 
      tags$div(
        class = "sidebar-logo", 
        boastUtils::psu_eberly_logo("reversed")
      )
    ), 
    dashboardBody(
      tabItems(
      tabItem(tabName = "overview", 
              h1("Log Transformation"),
              p("The goals of this app are to know when to use 
              log transformations to linearize data, how to analyze scenarios
                where a Log Transform is needed and when it is not."),
              p("There are three different datasets, which are about Animals,
                Earthquakes, and Countries."),
              br(),
              h2("Instructions"),
              tags$ol(
                tags$li("In the application, you will see a number of 
                        different data sets you can go through."), 
                tags$li("Go through each data set and look through 
                the different variables to check whether
                        a log transformation would help."), 
                tags$li("Use the 'Show Histogram' button to see each 
                        individual variable more clearly."), 
                tags$li("You can also examine log transforms on 
                        variables in your own data set!")
              ), 
              div(
                style = "text-align: center", 
                bsButton(
                  inputId = "go1", 
                  label = "GO!", 
                  size = "large", 
                  icon = icon("bolt"), 
                )
              ), 
              br(), 
              br(), 
              h2("Acknowledgement"), 
              p("This app was coded and developed by Alex Chen. The was further
              updated by Daehoon Gwak in July 2020.", 
                br(), 
                br(), 
                br(), 
                div(class = "updated", "Last Update: 7/31/2020 by DG")
              )
      ), 
        tabItem(
          tabName = "prerequisite", 
                h2("Prerequisites"), 
                tags$ol(
                  tags$li("Make a distribution more symmetric."), 
                  tags$li("Make the relationship between distributions more 
                          linear."), 
                  tags$li("Make a relationship more interpretable.")
                ), 
                br(), 
                div(
                  style = "text-align:center", 
                  bsButton(
                    inputId = "go2", 
                    label = "GO!", 
                    icon("bolt"), 
                    size = "large"
                  )
                )
              ), 
    tabItem(tabName = "explore", 
            h2("Log Transformation Task"), 
              conditionalPanel(
                "input.inputs == 'Animals'", 
                h4(" Data Description"), 
                p("This dataset describes the correlation between the 
                  brain weight(g) and body weight(kg) of different animals")
              ), 
              conditionalPanel(
                "input.inputs == 'Earthquakes'", 
                h4(" Data Description"), 
                p("This data set describes different earthquakes that occurred 
                  in the US using the magnitude, distance from where it was 
                  recorded and the ground acceleration of the earthquake")
              ), 
              conditionalPanel(
                "input.inputs == 'Countries'", 
                h4(" Data Description"), 
                p("This data set contains 154 countries and data about them 
                  such as GDP, income per capita, literacy rate,  
                  and money spent on military")
              ), 
            sidebarLayout(
              sidebarPanel(
                selectInput("inputs", "Select Data Set", choices = c('None',
                                      'Animals', 'Earthquakes', "Countries")), 
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
                              c("magnitude"="mag", "distance" = "dist", 
                                "Peak Acceleration" = "accel")
                  ), 
                  #bsPopover("Xquake", "Choose your X-Variable", "Choose which variable you want on the X Axis in the Earthquake Dataset", placement = "top"),
                  selectInput("Yquake", 
                              "Select Your Y-Axis", 
                             c("magnitude" = "mag", "distance" = "dist",
                               "Peak Acceleration" = "accel"))
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
                # conditionalPanel(
                #   condition = "input.inputs == 'Input your own'",
                #   "Be careful when entering a dataset with non-positive values",
                #   fileInput('file', 'Choose info-file to upload',
                #             accept = c(
                #               'text/csv',
                #               'text/comma-separated-values',
                #               'text/tab-separated-values',
                #               'text/plain',
                #               '.csv',
                #               '.tsv'
                #             )
                #   ),
                #   bsPopover("file", "File Upload", "If you have your own dataset you would like to enter, upload it through this"),
                #   checkboxInput('header', 'Header', TRUE),
                #   bsPopover("header", "Header", "Please check whether there is a header for your data or not"),
                #   radioButtons('sep', 'Separator',
                #                c(Comma = ',',
                #                  Semicolon = ';',
                #                  tab = '\t'),
                #                ','),
                #   bsPopover("sep", "Separator", "Choose which separation your file is using(If you do not know, it is most likely separating with commas"),
                #   radioButtons('quote', 'Quote',
                #                c(None = '',
                #                  'Double Quote' = '"',
                #                'Single Quote' = "'"),
                #               '"'),
                #   bsPopover("quote", "Quotes", "Check what kind of data you have and whether you need to use quotes or not"),
                #   selectInput("columns", "Select Your X-Axis", choices = NULL),
                #   #bsPopover("columns", "Choose your X-Variable", "Choose which variable you want on the X Axis in the Dataset you inputted"),
                #   selectInput("columns2", "Select Your Y-Axis", choices = NULL)
                #   #bsPopover("columns", "Choose your Y-Variable", "Choose which variable you want on the Y Axis in the Dataset you inputted")
                #   #tableOutput("fileinput")
                # ),
                checkboxGroupInput("transforms", "Transform X or Y",
                                   c("Transform X", "Transform Y"),
                                   selected = NULL), 
                bsPopover("transforms", "Transformation", 
                          "Decide whether you want to log transform the
                          X, Y, or both axes"), 
                actionButton('hist', "Show histograms"), 
                bsPopover("hist", "Histogram", "Click me if you want
                          to see the histograms of both the X and Y
                          axes you have selected"), 
                checkboxInput('loghist1', 'Show Log: XValue Hist'), 
                bsPopover("loghist1", "Log Transform of Histogram(X-Variable)", 
                          "Check this box if you want to see the log 
                          transformation of the X-axis in the Histogram"), 
                checkboxInput('loghist2', 'show Log: YValue Hist'), 
                bsPopover("loghist2", "Log Transform of Histogram(Y-Variable)", 
                          "Check this box if you want to see the log 
                          transformation of the Y-axis in the Histogram"), 
                #h4("Variable Descriptions"),
                conditionalPanel(
                  condition = "input.inputs == 'Animals'", 
                  downloadButton("animalDownload", "Download"), 
                  #bsPopover("animalDownload", "", "Download this data set!")
                  # textOutput("Animal1"),
                  # textOutput("Animal2"),
                  # bsPopover("Animal1", "Body", "Body Weight in kg"),
                  # bsPopover("Animal2", "Brain", "Brain weight in g")
                  h4("Challenge Question"), 
                  p("Does it help to make a log transform on both variables?"), 
                  textOutput("animalQ")
                ), 
                conditionalPanel(
                  condition = "input.inputs == 'Earthquakes'", 
                  downloadButton("quakeDownload", "Download"), 
                  #bsPopover("quakeDownload", "", "Download this data set!")
                  # textOutput("Quake1"),
                  # textOutput("Quake2"),
                  # textOutput("Quake3"),
                  # bsPopover("Quake1", "Magnitude", "Magnitude of earthquake"),
                  # bsPopover("Quake2", "Distance", "Distance in km from station recorded"),
                  # bsPopover("Quake3", "Peak Acceleration", "Ground Acceleration in units of gravity"),
                  h4("Challenge Question"),
                  p("Which two variables would benefit most from a log 
                    transformation?  Why?"), 
                  textOutput("quakeQ")
                ), 
                conditionalPanel(
                  condition = "input.inputs == 'Countries'", 
                  downloadButton("worldDownload", "Download"), 
                  #bsPopover("worldDownload", "", "Download this data set!")
                  # textOutput("World1"),
                  # textOutput("World2"),
                  # textOutput("World3"),
                  # textOutput("World4"),
                  # bsPopover("World1", "GDP", "Gross Domestic Product per capita"),
                  # bsPopover("World2", "Income", "Income per capita"),
                  # bsPopover("World3", "Literacy", "% of population literate"),
                  # bsPopover("World4", "Military", "Dollars(USD) spent on military"),
                  h4("Challenge Question"),
                  p("Which two variables would benefit most from a log 
                    transformation?  Why?"), 
                  textOutput("worldQ")
                )
                #selectInput("plottype", "Plot Type", choices = c("Dot Plot", "Histogram"))
              ), 
              # Show a plot of the generated distribution
              mainPanel(
                conditionalPanel(
                  condition = "input.inputs == 'Countries'", 
                  plotOutput("worldPlot"),
                  bsPopover("worldPlot", "World Dataset Plot", 
                            "Plot for the World Dataset given X and 
                            Y variables chosen by the user"), 
                  plotOutput("worldBars"), 
                  bsPopover("worldBars", "World X-Variable Histogram", 
                            "Histogram of the selected X-Variable"), 
                  plotOutput("worldBars2"), 
                  bsPopover("WorldBars2", "World Y-Variable Histogram", 
                            "Histogram of the selected Y-Variable")
                  # plotOutput("worldBars3"),
                  # plotOutput("worldBars4")
                ), 
                conditionalPanel(
                  condition = "input.inputs == 'Animals'", 
                  plotOutput("animalPlot"), 
                  bsPopover("animalPlot", "Animal Dataset Plot",
                            "Plot for the Animal Dataset given X and Y
                            variables chosen by the user"), 
                  plotOutput("animalBars"), 
                  bsPopover("animalBars", "Animal X-Variable Histogram", 
                            "Histogram of the selected X-Variable"), 
                  plotOutput("animalBars2"), 
                  bsPopover("animalBars2", "Animal Y-Variable Histogram", 
                            "Histogram of the selected Y-Variable")
                ), 
                conditionalPanel(
                  condition = "input.inputs == 'Earthquakes'", 
                  plotOutput("quakePlot"), 
                  bsPopover("quakePlot", "Earthquake Dataset Plot", 
                            "Plot for the Earthquake Dataset given X and Y
                            variables chosen by the user"), 
                  plotOutput("quakeBar"), 
                  bsPopover("quakeBar", "Earthquake X-Variable Histogram", 
                            "Histogram of the selected X-Variable"), 
                  plotOutput("quakeBar2"), 
                  bsPopover("quakeBar2", "EarthQuake Y-Variable Histogram",
                            "Histogram of the selected Y-Variable")
                )
                # conditionalPanel(
                #   condition = "input.inputs == 'Input your own'",
                #   tableOutput("fileinput"),
                #   bsPopover("fileinput", "Inputted Dataset Sample Table", "Sample table for the Inputted Dataset"),
                #   plotOutput("filePlot"),
                #   bsPopover("filePlot", "Inputted Dataset Plot", "Plot for the Inputted Dataset given X and Y variables chosen by the user"),
                #   plotOutput("fileBars"),
                #   bsPopover("fileBars", "Inputted X-Variable Histogram", "Histogram of the seleted X-Variable"),
                #   plotOutput("fileBars2"),
                #   bsPopover("fileBars2", "Inputted Y-Variable Histogram", "Histogram of the selected Y-Variable")
                # )
                # # conditionalPanel(
                #   condition = "input.Xanimal == 'none'",
                #   plotOutput("EmptyPlot")
                # ),
                # conditionalPanel(
                #   condition = "input.Yanimal == 'none'",
                #   plotOutput("EmptyPlot")
                # )
                # verbatimTextOutput("summary")
                # Add in option to make it a histogram
              )
            )
          ), 
    tabItem(
      tabName = "References", 
      h2("References"), 
      p(     #shinyBS
        class = "hangingindent", 
        "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, 
        R package. Available from 
        https://CRAN.R-project.org/package=shinyBS"
      ), 
      p(     #Boast Utilities 
        class = "hangingindent", 
        "Carey, R. (2019), boastUtils: BOAST Utilities, R Package.
        Available from 
        https://github.com/EducationShinyAppTeam/boastUtils"
      ), 
      p(     #shinydashboard
        class = "hangingindent",
        "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R Package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
      ), 
      p(     #shiny
        class = "hangingindent",
        "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. 
        (2019), shiny: Web application framework for R, R Package.
        Available from https://CRAN.R-project.org/package=shiny"
      ), 
      p(     #Dataset 'Countries'
        class = "hangingindent",
        "Countries dataset is available from
            https://www.stat.berkeley.edu/~s133/resources.html"
      ), 
      p(     #DAAG
        class = "hangingindent",
        "Maindonald, J.H., and Braun, W.J. (2020), DAAG: Data Analysis 
        and Graphics Data and Functions, R package. Available from
        https://cran.r-project.org/web/packages/DAAG/index.html"
      ), 
      p(     #shinyWidgets
        class = "hangingindent",
        "Perrier, V., Meyer, F., Granjon, D., Fellows, I., and Davis, W. 
        (2020), shinyWidgets: Custom Inputs Widgets for Shiny, R package. 
        Available from 
        https://cran.r-project.org/web/packages/shinyWidgets/index.html"
      ), 
      p(     #MASS
        class = "hangingindent",
        "Ripley, B., Venables, B., Bates, D.M., Hornik, K., Gebhardt, A., and
        Firth, D. (2020), MASS: Support Functions and Datasets for Venables
        and Ripley's MASS, R package. Available from
        https://cran.r-project.org/web/packages/MASS/index.html"
      )
    #)
    )
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
)
)
