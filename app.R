# Load packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(MASS)
library(scales)

# Define FD binwidth function ----
fdWidth <- function(x){
  ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))
}

# Load and preprocess data ----
worldData <- read.csv('world2.csv')
worldData <- worldData[,-c(1,2,7)]
worldData <- worldData[-c(36),]
worldData$income <- as.numeric(worldData$income)
countryVars <- c("gdp", "income", "literacy", "military")

quakeData <- datasets::attenu
colnames(quakeData) <- c("event", "magnitude", "station", "distance", "peak acceleration")
earthquakeVars <- c("magnitude", "distance", "peak acceleration")

animalData <- MASS::Animals
animalVars <- c("body", "brain")

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "green",  
    ## Header ----
    dashboardHeader(
      titleWidth = 250, 
      title = "Log Transformations", 
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Log_Transformations") 
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ), 
    ## Sidebar ----
    dashboardSidebar(
      width = 250, 
      sidebarMenu(
        id = "pages", 
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")), 
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")), 
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ), 
      tags$div(
        class = "sidebar-logo", 
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview page ----
        tabItem(
          tabName = "overview", 
          h1("Log Transformations"), 
          p("The goals of this app are to help a user get a sense of when to use
            log transformations to linearize data, how to analyze scenarios where
            a log transformation is needed and when it is not."), 
          p("We will use three different datasets in this app, which are about
            Animals, Earthquakes, and Countries."), 
          br(), 
          h2("Instructions"), 
          tags$ol(
            tags$li("In the application, you will see a number of different data
                    sets you can work with."),  
            tags$li("Go through each data set and look at the different variables
                    to check whether a log transformation would help."),  
            tags$li("Use the 'Show Histogram' button to see each individual
                    variable more clearly.")
          ), 
          div(
            style = "text-align: center;", 
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
              updated by Daehoon Gwak in November 2020 and by Paridhi Khandelwal
              in October 2021.", 
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 11/19/2021 by NJH")
          )
        ),
        ### Explore page ----
        tabItem(
          tabName = "explore", 
          h2("Exploring Log Transformations"),
          p("Select a data set and variables to explore to see whether applying 
            a log transformation can be helpful."),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              selectInput(
                inputId = "selectData",
                label = "Select Data Set",
                choices = c('Animals', 'Earthquakes', "Countries")
              ) 
            ), 
            column(
              width = 8,
              offset = 0,
              uiOutput("dataDescription")
            )
          ), 
          fluidRow(
            column(
              width = 4, 
              offset = 0,
              wellPanel(
                selectInput(
                  inputId = "horizVar",
                  label = "Variable on horizontal axis",
                  choices = c("A", "B", "C"),
                ),
                selectInput(
                  inputId = "vertVar",
                  label = "Variable on vertical axis",
                  choices = c("A", "B", "C")
                ),
                checkboxGroupInput(
                  inputId = "transforms", 
                  label = "Apply log transformation to", 
                  choices = c("horizontal", "vertical"), 
                  selected = NULL
                ),
                bsPopover(
                  id = "transforms", 
                  title = "Transformation", 
                  content = paste("Decide whether you want to log transform",
                                  "the horizontal, the vertical, or both axes."),
                  placement = "top"
                ),
                hr(),
                p("Why not apply transform choices to histograms?"),
                checkboxInput(
                  inputId = 'loghist1', 
                  label = 'Show Log: XValue Hist'
                ), 
                bsPopover(
                  id = "loghist1", 
                  title = "Log Transform of Histogram(X-Variable)",
                  content = "Check this box if you want to see the log transformation of the X-axis in the Histogram"
                ), 
                checkboxInput(
                  inputId = 'loghist2', 
                  label = 'Show Log: YValue Hist'
                ), 
                bsPopover(
                  id = "loghist2",
                  title = "Log Transform of Histogram(Y-Variable)", 
                  content = "Check this box if you want to see the log transformation of the Y-axis in the Histogram"
                )
              )
            ), 
            column(
              width = 8, 
              offset = 0,
              plotOutput("scatterPlot"),
              plotOutput("horizontalHist"),
              plotOutput("verticalHist")
            )
          )
        ),
        ### References page ----
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
      p(     #shinyWidgets
        class = "hangingindent",
        "Perrier, V., Meyer, F., Granjon, D., Fellows, I., and Davis, W. 
        (2020), shinyWidgets: Custom Inputs Widgets for Shiny, R package. 
        Available from 
        https://cran.r-project.org/web/packages/shinyWidgets/index.html"
      ),
      p(     #MASS
        class = "hangingindent",
        "Venables, W. N. and Ripley, B. D. (2002), MASS: Support Functions and 
        Datasets for Venables and Ripley's MASS, R Package. Available from
            https://CRAN.R-project.org/package=MASS"
      ),
      p(     #Attenu Earthquake
        class = "hangingindent",
        "Joyner, W. B., Boore, D. M., and Porcella, R. D. (1981), 
        Peak Horizontal Acceleration and Velocity from Strong-Motion Records Including Records from the 1979 Imperial Valley California Earthquake"
      )
    )
   )
 ))
)

# Define the disable function ----
disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

# Define the server ----
server <- function(input, output, session) {
  dataSet <- reactiveVal(NULL)
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session, 
      title = "Instructions:", 
      text = "Pick a data set and view the effect of 
      the log transform on the X and or Y variables.", 
      type = "info", 
      btn_colors = "green"
    )
  })
  #Go to Explore tab ----
  observeEvent(input$go1, {
    updateTabItems(session, "pages", "explore")
  })
  
  ## Results of selecting data set ----
  observeEvent(
    eventExpr = input$selectData,
    handlerExpr = {
      ### Update data description ----
      output$dataDescription <- renderUI(
        switch(
          EXPR = input$selectData,
          Animals = "Th Animals data set describes the correlation between the 
                      brain weight (g) and body weight (kg) of different animals.",
          Earthquakes = "The Earthquakes data set describes different earthquakes 
                      that occurred in the US using the magnitude, distance from 
                      where it was recorded and the ground acceleration of 
                      the earthquake.",
          Countries = "The Countries data set contains 154 countries and data about 
                      them such as GDP, income per capita, literacy rate, 
                      and money spent on military."
        )
      )
      
      ### Update variable choices ----
      updateSelectInput(
        session = session,
        inputId = "horizVar",
        choices = switch(
          EXPR = input$selectData,
          Animals = animalVars,
          Earthquakes = earthquakeVars,
          Countries = countryVars
        ),
        selected = switch(
          EXPR = input$selectData,
          Animals = "body",
          Earthquakes = "magnitude",
          Countries = "gdp"
        )
      )
      
      updateSelectInput(
        session = session,
        inputId = "vertVar",
        choices = switch(
          EXPR = input$selectData,
          Animals = animalVars,
          Earthquakes = earthquakeVars,
          Countries = countryVars
        ),
        selected = switch(
          EXPR = input$selectData,
          Animals = "brain",
          Earthquakes = "distance",
          Countries = "income"
        )
      )
      
      ### Set data set ----
      dataSet(
        switch(
          EXPR = input$selectData,
          Animals = animalData,
          Earthquakes = quakeData,
          Countries = worldData
        )
      )
    }
  )
  
  ## Update transformation choices ----
  observeEvent(
    eventExpr = c(input$horizVar, input$vertVar),
    handlerExpr = {
      updateCheckboxGroupInput(
        session = session,
        inputId = "transforms",
        choiceNames = c(input$horizVar, input$vertVar),
        choiceValues = c("horiz", "vert"),
        selected = NULL
      )
    }
  )
  
  ## Main scatter plot ----
  output$scatterPlot <- renderPlot(
    expr = {
      mainPlot <- ggplot(
        data = dataSet(),
        mapping = aes_string(x = input$horizVar, y = input$vertVar)
      ) +
        geom_point(size = 2) +
        theme_bw() +
        labs(
          title = paste(input$vertVar , "vs", input$horizVar)
        ) + 
        theme(
            plot.title = element_text(hjust = 0.5),
            text = element_text(size = 18)
          )
      
      if ("horiz" %in% input$transforms) {
        mainPlot <- mainPlot +
          scale_x_continuous(
            trans = "log",
            labels = scales::number_format(
              accuracy = 0.01,
              big.mark = ","
            )
          ) +
          labs(
            x = paste0("Log(", input$horizVar, ")")
          )
      }
      
      if ("vert" %in% input$transforms) {
        mainPlot <- mainPlot +
          scale_y_continuous(
            trans = "log",
            labels = scales::number_format(
              accuracy = 0.01,
              big.mark = ","
            )
          ) +
          labs(
            y = paste0("Log(", input$vertVar, ")")
          )
      }
      
      mainPlot
    },
    alt = reactive(paste("A scatter plot of", isolate(input$vertVar), "and", 
                isolate(input$horizVar), "from the", 
                isolate(input$selectData), "data set."))
  )
  
  ## Horizontal histogram ----
  output$horizontalHist <- renderPlot(
    expr = {
      firstHist <- ggplot(
        data = dataSet(),
        mapping = aes_string(x = input$horizVar)
      ) +
        geom_histogram(
          color = "black",
          fill = boastPalette[3],
          binwidth = fdWidth,
          closed = "left",
          boundary = 0
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 18)
        )
      
      if ("horiz" %in% input$transforms) {
        firstHist <- firstHist +
          scale_x_continuous(
            trans = "log",
            labels = scales::number_format(
              accuracy = 0.01,
              big.mark = ","
            )
          ) +
          labs(
            x = paste0("Log(", input$horizVar, ")"),
            title = paste0("Histogram of Log(", input$horizVar, ")")
          )
      } else {
        firstHist <- firstHist +
          labs(
            title = paste("Histogram of", input$horizVar)
          )
      }
      
      firstHist
    },
    alt = reactive(paste("A histogram of", isolate(input$horizVar), "from the", 
                         isolate(input$selectData), "data set."))
  )
  
  ## Vertical histogram ----
  output$verticalHist <- renderPlot(
    expr = {
      secondHist <- ggplot(
        data = dataSet(),
        mapping = aes_string(x = input$vertVar)
      ) +
        geom_histogram(
          color = "black",
          fill = boastPalette[3],
          binwidth = fdWidth,
          closed = "left",
          boundary = 0
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 18)
        )
      
      if ("vert" %in% input$transforms) {
        secondHist <- secondHist +
          scale_x_continuous(
            trans = "log",
            labels = scales::number_format(
              accuracy = 0.01,
              big.mark = ","
            )
          ) +
          labs(
            x = paste0("Log(", input$vertVar, ")"),
            title = paste0("Histogram of Log(", input$vertVar, ")")
          )
      } else {
        secondHist <- secondHist +
          labs(
            title = paste("Histogram of", input$vertVar)
          )
      }
      
      secondHist
    },
    alt = reactive(paste("A histogram of", isolate(input$vertVar), "from the", 
                         isolate(input$selectData), "data set."))
  )
  

  # Animal Plots ----
  
  output$animalPlot <- 
    renderPlot({
      #If they don't check on checkbox
      
      if(length(input$transforms) == 0)
      {
        ggplot(
          data = animalData,
          mapping = aes_string(x = input$Xanimal, y= input$Yanimal)
        ) +geom_point() +theme_bw() + # This is the preferred theme
          labs(
            title = paste(input$Xanimal , "vs", input$Yanimal))+  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            )
        
      }
      
      
      # If they checkbox one of them
      else if(length(input$transforms) == 1)
      {
        # If they only checkbox the Transform Y option
        if(input$transforms == 'Transform Y')
        {
          ggplot(
            data = animalData,
            mapping = aes_string(x = input$Xanimal, y=input$Yanimal)
          ) +geom_point() + scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+theme_bw() + 
            labs(
              title = paste("Log:", input$Yanimal))+  theme(
                plot.title = element_text(hjust = 0.5)
              )+theme(
                text = element_text(size = 18)
              )
        }

        else if(input$transforms == 'Transform X')
        {
          ggplot(
            data = animalData,
            mapping = aes_string(x = input$Xanimal, y=input$Yanimal)
          ) +geom_point() + scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+theme_bw() + 
            labs(
              title = paste("Log:", input$Xanimal))+  theme(
                plot.title = element_text(hjust = 0.5)
              )+theme(
                text = element_text(size = 18)
              ) 
        }      
        
      }
      #If they check both boxes
      else #Doesn't plot line, but plots R-squared value
      {
        ggplot(
          data = animalData,
          mapping = aes_string(x = input$Xanimal, y=input$Yanimal)) +geom_point() + 
          scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+
          scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+
          theme_bw() + 
          labs(
            title = paste("Log x vs y"))+  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            ) 
      }
    })
  
  #Histograms of both variables
  output$animalBars <-
    renderPlot({
      if(input$loghist1 == TRUE)
      {
        ggplot(
          data = animalData,
          mapping = aes_string(x = input$Xanimal)
        ) + scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+geom_histogram(
          color = "black", 
          # gives bars a black edging  
          fill = boastPalette[3], # makes the bars green 
          binwidth = function(x){ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))}
        ) +theme_bw() + # This is the preferred theme
          labs(
            title = paste("Histogram of Log", input$Xanimal))+  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            ) 
      }
      
      #Use binwidth 
      else{
        ggplot(
          data = animalData,
          mapping = aes_string(x = input$Xanimal)
        ) +geom_histogram(
          color = "black", 
          # gives bars a black edging  
          fill = boastPalette[3], # makes the bars green 
          binwidth = function(x){ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))}
        ) +theme_bw() + # This is the preferred theme
          labs(
            title = paste("Histogram of", input$Xanimal))+  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            ) 
        
      }
    })
  output$animalBars2 <-
    renderPlot({
      if(input$loghist2 == TRUE)
      {
        ggplot(
          data = animalData,
          mapping = aes_string(x = input$Yanimal)
        ) + scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+geom_histogram(
          color = "black", 
          # gives bars a black edging  
          fill = boastPalette[3], # makes the bars green 
          binwidth = function(x){ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))}
        ) +theme_bw() + # This is the preferred theme
          labs(
            title = paste("Histogram of Log", input$Yanimal))+  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            ) 
      }
      else{
        ggplot(
          data = animalData,
          mapping = aes_string(x = input$Yanimal)
        ) +geom_histogram(
          color = "black", 
          # gives bars a black edging  
          fill = boastPalette[3], # makes the bars green 
          binwidth = function(x){ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))}
        ) +theme_bw() + # This is the preferred theme
          labs(
            title = paste("Histogram of", input$Yanimal)) +  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            ) 
        
      }
    })
  
  # Plots for Countries
  output$worldPlot <-
    renderPlot({
      if(length(input$transforms) == 0)
      {
        ggplot(
          data = worldData,
          mapping = aes_string(x = input$Xworld, y= input$Yworld)
        ) +geom_point() +theme_bw() + # This is the preferred theme
          labs(
            title = paste(input$Xworld , "vs", input$Yworld))+  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            )

      }
      else if(length(input$transforms) == 1)
      {
        if(input$transforms == 'Transform Y')
        {
          ggplot(
            data = worldData,
            mapping = aes_string(x = input$Xworld, y=input$Yworld)
          ) +geom_point() + scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+theme_bw() + 
            labs(
              title = paste("Log", input$Yworld, "vs", input$Xworld))+  theme(
                plot.title = element_text(hjust = 0.5)
              )+theme(
                text = element_text(size = 18)
              )
        }
        else if(input$transforms == 'Transform X')
        {
          ggplot(
            data = worldData,
            mapping = aes_string(x = input$Xworld, y=input$Yworld)
          ) +geom_point() + scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+theme_bw() + 
            labs(
              title = paste("Log", input$Xworld, "vs", input$Yworld))+ 
            
            theme(
                plot.title = element_text(hjust = 0.5)
              )+theme(
                text = element_text(size = 18)
              )
        }
      }
      else
      {
        ggplot(
          data = worldData,
          mapping = aes_string(x = input$Xworld, y=input$Yworld)) +geom_point() + 
          scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+
          scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+
          theme_bw() + 
          labs(
            title = paste("Log", input$Xworld,"vs", "Log", input$Yworld))+  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            ) 
      }
    })
  output$worldBars <-
    renderPlot({
      if(input$loghist1 == TRUE)
      {
        ggplot(
          data = worldData,
          mapping = aes_string(x = input$Xworld)
        ) + scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+geom_histogram(
          color = "black", 
          # gives bars a black edging  
          fill = boastPalette[3], # makes the bars green 
          binwidth = function(x){ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))}
        ) +theme_bw() + # This is the preferred theme
          labs(
            title = paste("Histogram of Log", input$Xworld))+  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            ) 
      }
      else{
        ggplot(
          data = worldData,
          mapping = aes_string(x = input$Xworld)
        ) +geom_histogram(
          color = "black", 
          # gives bars a black edging  
          fill = boastPalette[3], # makes the bars green 
          binwidth = function(x){ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))}
        ) +theme_bw() + # This is the preferred theme
          labs(
            title = paste("Histogram of", input$Xworld))+  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            ) 
      }
    })
  output$worldBars2 <-
    renderPlot({
      if(input$loghist2 == TRUE)
      {
        ggplot(
          data = worldData,
          mapping = aes_string(x = input$Yworld)
        ) + scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+geom_histogram(
          color = "black", 
          # gives bars a black edging  
          fill = boastPalette[3], # makes the bars green 
          binwidth = function(x){ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))}
        ) +theme_bw() + # This is the preferred theme
          labs(
            title = paste("Histogram of Log", input$Yworld))+  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            ) 
      }
      else{
        ggplot(
          data = worldData,
          mapping = aes_string(x = input$Yworld)
        ) +geom_histogram(
          color = "black", 
          # gives bars a black edging  
          fill = boastPalette[3], # makes the bars green 
          binwidth = function(x){ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))}
        ) +theme_bw() + # This is the preferred theme
          labs(
            title = paste("Histogram of", input$Yworld))+  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            ) 
      }
    })
  

  # Plots for earthquakes
  
  output$quakePlot <-
    renderPlot({
      #If they don't checkbox anything
      if(length(input$transforms) == 0)
      {
        ggplot(
          data = quakeData,
          mapping = aes_string(x = input$Xquake, y= input$Yquake)
        ) +geom_point() +theme_bw() + # This is the preferred theme
          labs(
            title = paste(input$Xquake , "vs", input$Yquake))+  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            )
        

      }
      # If they checkbox one of them
      else if(length(input$transforms) == 1)
      {
        # If they only checkbox the Transform Y option
        if(input$transforms == 'Transform Y')
        {
          ggplot(
            data = quakeData,
            mapping = aes_string(x = input$Xquake, y=input$Yquake)
          ) +geom_point() + scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+theme_bw() + 
            labs(
              title = paste("Log", input$Yquake, "vs", input$Xquake))+  theme(
                plot.title = element_text(hjust = 0.5)
              )+theme(
                text = element_text(size = 18)
              )
        }
        # If they only checkbox the Transform X option
        else if(input$transforms == 'Transform X')
        {
          ggplot(
            data = quakeData,
            mapping = aes_string(x = input$Xquake, y=input$Yquake)
          ) +geom_point() + scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+theme_bw() + 
            labs(
              title = paste("Log", input$Xquake, "vs", input$Yquake))+  theme(
                plot.title = element_text(hjust = 0.5)
              )+theme(
                text = element_text(size = 18)
              )
        }
      }
      #If they check both boxes
      else #Doesn't plot line, but plots R-squared value
      {
        ggplot(
          data = quakeData,
          mapping = aes_string(x = input$Xquake, y=input$Yquake)) +geom_point() + 
          scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+
          scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+
          theme_bw() + 
          labs(
            title = paste("Log", input$Xquake, "vs",input$Yquake ))+  theme(
              plot.title = element_text(hjust = 0.5)
            )+theme(
              text = element_text(size = 18)
            ) 
      }
    })


    output$quakeBar2 <-
      renderPlot({
        if(input$loghist2 == TRUE)
        {
          ggplot(
            data = quakeData,
            mapping = aes_string(x = input$Yquake)
          ) + scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+geom_histogram(
            color = "black", 
            # gives bars a black edging  
            fill = boastPalette[3], # makes the bars green 
            binwidth = function(x){ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))}
          ) +theme_bw() + # This is the preferred theme
            labs(
              title = paste("Histogram of Log", input$Yquake))+  theme(
                plot.title = element_text(hjust = 0.5)
              )+theme(
                text = element_text(size = 18)
              ) 
        }
        else{
          ggplot(
            data = quakeData,
            mapping = aes_string(x = input$Yquake)
          ) +geom_histogram(
            color = "black", 
            # gives bars a black edging  
            fill = boastPalette[3], # makes the bars green 
            binwidth = function(x){ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))}
          ) +theme_bw() + # This is the preferred theme
            labs(
              title = paste("Histogram of", input$Yquake))+  theme(
                plot.title = element_text(hjust = 0.5)
              )+theme(
                text = element_text(size = 18)
              ) 
        }
      })
    output$quakeBar <-
      renderPlot({
        if(input$loghist1 == TRUE)
        {
          ggplot(
            data = quakeData,
            mapping = aes_string(x = input$Xquake)
          ) + scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01))+geom_histogram(
            color = "black", 
            # gives bars a black edging  
            fill = boastPalette[3], # makes the bars green 
            binwidth = function(x){ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))}
          ) +theme_bw() + # This is the preferred theme
            labs(
              title = paste("Histogram of Log", input$Xquake))+  theme(
                plot.title = element_text(hjust = 0.5)
              )+theme(
                text = element_text(size = 18)
              ) 
        }
        else{
          ggplot(
            data = quakeData,
            mapping = aes_string(x = input$Xquake)
          ) +geom_histogram(
            color = "black", 
            # gives bars a black edging  
            fill = boastPalette[3], # makes the bars green 
            binwidth = function(x){ifelse(IQR(x) == 0, 0.1, 2 * IQR(x) / (length(x)^(1/3)))}
          ) +theme_bw() + # This is the preferred theme
            labs(
              title = paste("Histogram of", input$Xquake))+  theme(
                plot.title = element_text(hjust = 0.5)
              )+theme(
                text = element_text(size = 18)
              ) 
        }
      })
    output$fileBars2 <-
      renderPlot({
        inFile = input$file
        req(inFile)
        f <- read.table(inFile$datapath, header = input$header,
                        sep = input$sep, quote = input$quote)
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
    output$fileBars <-
      renderPlot({
        inFile = input$file
        req(inFile)
        f <- read.table(inFile$datapath, header = input$header,
                        sep = input$sep, quote = input$quote)
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
  
  output$fileinput <- renderTable({
    inFile = input$file
    req(inFile)
    f <- read.table(inFile$datapath, header = input$header,
                    sep = input$sep, quote = input$quote)
    vars = names(f)
    updateSelectInput(session, "columns", "Select Your X-Axis", choices = vars)
    updateSelectInput(session, "columns2", "Select Your Y-Axis", choices = vars)
    head(f, 5)
  })
  
  output$animalQ <- renderText({
    if(input$Xanimal == "body" && input$Yanimal == "brain" 
       && length(input$transforms) == 2){
      "Correct"
    }
  })
  
  output$quakeQ <- renderText({
    if(input$Xquake == "distance" && input$Yquake == "Peak Acceleration" 
       && length(input$transforms) == 2){
      "Correct"
    }
  })
  
  output$worldQ <- renderText({
    if(input$Xworld == "gdp" && input$Yworld == "military" 
       && length(input$transforms) == 2){
      "Correct"
    }
  })
  
  values = reactiveValues(
    count = 0
  )
  output$q1 <- renderText({
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
  output$q2 <- renderText({
    "Submit when finished!"
    input$q2A
    isolate(
      if(input$Xquake == 'distance' &&
         input$Yquake == 'Peak Acceleration'&&
         length(input$transforms) == 2){
        values$count = values$count + 5
        text = "Correct!"
        updateButton(session, "q2A", disabled = TRUE)
      }
      else if(input$Xquake == "magnitude" && input$Yquake == "magnitude"){
        text = "Submit when finished!"
      }
      else{
        values$count <- values$count - 2
        text = "Wrong!"
      })
    text
    # if(text == "Correct!"){
    #   disableActionButton("q2A", session)
    # }
  })
  output$q3 <- renderText({
    "Submit when finished!"
    input$q3A
    isolate(
      if(input$Xworld == 'gdp' &&
         input$Yworld == 'military'&&
         length(input$transforms) == 2){
        values$count <- values$count + 5
        "Correct!"
        updateButton(session, "q3A", disabled = TRUE)
      }
      else if(input$Xworld == "gdp" && input$Yworld == "gdp"){
        "Submit when finished!"
      }
      else{
        values$count <- values$count - 2
        "Wrong!"
      })
  })
  output$Total <- renderText({
    p(paste("Your Total Score is:", values$count))
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
    if((input$drp1 == "body" || input$drp1 == "brain") && input$Log1x == TRUE 
       && (input$drp2 == "brain" || input$drp2 == "body") && input$Log1y == TRUE){
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
      if ((input$drp4 == "distance" || input$drp4 == "Peak Acceleration")  
          && input$Log2x == TRUE){ values$count = values$count + 15}
      else{
        values$count = values$count - 6
      }
    }
    if (!is.null(input$drp5)){
      if ((input$drp5 == "Peak Acceleration" || input$drp5 == "distance") 
          && input$Log2y == TRUE){
        values$count = values$count + 15
      }
      else{
        values$count = values$count - 6
      }
    }
    if((input$drp4 == "distance" || input$drp4 == "Peak Acceleration")  
       && input$Log2x == TRUE && (input$drp5 == "Peak Acceleration" 
                        || input$drp5 == "distance") && input$Log2y == TRUE){
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
    if((input$drp7 == "gdp" || input$drp7 == "military") && input$Log3x == T 
       && (input$drp8 == "military" || input$drp8 == "gdp") && input$Log3y == T){
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
  output$score <- renderText({
    values$count
  })
  
  output$score1 <- renderText({
    values$count
  })
  
  output$score2 <- renderText({
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
      x <- data,
      file <- file.path(outputDir, fileName), 
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
    pattern = paste0(substring(x, 1, 4), "[", substring(y, 5, 5), "-", 
                     substring(x, 5, 5), "]", sep = '')
  }
  else{
    pattern = paste0(substring(y, 1, 4), "[",substring(y, 5, 5), "-9]|", 
                     substring(x,1,4), "[0-", substring(x, 5, 5), "]")
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
    output$highscore <- renderTable({
      if(is.null(loadData()) == TRUE){
        data.frame()
      }
      else{
        head(data(), 3)
      }
    })
  })
  observeEvent(input$weekshowhigh, {
    output$weekhighscore <- renderTable({
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
  output$Animal1 <- renderText({
    "body"
  })
  output$Animal2 = renderText({
    "brain"
  })
  output$Quake1 <- renderText({
    "magnitude"
  })
  output$Quake2 <- renderText({
    "distance"
  })
  output$Quake3 <- renderText({
    "Peak Acceleration"
  })
  output$World1 <- renderText({
    "gdp"
  })
  output$World2 <- renderText({
    "income"
  })
  output$World3 <- renderText({
    "literacy"
  })
  output$World4 <- renderText({
    "military"
  })
  #Make Next Button undisabled when full score
  observeEvent(input$submitA, {
    updateButton(session, "next2", disabled = F)
    updateButton(session, "submitA", disabled = T)
    #}
  })
  observeEvent(input$submitB, {
    updateButton(session, "next3", disabled = F)
    updateButton(session, "submitB", disabled = T)
  })
  #Top 3 finish give button to submit score
  observeEvent(input$submitC,{
    updateButton(session, "next4", disabled = F)
    updateButton(session, "submitC", disabled = T)
    output$finish <- renderUI({
      textInput("initials", "Input your name")
      bsButton("finish", "Finished!", style = "primary")
    })})
  observeEvent(input$submitC,{
    output$initials <- renderUI({
      textInput("initials", "Input your name", value = "")
    })})
  output$names <- renderText({
    df = data.frame(data())
    df2 = df[,1]
    if(input$initials %in% df2){
      updateButton(session, "finish", disabled = T)
      "Nickname unusable"
    }
    else{
      updateButton(session, "finish", disabled = F)
      "Nickname usable"
    }
  })
  observeEvent(input$finish, {
    output$badge <- renderUI({
      if(is.null(input$initials) == T){
        place = 0
      }
      else{
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
}


# App Call----
boastUtils::boastApp(ui = ui, server = server)
