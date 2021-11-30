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
                )
              )
            ), 
            column(
              width = 8, 
              offset = 0,
              plotOutput("scatterPlot") 
            )
          ),
          fluidRow(
            column(
              width = 6,
              offset = 0,
              plotOutput("horizontalHist")
            ),
            column(
              width = 6,
              offset = 0,
              plotOutput("verticalHist")
            )
          )
        ),
        ### References page ----
        tabItem(
          tabName = "References", 
          h2("References"), 
          p( 
            class = "hangingindent", 
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny
            (v 0.61) [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ), 
          p(
            class = "hangingindent", 
            "Carey, R. and Hatfield, N. J. (2021). boastUtils: BOAST Utilities,
            (v 0.1.11.1) [R package]. Available from 
            https://github.com/EducationShinyAppTeam/boastUtils"
          ), 
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2021). shinydashboard: Create
            dashboards with 'Shiny', (v 0.7.2) [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ), 
          p( 
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny: 
            Web application framework for R, (v 1.7.1) [R package]. Available from
            https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Joyner, W. B., Boore, D. M., and Porcella, R. D. (1981). Peak
            Horizontal Acceleration and Velocity from Strong-Motion Records
            Including Records from the 1979 Imperial Valley California earthquake.
            [Data file]. USGS Open File report 81-365. Menlo Park, CA."
          ),
          p(
            class = "hangingindent",
            "Literacy, Gross Domestic Product, Income and Military Expenditures
            for 154 Countries. (n.d.) [Data set]. Available from
            https://www.stat.berkeley.edu/~s133/resources.html"
          ), 
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2021), shinyWidgets: Custom
            inputs widgets for shiny, (v 0.6.2) [R package]. Available from 
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
          ),
        p(
          class = "hangingindent",
          "Rousseeuw, P. J., and Leroy, A. M. (1987). Animals [Data set]. In
          W. N. Venables and B. D. Ripley's, MASS R Package (2002). Available
          from https://CRAN.R-project.org/package=MASS"
        ),
        p(
          class = "hangingindent",
          "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
          Springer-Verlag, New York."
        ),
        p(
          class = "hangingindent",
          "Wickham, H. and Seidel, D. (2020). scales: Scale functions for
          visualization, (v 1.1.1) [R package]. Available from
          https://CRAN.R-project.org/package=scales"
        ),
        br(),
        br(),
        br(),
        boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define the server ----
server <- function(input, output, session) {
  ## Per user elements ----
  dataSet <- reactiveVal(NULL)
  
  ## Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
    sendSweetAlert(
      session = session, 
      title = "Instructions", 
      text = "Pick a data set and view the effect of 
      the log transform on the X and or Y variables.", 
      type = "info"
    )
  })
  
  ## Go to Explore page ----
  observeEvent(
    eventExpr = input$go1, 
    handlerExpr = {
      updateTabItems(
        session = session, 
        inputId = "pages", 
        selected = "explore"
      )
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
      ### Logic check for switching data sets ----
      attributes <- c(input$horizVar, input$vertVar)
      validate(
        need(
          expr = all(attributes %in% names(dataSet())),
          message = "One moment please..."
        )
      )
      ### Make the main scatter plot ----
      mainPlot <- ggplot(
        data = dataSet(),
        mapping = aes_string(x = input$horizVar, y = input$vertVar)
      ) +
        geom_point(size = 2) +
        theme_bw() +
        labs(
          title = paste(
            paste0(toupper(substring(text = input$vertVar, first = 1, last = 1)),
                   substring(text = input$vertVar, first = 2)),
              "vs",
            paste0(toupper(substring(text = input$horizVar, first = 1, last = 1)),
                   substring(text = input$horizVar, first = 2))
          )
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
      ### Logic check for switching data sets ----
      validate(
        need(
          expr = all(input$horizVar %in% names(dataSet())),
          message = "One moment please..."
        )
      )
      ### Make the histogram for the attribute on horizontal axis ----
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
            title = paste(
              "Histogram of",
              paste0(toupper(substring(text = input$horizVar, first = 1, last = 1)),
                     substring(text = input$horizVar, first = 2))
            )
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
      ### Logic check for switching data sets ----
      validate(
        need(
          expr = all(input$vertVar %in% names(dataSet())),
          message = "One moment please..."
        )
      )
      ### Make the histogram for the attribute on vertical axis ----
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
            title = paste(
              "Histogram of",
              paste0(toupper(substring(text = input$vertVar, first = 1, last = 1)),
                     substring(text = input$vertVar, first = 2))
              )
          )
      }
      
      secondHist
    },
    alt = reactive(paste("A histogram of", isolate(input$vertVar), "from the", 
                         isolate(input$selectData), "data set."))
  )

}

# App Call----
boastUtils::boastApp(ui = ui, server = server)
