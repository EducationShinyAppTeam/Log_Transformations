library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)

worlddata = read.csv('world2.csv')
worlddata = worlddata[,-c(1,2,7)]
worlddata = worlddata[-c(36),]
worlddata$income = as.numeric(worlddata$income)
quakedata = datasets::attenu
animaldata = MASS::Animals

colnames(attenu) <- c("event", "magnitude", "station", "distance", "Peak Acceleration")

## App Meta Data----------------------------------------------------------------
APP_TITLE <<- "Log_Transformation"
APP_DESCP  <<- paste(
  "This app provides the information when to use log transformations", 
  "to linearize data, how to analyze scenarios where a Log Transform", 
  "is needed and when it is not."
)
## End App Meta Data------------------------------------------------------------

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "green",  
    ### Create the app header
    dashboardHeader(
      titleWidth = 250, 
      title = "Log Transformations", 
      tags$li(class="dropdown", 
              actionLink("info", icon("info"), class="myClass")), 
      tags$li(
        class = "dropdown", 
        tags$a(target = "_blank", icon("comments"), 
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Log_Transformations"
        )
      ), 
      tags$li(class='dropdown', 
              tags$a(href="https://shinyapps.science.psu.edu/", 
                     icon('home', lib='font-awesome')))), 
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
    dashboardBody(
      tabItems(
      tabItem(tabName = "overview", 
              h1("Log Transformation"), 
              p("The goals of this app are to know when to use log 
              transformations to linearize data, how to analyze scenarios
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
              updated by Daehoon Gwak in November 2020.", 
                br(), 
                br(), 
                br(), 
                div(class = "updated", "Last Update: 12/10/2020 by DG")
              )
      ), 
    tabItem(tabName = "explore", 
            h2("Log Transformation Task"), 
              fluidRow(
                  column(
                    width = 3,
                    selectInput(
                      inputId = "inputs",
                      label = "Select Data Set",
                      choices = c('Animals', 'Earthquakes', "Countries")
                    ) 
                  ), 
                  column(
                    width = 9, 
                    conditionalPanel(
                      condition = "input.inputs == 'Animals'", 
                      h2(" Data Description"), 
                      p("This dataset describes the correlation between the 
                      brain weight(g) and body weight(kg) of different animals"
                      )
                    ), 
                    conditionalPanel(
                      condition = "input.inputs == 'Earthquakes'", 
                      h2(" Data Description"), 
                      p("This data set describes different earthquakes that 
                      occurred in the US using the magnitude, distance from 
                      where it was recorded and the ground acceleration of 
                      the earthquake"
                      )
                    ), 
                    conditionalPanel(
                      condition = "input.inputs == 'Countries'", 
                      h2(" Data Description"), 
                      p("This data set contains 154 countries and data about 
                      them such as GDP, income per capita, literacy rate, 
                      and money spent on military"
                      )
                    )
                  )
              ), 
              fluidRow(
                #sidelayout
                column(
                  width = 3, 
                  br(), 
                  wellPanel(
                    fluidRow(
                      conditionalPanel(
                        condition = "input.inputs == 'Animals'", 
                        selectInput(inputId = "Xanimal", 
                                    label = "Select Your X-Axis", 
                                    c('body', 'brain')), 
                        selectInput(inputId = "Yanimal", 
                                    label = "Select Your Y-Axis", 
                                    c('brain', 'body'))
                      ), 
                      conditionalPanel(
                        condition = "input.inputs == 'Countries'", 
                        selectInput(
                          inputId = "Xworld", 
                          label = "Select Your X-Axis", 
                          c('gdp', 'income', 'literacy', 'military')
                        ), 
                        selectInput(
                          inputId = "Yworld", 
                          label = "Select Your Y-Axis", 
                          c('income','gdp', 'literacy', 'military')
                        )
                      ), 
                      conditionalPanel(
                        condition = "input.inputs == 'Earthquakes'", 
                        selectInput(
                          inputId = "Xquake", 
                          label = "Select Your X-Axis", 
                          c("magnitude", 
                            "distance", 
                            "Peak Acceleration")
                        ), 
                        selectInput(
                          inputId = "Yquake", 
                          label = "Select Your Y-Axis", 
                          c("distance",
                            "magnitude",
                            "Peak Acceleration")
                        )
                      ), 
                      checkboxGroupInput(
                        inputId = "transforms", 
                        label = "Transform X or Y", 
                        c("Transform X", "Transform Y"), 
                        selected = NULL
                      ), 
                      bsPopover(
                        id = "transforms", 
                        title = "Transformation", 
                        "Decide whether you want to log transform the
                          X, Y, or both axes"
                      ), 
                      checkboxInput(
                        inputId = 'loghist1', 
                        label = 'Show Log: XValue Hist'
                      ), 
                      bsPopover(
                        id = "loghist1", 
                        title = "Log Transform of Histogram(X-Variable)",
                        "Check this box if you want to see the log
                          transformation of the X-axis in the Histogram"
                      ), 
                      checkboxInput(
                        inputId = 'loghist2', 
                        label = 'Show Log: YValue Hist'
                      ), 
                      bsPopover(
                        id = "loghist2",
                        title = "Log Transform of Histogram(Y-Variable)", 
                        "Check this box if you want to see the log
                          transformation of the Y-axis in the Histogram"
                      )
                    )
                  )
                ), 
                column(
                  width = 9, 
                  #main panel
                  fluidRow(
                    conditionalPanel(
                      condition = "input.inputs == 'Countries'", 
                      plotOutput(outputId = "worldPlot"), 
                      plotOutput(outputId = "worldBars"),
                      plotOutput(outputId = "worldBars2")
                    ), 
                    conditionalPanel(
                      condition = "input.inputs == 'Animals'", 
                      plotOutput(outputId = "animalPlot"), 
                      plotOutput(outputId = "animalBars"), 
                      plotOutput(outputId = "animalBars2")
                    ), 
                    conditionalPanel(
                      condition = "input.inputs == 'Earthquakes'", 
                      plotOutput(outputId = "quakePlot"), 
                      plotOutput(outputId = "quakeBar"), 
                      plotOutput(outputId = "quakeBar2")
                    )
                  ))
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
  # Adding in Data
 
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
  
  output$animalPlot <- 
    renderPlot({
      #If they don't check on checkbox
      
      if(length(input$transforms) == 0)
      {
        ggplot(
          data = animaldata,
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
            data = animaldata,
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
            data = animaldata,
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
          data = animaldata,
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
          data = animaldata,
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
          data = animaldata,
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
          data = animaldata,
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
          data = animaldata,
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
          data = worlddata,
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
            data = worlddata,
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
            data = worlddata,
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
          data = worlddata,
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
          data = worlddata,
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
          data = worlddata,
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
          data = worlddata,
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
          data = worlddata,
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
          data = quakedata,
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
            data = quakedata,
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
            data = quakedata,
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
          data = quakedata,
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
            data = quakedata,
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
            data = quakedata,
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
            data = quakedata,
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
            data = quakedata,
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
