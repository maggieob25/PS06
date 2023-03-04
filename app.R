#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggpmisc)

salaries <- read.csv("Salaries.csv")
selected_team_name <- "SFN"
coloroptions <- c("hotpink", "firebrick3", "palegreen3", "darkorange1", "mediumpurple2", "lightblue2")

ui <- fluidPage(
  titlePanel("Baseball Salaries"),
  h3("Maggie O'Brien - INFO 201"),
  tabsetPanel(
    tabPanel("Summary",
             p("This app uses data from", tags$b("Kaggle,"), "reporting the 
               salaries of some (not all) Major League Baseball players.  
               Several teams have missing data at various years due to either 
               lack of data collection or the teams disbanding.  For example,
               the Montreal Expos had their last season in 2004, and therefore 
               data collection ends in 2004 for this team code. \n"),
             p("This data has 25576 observations and 5 different variables. The 
               box to the right demonstrates a small (random) data sample."),
             p(tags$em("Salary"), "is displayed in $ USD and", tags$em("playerID"),
               "represents the name of a player, which can be deduced with a 
               Google search."),
      mainPanel(
        verbatimTextOutput("summary"),
        actionButton("trendline", "Display graph"), 
        p("To see graph without trendline, click above button twice."), 
        plotOutput("plot2")
      ),
      sidebarPanel(
        img(alt = "MLB Teams",
            src = "https://upload.wikimedia.org/wikipedia/commons/a/a6/Major_League_Baseball_logo.svg"),
        p("Please reference team codes here:"),
        tags$a("MLB Team Codes 2015",
            href = 'legacy.baseballprospectus.com/sortable/extras/team_codes.php?this_year=2015')
      )
    ),
    tabPanel("Plot",
             plotOutput("plot"),
      sidebarPanel(
        radioButtons(inputId = "color",
                     label = "Select a color",
                     choices = coloroptions,
                     selected = coloroptions[1]),
        sliderInput("Year", "Select a year", min = min(salaries$yearID), 
                    max = max(salaries$yearID), value = min(salaries$yearID),
                    step = 1),
        radioButtons(inputId = "team", label = "Select a team", 
                     choices = unique(salaries$teamID),
                     selected = selected_team_name)
        ),
      h3("Test the widgets to see who the most paid MLB player per year or per 
         team was!  Default setting is SF Giants (biased) and 1985. \n"),  
      textOutput(outputId = "output"),
      textOutput(outputId = "output2")
      ),
    tabPanel("Table", 
             sidebarPanel(
               p("This panel will produce a table dependent on which
                 factor is selected.  Salaries are ranked from highest to lowest.
                 \n"),
               selectInput(inputId = "average", label = "Average salary per", 
                           choices = c("team", "year", "league"),
                           selectize = FALSE)
             ),
             tableOutput("data_table"),
             )
  )
  )

server <- function(input, output) {
  selected_year <- reactive({
    salaries %>%
      filter(yearID %in% input$Year,
             teamID %in% input$team) %>%
      mutate(salary_nearest = round(salary/100000, 0)*100000) %>%
      group_by(salary_nearest) %>%
      summarize(count = n())
  })
  
  output$plot <- renderPlot( {
    selected_year() %>%
      ggplot(aes(x=salary_nearest, y=count)) + 
      geom_bar(stat = "identity", fill=input$color) +
      labs(x = "Salary ($ USD)",
           y = "Count",
           title = "MLB Baseball Player Salaries Over Time") 
  })
  
  trendlineadded <- FALSE
  observeEvent(input$trendline, {
    if(trendlineadded) {
      output$plot2 <- renderPlot( {
        salaries %>%
          group_by(yearID) %>%
          summarize(mean = mean(salary)) %>%
          ggplot(aes(x=yearID, y=mean)) + 
          geom_point() +
          labs(x = "Year",
               y = "Mean Salary ($ USD)",
               title = "Mean Salary from 1985-2015") 
      })
      trendlineadded <<- FALSE
    } else {
      output$plot2 <- renderPlot( {
        salaries %>%
          group_by(yearID) %>%
          summarize(mean = mean(salary)) %>%
          ggplot(aes(x=yearID, y=mean)) + 
          geom_point() +
          labs(x = "Year",
               y = "Mean Salary ($ USD)",
               title = "Mean Salary from 1985-2015")+
          geom_smooth(method = "lm")
      }) 
      trendlineadded <<- TRUE
    }
  })
  
  output$output <- renderText ({
    filtered_data <- salaries %>%
      filter(yearID %in% input$Year) %>%
      arrange(desc(salary))
    
    top_players <- head(filtered_data$playerID, n = 5)
    player_names <- paste(top_players, collapse = ", ")
    
    paste("The most paid players in", input$Year, "were", player_names, "\n")
  })  
    
  output$output2 <- renderText ({
    
    filtered_data2 <- salaries %>%
      filter(teamID %in% input$team) %>%
      arrange(desc(salary))
    
    top_players_2 <- head(filtered_data2$playerID, n = 5)
    player_names_2 <- paste(top_players_2, collapse = ", ")
    
    paste("The all-time most paid players for", input$team, "were", player_names_2, "\n")
  })
  
  output$summary <- renderPrint({ 
    sample <- sample_n(salaries, 5, replace = FALSE)
    print(sample)
    })
  
  
  output$data_table <- renderTable({
    average_by <- switch(input$average,
                         "team" = "teamID",
                         "year" = "yearID",
                         "league" = "lgID")
    avg_salaries <- salaries %>%
      group_by(.data[[average_by]]) %>%
      summarise(avg_salary = mean(salary)) %>%
      arrange(rank(desc(avg_salary)))
    })
}


shinyApp(ui = ui, server = server)