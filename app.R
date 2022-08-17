library(dplyr)
library(shiny)
library(plotly)

newdf <- read.csv("newdf.csv")
df <- read.csv("df.csv")
statedf <- read.csv("statedf.csv")

# Define UI

bar <- fluidPage(
  titlePanel(title = "States' average population and jail population (age 15-64) in 1999-2018"), 
  sidebarLayout(
    sidebarPanel(
      h4("Chose the state to highlight"), 
      radioButtons(
        inputId = "barstate", 
        label = "Select a state", 
        choices = df$state
      )
    ), 
    mainPanel(
      plotlyOutput("bar_1"), 
      plotlyOutput("bar_2")
    )
  )
)

line <- fluidPage(
  titlePanel(title = "Race Incarceration Proportion Trends"), 
  sidebarLayout(
    sidebarPanel(h4("Choose the state and race"), 
                 selectInput(
                   inputId = "linestate", 
                   label = "Select a state", 
                   choices = df$state, 
                   selectize = FALSE
                 ), 
                 selectInput(
                   inputId = "linerace", 
                   label = "Select a race", 
                   choices = c("Asian American/Pacific Islander", "Black", "Latinx", "Native American", "White"), 
                   selectize = FALSE
                 )
                 ), 
    mainPanel(textOutput("line_state"), 
              textOutput("line_race"), 
              plotlyOutput("line_plot")
              )
  )
)

map <- fluidPage(
  titlePanel(title = "Race Incarceration Distribution on Map"), 
  sidebarLayout(
    sidebarPanel(h4("Select a race and view the distribution"), 
                 selectInput(
                   inputId = "mapyear", 
                   label = "Select a year", 
                   choices = c("1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"), 
                   selectize = FALSE
                 ), 
                 selectInput(
                   inputId = "maprace", 
                   label = "Select a race", 
                   choices = c("Asian American/Pacific Islander", "Black", "Latinx", "Native American", "White"), 
                   selectize = FALSE
                 )
                 ), 
    mainPanel(plotlyOutput("map_plot"))
  )
)

ui <- navbarPage("US Prison System Analysis", 
                 tabPanel(
                   "Introduction",
                   p("Racism in the United States has always been a hot topic of discussion. For this assignment, I focused on the populations of races and the trends of racial incarceration population of ten states of U.S. in between 1999 to 2018, to figure out how incarceration on races changed in U.S. during 1999 to 2018.")
                 ),
                 tabPanel(
                   "State Information", bar
                 ),
                 tabPanel(
                   "Line Plot", line
                 ), 
                 tabPanel(
                   "Race Incarceration Comparison on U.S. Map", map
                 )
)

# Define server logic 
server <- function(input, output) {
  
  #bar page outputs
  output$bar_1 <- renderPlotly({
    if(input$barstate == "CA"){
      bar1 <- plot_ly(
        df, x = ~state, y = ~meanpop_allyear_15to64, type = "bar", 
        marker = list(color = c("rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "WA"){
      bar1 <- plot_ly(
        df, x = ~state, y = ~meanpop_allyear_15to64, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
        )
    }else if(input$barstate == "AZ"){
      bar1 <- plot_ly(
        df, x = ~state, y = ~meanpop_allyear_15to64, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "NM"){
      bar1 <- plot_ly(
        df, x = ~state, y = ~meanpop_allyear_15to64, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "TX"){
      bar1 <- plot_ly(
        df, x = ~state, y = ~meanpop_allyear_15to64, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "FL"){
      bar1 <- plot_ly(
        df, x = ~state, y = ~meanpop_allyear_15to64, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "OH"){
      bar1 <- plot_ly(
        df, x = ~state, y = ~meanpop_allyear_15to64, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "PA"){
      bar1 <- plot_ly(
        df, x = ~state, y = ~meanpop_allyear_15to64, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "IL"){
      bar1 <- plot_ly(
        df, x = ~state, y = ~meanpop_allyear_15to64, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "NY"){
      bar1 <- plot_ly(
        df, x = ~state, y = ~meanpop_allyear_15to64, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)"))
      )
    }
    
    bar1 <- bar1 %>% layout(title = "average 15 to 64 year olds population between 1999 and 2018", 
                            xaxis = list(categoryorder = "total descending"), 
                            yaxis = list(title = "population count"))
    bar1
  })
  
  output$bar_2 <- renderPlotly({
    if(input$barstate == "CA"){
      bar2 <- plot_ly(
        df, x = ~state, y = ~jailpop_allyear, type = "bar", 
        marker = list(color = c("rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "WA"){
      bar2 <- plot_ly(
        df, x = ~state, y = ~jailpop_allyear, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "AZ"){
      bar2 <- plot_ly(
        df, x = ~state, y = ~jailpop_allyear, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "NM"){
      bar2 <- plot_ly(
        df, x = ~state, y = ~jailpop_allyear, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "TX"){
      bar2 <- plot_ly(
        df, x = ~state, y = ~jailpop_allyear, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "FL"){
      bar2 <- plot_ly(
        df, x = ~state, y = ~jailpop_allyear, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "OH"){
      bar2 <- plot_ly(
        df, x = ~state, y = ~jailpop_allyear, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "PA"){
      bar2 <- plot_ly(
        df, x = ~state, y = ~jailpop_allyear, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "IL"){
      bar2 <- plot_ly(
        df, x = ~state, y = ~jailpop_allyear, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)", "rgba(40,145,255,0.4)"))
      )
    }else if(input$barstate == "NY"){
      bar2 <- plot_ly(
        df, x = ~state, y = ~jailpop_allyear, type = "bar", 
        marker = list(color = c("rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,0.4)", "rgba(40,145,255,1)"))
      )
    }
    
    bar2 <- bar2 %>% layout(title = "average jail popilation between 1999 to 2018", 
                            xaxis = list(categoryorder = "total descending"), 
                            yaxis = list(title = "population count"))
    bar2
  })
  
  #line page outputs
  output$line_state <- renderText(input$linestate)
  output$line_race <- renderText(input$linerace)
  output$line_plot <- renderPlotly({
    year1999to2018 <- data.frame(year = c(1999:2018))
    chosendf <- filter(statedf, statename == input$linestate)
    if(input$linerace == "Asian American/Pacific Islander"){
      y = chosendf$mean_aapi_pop_15to64
    }else if(input$linerace == "Black"){
      y = chosendf$mean_black_pop_15to64
    }else if(input$linerace == "Latinx"){
      y = chosendf$mean_latinx_pop_15to64
    }else if(input$linerace == "Native American"){
      y = chosendf$mean_native_pop_15to64
    }else if(input$linerace == "White"){
      y = chosendf$mean_white_pop_15to64
    }
    lineplot <- plot_ly(chosendf, 
            x = ~year1999to2018$year, 
            y = ~y, 
            type = "scatter", 
            mode = "lines")
    lineplot <- lineplot %>% layout(title = "Jail popolation trending of races 1999-2018", 
                                    xaxis = list(title = "year"), 
                                    yaxis = list(title = "jail population"))
    lineplot
  })
  
  #map page output
  output$map_plot <- renderPlotly({
    
    chosenyeardf <- filter(statedf, year == input$mapyear)
    
    
    l <- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = "usa", 
      projection = list(type = "albers usa"), 
      showlakes = TRUE, 
      lakecolor = toRGB("white")
    )
    map <- plot_geo(
      chosenyeardf, locationmode = "USA-states"
    )
    
    if(input$maprace == "Asian American/Pacific Islander"){
      map <- map %>% add_trace(
        z = ~aapi_jail_prop, locations = ~statename, 
        color = ~aapi_jail_prop, colors = "Blues"
      )
    }else if(input$maprace == "Black"){
      map <- map %>% add_trace(
        z = ~black_jail_prop, locations = ~statename, 
        color = ~black_jail_prop, colors = "Blues"
      )
    }else if(input$maprace == "Latinx"){
      map <- map %>% add_trace(
        z = ~latinx_jail_prop, locations = ~statename, 
        color = ~latinx_jail_prop, colors = "Blues"
      )
    }else if(input$maprace == "Native American"){
      map <- map %>% add_trace(
        z = ~native_jail_prop, locations = ~statename, 
        color = ~native_jail_prop, colors = "Blues"
      )
    }else if(input$maprace == "White"){
      map <- map %>% add_trace(
        z = ~white_jail_prop, locations = ~statename, 
        color = ~white_jail_prop, colors = "Blues"
      )
    }
    
    map <- map %>% colorbar(title = "population proportion")
    map <- map %>% layout(
      title = "Jail popolation trending of races 1999-2018 on map", 
      geo = g
    )
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
