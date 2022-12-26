##### Shiny App for Exploring NFL Play-by-Play Runs Data #####

# Load required packages -------------------------------
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggtext)
library(nflverse)
library(shinydashboard)
library(forcats)
library(DT)

# Load data -------------------------------

play_by_play <- load_pbp(
  seasons = most_recent_season(),
  file_type = getOption("nflreadr.prefer", default = "rds")
)

play_by_play_runs <- play_by_play %>% filter(play_type=="run")

play_yardsgained <- play_by_play_runs %>% group_by(week) %>% arrange(desc(yards_gained), .by_group = TRUE)

Bad_runs <- play_yardsgained %>% filter(yards_gained<1)
Neutral_runs <- play_yardsgained %>% filter(between(yards_gained,1,4))
Good_runs <- play_yardsgained %>% filter(between(yards_gained,4,10))
Explosive_runs <- play_yardsgained %>% filter(yards_gained>10)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "NFL Runs dashboard by Ismael Isak"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Options", tabName = "CSF2", icon=icon("bar-chart")),
      selectInput("team", "Team", c("ALL", "ARI", "ATL" ,"BAL" ,"BUF" ,"CAR" ,"CHI",
                                        "CIN", "CLE", "DAL", "DEN", "DET" ,"GB" ,
                                        "HOU", "IND" ,"JAX", "KC" , "LA" , "LAC",
                                        "LV" , "MIA", "MIN", "NE" , "NO" , "NYG",
                                        "NYJ" ,"PHI", "PIT", "SEA", "SF" , "TB" ,
                                        "TEN", "WAS")),
                  selectInput("run", "Select Run Type", c("Bad (< 1 yard)", "Neutral (1-4 yards)", "Good (4-10 yards)" ,"Explosive (> 10 yards)"))
  )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidPage(
      box(plotOutput("plot1", height = 500), width = 7),
      
      box(
        title = "Choose the week",
        selectInput("week", "Week number:", c(1:15)),actionButton("show", "In case of error", width='100pt'), width=5
      ),
      box(
          titlePanel("Team/Play ID"),
        mainPanel(width = 8,
                  DT::dataTableOutput("mytable")), width=5)
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  
  

  
  output$plot1 <- renderPlot({

    if(input$run=="Bad (< 1 yard)"){
    
    if(input$team=="ALL"){
        p1 <- Bad_runs %>% filter(week==input$week) %>% group_by(posteam) %>% summarise(mean_yards_gained = mean(yards_gained))
        ggplot(p1, aes(x = fct_reorder(posteam, mean_yards_gained), y = mean_yards_gained)) +
          geom_col(aes(color = posteam, fill = posteam), width = 0.5) +
          scale_color_nfl(type = "secondary") +
          scale_fill_nfl(alpha = 0.4) +
          scale_x_nfl() +
          theme_minimal() +
          # theme_*_nfl requires gridtext version > 0.1.4
          theme_x_nfl()+
          xlab("Teams")+
          ylab("Yards Lost")} else {
            
            p1 <- Bad_runs %>% filter(posteam==input$team, week==input$week)
            p1 <- data.frame(posteam=input$team, yards_gained=p1$yards_gained, play_id=p1$play_id)
            ggplot(p1, aes(x=play_id,y=yards_gained)) +
              geom_point(size=3) +
              theme_minimal() +
              # theme_*_nfl requires gridtext version > 0.1.4
              theme(axis.text=element_blank()) +
              xlab("Play") +
              ylab("Yards Lost") +
              geom_segment(aes(x=play_id, 
                               xend=play_id, 
                               y=yards_gained, 
                               yend=1))
          }
    }  else if(input$run=="Neutral (1-4 yards)"){
      
      if(input$team=="ALL"){
        p1 <- Neutral_runs %>% filter(week==input$week) %>% group_by(posteam) %>% summarise(mean_yards_gained = mean(yards_gained))
        ggplot(p1, aes(x = fct_reorder(posteam, mean_yards_gained), y = mean_yards_gained)) +
          geom_col(aes(color = posteam, fill = posteam), width = 0.5) +
          scale_color_nfl(type = "secondary") +
          scale_fill_nfl(alpha = 0.4) +
          scale_x_nfl() +
          theme_minimal() +
          # theme_*_nfl requires gridtext version > 0.1.4
          theme_x_nfl()+
          xlab("Teams")+
          ylab("Yards Gained")} else {
            
            p1 <- Neutral_runs %>% filter(posteam==input$team, week==input$week)
            p1 <- data.frame(posteam=input$team, yards_gained=p1$yards_gained, play_id=p1$play_id)
            ggplot(p1, aes(x=play_id,y=yards_gained)) +
              geom_point(size=3) +
              theme_minimal() +
              # theme_*_nfl requires gridtext version > 0.1.4
              theme(axis.text=element_blank()) +
              xlab("Play") +
              ylab("Yards Gained") +
              geom_segment(aes(x=play_id, 
                               xend=play_id, 
                               y=1, 
                               yend=yards_gained))
          }
    } else if(input$run=="Good (4-10 yards)"){
      
      if(input$team=="ALL"){
        p1 <- Good_runs %>% filter(week==input$week) %>% group_by(posteam) %>% summarise(mean_yards_gained = mean(yards_gained))
        ggplot(p1, aes(x = fct_reorder(posteam, mean_yards_gained), y = mean_yards_gained)) +
          geom_col(aes(color = posteam, fill = posteam), width = 0.5) +
          scale_color_nfl(type = "secondary") +
          scale_fill_nfl(alpha = 0.4) +
          scale_x_nfl() +
          theme_minimal() +
          # theme_*_nfl requires gridtext version > 0.1.4
          theme_x_nfl()+
          xlab("Teams")+
          ylab("Yards Gained")} else {
            
            p1 <- Good_runs %>% filter(posteam==input$team, week==input$week)
            p1 <- data.frame(posteam=input$team, yards_gained=p1$yards_gained, play_id=p1$play_id)
            ggplot(p1, aes(x=play_id,y=yards_gained)) +
              geom_point(size=3) +
              theme_minimal() +
              # theme_*_nfl requires gridtext version > 0.1.4
              theme(axis.text=element_blank()) +
              xlab("Play") +
              ylab("Yards Gained") +
              geom_segment(aes(x=play_id, 
                               xend=play_id, 
                               y=4, 
                               yend=yards_gained))
          }
    } else {
      
      if(input$team=="ALL"){
        p1 <- Explosive_runs %>% filter(week==input$week) %>% group_by(posteam) %>% summarise(mean_yards_gained = mean(yards_gained))
        ggplot(p1, aes(x = fct_reorder(posteam, mean_yards_gained), y = mean_yards_gained)) +
          geom_col(aes(color = posteam, fill = posteam), width = 0.5) +
          scale_color_nfl(type = "secondary") +
          scale_fill_nfl(alpha = 0.4) +
          scale_x_nfl() +
          theme_minimal() +
          # theme_*_nfl requires gridtext version > 0.1.4
          theme_x_nfl()+
          xlab("Teams")+
          ylab("Yards Gained")} else {
            
            p1 <- Explosive_runs %>% filter(posteam==input$team, week==input$week)
            p1 <- data.frame(posteam=input$team, yards_gained=p1$yards_gained, play_id=p1$play_id)
            ggplot(p1, aes(x=play_id,y=yards_gained)) +
              geom_point(size=3) +
              theme_minimal() +
              # theme_*_nfl requires gridtext version > 0.1.4
              theme(axis.text=element_blank()) +
              xlab("Play") +
              ylab("Yards Gained") +
              geom_segment(aes(x=play_id, 
                               xend=play_id, 
                               y=1, 
                               yend=yards_gained))
          }
    }
     
        }
    
  )
  
  observeEvent(input$show, {
    showNotification("Avoid by-week for teams.")
  })
  
  ## Need to do a similar if else loop here for the table
  
  output$mytable <- DT::renderDataTable( {
    
    if(input$run=="Bad (< 1 yard)"){
      
      if(input$team=="ALL"){
        
        data_table <- Bad_runs %>% filter(week==input$week) %>% group_by(posteam) %>% summarise(mean_yards_gained = mean(yards_gained), 
                                                                                                n=n()) %>% arrange(mean_yards_gained, 
                                                                                                                   .by_group = TRUE)
        
        data_table <- data.frame(Team=data_table$posteam, 'Avg Yards Lost'=round(abs(data_table$mean_yards_gained), digits = 3), 
                                 'Number of plays'=data_table$n)
        
        data_table
      } else{
        
        data_table <- Bad_runs %>% filter(week==input$week, posteam==input$team) %>% group_by(play_id) %>% 
          summarise(yards_gained = yards_gained) %>% arrange(yards_gained, .by_group = TRUE)
        
        data_table <- data.frame('Play ID'=data_table$play_id, 'Avg Yards Lost'=round(abs(data_table$mean_yards_gained), digits = 3))
        
        data_table
      }
    } else if(input$run=="Neutral (1-4 yards)"){
      
     
        
        if(input$team=="ALL"){
          
          data_table <- Neutral_runs %>% filter(week==input$week) %>% group_by(posteam) %>% summarise(mean_yards_gained = mean(yards_gained),
        n=n()) %>% arrange(desc(mean_yards_gained), .by_group = TRUE)
          
          data_table <- data.frame(Team=data_table$posteam, 'Avg Yards Gained'=round(abs(data_table$mean_yards_gained), digits = 3),
        'Number of plays'=data_table$n)
          
          data_table
        } else{
          
          data_table <- Neutral_runs %>% filter(week==input$week, posteam==input$team) %>% group_by(play_id) %>% 
        summarise(yards_gained = yards_gained) %>% arrange(desc(yards_gained), .by_group = TRUE)
          
          data_table <- data.frame('Play ID'=data_table$play_id, 'Yards Gained'=round(abs(data_table$yards_gained), digits = 3))
          
          data_table
        }
    } else if(input$run=="Good (4-10 yards)"){
      
      if(input$team=="ALL"){
        
        data_table <- Good_runs %>% filter(week==input$week) %>% group_by(posteam) %>% summarise(mean_yards_gained = mean(yards_gained),
                                                                                                    n=n()) %>% arrange(desc(mean_yards_gained)
                                                                                                                       , .by_group = TRUE)
        
        data_table <- data.frame(Team=data_table$posteam, 'Avg Yards Gained'=round(abs(data_table$mean_yards_gained), digits = 3),
                                 'Number of plays'=data_table$n)
        
        data_table
      } else{
        
        data_table <- Good_runs %>% filter(week==input$week, posteam==input$team) %>% group_by(play_id) %>% 
          summarise(yards_gained = yards_gained) %>% arrange(desc(yards_gained), .by_group = TRUE)
        
        data_table <- data.frame('Play ID'=data_table$play_id, 'Yards Gained'=round(abs(data_table$yards_gained), digits = 3))
        
        data_table
      }
    } else if(input$run=="Explosive (> 10 yards)"){
      
      if(input$team=="ALL"){
        
        data_table <- Explosive_runs %>% filter(week==input$week) %>% group_by(posteam) %>% summarise(mean_yards_gained = mean(yards_gained),
                                                                                                 n=n()) %>% arrange(desc(mean_yards_gained)
                                                                                                                    , .by_group = TRUE)
        
        data_table <- data.frame(Team=data_table$posteam, 'Avg Yards Gained'=round(abs(data_table$mean_yards_gained), digits = 3),
                                 'Number of plays'=data_table$n)
        
        data_table
      } else{
        
        data_table <- Explosive_runs %>% filter(week==input$week, posteam==input$team) %>% group_by(play_id) %>% 
          summarise(yards_gained = yards_gained) %>% arrange(desc(yards_gained), .by_group = TRUE)
        
        data_table <- data.frame('Play ID'=data_table$play_id, 'Yards Gained'=round(abs(data_table$yards_gained), digits = 3))
        
        data_table
      }
      
    }
    
    
  },
                                         options = list(scrollX = TRUE),
                                         rownames = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
