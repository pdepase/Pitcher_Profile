library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(hrbrthemes)
library(flexdashboard)
library(shinydashboard)
library(feather)
library(formattable)

## LOAD IN NECESSARY DATAFRAMES ## 
Standards <- read_feather("Standards.feather")
autocomplete_list <- read_feather("autocomplete_list.feather")
pitch_names_list <- readRDS("pitch_names_list.rds")



## LOAD IN LISTS FOR QUERY SEARCH OPTIONS ##

balls_list <- c("Any",0,1,2,3)
strikes_list <- c("Any",0,1,2,"<2")
runners_list <- c("Any","Empty","1st Only"," 2nd Only", " 3rd Only", "1st and 2nd", "1st and 3rd", "2nd and 3rd", "Bases Loaded", "Scoring Pos" )
home_list <- c("Any","Home", "Away")
handed_list <- c("Any","Right", "Left")
locations <- c("Any","In", "Center", "Away", "Down", "Middle", "Up","Below Zone", "Above Zone","In (Ball)", "Away (Ball)", "In Zone", "Out of Zone", "Armside", "Gloveside")







events <- c("Any", "Single", "Double", "Triple", "Home Run", "Strikeout","Field Out", "Sac Fly", "Sac Bunt", "Error","Walk" )

events_values <- c("Any","single","double","triple","home_run","strikeout","field_out","sac_fly","sac_bunt","field_error","walk")

pitch_events <- c("Any","Ball","Called Strike","Swinging Strike","Foul Tip", "Foul"," Hit Into Play","HBP")

pitch_events_value <- c("Any","ball","called_strike","swinging_strike","foul","foul_tip","hit_into_play","hit_by_pitch")


BIPs <- c("Any","Groundball","Line Drive", "Flyball", "Pop Up")
BIP_value <- c("Any","ground_ball","line_drive","fly_ball", "popup")



## SPECIFIC CSS CODE FOR A CHECKBOX OPTION TO QUERY ##

controls <-
  list(h5("PA Results"),  tags$div(align = 'left', 
                                   class = 'multicol', 
                                   checkboxGroupInput(inputId  = 'PA_result', 
                                                      choiceNames = events,
                                                      choiceValues = events_values,
                                                      label = NULL,
                                                      selected = events_values,
                                                      inline   = FALSE))) 


controls2 <-
  list(h5("BIP Type"),checkboxGroupInput(inputId  = 'BIP_Type', 
                                         choiceNames = BIPs,
                                         choiceValues = BIP_value,
                                         label = NULL,
                                         selected = BIP_value,
                                         inline   = FALSE)) 


controls3 <-
  list(h5("Pitch Results"),checkboxGroupInput(inputId  = 'Pitch_results', 
                                              choiceNames = pitch_events,
                                              choiceValues = pitch_events_value,
                                              label = NULL,
                                              selected = pitch_events_value,
                                              inline   = FALSE)) 




tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 150px;
                                   -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 5;    /* Firefox */ 
                                   column-count: 5; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")))) 

##FULL UI STARTS HERE ##

ui <- navbarPage("Pitcher Profiles", id="tabs", theme = shinytheme("cerulean"),
                
                tabPanel("Overview",
                         
                         
                         
                         dashboardBody(
                           useShinyjs(),
                           
                           
                           
                           
                           fluidRow(h1( textOutput(outputId = "Name_and_Team"), align="center" )), 
                           
                           fluidRow(splitLayout(cellWidths = c("47.5%", "52.5%"),
                                                h3( textOutput(outputId = "Table1_Title"), align="center" ),
                                                h3( textOutput(outputId = "Plots_Title"), align="center" )
                           )), 
                           
                           
                           fluidRow(splitLayout(cellWidths = c("46.5%","1%", "17.5%","17.5%","17.5%"),
                                                tableOutput(outputId = "PC_table"),
                                                "",
                                                column( width = 12, list( gaugeOutput("Kgauge", height = "110px"),gaugeOutput("wOBAgauge",height = "150px"))),
                                                column( width = 12,list(  gaugeOutput("BBgauge", height = "110px"),gaugeOutput("xwOBAgauge", height = "150px"))),
                                                column( width = 12, list( gaugeOutput("HRgauge", height = "110px"),gaugeOutput("FBVgauge", height = "150px")))
                           )),
                           
                           
                           fluidRow(h3(textOutput(outputId = "Percentile_text") ), align="center"),
                           
                           
                           fluidRow(splitLayout(cellWidths = c("4%","24%","24%","24%","24%"),
                                                "",
                                                plotOutput(outputId = "P1plot", width="250px",height="300px"),
                                                plotOutput(outputId = "P2plot", width="250px",height="300px"),
                                                plotOutput(outputId = "P3plot", width="250px",height="300px"),
                                                plotOutput(outputId = "P4plot", width="250px",height="300px")
                           )),
                           
                           fluidRow(h3( textOutput(outputId = "Table2_Title"), align="center" )), 
                           
                           
                           fluidRow( splitLayout(cellWidths = c("0.5%","99.5%"),
                                                 "",
                                                 tableOutput(outputId = "PR_table")
                           ), align="center"),
                           
                           
                           fluidRow(h3( textOutput(outputId = "Table3_Title"), align="center" )), 
                           fluidRow( splitLayout(cellWidths = c("1%","98%","1%"),
                                                 "",
                                                 tableOutput(outputId = "Results_table"),
                                                 ""), align="center"),
                           
                           fluidRow(plotOutput(outputId = "BreakPlot", width="500px",height="500px"), align="center"),
                           plotOutput(outputId =  "USG_Lines")
                         )
                ),
                
                tabPanel("Distributions",
                         dashboardBody(
                           useShinyjs(),
                           fluidRow( h1("2023 Pitch Type Distributions" ),align="center"),
                           fluidRow(plotOutput("velo_dist"),align="center"),
                           fluidRow(plotOutput("VB_dist"),align="center"),
                           fluidRow(plotOutput("HB_dist"),align="center"),
                           fluidRow(plotOutput("RH_dist"),align="center"),
                           fluidRow(plotOutput("RS_dist"),align="center"),
                           fluidRow(plotOutput("EV_dist"),align="center"),
                           fluidRow(plotOutput("LA_dist"),align="center")
                           
                           
                           
                         )
                ),
                
                
                
                
                
                
                tabPanel("Trends",
                         dashboardBody(
                           useShinyjs(),
                           fluidRow( h1("Season Trends"),align="center"),
                           plotOutput(outputId = "Velo_Time"),
                           
                           plotOutput(outputId = "VB_Time"),
                           
                           plotOutput(outputId = "HB_Time"),
                           
                           
                           fluidRow( plotOutput(outputId = "ReleasePlot",width="500px",height="500px"), align='center')
                         )
                ),
                
                
                
                
                
                
                
                
                
                tabPanel("Zones",
                         dashboardBody(
                           useShinyjs(),
                           fluidRow(  h1("Results by Zone (Pitchers View)" ),align="center"),
                           fluidRow(h3( textOutput(outputId = "Pitch1_Zones_Title"), align="center" )),
                           
                           fluidRow(splitLayout(cellWidths = c("1%","24.75%", "24.75%","24.75%","24.75%"),
                                                "",
                                                plotOutput(outputId = "Pitch1_SwMiss", width="275px",height="275px"),
                                                plotOutput(outputId = "Pitch1_HardHit", width="275px",height="275px"),
                                                plotOutput(outputId = "Pitch1_wOBA", width="275px",height="275px"),
                                                plotOutput(outputId = "Pitch1_BA", width="275px",height="275px")
                                                
                                                
                           )),
                           
                           
                           fluidRow(h3( textOutput(outputId = "Pitch2_Zones_Title"), align="center" )),
                           
                           
                           fluidRow(splitLayout(cellWidths = c("1%","24.75%", "24.75%","24.75%","24.75%"),
                                                "",
                                                plotOutput(outputId = "Pitch2_SwMiss", width="275px",height="275px"),
                                                plotOutput(outputId = "Pitch2_HardHit", width="275px",height="275px"),
                                                plotOutput(outputId = "Pitch2_wOBA", width="275px",height="275px"),
                                                plotOutput(outputId = "Pitch2_BA", width="275px",height="275px")
                                                
                                                
                           )),
                           
                           
                           fluidRow(h3( textOutput(outputId = "Pitch3_Zones_Title"), align="center" )),
                           
                           fluidRow(splitLayout(cellWidths = c("1%","24.75%", "24.75%","24.75%","24.75%"),
                                                "",
                                                plotOutput(outputId = "Pitch3_SwMiss", width="275px",height="275px"),
                                                plotOutput(outputId = "Pitch3_HardHit", width="275px",height="275px"),
                                                plotOutput(outputId = "Pitch3_wOBA", width="275px",height="275px"),
                                                plotOutput(outputId = "Pitch3_BA", width="275px",height="275px")
                                                
                                                
                           )),
                           
                           
                           fluidRow(h3( textOutput(outputId = "Pitch4_Zones_Title"), align="center" )),
                           
                           
                           
                           fluidRow(splitLayout(cellWidths = c("1%","24.75%", "24.75%","24.75%","24.75%"),
                                                "",
                                                plotOutput(outputId = "Pitch4_SwMiss", width="275px",height="275px"),
                                                plotOutput(outputId = "Pitch4_HardHit", width="275px",height="275px"),
                                                plotOutput(outputId = "Pitch4_wOBA", width="275px",height="275px"),
                                                plotOutput(outputId = "Pitch4_BA", width="275px",height="275px")
                                                
                                                
                           )))),
                
                tabPanel("Queries", 
                         dashboardBody(
                           useShinyjs(),
                           fluidRow(  h1("Pitcher Query Tool" ),align="center"),
                           fluidRow(  h3("-------------------- Situation --------------------" ),align="center"),
                           fluidRow(splitLayout(cellWidths = c("9%", "6%","6%","6%","10%","7%","7%","20%","20%","9%"),
                                                "",
                                                selectizeInput(inputId = "balls", "Balls", options =list(dropdownParent = 'body'), choices= balls_list),
                                                selectizeInput(inputId = "strikes", "Strikes", options =list(dropdownParent = 'body'), strikes_list),
                                                selectizeInput(inputId = "outs", "Outs", options =list(dropdownParent = 'body'), strikes_list),
                                                selectizeInput(inputId = "runners", "Runners", options =list(dropdownParent = 'body'), runners_list),
                                                selectizeInput(inputId = "batter_hand", "R/L", options =list(dropdownParent = 'body'), handed_list),
                                                selectizeInput(inputId = "home", "H/A", options =list(dropdownParent = 'body'), home_list),
                                                dateInput('start_date',
                                                          label = 'Date input: yyyy-mm-dd',
                                                          value = "2023-03-31" ),
                                                dateInput('end_date',
                                                          label = 'Date input: yyyy-mm-dd',
                                                          value = Sys.Date() ),
                                                ""
                                                
                                                
                           )),
                           
                           fluidRow(  h3("-------------------- Pitch Type/Profile --------------------" ),align="center"),
                           fluidRow(splitLayout(cellWidths = c("1%","15%","1%","8%","8%","8%","8%","8%","8%","8%","8%","8%","8%","3%"),
                                                "",
                                                selectizeInput(inputId = "pitch_type", "Pitch Type", options =list(dropdownParent = 'body'), choices= pitch_names_list),
                                                "",
                                                numericInput(inputId = "VeloMin",label = "Min Velo",0,width = "75px"),
                                                numericInput(inputId = "VeloMax",label = "Max Velo",150,width = "75px"),
                                                numericInput(inputId = "VBMin",label = "Min VB",(-30),width = "75px"),
                                                numericInput(inputId = "VBMax",label = "Max VB",30,width = "75px"),
                                                numericInput(inputId = "HBMin",label = "Min HB",(-30),width = "75px"),
                                                numericInput(inputId = "HBMax",label = "Max HB",30,width = "75px"),
                                                numericInput(inputId = "RHMin",label = "Min RH",0,width = "75px"),
                                                numericInput(inputId = "RHMax",label = "Max RH",10,width = "75px"),
                                                numericInput(inputId = "RSMin",label = "Min RS",-10,width = "75px"),
                                                numericInput(inputId = "RSMax",label = "Max RS",91,width = "75px"),
                                                ""
                           )),
                           
                           
                           fluidRow(  h3("-------------------- Results --------------------" ),align="center"),
                           
                           fluidRow(tweaks, splitLayout(cellWidths = c("2%","15%","40%","14%","29%"),
                                                        "",
                                                        column(width = 12,controls3),
                                                        column(width = 12, controls),
                                                        column(width = 12,controls2),
                                                        selectizeInput(inputId = "pitch_location","Pitch Location", options =list(dropdownParent = 'body'),  choices=locations)
                                                        
                                                        
                                                        
                                                        
                           )),
                           fluidRow(h3(actionButton(inputId = "go", label = "GO")),align="center"),
                           
                           fluidRow(h2("-------------------- Query Results --------------------" ),align="center"),
                           
                           
                           fluidRow(splitLayout(cellWidths = c("0.25%","34%","65.75%"),
                                                "",
                                                plotOutput(outputId = "query_plot", width = "400px", height = "400px"),
                                                column( width = 12,
                                                        list( tableOutput(outputId = "query_table"),
                                                              tableOutput(outputId = "query_table2")))
                                                
                           ))
                           
                           
                           
                         )),
                
                tabPanel(selectizeInput( inputId = "Pitcher",NULL,autocomplete_list  )),
                
                tabPanel( actionButton(inputId = "search", label = "Search"))
                
                
                
)