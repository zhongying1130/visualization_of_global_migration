library(reshape)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(leaflet)
library(geosphere)
library(ggplot2)
library(plyr)
library(plotly)
library(parsetR)
library(networkD3)
library(circlize)
library(shiny)

library(shinydashboard)

dashboardPage(
  
  
  dashboardHeader(title = "Global Migration Flow"),
  dashboardSidebar(
    
    sidebarMenu(
      
      
  
      selectInput("country", label = h3("Select Country"), 
                  choices = coun_select, 
                  selected = "AU"),
      
      radioButtons("radio", label = h3("Choose Migration Type"),
                   choices = list("Emmigration" = "ISOCODE.d", "Immigration" = "ISOCODE.o"), 
                   selected = "ISOCODE.d"),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      radioButtons("year", label = h3("Select year"),
                   choices = list("2015" = 2015, "2010" = 2010, 
                                  "2005" = 2005, "2000" = 2000,
                                  "1995" = 1995, "1990" = 1990), 
                   selected = 2015),
    
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
  
               
      selectInput("destin_select", label = h3("Select Destination Region"), 
                           choices = c(
                             "Developed region: G7",
                             "Developed region: nonG7",
                             "Emerging region: BRIC",
                             "Emerging region: MIKT",
                             "Emerging region: G20",
                             "Developing region: Latin America & Caribbean",
                             "Developing region: Africa",
                             "Developing region: Other",
                             "Least developed region: Africa",
                             "Least developed region: Other"
                           ), 
                           selected = "Developed region: G7"),
               
        selectInput("origin_select", label = h3("Select Source Region"), 
                           choices = c(
                             "Developed region: G7",
                             "Developed region: nonG7",
                             "Emerging region: BRIC",
                             "Emerging region: MIKT",
                             "Emerging region: G20",
                             "Developing region: Latin America & Caribbean",
                             "Developing region: Africa",
                             "Developing region: Other",
                             "Least developed region: Africa",
                             "Least developed region: Other"
                           ),
                           selected = "Emerging region: BRIC"),
               
        actionButton(inputId = "update",
                            label = "Show Flow Chart")
               
               
               
        )
    ),
  
  dashboardBody(
    fluidRow(
      box( p ("The app may take several seconds to load, be patient"),
           width = 12)
    ),
    
    fluidRow(
      
      #box(title = "Flow map", status = "primary",  leafletOutput("map",width="150%",height="500px"))
      box(
        p("The map shows the average migration stock during 1995 to 2015, "),
        title = "Flow map",
        collapsible = TRUE,
        width = 9,
        height = "520px",
        leafletOutput("map")
      ),
    
      box(
        title = "Migration Trend for country", status = "primary", solidHeader = TRUE,
        width = 3,
        height = "520px",
        collapsible = TRUE,
        div(plotlyOutput('trendPlot'))
      )
    ),
    
    
    
    fluidRow(
      box(
        title = "Rank of top 20 destination or source country", solidHeader = TRUE,
        width = 8,
        div(plotOutput("bumpchart", height = "520px"))
        ),
        
      box(
        width = 4,
        p("The Instrution"),
        fluidRow(
          box(
            width = 12,
            p("For detailed instructions, please check the document attached to this project")
          )
        )
      )
      
    ),
    
    
    fluidRow(
     tabBox(
       "Tab box title",
       width = 12,
       tabPanel(
         "Inputs",
         status = "warning", solidHeader = TRUE,
         plotOutput("chord", height = "550px")
       ),
       tabPanel(
         width = 12,
         "Migration Flow between Countries",
         #title = "Migration Flow between Countries", 
         status = "primary", solidHeader = TRUE,
         collapsible = TRUE,
         parsetOutput("sankey", width = "100%", height = "500px")
       )
     )
      # A static infoBox
      # infoBox("New Orders", 10 * 2, icon = icon("credit-card"))
      
    )
    
    
    
  )
)
