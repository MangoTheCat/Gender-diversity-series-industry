library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

load("myData.RData")

# Define UI for gender diversity app ----
ui <- fluidPage(
  
  # App title ----
  #titlePanel("Gender diversity: film industry VS series industries"),
  
  fluidRow(
    
    column(2,
           wellPanel(
             # Input: Selector for variable to plot ----
             selectInput("variable", "Year", 
                         c("2017", "2016", "2015", "2014", 
                           "2013", "2012", "2011", "2010",
                           "2009", "2008", "2007"))
           )
    ),
    
    
    column(9,
           # Output: Plot of the requested variable against mpg ----
           plotOutput("plot")
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    percentages_movies_long$industry <- rep("Film industry", 88)
    percentages_long$industry <- rep("Series industry", 96)
    
    # Combine these 2 long dataframes
    percentages_movies_series <- bind_rows(percentages_long, percentages_movies_long)
    
    # Filter with year=2017
    percentages_movies_series_year <- percentages_movies_series %>%
      filter(year == input$variable)
    
    # Ploting our barplot 2017
    percentages_movies_series_year$percentage <- as.numeric(format(percentages_movies_series_year$percentage, 
                                                                   digits = 2))
    
    ggplot(percentages_movies_series_year, aes(x = category,
                                               y = percentage,
                                               group = category,
                                               fill = category)) +
      geom_bar(stat = "identity") +
      facet_wrap(~industry) +
      coord_flip() + # Horizontal bar plot
      geom_text(aes(label = percentage), hjust=-0.1, size=3) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.y=element_blank(),
            axis.title.x=element_text(size = 15),
            strip.text.x = element_text(size = 15, face="bold"),
            plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
            legend.title=element_blank(), # center the title
            legend.text=element_text(size=15)) + 
      labs(title = paste("Percentages of women in", input$variable),
           x = "",
           y = "Percentages") +
      guides(fill = guide_legend(reverse=TRUE)) + # reverse the order of the legend
      scale_fill_manual(values = brewer.pal(8, "Spectral")) # palette used to fill the bars and legend boxs
  },
  height = 400, width = 650)
}

shinyApp(ui, server)
