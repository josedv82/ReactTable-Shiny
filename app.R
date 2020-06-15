
#Jose Fernandez
#ReactTable Concept
###############################################

#import libraries
library(reactable)

#import data (not attached to example)
datos <- read.csv("SO.csv")


#function to embbed bar chart within chart
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

#function to create color grades for background
orange_pal <- function(x) rgb(colorRamp(c("#6c7b5b","#f8b195","#f67280"))(x), maxColorValue = 255)

#server logic
server <- function(input, output) {
  
  #Table
  output$table <- renderReactable({
    
    reactable(datos,

              pagination = FALSE,
              highlight = TRUE,
              showSortIcon = FALSE,
              borderless = TRUE,
              filterable = TRUE,
              groupBy = c("Team"),
              defaultSorted = c("Team", "Minutes", "Player"),
              
              
              columns = list(
                
                
                Team = colDef(html = T,
                              style = list(fontFamily = "monospace", whiteSpace = "pre", fontWeight = "bold"),
                              class = "image",
                              maxWidth = 150,
                              header = JS("function(colInfo) {
                                           return colInfo.column.name + '<div style=\"color: #999\">Season 2018-19</div>'}")
                              ),
                
                Player = colDef(html = T,
                                style = list(fontFamily = "monospace", whiteSpace = "pre", fontWeight = "bold"),
                                align = "center",
                                class = "imageText",
                                maxWidth = 200),
                
                Games = colDef(format = colFormat(digits = 0), 
                               defaultSortOrder = "desc",
                               maxWidth = 80,
                               align = "center",
                               class = "row1",
                               style = function(value) {
                                 normalized <- (value - min(datos$Games)) / (max(datos$Games) - min(datos$Games))
                                 color <- orange_pal(normalized)
                                 list(fontFamily = "monospace", whiteSpace = "pre", background = color, color = "black")}
                               ),
                
                Minutes = colDef(format = colFormat(digits = 1), 
                                 defaultSortOrder = "desc",
                                 cell = function(value) {
                                   width <- paste0(value * 100 / max(datos$Minutes), "%")
                                   value <- format(value, big.mark = ",")
                                   # Fix each label using the width of the widest number (incl. thousands separators)
                                   value <- format(value, width = 6, justify = "right")
                                   bar_chart(value, width = width, fill = "#f67280")
                                 },
                                 maxWidth = 250,
                                 align = "left",
                                 class = "row2",
                                 style = list(fontFamily = "monospace", whiteSpace = "pre"))
                
              )
    )
    
  })
  
}

#User Interface
ui <- fluidPage(
  
  
  #specs for different columns
  tags$head(tags$style(".row1{font-weight: bold; padding-top: 30px; width: 60px; height: 60px; border-radius: 10%; color: #000; font-size: 14px;}"))
  tags$head(tags$style(".row2{font-weight: bold; padding-top: 30px;}")),
  tags$head(tags$style(".image{font-weight: bold; padding-top: 0px; padding-bottom: 0px}")),
  tags$head(tags$style(".imageText{font-weight: bold; padding-top: 0px; padding-bottom: 0px}")),
  
  reactableOutput("table")
  
  
)


shinyApp(ui = ui, server = server)

