library(shiny)
library(glue)

ui <- fluidPage( 
  textInput(inputId = 'card_id',label = 'I want to see the historical click rate of Card ID:', placeholder = 'Input card ID of interest'),
  textInput(inputId = 'start_date',label = 'Between', placeholder = 'Input observation start date'),
  textInput(inputId = 'end_date',label = 'and', placeholder = 'Input observation end date'),
  actionButton(inputId = 'run_query', label = 'Run Query'),
  textOutput('query_out')   
)

server <- shinyServer(function(input, output, session) {   
  card_id <- reactive({input$card_id})
  start_date <- reactive({input$start_date})
  end_date <- reactive({input$end_date})
  
  query <- 
  "
  WITH params AS (
  SELECT
  DATE('{start_date()}') AS start_date,
  DATE('{end_date()}') AS end_date
  )

  SELECT
  SUM(CASE WHEN event_name = ‘clicked_card’ THEN 1 ELSE 0 END) / COUNT(customer_id) AS historical_click_rate
  FROM `datalake.card_clicked_event`, params
  WHERE TRUE
  AND DATE(event_timestamp, ‘Asia/Jakarta’) BETWEEN start_date AND end_date
  AND card_id = ‘{card_id()}’  
  "
  
  query_glued <- reactive({
    glue(query)
    })
  
  output$query_out <- renderText({
    query_glued()
    })   
})

shinyApp(ui = ui, server = server)