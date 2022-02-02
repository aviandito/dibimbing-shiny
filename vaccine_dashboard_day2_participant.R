# Shiny Vaccination Dashboard Class Day 2 v2
# Version: Class Instructor 
# Created by Mohammad Aviandito & Calvin Sibarani
# Last updated: May 2021

require(tidyverse)
require(plotly)
require(scales)
require(shiny)
require(shinydashboard)
source('data_vac.R')

# Run data prep function, and region prep function for map region zooming
world_vac <- world_vac_prep()
region <- region_prep()

# Define UI layout
ui <- dashboardPage(
    dashboardHeader(title = "Vaccine Dashboard"),
    # Sidebar to put all the UI elements
    dashboardSidebar(
        # Input for Country selection
        # TASK: 
        # 1. Use the correct UI for selection dropdown, which is the `selectInput`
        # 2. Put a descriptive label for the input, e.g. "Select country:"
        # 3. (No coding needed) Understand how we put possible choices into a `selectInput`, 
        # which is by providing a list of possible selections
        # 4. (No coding needed) Understand how we put default choice for `selectInput`, 
        # which is by using `selected`
        Input(
            inputId = "country_input",
            label = "",
            choices = c("Global", unique(world_vac$location)),
            selected = "Global"
            ),
        # Input for date range selection
        # TASK:
        # 1. Use the correct UI for selection dropdown, which is the `dateRangeInput`
        # 2. Put a descriptive label for the input, e.g. "Select Date:"
        # 3. (No coding needed) Understand about `start` and `end`, which is the default view
        # 4. (No coding needed) Understand about `min` and `max`, which is the min/max date
        # that can be inputted by the user
        Input(
           inputId = "date_range_input", 
           label = "",
           start = min(world_vac$date),
           end = max(world_vac$date),
           min = min(world_vac$date),
           max = max(world_vac$date)
           ),
        # Input for button to reset selection
        # TASK:
        # 1. Use the correct UI for an action button, which is `actionButton`
        # 2. Put a descriptive label for the button, e.g. "Reset Input"
        Button(inputId = "reset_input", label = "")
    ),
    dashboardBody(
        # 1st row, should be the position of valueBoxes
        fluidRow(
            # Value box
            valueBoxOutput(outputId = "doses_given", width = 4),
            valueBoxOutput(outputId = "fully_vaccinated", width = 4),
            valueBoxOutput(outputId = "pct_pop", width = 4)
        ),
        # 2nd row, should be the position of Time series & maps
        fluidRow(
            width = 12,
            # Plot Time Series
            column(width = 6, plotlyOutput("plot_time")),
            # Plot Map
            column(width = 6, plotlyOutput("plot_map"))
        ),
        # 3rd row, should be the position of table
        fluidRow(
            # Table
            dataTableOutput(outputId = "dynamic_table")
        )
    )
)

server <- function(input, output, session) {
    # Reactive object of the selected country from input
    # TASK:
    # 1. Use the correct input from our UI, which is the `input$country_input`
    # 2. selected_country() should be a reactive object. Add `reactive` before the brackets
    selected_country <- ({
        input$
    })
    
    # Reactive object of the selected start date from input
    # TASK:
    # 1. Use the correct input from our UI, which is the `input$date_range_input`
    # The first element of it (`input$date_range_input[1]`) is the start date
    # 2. start_date() should be a reactive object. Add `reactive` before the brackets
    start_date <- ({
        as.character(input$[])
    })
    
    # Reactive object of the selected end date from input
    # TASK:
    # 1. Use the correct input from our UI, which is the `input$date_range_input`
    # The second element of it (`input$date_range_input[2]`) is the end date.
    # 2. end_date() should be a reactive object. Add `reactive` before the brackets
    end_date <- ({
        as.character(input$[])
    })
    
    # Use observeEvent to observe for button pressed. Reset input if the button is pressed
    # TASK:
    # 1. Observe the correct input, which is our action button, labeled by `input$reset_input`
    # 2. Update our country input to default selection, which is "Global"
    # 3. Update our date range input to default start and end date, which are `min(world_vac$date)`
    # and `max(world_vac$date)` respectively
    observeEvent(input$, {
        updateSelectInput(session = session, inputId = "country_input", selected = "")
        updateDateRangeInput(session = session, inputId = "date_range_input", 
                             start = ,
                             end = )
    })
    
    # Condition to filter dataset based on input. Make the dataset `world_vac_filtered` as a reactive object
    # TASK:
    # selected_country(), start_date(), and end_date() are reactive objects that we defined earlier. 
    # In below case, the variables won't work since it is missing the bracket `()`. 
    # Add it to the end of the variable names.
    world_vac_filtered <- reactive({
        # Only filter for date if selected country is 'Global'
        if(selected_country == 'Global'){
            world_vac %>%
                filter(date >= start_date & date <= end_date)
        # Else do country filtering
        } else {
            world_vac %>%
                filter(location == selected_country,
                       date >= start_date & date <= end_date)
        } 
    })
    
    # Functions to prepare reactive value boxes numbers
    value_box_summary <- reactive({
        value_box_prep(world_vac_filtered())
    })
    
    total_doses_given <- reactive({
        value_box_summary()[1]
    })

    total_fully_vaccinated <- reactive({
        value_box_summary()[2]
    })

    total_pct_vaccinated <- reactive({
        value_box_summary()[3]
    })
    
    # Value box for doses given. Use the reactive object `total_doses_given()`
    output$doses_given <- renderValueBox({
        valueBox(
            value = paste(format(round(total_doses_given() / 1e6, 1), trim = TRUE), "M"),
            subtitle = "Doses given"
        )
    })
    
    # Value box for total vaccinated. Use the reactive object `total_fully_vaccinated()`
    output$fully_vaccinated <- renderValueBox({
        valueBox(value = paste(format(round(total_fully_vaccinated() / 1e6, 1), trim = TRUE), "M"),
                 subtitle = "Fully vaccinated"
        )
    })

    # Value box for pct population. Use the reactive object `total_pct_vaccinated()`
    output$pct_pop <- renderValueBox({
        valueBox(value = round(total_pct_vaccinated(), 4) * 100,
                 subtitle = "% of population fully vaccinated"
        )
    })

    # Prepare time series data. Make ts_df as a reactive object
    ts_df <- reactive({
        timeseries_prep(world_vac_filtered())
    })

    # Plot Time Series. Use the reactive ts_df() as the data source
    output$plot_time <- renderPlotly({
        ts_df() %>%
            plot_ly(x = ~ date) %>%
            add_lines(y = ~ val,
                      color = ~ var) %>%
            layout(hovermode = 'compare',
                   title = 'Number of vaccination by date')
    })

    # Prepare map viz data on unfiltered data
    df_map_viz <- map_prep(world_vac)
    
    # Prepare map viz center coordinate when country is changed. Make map_scope as a reactive object
    # TASK:
    # 1. `selected_country()` is a reactive object that we defined earlier. 
    # In below case, the variables won't work since it is missing the bracket `()`. 
    # Add it to the end of the variable names.
    # 2. `map_scope()` should be a reactive object. Add `reactive` before the brackets
    map_scope <- ({
        region_selected <- region %>%
            filter(location == selected_country)
    
        region_selected$scope
    })
    
    # Geo setting for region zooming based on the country selected. Make g as a reactive object
    # TASK:
    # 1. `selected_country()` and `map_scope()` are reactive objects that we defined earlier. 
    # In below case, the variables won't work since it is missing the bracket `()`. 
    # Add it to the end of the variable names.
    # 2. `g()` should be a reactive object. Add `reactive` before the brackets
    g <- ({
        # use 'world' scope if selected country is 'Global'
        if(selected_country == 'Global'){
            list(scope = 'world',
                 showframe = FALSE)
        # else use selected map_scope as scope
        } else {
            list(scope = map_scope,
                 showframe = FALSE)
        }
    })

    # Plot Map. Use reactive g() as geo
    # TASK:
    # g() is a reactive objects that we defined earlier. 
    # In below case, the variables won't work since it is missing the bracket `()`. 
    output$plot_map <- renderPlotly({
        df_map_viz %>%
            filter(pct_pop_fully_vaccinated <=1) %>%
            plot_ly(type = 'choropleth', showscale = F) %>%
            add_trace(z = ~pct_pop_fully_vaccinated,
                      color = ~pct_pop_fully_vaccinated,
                      text = ~location,
                      locations = ~iso_code,
                      zmin = 0, zmax = 0.05) %>%
            layout(title = '% Population Fully Vaccinated by Country',
                   geo = g)
    })

    # Function to prepare table. Prepare `world_vac_all` as a reactive object
    world_vac_all <- reactive({
        table_prep(world_vac_filtered())
    })

    # Table output. Use reactive `world_vac_all()` as table input
    # TASK:
    # world_vac_all() is a reactive objects that we defined earlier. 
    # In below case, the variables won't work since it is missing the bracket `()`.
    output$dynamic_table <- renderDataTable(
        expr = world_vac_all,
        options = list(
            pageLength = 5, # default showing 5 rows
            autoWidth = FALSE # auto fit the width of the table
        )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

# MIT License
# 
# Copyright (c) 2021 Mohammad Aviandito & Calvin Sibarani
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#     
#     The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.