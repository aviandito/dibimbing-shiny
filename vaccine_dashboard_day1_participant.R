# Shiny Vaccination Dashboard Class Day 1 v1
# Version: Participant
# Created by Aviandito & Calvin
# Last updated: May 2021

require(tidyverse)
require(plotly)
require(scales)
require(shiny)
require(shinydashboard)
source('data_vac.R')

# Run data prep function
world_vac <- world_vac_prep()

# Define UI layout
ui <- dashboardPage(
    dashboardHeader(title = "Vaccine Dashboard"),
    dashboardSidebar(),
    dashboardBody(
        # 1st row, should be the position of valueBoxes
        fluidRow(
            # Value box
            # TASK: 
            # 1. Display the correct type of output for valueBox, which is `valueBoxOutput` 
            # 2. Put the correct output IDs from the Server
            Output(outputId = , width = 4),
            Output(outputId = , width = 4),
            Output(outputId = , width = 4)
        ),
        # 2nd row, should be the position of Time series & maps
        # TASK: 
        # 1. Display the correct type of output for Plotly, which is `plotlyOutput`
        # 2. Put the correct output IDs from the Server
        fluidRow(
            width = 12,
            # Plot Time Series
            column(width = 6, Output(outputId = )),
            # Plot Map
            column(width = 6, Output(outputId = ))
        ),
        # 3rd row, should be the position of table
        # TASK: 
        # 1. Display the correct type of output for dataTable, which is `dataTableOutput`
        # 2. Put the correct output IDs from the Server
        fluidRow(
            # Table
            Output(outputId = )
        )
    )
)

server <- function(input, output, session) {
    # Functions to prepare value boxes numbers
    value_box_summary <- value_box_prep(world_vac)
    total_doses_given <- value_box_summary[1]
    total_fully_vaccinated <- value_box_summary[2]
    total_pct_vaccinated <- value_box_summary[3]
    
    # Value box for doses given
    # TASK: 
    # 1. Render the correct type of output for ValueBox, which is `renderValueBox`
    # 2. Put the correct value into it, which is `total_doses_given`
    output$doses_given <- render({
        valueBox(
            value = , "M"),
            subtitle = "Doses given"
        )
    })
    
    # Value box for total vaccinated
    # TASK: 
    # 1. Render the correct type of output for ValueBox, which is `renderValueBox` 
    # 2. Put the correct value into it, which is `total_fully_vaccinated`
    output$fully_vaccinated <- render({
        valueBox(value = , trim = TRUE), "M"),
                 subtitle = "Fully vaccinated"
        )
    })
    
    # Value box for pct population
    # TASK: 
    # 1. Render the correct type of output for ValueBox, which is `renderValueBox`
    # 2. Put the correct value into it, which is `total_pct_vaccinated`
    output$pct_pop <- render({
        valueBox(value = ,
                 subtitle = "% of population fully vaccinated"
        )
    })
    
    # Prepare time series data
    ts_df <- timeseries_prep(world_vac)
    
    # Plot Time Series
    # TASK: 
    # 1. Render the correct type of output for Plotly, which is `renderPlotly`
    # 2. Put the correct dataframe into it, which is `ts_df`
    output$plot_time <- render({
         %>% 
            plot_ly(x = ~ date) %>% 
            add_lines(y = ~ val, 
                      color = ~ var) %>%
            layout(hovermode = 'compare',
                   title = 'Number of vaccination by date')
    })
    
    # Prepare map viz data
    df_map_viz <- map_prep(world_vac)
    
    # Plot Map
    # TASK: 
    # 1. Render the correct type of output for Plotly, which is `renderPlotly` 
    # 2. Put the correct dataframe into it, which is `df_map_viz`
    output$plot_map <- render({
         %>%
            filter(pct_pop_fully_vaccinated <=1) %>%
            plot_ly(type = 'choropleth', showscale = F) %>%
            add_trace(z = ~pct_pop_fully_vaccinated,
                      color = ~pct_pop_fully_vaccinated,
                      text = ~location,
                      locations = ~iso_code,
                      zmin = 0, zmax = 0.05, span = I(0)) %>%
            layout(title = '% Population Fully Vaccinated by Country',
                   geo = list(showframe = FALSE))
    })
    
    # Function to prepare table
    world_vac_all <- table_prep(world_vac) 
    
    # Table output
    # TASK: 
    # 1. Render the correct type of output for DataTable, which is `renderDataTable`
    # 2. Put the correct dataframe into it, which is `world_vac_all`
    output$dynamic_table <- render(
        expr = , 
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