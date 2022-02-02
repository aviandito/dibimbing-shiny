# Shiny Vaccination Dashboard Class Day 1 v1
# Version: Class Instructor 
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
            valueBoxOutput(outputId = "doses_given", width = 4),
            valueBoxOutput(outputId = "fully_vaccinated", width = 4),
            valueBoxOutput(outputId = "pct_pop", width = 4)
        ),
        # 2nd row, should be the position of Time series & maps
        fluidRow(
            width = 12,
            # Plot Time Series
            column(width = 6, plotlyOutput(outputId = "plot_time")),
            # Plot Map
            column(width = 6, plotlyOutput(outputId = "plot_map"))
        ),
        # 3rd row, should be the position of table
        fluidRow(
            # Table
            dataTableOutput(outputId = "dynamic_table")
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
    output$doses_given <- renderValueBox({
        valueBox(
            value = paste(format(round(total_doses_given / 1e6, 1), trim = TRUE), "M"),
            subtitle = "Doses given"
        )
    })
    
    # Value box for total vaccinated
    output$fully_vaccinated <- renderValueBox({
        valueBox(value = paste(format(round(total_fully_vaccinated / 1e6, 1), trim = TRUE), "M"),
                 subtitle = "Fully vaccinated"
        )
    })
    
    # Value box for pct population
    output$pct_pop <- renderValueBox({
        valueBox(value = round(total_pct_vaccinated, 4) * 100,
                 subtitle = "% of population fully vaccinated"
        )
    })
    
    # Prepare time series data
    ts_df <- timeseries_prep(world_vac)

    # Plot Time Series
    output$plot_time <- renderPlotly({
        ts_df %>%
            plot_ly(x = ~ date) %>%
            add_lines(y = ~ val,
                      color = ~ var) %>%
            layout(hovermode = 'compare',
                   title = 'Number of vaccination by date')
    })

    # Prepare map viz data
    df_map_viz <- map_prep(world_vac)

    # Plot Map
    output$plot_map <- renderPlotly({
        df_map_viz %>%
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
