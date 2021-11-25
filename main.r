# use install.packages("needs") if not already installed
library(needs)

# packages
needs(
    shiny,
    magrittr,
    plotly,
    jsonlite,
    dplyr
)
 
# get data
state_abbreviations <- setNames(state.name, state.abb)

hospital_stats <- read.csv("https://healthdata.gov/resource/g62h-syeh.csv") %>%
    mutate(state_full = as.character(state_abbreviations[state]))

states <- fromJSON("state-boundaries.geojson")

map_base <- plot_ly(hospital_stats) %>%
    layout(
        geo = list(
            scope = "usa",
            bgcolor = "transparent"
        ),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
    )

generate_map <- function(data_option) {
    map_base %>%
    add_trace(
        type = "choropleth",
        geojson = states,
        # locationmode = "USA-states",
        locationmode = "geojson-id",
        featureidkey = "properties.NAME",
        locations = ~state_full,
        z = hospital_stats[[data_option]]
    )
}

test <- hospital_stats %>%
    group_by(state_full) %>%
    summarize(
        covid_inpatients = sum(inpatient_beds_used_covid, na.rm = TRUE)
    ) %>%
    plot_ly() %>%
    layout(
        geo = list(
            scope = "usa",
            bgcolor = "transparent"
        ),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
    ) %>%
    add_trace(
        type = "choropleth",
        geojson = states,
        # locationmode = "USA-states",
        locationmode = "geojson-id",
        featureidkey = "properties.NAME",
        locations = ~state_full,
        z = ~covid_inpatients
    )

ui <- fluidPage(
    mainPanel(
        selectInput(
            "data_select",
            "yee",
            c("deaths_covid", "inpatient_beds_used_covid"),
            selected = "deaths_covid"
        ),
        plotlyOutput(outputId = "map")
    )
)

server <- function(input, output) {
    output$map <- renderPlotly({generate_map(input$data_select)})
}

shinyApp(ui = ui, server = server)
