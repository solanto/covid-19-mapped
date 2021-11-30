# if not already installed, call:
# install.packages("needs")
# needs(remotes)
# install_github("hrbrmstr/albersusa")

# ---- packages

library(needs)

needs(
    shiny,
    magrittr,
    plotly,
    jsonlite,
    dplyr,
    leaflet,
    tigris,
    formattable
)

library(albersusa)

# ---- get data

# ------ states

state_abbreviations <- setNames(state.name, state.abb)

hospital_data <-
    "https://healthdata.gov/resource/g62h-syeh.csv" %>%
    read.csv() %>%
    group_by(state) %>%
    summarize(
        hospitalizations = sum(inpatient_beds_used_covid, na.rm = TRUE)
    ) %>%
    mutate(state = as.character(state_abbreviations[state]))

csse_data <-
    (Sys.Date() - 1) %>% # yesterday
    format("%m-%d-%Y") %>%
    paste(
        "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",
        .,
        ".csv",
        sep = ""
    ) %>%
    read.csv()

state_data <-
    full_join(
        hospital_data,
        csse_data,
        by = c("state" = "Province_State")
    ) %>%
    mutate(
        name = state,
        deaths = Deaths,
        confirmed = Confirmed
    ) %>%
    select(
        name,
        deaths,
        confirmed,
        hospitalizations
    ) %>%
    left_join(
        usa_sf(),
        .,
        by = "name"
    )

rm(hospital_data, csse_data)

# ---- shiny setup

data_options <- c(
    "Deaths" = "deaths",
    "Confirmed cases" = "confirmed",
    "Hospitalizations" = "hospitalizations"
)

ui <- fluidPage(
    tags$head(
        tags$style("
            .leaflet-container {
                background-color: transparent;
            }
        ")
    ),
    mainPanel(
        selectInput(
            inputId = "data_select",
            label = "yee",
            choices = names(data_options),
            selected = names(data_options)[1]
        ),
        leafletOutput("map")
    )
)

epsg2163 <- leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "EPSG:2163",
    proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
    resolutions = 2 ^ (16:7)
)

get_map <- function(field) {
    displayed <- state_data[[field]]
    color_palette <- colorNumeric("OrRd", domain = displayed)
    
    return (
        state_data %>%
        leaflet(
            options = leafletOptions(
                crs = epsg2163,
                zoomControl = FALSE
                # TODO: https://stackoverflow.com/a/58082967
            )
        ) %>%
        addPolygons(
            popup = paste(
                state_data$name,
                displayed %>% comma(digits = 0) %>% as.character(),
                sep = ": "
            ),
            color = "#333",
            fillColor = color_palette(displayed),
            opacity = 1,
            fillOpacity = 1,
            weight = 1
        ) %>%
        addLegend(
            "bottomright",
            pal = color_palette,
            values = displayed,
            opacity = 1
        )
    )
}

server <- function(input, output) {
    output$map <- renderLeaflet({
        get_map(as.character(data_options[input$data_select]))
    })
}

shinyApp(ui = ui, server = server)
