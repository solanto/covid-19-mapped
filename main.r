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
    formattable,
    rlang
)

library(albersusa)

# ---- get data

yesterday <-
    (Sys.Date() - 1) %>%
    format("%m-%d-%Y")

latest_csse_upload <- function(base_url) {
    paste(
        base_url,
        yesterday,
        ".csv",
        sep = ""
    ) %>%
    read.csv()
}

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
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/" %>%
    latest_csse_upload()

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

# we don't need these anymore
rm(hospital_data, csse_data)

# ------ counties

county_data <-
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/" %>%
    latest_csse_upload() %>%
    filter(Country_Region == "US") %>%
    mutate(
        fips = formatC(
            FIPS,
            width = 5,
            flag = "0"
        ),
        deaths = Deaths,
        confirmed = Confirmed
    ) %>%
    select(
        fips,
        deaths,
        confirmed
    ) %>%
    left_join(
        counties_sf(),
        .,
        by = "fips"
    )

# ---- shiny setup

data_options <- c(
    "Deaths" = "deaths",
    "Confirmed cases" = "confirmed",
    "Hospitalizations" = "hospitalizations"
)

data_levels <- env(
    "State" = state_data,
    "County" = county_data
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
            inputId = "option_select",
            label = "yee",
            choices = names(data_options),
            selected = "Confirmed cases"
        ),
        selectInput(
            inputId = "level_select",
            label = "owo",
            choices = names(data_levels),
            selected = "State"
        ),
        leafletOutput("map")
    )
)

# map projection
epsg2163 <- leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "EPSG:2163",
    proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
    resolutions = 2 ^ (16:7)
)

get_map <- function(level, option) {
    data <- data_levels[[level]]

    displayed <-
        option %>%
        data_options[.] %>%
        as.character() %>%
        data[[.]]

    color_palette <- colorNumeric("OrRd", domain = displayed)
    
    return (
        data %>%
        leaflet(
            options = leafletOptions(
                crs = epsg2163,
                zoomControl = FALSE
                # TODO: https://stackoverflow.com/a/58082967
            )
        ) %>%
        addPolygons(
            popup = paste(
                data$name,
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
        get_map(
            level = input$level_select,
            option = input$option_select
        )
    })
}

shinyApp(ui = ui, server = server)
