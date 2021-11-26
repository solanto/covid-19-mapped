# if not already installed, call:
# install.packages("needs")
# needs(remotes)
# install_github("hrbrmstr/albersusa")

# 📦 packages 📦

library(needs)

needs(
    shiny,
    magrittr,
    plotly,
    jsonlite,
    dplyr,
    leaflet,
    tigris
)

library(albersusa)

# 📦 -------- 📦

# 🗃️ get data 🗃️

state_abbreviations <- setNames(state.name, state.abb)

hospital_stats <- read.csv("https://healthdata.gov/resource/g62h-syeh.csv") %>%
    mutate(state_full = as.character(state_abbreviations[state]))

test_stats <- read.csv("12-31-2020.csv")

# 🗃️ -------- 🗃️

# ✨ shiny setup ✨

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
            "data_select",
            "yee",
            c(
                "critical_staffing_shortage_today_yes",
                "deaths_covid",
                "inpatient_beds_used_covid"
            ),
            # c("Deaths", "Active"),
            selected = "critical_staffing_shortage_today_yes"
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

server <- function(input, output) {
    output$map <- renderLeaflet({
        usa_sf() %>%
            left_join(
                test_stats,
                by = c("name" = "Province_State")
            ) %>%
            leaflet(options = leafletOptions(
                crs = epsg2163,
                zoomControl = FALSE
                # TODO: https://stackoverflow.com/a/58082967
            )) %>%
            addPolygons(
                popup = ~as.character(Deaths),
                color = "#333",
                fillColor = ~colorNumeric("OrRd", domain = Deaths)(Deaths),
                opacity = 1,
                fillOpacity = 1,
                weight = 1
            )
    })
}

shinyApp(ui = ui, server = server)

# ✨ ----------- ✨
