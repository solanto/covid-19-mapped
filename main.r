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
    formattable,
    sass
)

library(albersusa)

# ---- build styles

public_dir <- "dist"

dir.create(public_dir, showWarnings = FALSE)

styles_path <-
    paste(
        public_dir,
        "/styles.css",
        sep = ""
    )

sass(
    sass_file("styles.scss"),
    output = styles_path
)

addResourcePath(
    prefix = public_dir,
    directoryPath = public_dir
)

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

data_options <- c(
    "deaths due to COVID-19" = "deaths",
    "confirmed COVID-19 cases" = "confirmed",
    "hospitalizations for COVID-19" = "hospitalizations"
)

data_levels <- list(
    "state" = {
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

        # final data
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
        dplyr::select(
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
    },
    "county" = {
        pad_fips <- . %>%
            formatC(
                width = 5,
                flag = "0"
            )

        hospital_data <-
            "https://healthdata.gov/resource/di4u-7yu6.csv" %>%
                read.csv() %>%
                mutate(
                    fips = pad_fips(fips),
                    hospitalizations = confirmed_covid_hosp_last_7_days / 7
                ) %>%
                dplyr::select(
                    fips,
                    hospitalizations
                )

        # final data
        "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/" %>%
            latest_csse_upload() %>%
            filter(Country_Region == "US") %>%
            mutate(
                fips = pad_fips(FIPS),
                deaths = Deaths,
                confirmed = Confirmed
            ) %>%
            dplyr::select(
                fips,
                deaths,
                confirmed
            ) %>%
            full_join(
                hospital_data,
                by = "fips"
            ) %>%
            left_join(
                counties_sf(),
                .,
                by = "fips"
            )
    }
)

# free up memory
rm(hospital_data, csse_data)

# ---- prerender views

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

    color_palette <- colorNumeric("Reds", domain = displayed)
    
    return (
        leaflet(
            options = leafletOptions(
                crs = epsg2163,
                zoomControl = FALSE
                # TODO: https://stackoverflow.com/a/58082967
            )
        ) %>%
            addPolygons(
                data = data,
                popup = paste(
                    data$name,
                    displayed %>% comma(digits = 0) %>% as.character(),
                    sep = ": "
                ),
                color = color_palette(displayed),
                fillColor = color_palette(displayed),
                opacity = 1,
                fillOpacity = 1,
                weight = 2,
            ) %>%
            addLegend(
                "bottomright",
                pal = color_palette,
                values = displayed,
                opacity = 1,
                na.label = "no data"
            )
    )
}

maps <- data_levels %>%
    names() %>%
    set_names(., .) %>%
    lapply(
        function(level) {
            lapply(
                names(data_options) %>% set_names(data_options),
                . %>% get_map(level, .)
            )
        }
    )

summary_figures <-
    data_options %>% 
    set_names(data_options) %>% 
    lapply(
        . %>%
            data_levels$state[[.]] %>%
            sum(na.rm = TRUE) %>%
            comma(digits = 0)
    )

summaries <- list(
    "deaths" = paste(
        "In total,",
        summary_figures$deaths,
        "people have died due to COVID-19 in the US."
    ),
    "confirmed" = paste(
        summary_figures$confirmed,
        "COVID-19 cases have been documented in the US."
    ),
    "hospitalizations" = paste(
        "In total, COVID-19 has resulted in",
        summary_figures$hospitalizations,
        "hosptalizations in the US."
    )
)
        
# ---- shiny setup

ui <- fluidPage(
    tags$head(
        tags$link(
            href = styles_path,
            rel = "stylesheet"
        ),
        tags$link(
            href = "https://fonts.googleapis.com",
            rel = "preconnect"
        ),
        tags$link(
            href = "https://fonts.googleapis.com",
            rel = "preconnect",
            crossorigin = TRUE
        ),
        tags$link(
            href = "https://fonts.googleapis.com/css2?family=PT+Serif:wght@700&family=Roboto&display=swap",
            rel = "stylesheet"
        )
    ),
    mainPanel(
        tags$section(
            selectInput(
                inputId = "option_select",
                label = "Map",
                choices = names(data_options),
                selected = "confirmed COVID-19 cases"
            ),
            selectInput(
                inputId = "level_select",
                label = "by",
                choices = names(data_levels),
                selected = "state"
            ),
            class = "user-input"
        ),
        tags$section(
            leafletOutput("map"),
            class = "map-display"
        ),
        tags$section(
            tags$p(
                textOutput("summary")
            )
        )
    )
)

server <- function(input, output) {
    option <- reactive(
        data_options[[input$option_select]]
    )

    output$map <- renderLeaflet(
        maps[[input$level_select]][[option()]]
    )

    output$summary <- renderText(
        summaries[[option()]]
    )
}

shinyApp(ui = ui, server = server)
