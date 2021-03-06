# 0. preface -----------------------------------------

# to install `albersusa`, call:
# 
# ```R
# install.packages("remotes")
# library(remotes)
# install_github("hrbrmstr/albersusa")
# ```

# 1. packages ----------------------------------------

library(shiny)
library(magrittr)
library(dplyr)
library(leaflet)
library(formattable)
library(sass)
library(lubridate)
library(albersusa)

# 2. building styles ---------------------------------

style_dir <- "styles"

dir.create(style_dir, showWarnings = FALSE)

styles_path <-
    paste(
        style_dir,
        "/styles.css",
        sep = ""
    )

sass(
    sass_file("styles.scss"),
    output = styles_path
)

addResourcePath(
    prefix = style_dir,
    directoryPath = style_dir
)

# 3. getting data ------------------------------------

# CSSE data is posted midnight eastern time
# the day before the current day in eastern time
# formatted to match CSSE data filenames
yesterday <-
    today("America/New_York") %>%
    - 1 %>%
    format("%m-%d-%Y")

# download most recent data into a data frame
latest_csse_upload <- function(base_url) {
    # generate url
    paste(
        base_url,
        yesterday,
        ".csv",
        sep = ""
    ) %>%
    # download & parse
    read.csv()
}

# names (keys for) sets of data points to be offered
# keyed by each of the sets' names in the dashboard
data_options <- c(
    "deaths due to COVID-19" = "deaths",
    "confirmed COVID-19 cases" = "confirmed",
    "hospitalizations for COVID-19" = "hospitalizations"
)

# a named list of data frames
# ex: one frame for state-level data and one for county-level data
data_levels <- list(
    # begin state-level data wrangling
    "state" = {
        # 1/2 - prep
        
        # map state abbreviations to state names
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

        # 2/2 - final data
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
    }, # end state-level data wrangling
    # begin county-level data wrangling
    "county" = {
        # 1/2 - prep
        
        # format integer fips values as 5-character fips strings
        # for matching in table join
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

        # 2/2 - final data
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
    } # end county-level data wrangling
)

# free up memory
rm(hospital_data, csse_data)

# 4. pre-generating views ----------------------------

# map projection
epsg2163 <- leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "EPSG:2163",
    proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
    resolutions = 2 ^ (16:7)
)

# generate a map given a level (ex: state) & option (ex: deaths)
get_map <- function(level, option) {
    data <- data_levels[[level]]

    # extract data points from level data after lookup in options
    data_points <-
        option %>%
        data_options[.] %>%
        as.character() %>%
        data[[.]]

    color_palette <- colorNumeric("Reds", domain = data_points)
    
    return (
        # initialize map
        leaflet(
            options = leafletOptions(
                crs = epsg2163,
                zoomControl = FALSE
            )
        ) %>%
            # draw choropleth
            addPolygons(
                data = data,
                popup = paste(
                    data$name,
                    data_points %>% comma(digits = 0) %>% as.character(),
                    sep = ": "
                ),
                color = color_palette(data_points),
                fillColor = color_palette(data_points),
                opacity = 1,
                fillOpacity = 1,
                weight = 2,
            ) %>%
            addLegend(
                "bottomright",
                pal = color_palette,
                values = data_points,
                opacity = 1,
                na.label = "no data"
            )
    )
}

# maps generated for all level and option combinations
# resulting data structure example:
# 
# ```yaml
# state:
#   - deaths: get_map("state", "deaths")
#   - hospitalizations: get_map("state", "hospitalizations")
#   - confirmed: get_map("state, "confirmed")
# county:
#   - deaths: get_map("county", "deaths")
#   - hospitalizations: get_map("county", "hospitalizations")
#   - confirmed: get_map("county, "confirmed")
# ```
maps <-
    data_levels %>%
        names() %>% # ex: c("state", "county")
        set_names(., .) %>% # ex: c("state" = "state", "county" = "county")
        lapply( # for each level name
            function(level) {
                lapply( # for each option name
                    names(data_options) %>% set_names(data_options),
                    . %>% get_map(level, .)
                )
            }
        )

# numerical figures to be used in summary text
summary_figures <-
    data_options %>%
    set_names(data_options) %>% # index results by option names
    lapply( # for each data option
        . %>%
            data_levels$state[[.]] %>% # get data points
            sum(na.rm = TRUE) %>%
            comma(digits = 0) %>%
            tags$strong() # indicate urgency in html
    )

# summary text
summaries <- list(
    "deaths" = tags$p(
        "In total, ",
        summary_figures$deaths,
        " people have died due to COVID-19 in the US."
    ),
    "confirmed" = tags$p(
        summary_figures$confirmed,
        " COVID-19 cases have been documented in the US."
    ),
    "hospitalizations" = tags$p(
        "In total, COVID-19 has resulted in ",
        summary_figures$hospitalizations,
        " hospitalizations in the US."
    )
)
