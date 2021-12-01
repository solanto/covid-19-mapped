fluidPage(
    # stylesheets
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
    # dashboard content
    mainPanel(
        # user inputs for map data level & option
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
        # map display
        tags$section(
            leafletOutput("map"),
            class = "map-display"
        ),
        # summary text
        tags$section(
            htmlOutput("summary"),
            class = "summary"
        )
    )
)
