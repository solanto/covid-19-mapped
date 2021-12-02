fluidPage(
    title = "COVID-19 Mapped",
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
        ),
        tags$footer(
            tags$p(
                "Made with care by Andrew Solanto & Kristen Corlay. Source on ",
                tags$a(
                    "GitHub.",
                    href = "https://github.com/solanto/covid-19-mapped",
                    target = "_blank"
                )
            ),
            tags$p(
                "Data from",
                tags$a(
                    "HealthData.gov",
                    href = "https://healthdata.gov/",
                    target = "_blank"
                ),
                " and the ",
                tags$a(
                    "Johns Hopkins Center for Systems Science and Engineering.",
                    href = "https://systems.jhu.edu/",
                    target = "_blank"
                )
            )
        )
    )
)
