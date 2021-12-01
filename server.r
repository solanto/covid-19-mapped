function(input, output) {
    # get option key from option input
    option <- reactive(
        data_options[[input$option_select]]
    )

    # pull selected map & render
    output$map <- renderLeaflet(
        maps[[input$level_select]][[option()]]
    )

    # pull appropriate summary text & render
    output$summary <- renderUI(
        summaries[[option()]]
    )
}
