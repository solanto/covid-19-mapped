# COVID-19 mapped

Written for JHU's EN.560.291 CaSE Coding course.

## description

<a href="https://solanto.shinyapps.io/case-coding-final/">
  <img src="https://user-images.githubusercontent.com/20602415/144188232-8f11c916-f4c8-43ee-80ec-4c9fe5f35cd4.png" width="500" />
</a>

This app:

1. imports the latest COVID-19 data from [HealthData.gov](https://healthdata.gov) and the [Johns Hopkins Center for Systems Science and Engineering](https://systems.jhu.edu/)
2. wrangles and geocodes the data
3. generates choropleth maps and textual summaries from the data
4. defines an interactive [Shiny](https://www.rstudio.com/products/shiny/) dashboard
5. handles user input to switch maps and summaries on-the-fly

All data wrangling and view generation happens in [`global.r`](global.r). Using these results, the Shiny app is then defined in [`ui.r`](ui.r) and [`server.r`](server.r). Finally, the app is styled as detailed in [`styles.scss`](styles.scss), taking inspiration from Statistics Sweden's [Kommuner i siffror](https://kommunsiffror.scb.se/)  (*Municipalities in Numbers*) visualization.

## usage

### online

Access the app online on [shinyapps.io](https://solanto.shinyapps.io/covid-19-mapped/).

### local

To run the app locally, you'll need to have an [R interpreter](https://www.r-project.org/) installed. As well, to build dependencies, you will need [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

Through an R shell, install the project's dependencies.

```R
install.packages(c("shiny", "magrittr", "dplyr", "leaflet", "formattable", "sass", "remotes"))
library(remotes)
install_github("hrbrmstr/albersusa")
```

Clone the repository and run the following in an R shell whose working directory is the repository's root directory:

```R
library(shiny)
runApp()
```

Alternatively, open the project in RStudio and run the app using the IDE's built-in Shiny capabilities.
