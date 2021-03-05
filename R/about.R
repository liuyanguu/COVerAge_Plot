##
# The About panel
# Yang Liu
# 1/11/2020
##
get.about.panel <- function(update_string){
  tabPanel("About",
           p(),
           p("Purpose: Population characteristics are key to understanding the prevalence, spread and fatality of COVID-19 across countries. The COVID Age Database (COVerAGE-DB) provides a valuable insight into age-and sex-specific patterns of COVID-19 cases and deaths."
           ),
           br(),

            p("Source of data: COVerAGE-DB is an open-access database including cumulative counts of confirmed COVID-19 cases, deaths, and tests by age and sex. Original data and sources are provided alongside data and measures in standardized and age-harmonized formats. The data were extracted from reports published by official governmental institutions in a variety of formats. The project page and the medRxiv paper contain the technical details.
            The database is still in development. Today it includes 108 countries and 366 subnational areas."),
           
           br(),
           
           p("Limitation: The limitation of the COVerAGE-DB is the heterogeneous qualities of the underlying data, which is also difficult to evaluate. Case and death counts are likely underestimated, with underestimation expected to vary by age. As the definitions and practices of counting COVID-19 cases and deaths vary in each country, the data should be interpreted with caution when making comparisons across countries or within the same country over time.
             "),
           br(),
           
           p("This app was created and maintained by UNICEF.",
             "For any questions, feedback or suggestion, please contact us at yanliu@unicef.org."),
           p("Please use the latest version of Google Chrome, Mozilla Firefox or Internet Explorer."),
           br(), br(),
           p(update_string,
             br("Created in ",
                a("R", 
                  href = "http://www.r-project.org/", target = "_blank"),
                "| Powered by ",
                a("Shiny", 
                  href = "http://www.rstudio.com/shiny/", target = "_blank")),
             align = "center")
  )
}
