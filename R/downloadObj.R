#' downloadObj  File Download Modules
#'
#' downloadObj is the server function that facilitates the download
#'
#' @param place is the place name, typically the value of input$unit
#' @param oname the input data description object
#' @param dboj is the data object to be output
#' @export

downloadObj <- function(input, output, session, place, oname, dobj) {

  if(nchar(oname) == 9) {
    dname <- substr(oname,1,5)
    dtype <- substr(oname,6,9)
  }

  if(nchar(oname) == 10) {
    dname <- substr(oname,1,6)
    dtype <- substr(oname,7,10)
  }


  prefix <- switch(dname,
                   "stats" = " Basic Statistics",
                   "popf1" = " Pop Growth Comparison",
                   "popf2" = " Pop Growth",
                   "popf3" = " Pop Forecast",
                   "popf4" = " Components of Change",

                   "popa1" = " Age Distribution",
                   "popa2" = " Median Age",
                   "popa3" = " Age Forecast",
                   "popa4" = " Migration by Age",

                   "popc1" = " Income",
                   "popc2" = " Educational Attainment",
                   "popc3" = " Race Trend",
                   "popc4" = " Race Comparison",

                   "poph1" = " Housing Forecast",
                   "poph2" = " Housing Type",
                   "poph3" = " Owner Housing",
                   "poph4" = " Rental Housing",
                   "poph5" = " Owner-Occupied Housing Values",
                   "poph6" = " Rental Housing Values",

                   "popt1" = " Commuting Venn",
                   "popt2" = " Commuting Live",
                   "popt3" = " Commuting Work",
                   "popt4" = " Jobs and Migration",

                   "popei1" = " Jobs Forecast",
                   "popei2" = " Jobs by Industry",
                   "popei3" = " Jobs by Sector Detail",
                   "popei4" = " Jobs by Sector General",

                   "popem1" = " Jobs and Pop Forecast",
                   "popem2" = " Avg Weekly Wage",
                   "popem3" = " Residential Labor Force Forecast",
                   "popem4" = " Househould Income Sources"

  )

  suffix <- ifelse(dtype == "plot"," Plot.png",
            ifelse(dtype == "data"," Data.csv"," Table.docx"))

  output$download <-  downloadHandler(
    filename = function() {
      paste0(place,prefix,suffix)
    },
    content = function(file) {
      if(suffix == " Data.csv") {
        write.csv(dobj, file, row.names = FALSE)
      }
      if(suffix == " Plot.png") {
        ggsave(file, plot = dobj, width =8, height=6, units	="in", device = "png")
      }
      if(suffix == " Table.docx") {
        doc <- read_docx()
        doc <- body_add_flextable(doc, value = dobj)
        print(doc, target = file)
      }
    } #content
  ) #DowhloadHandler
} #downloadObj
