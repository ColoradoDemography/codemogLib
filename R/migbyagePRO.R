#' migbyagePRO Creates a Chart showing the 2000-2010 net Migration rate by age
#'
#' @param listID the list containing place id and Place names
#' @param base is the base text size for the ggplot2 object and codemog_theme()
#' @return ggplot2 graphic and data file
#' @export
#'
migbyagePRO <- function(listID, base=10) {
  
  # Collecting place ids from  idList, setting default values
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  
  state= 0

  # create a connection
  # save the password that we can "hide" it as best as we can by collapsing it
  pw <- {
    "demography"
  }

  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "dola",
                   host = "104.197.26.248", port = 5433,
                   user = "codemog", password = pw)
  rm(pw) # removes the password

  sqlPlace <- paste0("SELECT fips, county, agegroup, rate0010 FROM data.netmigrbyage WHERE fips = ",as.numeric(ctyfips),";")
  f.migPlace <- dbGetQuery(con, sqlPlace)

  sqlState <- paste0("SELECT fips, county, agegroup, rate0010 FROM data.netmigrbyage WHERE fips = ",state,";")
  f.migState <- dbGetQuery(con, sqlState)

  #Closing the connection
  dbDisconnect(con)
  dbUnloadDriver(drv)
  rm(con)
  rm(drv)

  # Preparing for merge
  f.migPlace[2] <- ctyname
  f.migState[2] <- "Colorado"


  f.migplot <- rbind(f.migPlace, f.migState)
  names(f.migplot)[2] <- "geoname"



  f.migplot$geoname <- factor(f.migplot$geoname, levels=c(ctyname, "Colorado"))
  pltTitle <- "Net Migration Rate by Age: 2000-2010"
  subTitle <- ctyname
  xTitle = "Age Group"




  p <- f.migplot %>%ggplot(aes(x=agegroup, y=rate0010, colour=geoname))+
    geom_line(size=1.5) +
    scale_colour_manual("Geography", values=c("#6EC4E8", "#00953A")) +
    theme_codemog(base_size=base)+
    geom_hline(yintercept=0, size=1.05) +
    labs(title = pltTitle,
         subtitle = subTitle,
         caption = captionSrc("SDO",""),
         x = xTitle,
         y= "Net Migration Rate (per 1,000)") +
    theme(plot.title = element_text(hjust = 0.5, size=18),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_line(colour = "gray80"),
          axis.text = element_text(size=12),
          legend.position= "bottom")


  #Preparing data set
  f.migD <- merge(f.migPlace,f.migState, by="agegroup")
  f.migData <- f.migD[,c(1,4,7)]
  names(f.migData) <- c("5-Year Age Group",paste0("2000-2010 Migration Rate: ",ctyname), "2000-2010 Migration Rate: Colorado")

  #Geneerating Text
  OutText <-paste0("This table allows a comparison of net migration by age between ",ctyname," and Colorado.")  
   OutText <-paste0(OutText," Colorado typically draws many young adults as net migrants; however, areas with colleges draw a number of 18 to 24 year olds while many other parts of the state attract mostly 25 to 35 year olds.")  
  
  outList <-list("plot"=p,"data"= f.migData,"text" = OutText)
  return(outList)
}
