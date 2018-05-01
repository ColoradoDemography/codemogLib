#' popTable The population table showing the annual growth rate in the Population Section
#'
#' @param listID the list containing place id and Place names
#' @param sYr Start Year
#' @param eYr End year
#' @param oType Output Type, html or latex
#' @return kable formatted  table and data file
#' @export
#'
popTable <- function(listID,sYr,eYr,oType) {

  # Collecting place ids from  idList, setting default values

  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  if(listID$PlFilter == "T") {
    placefips <- ""
    placename <- ""
  }
  #outputs the Population Growth Rate table in the population section..
  
  state <- "Colorado"
  ctynum <- as.numeric(ctyfips)
  placenum <- as.numeric(placefips)
  yrs <- as.character(setYrRange(sYr,eYr))
  
  #State Population and Growth Rate
  popCO=county_profile(0, sYr:eYr, "totalpopulation")%>%
    filter(year %in% yrs)%>%
    mutate(name="Colorado",
           totalpopulation=as.numeric(totalpopulation),
           year=as.numeric(year),
           growthRate=percent(signif((((totalpopulation/lag(totalpopulation))^(1/(year-lag(year)))) -1)*100),digits=1),
           Population=comma(totalpopulation))
  mCO <- popCO[,c(1,5,7,6)]
  
  #County Population and Growth Rate  *** need to account for multip county communities...
  mCty <- county_profile(ctynum, sYr:eYr, "totalpopulation")%>%
    filter(year %in% yrs)%>%
    arrange(county,year)%>%
    mutate(name=county,
           year=as.numeric(year),
           totalpopulation=as.numeric(totalpopulation),
           growthRate=percent(signif((((totalpopulation/lag(totalpopulation))^(1/(year-lag(year)))) -1)*100),digits=1),
           Population=comma(totalpopulation))
  mCty$Population  <- ifelse(mCty$totalpopulation == 0, " ",mCty$Population)
  
  
  
  
  if(nchar(placename) != 0) { #if a placename is present
    sqlStrPop1 <- paste0("SELECT countyfips, placefips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE placefips = ",placenum,";")
    # Postgres Call to gather municipal jobs numbers
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
    
    f.popPlace <-  dbGetQuery(con, sqlStrPop1)
    
    #closing the connections
    dbDisconnect(con)
    dbUnloadDriver(drv)
    rm(con)
    rm(drv)
    

    f.popPlace <- f.popPlace[which(f.popPlace$countyfips != 999), ]  # removing "Total" for multi-county cities
    f.popPlace$totalpopulation <- ifelse(is.na(f.popPlace$totalpopulation),0,f.popPlace$totalpopulation) #Fixing NA values
    f.popPlace$municipalityname <-gsub(' \\([P,p]art\\)','',f.popPlace$municipalityname)
    
    # Adding records for Municipalities incorpropated after the beginning date in the series
    if(min(f.popPlace$year) > sYr) {
      minYr <- min(f.popPlace$year) 
      newRows <- minYr - sYr
      newYr <- matrix(nrow=newRows, ncol=5)
      for(x in 1:newRows) {
        newYr[x,1] <- ctyfips
        newYr[x,2] <- placefips
        newYr[x,3] <- placename
        newYr[x,4] <- as.numeric(sYr + (x - 1))
        newYr[x,5] <- 0
      }

      f.newRec <- as.data.frame(newYr,stringsAsFactors=FALSE)  
      names(f.newRec) <- c("countyfips", "placefips", "municipalityname", "year", "totalpopulation")
      f.newRec$year <- as.numeric(f.newRec$year)
      f.newRec$totalpopulation <- as.numeric(f.newRec$totalpopulation)
      f.popPlace <- rbind(f.newRec,f.popPlace)
      }

    PP <-  f.popPlace %>% group_by(placefips, municipalityname, year)  %>% summarize(totalpopulation = sum(as.numeric(totalpopulation)))
    
    placX <- PP %>% 
      filter(year %in% yrs)%>%
      arrange(year)
    placX <- placX[which(placX$totalpopulation != 0),]
    
    placX$Population <- format(placX$totalpopulation,big.mark=",")
    placX$growthRate  <- percent((((placX$totalpopulation/lag(placX$totalpopulation))^(1/(placX$year-lag(placX$year)))) -1)*100,digits=1)
    placX$Population  <- ifelse(placX$totalpopulation == 0, " ",placX$Population)
     mPlace <- as.matrix(placX[,c(3,2,5,6)])
  }
  

  if(nchar(placename) != 0) { #if a placename is present
    m.OutTab <- cbind(mPlace,mCty,mCO)
    m.OutTab <- m.OutTab[,c(1,3,4,11,10,14,15)]
  }  else {
    m.OutTab <- cbind(mCty,mCO)
    m.OutTab <- m.OutTab[,c(3,7,6,10,11)] 
  } 
  
  
  m.OutTab <- as.matrix(m.OutTab)
  m.OutTab <- gsub("NA%","",m.OutTab)
  #Additional Suppressions
  m.OutTab <- gsub("NaN%","",m.OutTab)
  m.OutTab <- gsub("Inf%","",m.OutTab)
  
  
  
  if(nchar(placename) != 0) {
    names_spaced <- c("Year","Population","Growth Rate","Population","Growth Rate","Population","Growth Rate") 
    tblHead <- c(" " = 1, placename = 2, ctyname = 2, state = 2)
    names(tblHead) <- c(" ", placename, ctyname,state)
  } else {
    names_spaced <- c("Year","Population","Growth Rate","Population","Growth Rate")
    tblHead <- c(" " = 1, ctyname = 2, state = 2)
    names(tblHead) <- c(" ", ctyname,state)
  }
  
  
  
  
  
  
  if(oType == "html") {
    # Creating Final Table (kable)
    if(nchar(placename) != 0) {
      OutTab  <- m.OutTab %>%
        kable(format='html', table.attr='class="myTable"',
              caption = "Population Growth Rate",
              row.names=FALSE,
              align='lrrrrrr',
              col.names = names_spaced,
              escape = FALSE) %>%
        kable_styling(bootstrap_options = "condensed") %>%
        column_spec(1, width = "0.4in") %>%
        column_spec(2, width = "0.5in") %>%
        column_spec(3, width ="0.5in") %>%
        column_spec(4, width = "0.5in") %>%
        column_spec(5, width = "0.5in") %>%
        column_spec(6, width = "0.5in") %>%
        column_spec(7, width = "0.5in") %>%
        add_header_above(header=tblHead)  %>%
        add_footnote(captionSrc("SDO",""))
    }  else { 
      OutTab  <- m.OutTab %>%
        kable(format='html', table.attr='class="myTable"',
              caption = "Population Growth Rate",
              row.names=FALSE,
              align='lrrrr',
              col.names = names_spaced,
              escape = FALSE) %>%
        kable_styling(bootstrap_options = "condensed") %>%
        column_spec(1, width = "0.4in") %>%
        column_spec(2, width = "0.5in") %>%
        column_spec(3, width = "0.5in") %>%
        column_spec(4, width = "0.5in") %>%
        column_spec(5, width = "0.5in") %>%
        add_header_above(header=tblHead)  %>%
        add_footnote(captionSrc("SDO",""))
    }
    
    # Creating Final Data Set
    f.Out2 <- as.data.frame(m.OutTab)
    if(ncol(f.Out2) == 5) {
      names(f.Out2) <- c("Year",paste0("Population: ",ctyname),paste0("Growth Rate: ",ctyname),
                         "Population: Colorado","Growth Rate: Colorado")
    }
    if(ncol(f.Out2) == 7) {
      names(f.Out2) <- c("Year",paste0("Population: ",placename),paste0("Growth Rate: ",placename),
                         paste0("Population: ",ctyname),paste0("Growth Rate: ",ctyname),
                         "Population: Colorado","Growth Rate: Colorado")
    }
 
    
    
    # bind list
    outList <- list("table" = OutTab,"data" = f.Out2)
    
    return(outList)
  }
  
  if(oType == "latex") {
    if(nchar(placename) != 0) {
      OutTab <- m.OutTab %>%
        kable(digits=1,
              row.names=FALSE,
              align="lrrrrrr",
              col.names = names_spaced,
              caption="Population Growth Rate",
              format ="latex", booktabs=TRUE) %>%
        kable_styling(latex_options="HOLD_position",font_size=9) %>%
        row_spec(0, align="c") %>%
        column_spec(column=1:7, width="0.5in") %>%
        add_header_above(header=tblHead)  %>%
        add_footnote(captionSrc("SDO",""))
    }  else { 
      OutTab <- m.OutTab %>%
        kable(digits=1,
              row.names=FALSE,
              align="lrrrr",
              col.names = names_spaced,
              caption="Population Growth Rate",
              format ="latex", booktabs=TRUE) %>%
        kable_styling(latex_options="HOLD_position",font_size=9) %>%
        row_spec(0, align="c") %>%
        column_spec(column=1:5, width="0.5in") %>%
        add_header_above(header=tblHead)  %>%
        add_footnote(captionSrc("SDO",""))
    }
    
    # Building text
    RowN <- nrow(m.OutTab)
    prevYr <- m.OutTab[RowN-1,1]

    # Extracting last growth rates
    if(nchar(placename) != 0) {
        plGR <- gsub("%","",as.character(m.OutTab[RowN,3]))
        ctyGR <- gsub("%","",as.character(m.OutTab[RowN,5]))
        stGR <- gsub("%","",as.character(m.OutTab[RowN,7]))
    } else {
      # Extracting last growth rates
      ctyGR <- gsub("%","",as.character(m.OutTab[RowN,3]))
      stGR <- gsub("%","",as.character(m.OutTab[RowN,5]))
    }
    
    if(nchar(placename) != 0) {#Municipalities
      OutTxt_pl <- paste0("At the end of ",eYr, " the estimated population of ",placename, " was ", m.OutTab[RowN,2],", ")
      PopChgVal_pl <- as.numeric(gsub(",","",m.OutTab[RowN,2])) - as.numeric(gsub(",","",m.OutTab[RowN-1,2]))
      PopChgFmt_pl <- format(PopChgVal_pl,big.mark=",")
      PopChgTxt_pl <-  ifelse(PopChgVal_pl > 0, paste0("an increase of ",PopChgFmt_pl," over the population in ",prevYr,"."),
                              ifelse(PopChgVal_pl < 0, paste0("a decrease of ",PopChgFmt_pl," over the population in ",prevYr,"."),paste0("did not change between ",prevYr, " and ",eYr,".")
                              ))
      grTxtpl <- paste0("  The growth rate for ",placename," between ",prevYr," and ",eYr, " was ",plGR," percent")
      grTxtpl <- paste0(grTxtpl, " compared to ",ctyGR," percent for ",ctyname," and ",stGR," percent for the State of Colorado.")

      outText <- paste0(OutTxt_pl, PopChgTxt_pl,grTxtpl)
    } else {
      OutTxt_cty <- paste0("At the end of ",eYr, " the estimated population of ",ctyname, " was ", m.OutTab[RowN,2],", ")
      PopChgVal_cty <- as.numeric(gsub(",","",m.OutTab[RowN,2])) - as.numeric(gsub(",","",m.OutTab[RowN-1,2]))
      PopChgFmt_cty <- format(PopChgVal_cty,big.mark=",")
      PopChgTxt_cty <-  ifelse(PopChgVal_cty > 0, paste0("an increase of ",PopChgFmt_cty," over the population in ",prevYr,"."),
                               ifelse(PopChgVal_cty < 0, paste0("a decrease of ",PopChgFmt_cty," over the population in ",prevYr,"."),paste0("did not change between ",prevYr, " and ",eYr,".")
                               ))
      grTxtcty <- paste0("  The growth rate for ",ctyname," between ",prevYr," and ",eYr, " was ",ctyGR," percent")
      grTxtcty <- paste0(grTxtcty, " compared to ",stGR," percent for the State of Colorado.")
      
      outText <- paste0(OutTxt_cty,PopChgTxt_cty,grTxtcty)  
      }
    
    outlist <- list("table" = OutTab, "text" = outText)
    return(outlist)
  }
  
}