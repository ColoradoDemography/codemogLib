#' statsTable1 outputs the summary table in the stats section of the dashboard, draws data from the census API
#'
#' @param listID list containing id numbers and place names
#' @param sYr Start Year
#' @param eYr End year
#' @param ACS American Cummunity Survey Data series
#' @param oType Controls the rendering of the table, HTML or Latex
#' @return kable formatted table
#' @export
#'
statsTable1 <- function(listID,sYr,eYr,ACS,oType){
  #outputs the top table in the dashboard

  # Collecting place ids from  idList, setting default values
  
  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  placefips <- listID$plNum
  placename <- listID$plName
  #if(listID$PlFilter == "T") {
  #  placefips <- ""
  #  placename <- ""
  #}
  
  state <- "08"
  jobsChk <- 0
  
 
  
  if(nchar(placefips) != 0) {
    
    sqlStrPop1 <- paste0("SELECT placefips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE placefips = ",as.numeric(placefips)," and year = ", sYr,";")
    sqlStrPop2 <- paste0("SELECT placefips, municipalityname, year, totalpopulation FROM estimates.county_muni_timeseries WHERE placefips = ",as.numeric(placefips)," and year = ", eYr,";")
    sqlStrJobs <- paste0("SELECT placefips, year, jobs FROM estimates.muni_jobs_long WHERE placefips = ",as.numeric(placefips),";")
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
    
    f.tPopyr1p <-  dbGetQuery(con, sqlStrPop1)
    f.tPopyr2p <-  dbGetQuery(con, sqlStrPop2)
    f.muniJobsp <- dbGetQuery(con, sqlStrJobs)
    
    #closing the connections
    dbDisconnect(con)
    dbUnloadDriver(drv)
    rm(con)
    rm(drv)
    
    #Fixing NA Values
    f.tPopyr1p$totalpopulation <- ifelse(is.na(f.tPopyr1p$totalpopulation),0,f.tPopyr1p$totalpopulation)
    f.tPopyr2p$totalpopulation <- ifelse(is.na(f.tPopyr2p$totalpopulation),0,f.tPopyr2p$totalpopulation)
    
    f.tpop1ps <- f.tPopyr1p %>% group_by(municipalityname,year) %>% summarize(tpop1 = sum(totalpopulation))
    f.tpop2ps <- f.tPopyr2p %>% group_by(municipalityname,year) %>% summarize(tpop2 = sum(totalpopulation))
    
    f.tpopp <- cbind(f.tpop1ps, f.tpop2ps)
    f.tpopp$popchnp <- f.tpopp$tpop2 - f.tpopp$tpop1
    
    
    hhincp <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    MedHHValuep <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    
    Povertyp <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    Povertyp$pctPovertyp <- percent(as.numeric(Povertyp$b17001002)/as.numeric(Povertyp$b17001001)*100)
    
    Nativep <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    Nativep$pctNativep <- percent(as.numeric(Nativep$b05002003)/as.numeric(Nativep$b05002001)*100)
    
    #Correcting Missing Jobs Numbers.
    jobsChk <- as.numeric(f.muniJobsp[which(f.muniJobsp$year == eYr),3])
    if(jobsChk == -9) {
      jobsMis <- f.muniJobsp[order(-f.muniJobsp$year),]
      for(x in 1:nrow(jobsMis)) {
        if(jobsMis[x,3] != -9) {
          jobsPl <- jobsMis[x,]  # This is the place jobs value
          break
        }
      }
    } else {
      f.muniJobsp <- f.muniJobsp[which(f.muniJobsp$year == eYr),]
      } #jobsChk == -9
  }
  
  #Counties
  
  #Total Population
  f.tPopyr1c <- county_profile(as.numeric(ctyfips), sYr,"totalpopulation")
  f.tPopyr1c$tpop1 <- f.tPopyr1c$totalpopulation
  f.tPopyr2c <- county_profile(as.numeric(ctyfips), eYr,"totalpopulation")
  f.tPopyr2c$tpop2 <- f.tPopyr2c$totalpopulation
  f.tpopc <- merge(f.tPopyr1c, f.tPopyr2c, by="countyfips")
  f.tpopc$popchgc <- as.numeric(f.tpopc$tpop2) - as.numeric(f.tpopc$tpop1)
  f.tpopc$county.x <- paste0(f.tpopc$county.x," County")
  
  #Jobs
  if(jobsChk == -9){
    tJobsc <-  county_jobs(fips=as.numeric(ctyfips), year = jobsPl$year) #County
  } else {
    tJobsc <-  county_jobs(fips=as.numeric(ctyfips), year = eYr) #County
  } 

  
  #Prepping for multiple countins
  hhincc  <- data.frame()
  MedHHValuec <- data.frame()
  Povertyc <- data.frame()
  Nativec <- data.frame()
  
  for(i in 1:length(ctyfips)) {
    hhincc <- rbind(hhincc,codemog_api(data="b19013",db=ACS, geonum=paste("1", state, ctyfips[i], sep=""), meta="no"))
    MedHHValuec <- rbind(MedHHValuec,codemog_api(data="b25077",db=ACS, geonum=paste("1", state, ctyfips[i], sep=""), meta="no"))
    Povertyc <- rbind(Povertyc,codemog_api(data="b17001",db=ACS, geonum=paste("1", state, ctyfips[i], sep=""), meta="no"))
    Nativec <- rbind(Nativec,codemog_api(data="b05002",db=ACS, geonum=paste("1", state, ctyfips[i], sep=""), meta="no"))
  }
  
  Povertyc$pctPovertyc <- percent(as.numeric(Povertyc$b17001002)/as.numeric(Povertyc$b17001001)*100)
  Nativec$pctNativec <- percent(as.numeric(Nativec$b05002003)/as.numeric(Nativec$b05002001)*100)
  
  
  
  #state Values
  
  tPopyr1ST <- county_profile(300, sYr,"totalpopulation")
  f.tPopyr1ST <- tPopyr1ST %>% summarize(tpop1 = sum(as.numeric(totalpopulation)))
  tPopyr2ST <- county_profile(300, eYr,"totalpopulation")
  f.tPopyr2ST <- tPopyr2ST %>% summarize(tpop2 = sum(as.numeric(totalpopulation)))
  f.tpopST <- cbind(f.tPopyr1ST, f.tPopyr2ST)
  f.tpopST$popchgST <- as.numeric(f.tpopST$tpop2) - as.numeric(f.tpopST$tpop1)
  
  plNameST <- "Colorado"
  
  #Jobs
  if(jobsChk == -9){
    tJobsST <-  county_jobs(fips=300, year = jobsPl$year)
  } else {
    tJobsST <-  county_jobs(fips=300, year = eYr)
  } 
  f.tJobsST <- tJobsST %>%  summarize(totalJobs = sum(as.numeric(totalJobs)))
  
  hhincST <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  MedHHValueST <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, sep=""), meta="no")
  
  PovertyST <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, sep=""), meta="no")
  PovertyST$pctPovertyST <- percent(as.numeric(PovertyST$b17001002)/as.numeric(PovertyST$b17001001)*100)
  
  NativeST <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  NativeST$pctNativeST <- percent(as.numeric(NativeST$b05002003)/as.numeric(NativeST$b05002001)*100)
  
  #Median Household Income  B18140 is the total median earnings...  from the 2012-2016 ACS API
  hhincST <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  
  #median Househld Value
  MedHHValueST=codemog_api(data="b25077",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  
  
  
  
  
  #Preparing table

  if(nchar(placefips) == 0){
    mcol <- 3
  } else {
    mcol <- 4
  }

  outTab <- matrix(" ",nrow=7,ncol=mcol)
  nCol <- 1
  outTab[1,nCol] <- paste0("Population (",eYr,")",footnote_marker_symbol(1))
  outTab[2,nCol] <- paste0("Population Change (",sYr," to ",eYr, ")",footnote_marker_symbol(1))
  
  if(jobsChk == -9) {
    outTab[3,nCol] <- paste0("Total Employment (",jobsPl$year,")",footnote_marker_symbol(1))
  } else {
    outTab[3,nCol] <- paste0("Total Employment (",eYr,")",footnote_marker_symbol(1))
  }
  
  outTab[4,nCol] <- paste0("Median Household Income",footnote_marker_symbol(2))
  outTab[5,nCol] <- paste0("Median House Value",footnote_marker_symbol(2))
  outTab[6,nCol] <- paste0("Percentage of Population with Incomes lower than the Poverty Line",footnote_marker_symbol(2))
  outTab[7,nCol] <- paste0("Percentage of Population Born in Colorado",footnote_marker_symbol(2))
  nCol <- nCol + 1 
  
  if(nchar(placefips) != 0){
    #place
    f.tpopp <- f.tpopp[1,]
    outTab[1,nCol] <- format(as.numeric(f.tpopp$tpop2),nsmall=0, big.mark=",")
    outTab[2,nCol] <- format(as.numeric(f.tpopp$popchnp),nsmall=0, big.mark=",")
    if(jobsChk == -9){
      outTab[3,nCol] <- format(round(as.numeric(jobsPl$jobs),digits=0),nsmall=0, big.mark=",")
    } else {
      outTab[3,nCol] <- format(round(as.numeric(f.muniJobsp$jobs),digits=0),nsmall=0, big.mark=",")
    }
   
    outTab[4,nCol] <- paste0("$",format(as.numeric(hhincp$b19013001),nsmall=0, big.mark=","))
    if(MedHHValuep$b25077001 == "") {
      outTab[5,nCol] <- " "
    } else {
      outTab[5,nCol] <- paste0("$",format(as.numeric(MedHHValuep$b25077001),nsmall=0, big.mark=","))
    }

    outTab[6,nCol] <- Povertyp$pctPovertyp
    outTab[7,nCol] <- Nativep$pctNativep
    nCol <- nCol + 1
  }
  
  #County
    outTab[1,nCol] <- format(as.numeric(f.tpopc[i,9]),nsmall=0, big.mark=",")
    outTab[2,nCol] <- format(as.numeric(f.tpopc[i,10]),nsmall=0, big.mark=",")
    outTab[3,nCol] <- format(round(as.numeric(tJobsc[i,3]),digits=0),nsmall=0, big.mark=",")
    outTab[4,nCol] <- paste0("$",format(as.numeric(hhincc[i,8]),nsmall=0, big.mark=","))
    outTab[5,nCol] <- paste0("$",format(as.numeric(MedHHValuec[i,8]),nsmall=0, big.mark=","))
    outTab[6,nCol] <- Povertyc[i,67]
    outTab[7,nCol] <- Nativec[i,35]
    nCol <- nCol + 1
  
  
  #State
  outTab[1,nCol] <- format(as.numeric(f.tpopST$tpop2),nsmall=0, big.mark=",")
  outTab[2,nCol] <- format(as.numeric(f.tpopST$popchgST),nsmall=0, big.mark=",")
  outTab[3,nCol] <- format(round(as.numeric(f.tJobsST$totalJobs),digits=0),nsmall=0, big.mark=",")
  outTab[4,nCol] <- paste0("$",format(as.numeric(hhincST$b19013001),nsmall=0, big.mark=","))
  outTab[5,nCol] <- paste0("$",format(as.numeric(MedHHValueST$b25077001),nsmall=0, big.mark=","))
  outTab[6,nCol] <- PovertyST$pctPovertyST
  outTab[7,nCol] <- NativeST$pctNativeST

 

 
  # Create Column headings
 
 
  if(nchar(placefips) == 0) {
    names_spaced <- c(" ",  ctyname, "Colorado")
  } else {
    names_spaced <- c(" ", placename, ctyname, "Colorado")
  }
  

  if(oType == "html") {
    if(nchar(placefips) == 0) {
      outKable <-  kable(outTab, format='html', table.attr='class="cleanTab"',
                         digits=1,
                         row.names=FALSE,
                         align='lrr',
                         col.names = names_spaced,
                         caption="Community Quick Facts",
                         escape = FALSE)   %>%
        kable_styling() %>%
        column_spec(1, width = "4in") %>%
        column_spec(2, width = "0.4in") %>%
        column_spec(3, width = "0.4in") %>%
        footnote(symbol=c("Source: State Demography Office",captionSrc("ACS",ACS)))
    } else {
      outKable <-  kable(outTab, format='html', table.attr='class="cleanTab"',
                         digits=1,
                         row.names=FALSE,
                         align='lrrr',
                         col.names = names_spaced,
                         caption="Community Quick Facts",
                         escape = FALSE)   %>%
        kable_styling() %>%
        column_spec(1, width = "4in") %>%
        column_spec(2, width = "0.4in") %>%
        column_spec(3, width = "0.4in") %>%
        column_spec(4, width = "0.4in") %>%
        footnote(symbol=c("Source: State Demography Office",captionSrc("ACS",ACS)))
    } 
    #Generate Flextable
    outTab <- gsub("<sup>"," ",outTab)
    outTab <- gsub("</sup>","",outTab)
    outTab <- gsub("&dagger;"," ^",outTab)
    f.Flex <- as.data.frame(outTab)
    FlexOut <- regulartable(f.Flex)
    if(ncol(f.Flex) == 3) {
      FlexOut <- set_header_labels(FlexOut, V1 = "", 
                                   V2 = ctyname, V3 = "Colorado")
      FlexOut <- add_header(FlexOut,V1 ="Basic Statistics Table", top=TRUE)
      FlexOut <- add_footer(FlexOut,V1=paste0("* State Demography Office ||"," ^",captionSrc("ACS",ACS)))
      FlexOut <- merge_at(FlexOut,i=1,j = 1:3,part="header")
      FlexOut <- merge_at(FlexOut,i=1, j = 1:3, part = "footer")
      FlexOut <- align(FlexOut,i=1,j = 1, align="left",part="header")
      FlexOut <- align(FlexOut,i=2,j = 1:3, align="center",part="header")     
      FlexOut <- align(FlexOut,i=1, align="left",part="footer")
      FlexOut <- align(FlexOut, j=1, align="left", part="body")
      FlexOut <- autofit(FlexOut)
      FlexOut <- width(FlexOut, j = ~ V1, width = 4)
    }
    if(ncol(f.Flex) == 4) {
      FlexOut <- set_header_labels(FlexOut, V1 = "", V2=placename,
                                   V3 = ctyname, V4 = "Colorado")
      FlexOut <- add_header(FlexOut,V1 ="Basic Statistics Table", top=TRUE)
      FlexOut <- add_footer(FlexOut,V1=paste0("* State Demography Office ||"," ^",captionSrc("ACS",ACS)))
      FlexOut <- merge_at(FlexOut,i=1,j = 1:3,part="header")
      FlexOut <- merge_at(FlexOut, j = 1:4, part = "footer")
      FlexOut <- align(FlexOut,i=1,j = 1, align="left",part="header")
      FlexOut <- align(FlexOut,i=2,j = 1:4, align="center",part="header")     
      FlexOut <- align(FlexOut,i=1, align="left",part="footer")
      FlexOut <- align(FlexOut, j=1, align="left", part="body")
      FlexOut <- autofit(FlexOut)
      FlexOut <- width(FlexOut, j = ~ V1, width = 4)
    }
    
    outList <- list("table" = outKable, "FlexTable" = FlexOut)
    return(outList)
  }
  
  if(oType == "latex") {
    #redefining rows for latex footnotes
    outTab[1,1] <- paste0("Population (",eYr,")","+")
    outTab[2,1] <- paste0("Population Change (",sYr," to ",eYr, ")","+")
    outTab[3,1] <- paste0("Total Employment (",eYr,")","+")
    outTab[4,1] <- paste0("Median Household Income","^")
    outTab[5,1] <- paste0("Median House Value","^")
    outTab[6,1] <- paste0("Percentage of Population with Incomes lower than the Poverty Line","^")
    outTab[7,1] <- paste0("Percentage of Population Born in Colorado","^")
    
    add_mat <- matrix(" ",nrow=2,ncol=nCol)
    add_mat[1,1] <- "+Source: State Demography Office"
    add_mat[2,1] <- paste0("^",captionSrc("ACS",ACS))
    
    outTab <- rbind(outTab,add_mat)
    

    
        if(nchar(placename) == 0) {
      outKable <- outTab %>%
        kable(digits=1,
              row.names=FALSE,
              align="lrr",
              col.names = names_spaced,
              caption="Community Quick Facts",
              format ="latex", booktabs=TRUE,escape=TRUE) %>%
        kable_styling(latex_options=c("scale_down","HOLD_position"),font_size=10) %>%
        row_spec(0, align="c") %>%
        column_spec(1, width = "5in") 
      
    }  else  {
      outKable <- outTab %>%
      kable(digits=1,
            row.names=FALSE,
            align="lrrr",
            col.names = names_spaced,
            caption="Community Quick Facts",
            format ="latex", booktabs=TRUE, escape=TRUE) %>%
        kable_styling(latex_options=c("scale_down","HOLD_position"),font_size=10) %>%
        row_spec(0, align="c") %>%
        column_spec(1, width = "5in") 
    }
    OutTxt <- " "
    if(listID$multiCty == "T") {
      OutTxt <- "Note: For municipalities in multiple counties, comparison data from the largest county is displayed."
    }
    
    
    outList <- list("table" = outKable, "text" = OutTxt)
    return(outList)
  }
  
 
}