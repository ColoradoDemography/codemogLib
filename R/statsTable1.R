#' statsTable1 outputs the summary table in the stats section of the dashboard, draws data from the census API
#'
#' @param cty the County  FIPS code, including the state value
#' @param place the Place FIPS Code, including the state value.
#' @param sYr Start Year
#' @param eYr End year
#' @param ACS American Cummunity Survey Data series
#' @param oType Controls the rendering of the table, HTML or Latex
#' @return kable formatted table
#' @export
#'
statsTable1 <- function(cty,place,sYr,eYr,ACS,oType){
  #outputs the top table in the dashboard
 
  #Need to restructure this to support muni_est...
  state <- substr(cty,1,2)
  ctyfips <- substr(cty,3,5)
  if(nchar(place) != 0) {
    placefips <- substr(place,3,7)
    outTab <- matrix("",nrow=7,ncol=4)
    sqlStr <- paste0("SELECT placefips, year, jobs FROM estimates.muni_jobs_long WHERE placefips = ",as.numeric(placefips)," and year = ", eYr,";") 
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
    
    f.muniJobs <- dbGetQuery(con, sqlStr)
    
    #closing the connections
    dbDisconnect(con)
    dbUnloadDriver(drv)
    rm(con)
    rm(drv)
  } else {
    outTab <- matrix("",nrow=7,ncol=3)
  }

  #Population and Change Rows
 #Counties
    tPopyr1c <- county_profile(as.numeric(ctyfips), sYr,"totalpopulation")
    tPopyr2c <- county_profile(as.numeric(ctyfips), eYr,"totalpopulation")
    plNamec <- paste0(tPopyr1c$county," County")
    tJobsc <-  county_jobs(fips=as.numeric(ctyfips), year = eYr) #County
    hhincc <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    MedHHValuec <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")

    Povertyc <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    pctPovertyc <- percent(as.numeric(Povertyc$b17001002)/as.numeric(Povertyc$b17001001)*100)

    Nativec <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state, ctyfips, sep=""), meta="no")
    pctNativec <- percent(as.numeric(Nativec$b05002003)/as.numeric(Nativec$b05002001)*100)
    


    #Cost of Living Index  Removed in V1.
    #    coli=county_coli%>%
    #      filter(countyfips==as.numeric(ctyfips))%>%
    #      mutate(coli_level=paste(coli, level, sep=", "))%>%
    #     select(coli_level)
  
  #places
  if(nchar(place) !=0) {  #Places
    tPopyr1p <- muni_est(as.numeric(placefips), sYr,as.numeric(ctyfips),"totalpopulation")
    tPopyr2p <- muni_est(as.numeric(placefips), eYr,as.numeric(ctyfips),"totalpopulation")
    plNamep <- tPopyr1p$municipality
    hhincp <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    MedHHValuep <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")

    Povertyp <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    pctPovertyp <- percent(as.numeric(Povertyp$b17001002)/as.numeric(Povertyp$b17001001)*100)

    Nativep <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state, placefips, sep=""), meta="no")
    pctNativep <- percent(as.numeric(Nativep$b05002003)/as.numeric(Nativep$b05002001)*100)


    #Cost of Living Index removed in V1
    #    coli=county_coli%>%
    #      filter(countyfips==as.numeric(ctyfips))%>%
    #      mutate(coli_level=paste(coli, level, sep=", "))%>%
    #      select(coli_level)
  }


  #state Values

  tPopyr1ST <- county_profile(300, sYr,"totalpopulation")
  f.tPopyr1ST <- tPopyr1ST %>% summarize(totalpopulation = sum(as.numeric(totalpopulation)))
  tPopyr2ST <- county_profile(300, eYr,"totalpopulation")
  f.tPopyr2ST <- tPopyr2ST %>% summarize(totalpopulation = sum(as.numeric(totalpopulation)))
  
  plNameST <- "Colorado"
  tJobsST <-  county_jobs(fips=300, year = eYr) 
  f.tJobsST <- tJobsST %>%  summarize(totalJobs = sum(as.numeric(totalJobs)))
  
  hhincST <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  MedHHValueST <- codemog_api(data="b25077",db=ACS, geonum=paste("1", state, sep=""), meta="no")
  
  PovertyST <- codemog_api(data="b17001",db=ACS, geonum=paste("1", state, sep=""), meta="no")
  pctPovertyST <- percent(as.numeric(PovertyST$b17001002)/as.numeric(PovertyST$b17001001)*100)
  
  NativeST <- codemog_api(data="b05002",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  pctNativeST <- percent(as.numeric(NativeST$b05002003)/as.numeric(NativeST$b05002001)*100)
  
  #Median Household Income  B18140 is the total median earnings...  from the 2012-2016 ACS API
  hhincST <- codemog_api(data="b19013",db=ACS, geonum=paste("1", state,  sep=""), meta="no")

  #median Househld Value
  MedHHValueST=codemog_api(data="b25077",db=ACS, geonum=paste("1", state,  sep=""), meta="no")
  
  
  popchgc <- as.numeric(tPopyr2c$totalpopulation) - as.numeric(tPopyr1c$totalpopulation)
  popchgST <- as.numeric(f.tPopyr2ST$totalpopulation) - as.numeric(f.tPopyr1ST$totalpopulation)
  if(nchar(place) != 0) {
    popchgp <- as.numeric(tPopyr2p$totalpopulation) - as.numeric(tPopyr1p$totalpopulation)
  }

  #Preparing table

  outTab[1,1] <- paste0("Population (",eYr,")",footnote_marker_alphabet(1))
  outTab[2,1] <- paste0("Population Change (",sYr," to ",eYr, ")",footnote_marker_alphabet(1))
  outTab[3,1] <- paste0("County Employment (",eYr,")",footnote_marker_alphabet(1))
  outTab[4,1] <- paste0("Median Household Income",footnote_marker_alphabet(2))
  outTab[5,1] <- paste0("Median House Value",footnote_marker_alphabet(2))
  outTab[6,1] <- paste0("Percentage of Population with Incomes lower than the Poverty Line",footnote_marker_alphabet(2))
  outTab[7,1] <- paste0("Percentage of Population Born in Colorado",footnote_marker_alphabet(2))
  
  if(nchar(place) != 0){
    #place
    outTab[1,2] <- format(as.numeric(tPopyr2p$totalpopulation),nsmall=0, big.mark=",")
    outTab[2,2] <- format(as.numeric(popchgp),nsmall=0, big.mark=",")
    outTab[3,2] <- format(round(as.numeric(f.muniJobsp$jobs),digits=0),nsmall=0, big.mark=",")
    outTab[4,2] <- paste0("$",format(as.numeric(hhincp$b19013001),nsmall=0, big.mark=","))
    outTab[5,2] <- paste0("$",format(as.numeric(MedHHValuep$b25077001),nsmall=0, big.mark=","))
    outTab[6,2] <- pctPovertyp
    outTab[7,2] <- pctNativep
    
    #County
    outTab[1,3] <- format(as.numeric(tPopyr2c$totalpopulation),nsmall=0, big.mark=",")
    outTab[2,3] <- format(as.numeric(popchgc),nsmall=0, big.mark=",")
    outTab[3,3] <- format(round(as.numeric(tJobsc$totalJobs),digits=0),nsmall=0, big.mark=",")
    outTab[4,3] <- paste0("$",format(as.numeric(hhincc$b19013001),nsmall=0, big.mark=","))
    outTab[5,3] <- paste0("$",format(as.numeric(MedHHValuec$b25077001),nsmall=0, big.mark=","))
    outTab[6,3] <- pctPovertyc
    outTab[7,3] <- pctNativec
    
    #State
    outTab[1,4] <- format(as.numeric(tPopyr2ST$totalpopulation),nsmall=0, big.mark=",")
    outTab[2,4] <- format(as.numeric(popchgST),nsmall=0, big.mark=",")
    outTab[3,4] <- format(round(as.numeric(f.tJobsST$totalJobs),digits=0),nsmall=0, big.mark=",")
    outTab[4,4] <- paste0("$",format(as.numeric(hhincST$b19013001),nsmall=0, big.mark=","))
    outTab[5,4] <- paste0("$",format(as.numeric(MedHHValueST$b25077001),nsmall=0, big.mark=","))
    outTab[6,4] <- pctPovertyST
    outTab[7,4] <- pctNativeST
  }  else {
    #County
    outTab[1,2] <- format(as.numeric(tPopyr2c$totalpopulation),nsmall=0, big.mark=",")
    outTab[2,2] <- format(as.numeric(popchgc),nsmall=0, big.mark=",")
    outTab[3,2] <- format(round(as.numeric(tJobsc$totalJobs),digits=0),nsmall=0, big.mark=",")
    outTab[4,2] <- paste0("$",format(as.numeric(hhincc$b19013001),nsmall=0, big.mark=","))
    outTab[5,2] <- paste0("$",format(as.numeric(MedHHValuec$b25077001),nsmall=0, big.mark=","))
    outTab[6,2] <- pctPovertyc
    outTab[7,2] <- pctNativec
    
    #State
    outTab[1,3] <- format(as.numeric(f.tPopyr2ST$totalpopulation),nsmall=0, big.mark=",")
    outTab[2,3] <- format(as.numeric(popchgST),nsmall=0, big.mark=",")
    outTab[3,3] <- format(round(as.numeric(f.tJobsST$totalJobs),digits=0),nsmall=0, big.mark=",")
    outTab[4,3] <- paste0("$",format(as.numeric(hhincST$b19013001),nsmall=0, big.mark=","))
    outTab[5,3] <- paste0("$",format(as.numeric(MedHHValueST$b25077001),nsmall=0, big.mark=","))
    outTab[6,3] <- pctPovertyST
    outTab[7,3] <- pctNativeST 
  }
  


  # Create Column headings
  if(nchar(place) != 0) {
    names_spaced <- c(" ",plNamep, plNamec,plNameST)
  } else {
    names_spaced <- c(" ",plNamec,plNameST)
  }
  


  if(oType == "html") {
    if(nchar(place) != 0) {
      outKable <-  kable(outTab, format='html', table.attr='class="cleanTab"',
                         digits=1,
                         row.names=FALSE,
                         align='lrrr',
                         col.names = names_spaced,
                         caption="Community Quick Facts",
                         escape = FALSE)   %>%
        kable_styling() %>%
        column_spec(1, width = "3in") %>%
        column_spec(2, width = "0.5in") %>%
        column_spec(3, width = "0.5in") %>%
        column_spec(4, width = "0.5in") %>%
        footnote(alphabet=c("Source: State Demography Office",captionSrc("ACS",ACS)))
    } else {
      outKable <-  kable(outTab, format='html', table.attr='class="cleanTab"',
                         digits=1,
                         row.names=FALSE,
                         align='lrr',
                         col.names = names_spaced,
                         caption="Community Quick Facts",
                         escape = FALSE)   %>%
        kable_styling() %>%
        column_spec(1, width = "3in") %>%
        column_spec(2, width = "0.5in") %>%
        column_spec(3, width = "0.5in") %>%
        footnote(alphabet=c("Source: State Demography Office",captionSrc("ACS",ACS)))
    }
  }
  if(oType == "latex") {
    #Revising Footnotes
    outTab[1,1] <- paste0("Population (",eYr,")*")
    outTab[2,1] <- paste0("Population Change (",sYr," to ",eYr, ")*")
    outTab[3,1] <- paste0("County Employment (",eYr,")*")
    outTab[4,1] <- paste0("Median Household Income+")
    outTab[5,1] <- paste0("Median House Value+")
    outTab[6,1] <- paste0("Percentage of Population with Incomes lower than the Poverty Line+")
    outTab[7,1] <- paste0("Percentage of Population Born in Colorado+")
   if(nchar(place) != 0) {
     add_mat <- matrix(nrow=2,ncol=4)
   } else{
     add_mat <- matrix(nrow=2,ncol=3)
   }

    add_mat[1,1] <- "*Source: State demography Office"
    add_mat[2,1] <- paste0("+",captionSrc("ACS",ACS))

    outTab <- rbind(outTab,add_mat)
 if(nchar(place) != 0) {
   outKable <- outTab %>%
     kable(caption="Community Quick Facts",align="lr4r",
           col.names = names_spaced,
           format ="latex", booktabs=TRUE) %>%
     kable_styling(latex_options="HOLD_position") %>%
     column_spec(1, width = "4in") %>%
     column_spec(2, width = "1in") %>%
     column_spec(3, width = "1in") %>%
     column_spec(4, width = "1in")
    }  else {
    outKable <- outTab %>%
      kable(caption="Community Quick Facts",align="lrr",
            col.names = names_spaced,
            format ="latex", booktabs=TRUE) %>%
      kable_styling(latex_options="HOLD_position") %>%
      column_spec(1, width = "4in") %>%
      column_spec(2, width = "1in") %>%
      column_spec(3, width = "1in")
       }
  }
  return(outKable)
}
