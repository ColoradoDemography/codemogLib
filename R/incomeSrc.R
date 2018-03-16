#' incomeSrc Displays a table showinf sources of incomes
#'
#' @param level the data level from input$level
#' @param listID the list containing place id and Place names
#' @param ACS Specifies the ACS data set to be used, reads curACS from Shiny program
#' @param oType output type html table or latex table
#' @return kable formatted  table and data file
#' @export

incomeSrc <- function(level, listID, ACS, oType) {
  # Collecting place ids from  idList, setting default values
  # Currently Output Counties
 
  if(level == "Municipalities/Places") {
    if(listID$PlFilter == "F") {
      fipslist <- listID$plNum
      fipsname <- listID$plName
    } else {
      fipslist <- listID$ctyNum
      fipsname <- listID$ctyName
    }
  }
  if(level == "Counties") {
     fipslist <- listID$ctyNum
     fipsname <- listID$ctyName
  }
  
  
  state="08"
  
  #County Level Household Income Counts
  incC51=codemog_api(data="b19051", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  incC54=codemog_api(data="b19054", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  incC55=codemog_api(data="b19055", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  incC56=codemog_api(data="b19056", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  incC57=codemog_api(data="b19057", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  incC59=codemog_api(data="b19059", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  
  names(incC51)[c(8,9,10)] <- c("total", "in", "out")
  names(incC54)[c(8,9,10)] <- c("total", "in", "out")
  names(incC55)[c(8,9,10)] <- c("total", "in", "out")
  names(incC56)[c(8,9,10)] <- c("total", "in", "out")
  names(incC57)[c(8,9,10)] <- c("total", "in", "out")
  names(incC59)[c(8,9,10)] <- c("total", "in", "out")
  
  incCty <- do.call("rbind", list(incC51, incC54, incC55, incC56, incC57,  incC59))
  incCty$ID <- seq.int(nrow(incCty))
  incCty$type <- c("With earnings","With interest, dividends or net rental income", 
                   "With Social Security income", "With Supplemental Security Income (SSI)","With cash public assistance income", 
                   "With retirement income")
  
  # County Level MOE Counts
  incC51_moe=codemog_api(data="b19051_moe", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  incC54_moe=codemog_api(data="b19054_moe", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  incC55_moe=codemog_api(data="b19055_moe", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  incC56_moe=codemog_api(data="b19056_moe", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  incC57_moe=codemog_api(data="b19057_moe", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  incC59_moe=codemog_api(data="b19059_moe", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  
  names(incC51_moe)[c(8,9,10)] <- c("total_moe", "in_moe", "out_moe")
  names(incC54_moe)[c(8,9,10)] <- c("total_moe", "in_moe", "out_moe")
  names(incC55_moe)[c(8,9,10)] <- c("total_moe", "in_moe", "out_moe")
  names(incC56_moe)[c(8,9,10)] <- c("total_moe", "in_moe", "out_moe")
  names(incC57_moe)[c(8,9,10)] <- c("total_moe", "in_moe", "out_moe")
  names(incC59_moe)[c(8,9,10)] <- c("total_moe", "in_moe", "out_moe")
  
  incCty_moe <- do.call("rbind", list(incC51_moe, incC54_moe, incC55_moe, incC56_moe, incC57_moe,  incC59_moe))
  incCty_moe$ID <- seq.int(nrow(incCty_moe))
  incCty_moe$type <- c("With earnings","With interest, dividends or net rental income", 
                       "With Social Security income", "With Supplemental Security Income (SSI)","With cash public assistance income", 
                       "With retirement income")
  
  ctyHH <- as.numeric(unique(incCty$total))
  ctyHH_moe <- as.numeric(unique(incCty_moe$total_moe))
  
  #combining files
  incCty2 <- incCty[,c(11,12,9)]
  incCty_moe2 <- incCty_moe[,c(11,12,9)]
  incCtyHH <- merge(incCty2, incCty_moe2, by="ID")
  incCtyHH <- incCtyHH[,c(1:3,5)]
  names(incCtyHH) <- c("ID","type","total","total_moe")
  totRow <- data.frame(ID = 0,
                       type="All Households",
                       total=ctyHH,
                       total_moe=ctyHH_moe)
  incCtyHH <- rbind(totRow,incCtyHH)
  
  incCtyHH$pct <- percent((as.numeric(incCtyHH$total)/as.numeric(ctyHH))*100)
  incCtyHH$pct_moe <- percent((as.numeric(incCtyHH$total_moe)/as.numeric(ctyHH))*100)
  
  # County HH Income Value
  mincC00=codemog_api(data="b20003", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") # All household Incomes
  mincC61=codemog_api(data="b19061", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  mincC64=codemog_api(data="b19064", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  mincC65=codemog_api(data="b19065", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  mincC66=codemog_api(data="b19066", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  mincC67=codemog_api(data="b19067", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  mincC69=codemog_api(data="b19069", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  
  
  names(mincC61)[8] <- "Agg_Income"
  names(mincC64)[8] <- "Agg_Income"
  names(mincC65)[8] <- "Agg_Income"
  names(mincC66)[8] <- "Agg_Income"
  names(mincC67)[8] <- "Agg_Income"
  names(mincC69)[8] <- "Agg_Income"
  
  
  mincCty <- do.call("rbind", list(mincC61, mincC64, mincC65, mincC66, mincC67,  mincC69))
  mincCty$ID <- seq.int(nrow(mincCty))
  mincCty$type <- c("With earnings","With interest, dividends or net rental income", 
                    "With Social Security income", "With Supplemental Security Income (SSI)","With cash public assistance income", 
                    "With retirement income")
  
  mincC00_moe=codemog_api(data="b20003_moe", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") # All household Incomes
  mincC61_moe=codemog_api(data="b19061_moe", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  mincC64_moe=codemog_api(data="b19064_moe", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  mincC65_moe=codemog_api(data="b19065_moe", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  mincC66_moe=codemog_api(data="b19066_moe", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  mincC67_moe=codemog_api(data="b19067_moe", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  mincC69_moe=codemog_api(data="b19069_moe", db=ACS, geonum=paste("1", "08", fipslist, sep=""),meta="no") 
  
  
  names(mincC61_moe)[8] <- "Agg_Income_moe"
  names(mincC64_moe)[8] <- "Agg_Income_moe"
  names(mincC65_moe)[8] <- "Agg_Income_moe"
  names(mincC66_moe)[8] <- "Agg_Income_moe"
  names(mincC67_moe)[8] <- "Agg_Income_moe"
  names(mincC69_moe)[8] <- "Agg_Income_moe"
  
  
  mincCty_moe <- do.call("rbind", list(mincC61_moe, mincC64_moe, mincC65_moe, mincC66_moe, mincC67_moe,  mincC69_moe))
  mincCty_moe$ID <- seq.int(nrow(mincCty_moe))
  mincCty_moe$type <- c("With earnings","With interest, dividends or net rental income", 
                        "With Social Security income", "With Supplemental Security Income (SSI)","With cash public assistance income", 
                        "With retirement income")
  
  # Creating Summary values
  mincCTot <- as.numeric(mincC00[1,8])
  mincCTot_moe <- as.numeric(mincC00_moe[1,8])
  
  
  #combining files
  mincCty2 <- mincCty[,c(9,10,8)]
  mincCty_moe2 <- mincCty_moe[,c(9,10,8)]
  mincCtyHH <- merge(mincCty2, mincCty_moe2, by="ID")
  mincCtyHH <- mincCtyHH[,c(1:3,5)]
  names(mincCtyHH) <- c("ID","type","Agg_Income","Agg_Income_moe")
  totRow <- data.frame(ID = 0,
                       type="All Households",
                       Agg_Income=mincCTot,
                       Agg_Income_moe=mincCTot_moe)
  mincCtyHH <- rbind(totRow,mincCtyHH)
  

  # Producing final File
  mincCtyF <- mincCtyHH[,c(1,3,4)]
  incCtyFin <- merge(incCtyHH, mincCtyF, by ="ID")
  
  incCtyFin$avg_income <- paste0("$",format(round(as.numeric(incCtyFin$Agg_Income)/as.numeric(incCtyFin$total),digits=0),big.mark=","))
  incCtyFin$avg_income_moe <- paste0("$",format(round(as.numeric(incCtyFin$Agg_Income_moe)/as.numeric(incCtyFin$total),digits=0),big.mark=","))
  
  # Building table
  totalHH <- format(as.numeric(incCtyFin[1,3]),big.mark=",")
  totalHH_moe <- format(as.numeric(incCtyFin[1,4]),big.mark=",")
  
  m.IncFin <- as.matrix(incCtyFin[,c(2,5,6,9,10)])
  m.IncFin[1,2] <- totalHH
  m.IncFin[1,3] <- totalHH_moe
  
  # table Heading
  tblHead1 <- c(fipsname = 5)
  # set vector names
  names(tblHead1) <- c(fipsname)
  
  tblHead2 <- c(" " = 1,"Total Households" = 2,"Mean Income" = 2)
  names(tblHead2) <- c(" ","Total Households","Mean Income")
  
  names_spaced <- c("Income Source","Estimate","Margin of Error", "Estimate","Margin of Error")

if(oType == "html") {
    inc_tab <- m.IncFin %>%
    kable(format='html', table.attr='class="cleanTable"',
          digits=1,
          row.names=FALSE,
          align='lrrrr',
          caption="Household Income Source(s)",
          col.names = names_spaced,
          escape = FALSE)  %>%
    kable_styling(bootstrap_options = "condensed",full_width = F,font_size = 12) %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width="3in") %>%
    column_spec(2, width="0.75in") %>%
    column_spec(3, width="0.75in") %>%
    column_spec(4, width="0.75in") %>%
    column_spec(3, width="0.75in") %>%
    add_indent(c(2:7)) %>%
    add_header_above(header=tblHead2) %>%
    add_header_above(header=tblHead1) %>%
    add_footnote(captionSrc("ACS",ACS))
  
  f.outData <- as.data.frame(m.IncFin)
  names(f.outData) <- c(paste0("Income Source: ",fipsname),"Households: Estimate",
                        "Households: MOE", "Average Income: Estimate", "Average Income; MOE")
  outList <- list("table" = inc_tab,"data" = f.outData)
  return(outList)
} 
  
  if(oType == "latex") {
    inc_tab <- m.IncFin %>%
      kable(
            row.names=FALSE,
            align='lrrrr',
            caption="Household Income Source(s)",
            col.names = names_spaced,
            format="latex", booktabs=TRUE)  %>%
      kable_styling(latex_options="HOLD_position") %>%
      row_spec(0, align = "c") %>%
      column_spec(1, width="3in") %>%
      column_spec(2, width="0.75in") %>%
      column_spec(3, width="0.75in") %>%
      column_spec(4, width="0.75in") %>%
      column_spec(3, width="0.75in") %>%
      add_indent(c(2:7)) %>%
      add_header_above(header=tblHead2) %>%
      add_header_above(header=tblHead1) %>%
      add_footnote(captionSrc("ACS",ACS))
     return(inc_tab)
  }
}