#' submitPush Writes a script tag with a dataLayer.push command  on clicking of the "View Profile" button
#'
#' @param lvl the selected level (Counties or Municipalities)
#' @param unit the selected location
#' @param topics the contents of the outChk checkboxes
#' @export
#'
submitPush <- function(lvl,unit,topics) {
 browser()
  lvlStr <- paste0("ga('set', 'dimension1' : '",lvl,"');")
  unitStr <-paste0("ga('set', 'dimension2' : '",unit,"');")
  
  if("stats" %in% topics) {
    statsStr <- "ga('set', 'dimension3' : 'yes');"
  }else {
    statsStr <- "ga('set', 'dimension3' : 'no');"
  }  
  if("popf" %in% topics) {
    popfStr <- "ga('set', 'dimension4' : 'yes');"
  }else {
    popfStr <- "ga('set', 'dimension4'  : 'no');"
  }  
  if("pop" %in% topics) {
    popStr <- "ga('set', 'dimension5' : 'yes');"
  }else {
    popStr <- "ga('set', 'dimension5' : 'no');"
  }
  if("popc" %in% topics) {
    popcStr <- "ga('set', 'dimension6' : 'yes');"
  }else {
    popcStr <- "ga('set', 'dimension6' : 'no');"
  }
  if("housing" %in% topics) {
    housingStr <- "ga('set', 'dimension7' : 'yes');"
  }else {
    housingStr <- "ga('set', 'dimension7' : 'no');"
  }  
  if("comm" %in% topics) {
    commStr <- "ga('set', 'dimension8' : 'yes');"
  }else {
    commStr <- "ga('set', 'dimension8' : 'no');"
  }  
  if("emplind" %in% topics) {
    emplindStr <- "ga('set', 'dimension9' : 'yes');"
  }else {
    emplindStr <- "ga('set', 'dimension9' : 'no');"
  } 
  if("emply" %in% topics) {
    emplyStr <- "ga('set', 'dimension10'  : 'yes');"
  }else {
    emplyStr <- "ga('set', 'dimension10' : 'no');"
  }  
    
  #assembling script
  outstr <-paste0(lvlStr,unitStr,statsStr,popfStr,
                  popStr,popcStr,housingStr,commStr,emplindStr,emplyStr)
  
  return(outstr)
}