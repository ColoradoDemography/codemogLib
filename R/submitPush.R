#' submitPush Writes a script tag with a dataLayer.push command  on clicking of the "View Profile" button
#'
#' @param lvl the selected level (Counties or Municipalities)
#' @param unit the selected location
#' @param topics the contents of the outChk checkboxes
#' @export
#'
submitPush <- function(lvl,unit,topics) {

  lvlStr <- paste0("'Level' : '",lvl,"'")
  unitStr <-paste0("'Unit' : '",unit,"'")
  
  if("stats" %in% topics) {
    statsStr <- "'Basic_Stats' : 'yes'"
  }else {
    statsStr <- "'Basic_Stats' : 'no'"
  }  
  if("popf" %in% topics) {
    popfStr <- "'Pop_Forecast' : 'yes'"
  }else {
    popfStr <- "'Pop_Forecast' : 'no'"
  }  
  if("pop" %in% topics) {
    popStr <- "'Pop_Age' : 'yes'"
  }else {
    popStr <- "'Pop_Age' : 'no'"
  }
  if("popc" %in% topics) {
    popcStr <- "'Pop_Other' : 'yes'"
  }else {
    popcStr <- "'Pop_Other' : 'no'"
  }
  if("housing" %in% topics) {
    housingStr <- "'Housing' : 'yes'"
  }else {
    housingStr <- "'Housing' : 'no'"
  }  
  if("comm" %in% topics) {
    commStr <- "'Commuting' : 'yes'"
  }else {
    commStr <- "'Commuting' : 'no'"
  }  
  if("emplind" %in% topics) {
    emplindStr <- "'Employ_Ind' : 'yes'"
  }else {
    emplindStr <- "'Employ_Ind' : 'no'"
  } 
  if("emply" %in% topics) {
    emplyStr <- "'Employ_Wage' : 'yes'"
  }else {
    emplyStr <- "'Employ_Wage' : 'no'"
  }  
    
  #assembling script
  outstr <-paste0("dataLayer.push({",lvlStr,",",unitStr,",",statsStr,",",popfStr,",",
                  popStr,",",popcStr,",",housingStr,",",commStr,",",emplindStr,",",emplyStr,", 'event':'viewProfile'})")
  
  return(outstr)
}