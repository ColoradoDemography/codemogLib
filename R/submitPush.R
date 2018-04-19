#' SubmitPush Writes a script tag with a dataLayer.push command  on clicking of the "View Profile" button
#'
#' @param lvl the selected level (Counties or Municipalities)
#' @param unit the selected location
#' @param topics the contents of the outChk checkboxes
#' @export
#'
submitPush <- function(lvl,unit,topics) {
  
  browser()
  lvlStr <- paste0("'level' : '",lvl,"'")
  unitStr <-paste0("'unit' : '",unit,"'")
  
  if("stats" %in% topics) {
    statsStr <- "'stats' : true"
  }else {
    statsStr <- "'stats' : false"
  }  
  if("popf" %in% topics) {
    popfStr <- "'popf' : true"
  }else {
    popfStr <- "'popf' : false"
  }  
  if("pop" %in% topics) {
    popStr <- "'pop' : true"
  }else {
    popStr <- "'pop' : false"
  }
  if("popc" %in% topics) {
    popcStr <- "'popc' : true"
  }else {
    popcStr <- "'popc' : false"
  }
  if("housing" %in% topics) {
    housingStr <- "'housing' : true"
  }else {
    housingStr <- "'housing' : false"
  }  
  if("comm" %in% topics) {
    commStr <- "'comm' : true"
  }else {
    commStr <- "'comm' : false"
  }  
  if("emplind" %in% topics) {
    emplindStr <- "'emplind' : true"
  }else {
    emplindStr <- "'emplind' : false"
  } 
  if("emply" %in% topics) {
    emplyStr <- "'emply' : true"
  }else {
    emplyStr <- "'emply' : false"
  }  
    
  #assembling script
  outstr <-paste0("<script>dataLayer.push({",lvlStr,",",unitStr,",",statsStr,",",popfStr,",",
                  popStr,",",popcStr,",",housingStr,",",commStr,",",emplindStr,",",emplyStr,", 'event':'viewProfile'})</script>")
  
  return(outstr)
}