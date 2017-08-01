uni.func <- function(my.col){
  if(is.null(my.col)) return(NULL)
  uni.vec <- trimws(unique(my.col))
  uni.vec[uni.vec %in% c("NA", "", "NA NA") | is.na(uni.vec)] <- "<em>Blank</em>"
  final.vec <- paste(uni.vec, collapse = ",  ")
  return(final.vec)
}