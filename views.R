# get_free_periods -------------------------------------------------------------
get_free_periods <- function(class_data, layout = 1L, transpose = FALSE)
{
  finalise <- function(m) as.data.frame(m)
  
  weekdays <- levels(class_data$weekday_hr)
  weekdays_short <- gsub("-", "", weekdays)
  
  named_classes <- stats::setNames(nm = unique(class_data$class))
  
  free_periods <- lapply(named_classes, function(class) {
    setdiff(weekdays, class_data$weekday_hr[which(class_data$class == class)])
  })
  
  # Arrange free periods in a matrix
  m <- do.call(cbind, lapply(free_periods, function(x) weekdays %in% x))
  
  if (layout == 1L) {
    mode(m) <- "character"
    m[m == "FALSE"] <- "|"
    m[m == "TRUE"] <- "X"
  }
  
  if (layout == 2L) {
    mode(m) <- "integer"
  }
  
  rownames(m) <- weekdays_short
  as.data.frame((if (transpose) t else identity)(m))
}
