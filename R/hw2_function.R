## QUESTION: The function should accept any of the offence descriptions found in Offence Level 1 
## and will accept a 2-element vector of postcodes. ----

#' Create a plot showing the correlation in a level 1 offence count between two postcodes by month
#'
#' \code{plot_postcodes_offencelevel1} takes a data.table object of crime data, a character string 
#'  of description in offence level 1, a two-element character vector of SA postcodes, and creates 
#'  a plot of counts in the input offence for two postcodes by month.
#' @param crime_data A data.table object with the following columns: 
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of offence description which can be found in 
#' offence level 1. This character string should be UPPERCASE.
#' @param postcodes A two-element character vector. Each element is an SA postcode. 
#' @export
#' @return  A ggplot object showing the correlation in offence count between the two input postcodes.
#' @examples
#' plot_postcodes_offencelevel1(crime_data, "ACTS INTENDED TO CAUSE INJURY", c(5159, 5173))

plot_postcodes_offencelevel1 <- function(crime_data, offence_description, postcodes) {
  require(data.table)
  require(ggplot2)
  
  # Error catching
  if (length(postcodes) != 2) {
    stop("Please enter two postcodes")
  }
  
  expected_colnames <- c("date", "suburb", "postcode", "offence_level_1", "offence_level_2", 
                         "offence_level_3", "offence_count")
  crime_data_colnames <- colnames(crime_data)
  
  if (!all.equal(expected_colnames, crime_data_colnames)) {
    stop(paste("Input table columns need to match: ",
               paste(expected_colnames, collapse = ", ")))
  }
  
  # Check that the input suburbs and offence description exist in crime_data
  if (any(!postcodes %in% crime_data$postcode) |
      !offence_description %in% crime_data$offence_level_1) {
    stop("The postcodes you have inputted do not exist in the postcode column 
         or the offence description you have inputted does not exist in the 
         offence_level_1 column")
  }
  
  # Make a data table for plotting using data.table transformations
  plot_data <- crime_data[offence_level_1 == offence_description & postcode %in% postcodes
                          , list("total_offence_count" = sum(offence_count))
                          , by = list(month(date), postcode)]
  
  # Transform the plot_data structure to allow us to plot correlations
  x <- postcodes[1]
  y <- postcodes[2]
  
  plot_data[, postcode := plyr::mapvalues(postcode, postcodes, c(x, y))]
  
  plot_data <- dcast(plot_data, month ~ postcode, fun = sum, 
                     fill = 0, value.var = "total_offence_count")
  
  plot_data.m <- melt(plot_data, id.vars = "month", variable.name = "Postcodes")
  
  # Generate the plot
  ggplot(plot_data.m, aes(as.factor(month), value, color = Postcodes)) +
    geom_count() +
    scale_size_area() +
    labs(x = "Month", y = "Offence Counts at Two Postcodes")
}