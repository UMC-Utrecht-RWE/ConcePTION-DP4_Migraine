
create_age_band <- function(ages, age_bands) {
  # Extract the lower and upper limits of the age bands
  lower_limits <- age_bands[-length(age_bands)]
  upper_limits <- age_bands[-1] - 1
  upper_limits[length(upper_limits)] <- upper_limits[length(upper_limits)] + 1  # Make last band inclusive
  
  # Create labels for the age bands
  labels <- paste(lower_limits, upper_limits, sep = "-")
  
  # Use cut() to categorize ages into bands
  age_band <- cut(ages, breaks = age_bands, labels = labels, include.lowest = TRUE, right = FALSE)
  
  return(age_band)
}


