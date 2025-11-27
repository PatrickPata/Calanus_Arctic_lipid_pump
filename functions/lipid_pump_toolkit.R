# Toolkit for lipid pump analysis
# P. Pata
# November 26, 2025


`%notin%` <- Negate(`%in%`)

# Function to display the images
open_images <- function(filenames) {
  for (file in filenames) {
    if (file.exists(file)) {
      if (.Platform$OS.type == "windows") {
        shell.exec(file)  # Opens the image in the default viewer on Windows
      } else {
        system(paste("xdg-open", shQuote(file)), wait = FALSE)  # Linux
        system(paste("open", shQuote(file)), wait = FALSE)  # macOS
      }
    } else {
      message(paste("File not found:", file))
    }
  }
}


calc_r2 <- function(x, y) {
  if (is.numeric(x) && is.numeric(y) && length(na.omit(x)) > 1 && length(na.omit(y)) > 1) {
    df <- data.frame(x = x, y = y)
    model <- lm(y ~ x, data = df, na.action = na.omit)
    return(round(summary(model)$r.squared, 3))
  } else {
    return(NA) # Return NA if column is not numeric or has insufficient data
  }
}