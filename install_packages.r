# Install required packages for the simple movie analysis
# Run this script first before running the main simple_analysis.r script

# List of required packages
required_packages <- c(
  "dplyr",
  "ggplot2"
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      cat("Installing package:", package, "\n")
      install.packages(package, dependencies = TRUE)
    } else {
      cat("Package", package, "is already installed\n")
    }
  }
}

# Install packages
cat("Installing required packages for movie recommendation system...\n")
install_if_missing(required_packages)

# Verify installation
cat("\nVerifying package installation...\n")
for (package in required_packages) {
  if (require(package, character.only = TRUE, quietly = TRUE)) {
    cat("✓", package, "loaded successfully\n")
  } else {
    cat("✗", package, "failed to load\n")
  }
}

cat("\nPackage installation complete!\n")
cat("You can now run the main simple_analysis.r script.\n")