# Load necessary libraries
if (!requireNamespace("languageserver", quietly = TRUE)) {
  install.packages("languageserver")
}

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

if (!requireNamespace("ggrepel", quietly = TRUE)) {
  install.packages("ggrepel")
}

if (!requireNamespace("maps", quietly = TRUE)) {
  install.packages("maps")
}

if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}

if (!requireNamespace("lintr", quietly = TRUE)) {
  install.packages("lintr")
}

library(lintr)

# Validate the .lintr file
lint_config <- tryCatch(
  read.dcf(".lintr"),
  error = function(e) {
    message("Error reading .lintr file: ", e$message)
    NULL
  }
)

if (!is.null(lint_config)) {
  message("The .lintr file is valid.")
} else {
  message("The .lintr file is invalid.")
}

library(ggplot2)
library(ggrepel)
library(tidyverse)
library(patchwork)

if (!requireNamespace("extrafont", quietly = TRUE)) {
  install.packages("extrafont")
}
library(extrafont)

# Maps and spacial operations
library(maps)
library(sf)
