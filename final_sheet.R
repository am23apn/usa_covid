library(dplyr)
library(readxl)

# Correct file path
file_path <- "C:/xampp/htdocs/usa_covid/us_state_vaccinations.xlsx"

# Check if the file exists
if (!file.exists(file_path)) {
  stop("File does not exist at the specified path: ", file_path)
}

# Load the data
df1 <- read_excel(file_path)

# Select columns and create df2
df2 <- df1 %>% select(1, 2, 4)

# View df2
if (exists("df2")) {
  View(df2)
} else {
  stop("df2 is not available.")
}
