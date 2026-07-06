library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

df <- read_excel("data-raw/practice-plate-reader.xlsx")
view(df)

# this is a function that extracts the cycle block from the raw data
extract_cycle <- function(df, start_row, cycle_label){
  block <- df[(start_row +3) : (start_row +10), 1:13]
  colnames(block) <- c("Row", as.character(1:12))
  block [,2:13] <- lapply(block[, 2:13], \(x) suppressWarnings(as.numeric(x)))
  block_long <- pivot_longer(block,
                             cols = -Row, 
                             names_to = "Col",
                             values_to = "OD") |>
    mutate(
      well = paste0(Row, Col),
      Cycle = cycle_label
    )
  return(block_long)
}

# this finds where the cycle starts and saves it as a variable
cycle_starts <- which(str_detect(df[[1]], "Cycle"))
 
# loop through each cycle block and extract
all_cycles <- lapply(cycle_starts, function(i) {
  cycle_label <- df[[1]][i]
  extract_cycle(df, i, cycle_label)
}) |> 
  bind_rows()

# converting cycle into time (so we can plot)
all_cycles <- all_cycles |> 
  mutate(Time_min = as.numeric(str_extract(Cycle, "(?<=\\().*?(?=h)")) * 60)

# merging metadata with experimental data
meta <- read_excel("data-raw/practice-plate-reader.xlsx",
                   sheet = "metadata")

# inner join (make sure dplyr is loaded)
data <- inner_join(all_cycles, meta, by = "well")
view(data)

# TO DO
# growth curves for each samples
# box plots for area under the curve


