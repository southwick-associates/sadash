# load raw data into sqlite

## State-specific Notes
# - 

library(tidyverse)
library(DBI)
library(salicprep)
source("code/params.R")

## state-specific file identification
files <- list.files(dir_raw, full.names = TRUE)
lic_file = files[1]
cust_file = files[2]
sale_file = files[3]

# Load Customers ----------------------------------------------------------------

# check format for import
read_lines(cust_file, n_max = 2)

## note: all the read functions for pulling in data will be state-specific
cust <- read_delim(
    cust_file,
    progress = FALSE,
    col_types = cols(.default = col_character())
)
problems(cust) # looking for import problems
check_raw_lines(cust, cust_file) # difference in row counts should usually be 1

cust <- mutate(cust, raw_cust_id = row_number())
glimpse(cust)

# Load Sales --------------------------------------------------------------------

read_lines(sale_file, n_max = 2)

sale <- read_delim(
    sale_file,
    progress = FALSE,
    col_types = cols(.default = col_character())
)
problems(sale)
check_raw_lines(sale, sale_file)

sale <- mutate(sale, raw_sale_id = row_number())
glimpse(sale)

# Load License Types -----------------------------------------------------------

read_lines(lic_file, n_max = 2)

lic <- read_csv(lic_file, col_types = cols(.default = col_character))
problems(lic)
check_raw_lines(cust, cust_file)

lic <- mutate(lic, raw_lic_id = row_number())
glimpse(lic)

# Write To Sqlite ---------------------------------------------------------------

con <- dbConnect(RSQLite::SQLite(), db_raw)
dbWriteTable(con, "lic", lic, overwrite = TRUE)
dbWriteTable(con, "cust", cust, overwrite = TRUE)
dbWriteTable(con, "sale", sale, overwrite = TRUE)
dbDisconnect(con)
