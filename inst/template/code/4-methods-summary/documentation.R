# Get documenation needed for dashboard

# Documentation (docx) to be updated for each dashboard. 
# - To be stored in the O365 group: [state] > Deliverables
# - Template in the O365 group: Analyst Docs > Docs to Share with States > XX Method...docx

# - Duplication Summary (included in Chelsea's documentation.pdf, stored in the 1-prep-license-data folder)
# - License Types: A stripped-down table to include in the summary
# - Potentially other state-specific info

library(tidyverse)
source("colde/params.R")

# License Types
db <- src_sqlite(db_license) 
lic <- tbl(db, "lic") %>% collect()
glimpse(lic)

dir.create("data", showWarnings = FALSE)
write_csv(lic, "data/lic-docx.csv", na = "")
