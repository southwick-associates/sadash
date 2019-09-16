# Get documenation needed for dashboard

# Documentation (docx) to be updated for each dashboard. 
# - To be stored in the O365 group: [state] > Deliverables
# - Template in the O365 group: Analyst Docs > Docs to Share with States > XX Method...docx

# - Duplication Summary (included in Chelsea's documentation.pdf, stored in the 1-prep-license-data folder)
# - License Types: A stripped-down table to include in the summary
# - Potentially other state-specific info

library(tidyverse)
source("params.R")

## License Types
lic <- tbl(db_license, "lic") %>%
    select(lic_id, description, type, priv, subtype, basic) %>%
    collect()
write_csv(lic, "4-methods-summary/lic-docx.csv", na = "")
