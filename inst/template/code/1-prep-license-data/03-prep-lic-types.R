# prepare lic table for production (data/lic-clean.csv)
# https://github.com/southwick-associates/salicprep/blob/master/github_vignettes/data-schema.md
# - add lic$type
# - add lic$duration
# - (maybe) add lic$lic_res

## State-specific Notes
# - 

library(tidyverse)

# You may be able to identify lic$type based on logic of a variable supplied by
#  the state (which we request). Otherwise, it will involve manually editing
#  a "data/lic-clean.csv" using, e.g., "E:\Program Files\Rons Editor\Editor.WinGUI.exe".
#  The lic$description column is helpful in this regard, although we may want
#  confirmation with someone from the agency on our decisions.

# 1. load data
lic <- read_csv("data/lic.csv")

# 2. create lic$type

# 3. create lic$duration

# 4. save to new file
write_csv(lic, "data/lic-clean.csv")
