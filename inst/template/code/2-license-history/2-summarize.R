# produce customer summaries for before and after history database production
# - optional step (might be useful for reference/validation):

# for running parameterized by-summary.R
# - summary.R shouldn't require state-specific tweaking
run_summary <- function(data_src = "db_license") {
    rmarkdown::render(
        input = file.path("code/2-license-history/by-summary.R"),
        output_file = file.path("summary", paste0(data_src, ".html")),
        knit_root_dir = getwd()
    )
}
run_summary("db_license") # ignores multi-year/lifetime carry-over
run_summary("db_history")
