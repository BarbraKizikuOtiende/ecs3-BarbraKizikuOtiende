# Loading packages
library(targets)
tar_option_set(
  packages = c(
    "data.table",
    "duckplyr",
    "dplyr",
    "readr",
    "stringr",
    "forcats",
    "janitor",
    "pointblank",
    "decoder",
    "ggplot2",
    "purrr",
    "fs",
    "zip",
    "leaflet"
  )
)
# Patient Data
 if (!fs::file_exists("data.zip")) {
curl::curl_download(
"https://github.com/eribul/cs/raw/refs/heads/main/data.zip",
"data.zip",
quiet = FALSE
)
}
# Making sure to not share any sensitive data usethis::edit_git_ignore("project")

# Loading the data file
patients <-
readr::read_csv(unz("data.zip", "data-fixed/patients.csv")) |>
setDT() |>
setkey(id)

# Removing missing data 
patients <- janitor::remove_empty(patients, quiet = FALSE)
# A column with only one constant value is also not very interesting
patients <- janitor::remove_constant(patients, quiet = FALSE)

# Performing Data quality checks (Set from expectations)
checks <-
patients |>
create_agent(label = "Validate patient demographic data") |>
col_vals_between(
where(is.Date),
as.Date("1900-01-01"),
as.Date(Sys.Date()),
na_pass = TRUE,
label = "Ensure all date variables fall between 1900 and today"
) |>
col_vals_gte(
deathdate,
vars(birthdate),
na_pass = TRUE,
label = "Ensure death date occurs after or on birth date"
) |>
col_vals_regex(
ssn, # Social security number
"[0-9]{3}-[0-9]{2}-[0-9]{4}$",
label = "Ensure SSN follows the format XXX-XX-XXXX"
) |>
col_is_integer(
id,
label = "Ensure patient ID values are integer"
) |>
interrogate()

# Exporting my report
export_report(checks, "patient_validation.html") 
# Factors
patients[, .N, marital]
patients[,
marital := factor(
marital,
levels = c("S", "M", "D", "W"),
labels = c("Single", "Married", "Divorced", "Widowed")
)
]

# Adding Factor Levels : identifying character variables that likely represent factors
fctr_candidates <-
patients[, which(lapply(.SD, uniqueN) < 10), .SDcols = is.character] |>
names()
patients[,
lapply(.SD, \(x) paste(unique(x), collapse = ", ")),
.SDcols = fctr_candidates
] |>
glimpse()
patients[,
names(.SD) := lapply(.SD, as.factor),
.SDcols = c("prefix", "suffix", "race", "ethnicity", "gender", "state")
]
patients[, .N, .(gender, race, state)][order(N)]
# reclassifying race
patients[, race := forcats::fct_lump_prop(race, prop = 0.05)]
# Calculating age
patients[, age := as.integer((as.IDate(Sys.Date()) - birthdate)) %/% 365.241]
patients[, hist(age)]
patients[is.na(deathdate), hist(age)] # People alive

# Unzipping the file payer_transitions inorder to know the ages of people living in the US from 1900 till Today (09/03/2026)
unzip("data.zip", files = "data-fixed/payer_transitions.csv")
lastdate <-
duckplyr::read_csv_duckdb("data-fixed/payer_transitions.csv") |>
summarise(lastdate = max(end_date)) |>
collect() |>
pluck("lastdate") |>
as.Date()

# Names
patients[, 
  (names(.SD)) := lapply(.SD, \(x) replace_na(as.character(x), "")),
  .SDcols = c("prefix", "middle")
]
patients[,
full_name := paste(
prefix,
first,
middle,
last,
fifelse(!suffix %in% c("", NA), paste0(", ", suffix), "")
)
]
patients[, full_name]

# Removing white spaces from all characters
patients[, names(.SD) := lapply(.SD, trimws), .SDcols = is.character]
# removing duplicated spaces between names 
patients[, full_name := stringr::str_replace(full_name, " ", " ")]
# removes the following columns from patients
patients[, c("prefix", "first", "middle", "last", "suffix", "maiden") := NULL]
# Checking for necessary data (drivers lisence)
patients[, driver := !is.na(drivers)][, drivers := NULL]

# Visualising how representative the sample is
install.packages("leaflet")
library(leaflet)
leaflet(data = patients) |>
addTiles() |>
addMarkers(~lon, ~lat, label = ~full_name)

# linking data to the procedures table.
zip::unzip("data.zip")
fs::dir_create("data-parquet")
csv2parquet <- function(file) {
new_file <-
file |>
stringr::str_replace("-fixed", "-parquet") |>
stringr::str_replace(".csv", ".parquet")
duckplyr::read_csv_duckdb(file) |>
duckplyr::compute_parquet(new_file)
}
fs::dir_ls("data-fixed/") |>
purrr::walk(csv2parquet, .progress = TRUE)
fs::dir_delete("data-fixed")
procedures <- duckplyr::read_parquet_duckdb("data-parquet/procedures.parquet")
procedures <-
procedures |>
select(patient, reasoncode_icd10, start) |>
filter(!is.na(reasoncode_icd10)) |>
collect()
setDT(procedures, key = "patient")

library(data.table)
proc_n_adults <-
procedures[
patients[, .(id, birthdate = as.IDate(birthdate))],
on = c(patient = "id")
] |>
_[year - year(birthdate) >= 18L, .N, .(reasoncode_icd10, year)]

icd <- as.data.table(decoder::icd10se)
cond_by_year <- icd[proc_n_adults, on = c(key = "reasoncode_icd10")]
exists("proc_n_adults")
cond_by_year <- setDT(decoder::icd10se)[
proc_n_adults,
on = c(key = "reasoncode_icd10")
]

# Visualisations
library(ggplot2)
top5 <- cond_by_year[, .(N = sum(N)), .(value)][order(-N)][1:5, value]
ggplot(cond_by_year[.(top5), on = "value"], aes(year, N, color = value)) +
geom_line() +
theme(legend.position = "bottom") +
guides(color = guide_legend(ncol = 1)) +
scale_color_discrete(
labels = function(x) str_wrap(x, width = 40)
)
