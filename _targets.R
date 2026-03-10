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
# Loading Patient's Data
list(
tar_target(
  zip_file,
  {
    if (!fs::file_exists("data.zip")) {
      curl::curl_download(
        "https://github.com/eribul/cs/raw/refs/heads/main/data.zip",
        "data.zip"
      )
    }
    "data.zip"
  },
  format = "file"
),
tar_target(
  patients_raw,
  readr::read_csv(unz(zip_file, "data-fixed/patients.csv"))
),

tar_target(
  patients_clean,
  {
    patients <- setDT(patients_raw)
    setkey(patients, id)

    patients <- janitor::remove_empty(patients, quiet = TRUE)
    patients <- janitor::remove_constant(patients, quiet = TRUE)

    patients
  }
),
# Validation checks
tar_target(
  validation_report,
  {
    checks <-
      patients_clean |>
      create_agent(label = "Validate patient demographic data") |>
      col_vals_between(
        where(is.Date),
        as.Date("1900-01-01"),
        as.Date(Sys.Date()),
        na_pass = TRUE
      ) |>
      col_vals_gte(
        deathdate,
        vars(birthdate),
        na_pass = TRUE
      ) |>
      col_vals_regex(
        ssn,
        "[0-9]{3}-[0-9]{2}-[0-9]{4}$"
      ) |>
      col_is_integer(id) |>
      interrogate()

    export_report(checks, "patient_validation.html")
  },
  format = "file"
),
 
# Factor processing
tar_target(
  patients_factors,
  {
    patients <- copy(patients_clean)

    patients[
      ,
      marital := factor(
        marital,
        levels = c("S","M","D","W"),
        labels = c("Single","Married","Divorced","Widowed")
      )
    ]

    patients[
      ,
      names(.SD) := lapply(.SD, as.factor),
      .SDcols = c("prefix","suffix","race","ethnicity","gender","state")
    ]

    patients[, race := forcats::fct_lump_prop(race, prop = 0.05)]

    patients
  }
),

# Age Calculation
tar_target(
  patients_age,
  {
    patients <- copy(patients_factors)

    patients[
      ,
      age := as.integer((as.IDate(Sys.Date()) - birthdate)) %/% 365.241
    ]

    patients
  }
),

# Name Cleaning
tar_target(
  patients_named,
  {
    patients <- copy(patients_age)

    patients[
      ,
      (names(.SD)) := lapply(.SD, \(x) replace_na(as.character(x), "")),
      .SDcols = c("prefix","middle")
    ]

    patients[
      ,
      full_name := paste(
        prefix,
        first,
        middle,
        last,
        fifelse(!suffix %in% c("", NA), paste0(", ", suffix), "")
      )
    ]

    patients[
      ,
      names(.SD) := lapply(.SD, trimws),
      .SDcols = is.character
    ]

    patients[
      ,
      full_name := stringr::str_replace(full_name, "  ", " ")
    ]

    patients[
      ,
      c("prefix","first","middle","last","suffix","maiden") := NULL
    ]

    patients[
      ,
      driver := !is.na(drivers)
    ][
      ,
      drivers := NULL
    ]

    patients
  }
),

# Mapping
tar_target(
  patient_map,
  {
    leaflet(data = patients_named) |>
      addTiles() |>
      addMarkers(~lon, ~lat, label = ~full_name)
  }
),
# Converting CSV to parquet
tar_target(
  parquet_files,
  {
    zip::unzip(zip_file)

    fs::dir_create("data-parquet")

    csv2parquet <- function(file){

      new_file <-
        file |>
        stringr::str_replace("-fixed","-parquet") |>
        stringr::str_replace(".csv",".parquet")

      duckplyr::read_csv_duckdb(file) |>
        duckplyr::compute_parquet(new_file)
    }

    fs::dir_ls("data-fixed/") |>
      purrr::walk(csv2parquet)

    fs::dir_delete("data-fixed")

    fs::dir_ls("data-parquet/")
  },
  format = "file"
),
# Procedures
tar_target(
  procedures,
  {
    duckplyr::read_parquet_duckdb("data-parquet/procedures.parquet") |>
      select(patient, reasoncode_icd10, start) |>
      filter(!is.na(reasoncode_icd10)) |>
      collect() |>
      setDT(key = "patient")
  }
),
# Adult procedures
tar_target(
  proc_n_adults,
  {
    procedures[
      patients_named[,.(id,birthdate = as.IDate(birthdate))],
      on = c(patient = "id")
    ][
      year(start) - year(birthdate) >= 18L,
      .N,
      .(reasoncode_icd10, year = year(start))
    ]
  }
),
# Conditioning mapping
tar_target(
  cond_by_year,
  {
    setDT(decoder::icd10se)[
      proc_n_adults,
      on = c(key = "reasoncode_icd10")
    ]
  }
),
# Visualizing
tar_target(
  condition_plot,
  {
    top5 <- cond_by_year[
      ,
      .(N = sum(N)),
      .(value)
    ][
      order(-N)
    ][1:5,value]

    ggplot(
      cond_by_year[.(top5), on = "value"],
      aes(year, N, color = value)
    ) +
      geom_line() +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(ncol = 1)) +
      scale_color_discrete(
        labels = function(x) stringr::str_wrap(x, width = 40)
      )
  }
)

)

