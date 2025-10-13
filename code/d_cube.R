library(DBI)
library(odbc)
con <- dbConnect(
  odbc::odbc(),
  dsn = "ZCU_Oracle",
  uid = "analytika",
  pwd = "AhojKedlubno1962"
)

# Get the idk_time
# dbGetQuery(con, "SELECT *
#   from st_dim_time
#   where AKAD_ROK = '2024/2025'")

cube_phd_raw <- dbGetQuery(
  con,
  paste(readLines("code/cube_phd.sql", warn = FALSE), collapse = "\n")
)

nrow(cube_phd_raw)

cube_phd = cube_phd_raw |>
  set_names(tolower(names(cube_phd_raw))) |>
  select(osobidno, os_cislo, pohlavi, typ, forma, fakulta_oboru_zkr) |>
  rename(gender = pohlavi, fak = fakulta_oboru_zkr) |> 
  distinct()

