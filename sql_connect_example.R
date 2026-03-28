#' Example on how to connect to SQL server 


libs <- c("dplyr", "odbc", "DBI", "dbplyr")

load_libraries <- function(libs){
  not_installed <- libs[!(libs %in% installed.packages()[, "Package"])]
  if(length(not_installed)) install.packages(not_installed)
  lapply(libs, require, character.only = TRUE)
}

load_libraries(libs)


con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  server = "tcp:",
  Database = "",
  UID = "",
  Port = "",
  TrustServerCertificate = "No",
  Timeout = 180,
  Authentication = "ActiveDirectoryInteractive"
)

table_name <- dplyr::tbl(con, from = "table_name") 
