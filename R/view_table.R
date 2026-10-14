# view_table.R
#
# Utility: pull any schema.table from econrisk.duckdb into R and open in View().
# Expects: `con` (DBI connection) to be in scope.
#
# Usage:
#   view_table("bronze.comtrade_export_hhi")
#   view_table("silver.comtrade_import_groups_ts")
#   view_table("silver.comtrade_export_groups_monthly", max_rows = 5000)
 
view_table <- function(table_name, max_rows = 50000) {
  # Accept "schema.table" or bare "table" (defaults to main schema)
  parts <- strsplit(table_name, "\\.")[[1]]
  if (length(parts) == 2) {
    sql <- sprintf("SELECT * FROM %s.%s LIMIT %d", parts[1], parts[2], max_rows)
  } else {
    sql <- sprintf("SELECT * FROM %s LIMIT %d", table_name, max_rows)
  }

  df <- tryCatch(
    DBI::dbGetQuery(con, sql) |> as.data.frame(),
    error = \(e) stop(sprintf("Could not read '%s': %s", table_name, conditionMessage(e)))
  )

  n_rows_total <- tryCatch(
    DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", table_name))$n,
    error = \(e) NA_integer_
  )

  if (!is.na(n_rows_total) && n_rows_total > max_rows)
    message(sprintf("  Showing %s of %s rows (increase max_rows to see more)",
                    format(max_rows, big.mark = ","),
                    format(n_rows_total, big.mark = ",")))

  # View(df, title = table_name)
  invisible(df)  # return silently so caller can assign if needed
}

# List all tables across all schemas
list_tables <- function() {
  DBI::dbGetQuery(con, "
    SELECT schema_name AS schema, table_name, estimated_size AS rows
    FROM duckdb_tables()
    ORDER BY schema_name, table_name
  ") |> as.data.frame()
}
