.sqlCreateTableAs <-  function(con, name, sql, with = NULL, ...) {
  name <- DBI::dbQuoteIdentifier(con, name)
  DBI::SQL(paste0(
    "CREATE TABLE ", name, "\n",
    if (!is.null(with)) paste0(with, "\n"),
    "AS\n",
    sql
  ))
}

setMethod("sqlCreateTableAs", signature("PrestoConnection"), .sqlCreateTableAs)

assignInNamespace(
  ".compute_tbl_presto",
  function(x, name, temporary = FALSE, ..., cte = FALSE) {
    if (rlang::is_bare_character(x) || dbplyr::is.ident(x) || dbplyr::is.sql(x)) {
      name <- unname(name)
    }
    if (identical(cte, TRUE)) {
      if (inherits(x$lazy_query, "lazy_base_remote_query")) {
        stop(
          "No operations need to be computed. Aborting compute.",
          call. = FALSE
        )
      }
      con <- dbplyr::remote_con(x)
      # We need to speicify sql_options here so that use_presto_cte is passed to
      # db_sql_render correctly
      # (see https://github.com/tidyverse/dbplyr/issues/1394)
      sql <- dbplyr::db_sql_render(
        con = dbplyr::remote_con(x), sql = x,
        sql_options = dbplyr::sql_options(), use_presto_cte = FALSE
      )
      con@session$addCTE(name, sql, replace = TRUE)
    } else {
      sql <- dbplyr::db_sql_render(
        dbplyr::remote_con(x), x, use_presto_cte = TRUE
      )
      name <- dbplyr::db_compute(
        dbplyr::remote_con(x), name, sql, temporary = temporary, ...
      )
    }
    name
  }, ns='RPresto'
)

