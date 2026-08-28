################### SOURCE WHATEVER FILES ######################
#' remotes::install_github('PEDSnet/argos')

library(argos)
library(srcr)
library(stringr)

library(RPresto) #' 1.4.8
library(DBI) #' 1.3.0: packageVersion("DBI")
library(dbplyr) #' 2.5.1 or 2.5.2
library(RPostgres)
library(assertthat)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(tibble) #' includes the view function 
library(squba)
library(readr)
library(usethis)

source(file.path(getwd(), 'setup', 'trino_patch.R'))
source(file.path(getwd(), 'code', 'funs.R'))
source(Sys.getenv('PEDSNET_TRINO_HTTR')) #' source the HTTR information

# library(DBI)
# db=config('db_src')
# dbExecute(db, 'create schema bridge_tasian_v62')


bridge_session_v62<-argos$new('idd_session')$init_session(db_src = srcr(Sys.getenv('PEDSNET_TRINO_V62')),
                                                        cdm_schema = 'pedsnet_dcc_v62',
                                                        vocabulary_schema = 'v62_vocabulary',
                                                        results_schema = 'bridge_tasian_v62',
                                                        cdm = 'pedsnet',
                                                        table_case = 'lower',
                                                        results_name_tag = '',
                                                        db_trace = TRUE,
                                                        retain_intermediates = TRUE)

set_argos_default(bridge_session_v62)

