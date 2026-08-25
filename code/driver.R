library(DBI)
library(dplyr)
library(dbplyr)
library(squba)
library(readxl)

#' -----------------------------------------------------------------------------
#' Check latest visit dates in the database
#' -----------------------------------------------------------------------------

latest_visits <- cdm_tbl('visit_occurrence') %>%
  group_by(site) %>%
  summarise(max_visit_date=max(as.Date(visit_start_date))) %>%
  ungroup()

#' -----------------------------------------------------------------------------
#' Diagnosis codesets
#' -----------------------------------------------------------------------------

# hernia_dx <- read_xlsx('specs/BRIDGE query codes_v3.xlsx', sheet = 'cong diaphargmatic hernia')
# atresia_dx <- read_xlsx('specs/BRIDGE query codes_v3.xlsx', sheet = 'esophageal atresia')
# gastroschisis_dx <- read_xlsx('specs/BRIDGE query codes_v3.xlsx', sheet = 'gastroschisis')
# hirschsprung_dx <- read_xlsx('specs/BRIDGE query codes_v3.xlsx', sheet = 'hirschsprung')
# anorectal_dx <- read_xlsx('specs/BRIDGE query codes_v3.xlsx', sheet = 'anorectal malf')
# 
# output_tbl(hernia_dx, 'hernia_dx', .chunk_size=1000)
# output_tbl(atresia_dx, 'atresia_dx', .chunk_size=1000)
# output_tbl(gastroschisis_dx, 'gastroschisis_dx', .chunk_size=1000)
# output_tbl(hirschsprung_dx, 'hirschsprung_dx', .chunk_size=1000)
# output_tbl(anorectal_dx, 'anorectal_dx', .chunk_size=1000)
# 
# all_dx <- results_tbl('hernia_dx') %>%
#   distinct(concept_id) %>%
#   mutate(dx_type = 'congenital_diaphragmatic_hernia') %>%
#   dplyr::union(results_tbl('atresia_dx') %>%
#                  distinct(concept_id) %>%
#                  mutate(dx_type = 'esophageal_atresia')) %>%
#   dplyr::union(results_tbl('gastroschisis_dx') %>%
#                  distinct(concept_id) %>%
#                  mutate(dx_type='gastroschisis')) %>%
#   dplyr::union(results_tbl('hirschsprung_dx') %>%
#                  distinct(concept_id) %>%
#                  mutate(dx_type='Hirschsprung_disease')) %>%
#   dplyr::union(results_tbl('anorectal_dx') %>%
#                  distinct(concept_id) %>%
#                  mutate(dx_type='anorectal_malformation')) %>%
#   left_join(vocabulary_tbl('concept'), by=c('concept_id')) %>%
#   select(dx_type, concept_id, concept_name, concept_code, vocabulary_id)
# 
# output_tbl(all_dx, 'all_dx_codes')

#' -----------------------------------------------------------------------------
#' Study Parameters
#' -----------------------------------------------------------------------------

current_db_version <- "v62"
study_start_date     <- as.Date("2009-01-01")
study_end_date     <- as.Date("2026-05-31")
ip_visit_concepts <- c(9201L, 2000001532L,
                       2000000048L, 2000000088L)
in_person_visit_concepts <- c(9201L, 2000001532L,
                              2000000048L, 2000000088L,
                              9203L,
                              9202L, 581399L, 44814710L, 42898160L
                              )

#' -----------------------------------------------------------------------------
#' Attrition Steps
#' -----------------------------------------------------------------------------

today <- Sys.Date()
min_birth <- today - lubridate::years(120)

#' -----------------------------------------------------------------------------
#' Get all "valid" patients - have a plausible birthdate and valid sex
person <- cdm_tbl("person") %>%
  select(
    person_id, site, birth_date,
    gender_concept_id, gender_concept_name,
    race_concept_id, race_concept_name,
    ethnicity_concept_id, ethnicity_concept_name) %>%
  filter(
    !is.na(birth_date),
    birth_date <= !!today,
    birth_date >= !!min_birth,
    gender_concept_id %in% c(8532, 8507))

attr_site <- count_bysite(tbl = person,
                         countcol = "person_id",
                         stepname = paste0('Valid patients in PEDSnet db ',current_db_version, ' (birthdate, valid sex)'),
                         stepnum = 1L)

#' -----------------------------------------------------------------------------
#' Get patients with >=1 diagnosis code for congenital diaphragmatic hernia, esophageal atresia,
#' gastroschisis, Hirschsprung disease, or anorectal malformation

all_dx_codes <- results_tbl('all_dx_codes')

bridge_conds <- get_conds(cohort = person,
                          codes = all_dx_codes,
                          study_start_date = as.Date(study_start_date),
                          study_end_date = as.Date(study_end_date),
                          min_age = 0L,
                          max_age = 99L
                          )

# bridge_conds %>% group_by(dx_type, site) %>% summarise(n=n()) %>% ungroup() %>%
#   pivot_wider(names_from=c('site'), values_from=c('n'))

bridge_cond_counts <- get_cond_counts(conds = bridge_conds)

attr_site <- count_bysite(tbl = bridge_conds,
                          countcol = "person_id",
                          stepname = paste0('Patients with ≥1 dx code for congenital diaphragmatic hernia, esophageal atresia, gastroschisis, Hirschsprung disease, or anorectal malformation'),
                          stepnum = 2L,
                          prev_log = attr_site
                          )

#' -----------------------------------------------------------------------------
#' Get patients with >=1 diagnosis code associated with any inpatient stay
#' Index date: defined as the earliest date meeting the criteria

inpatient_dx <- inpatient_filter(conds = bridge_conds,
                                 ip_visit_concepts = ip_visit_concepts)

attr_site <- count_bysite(tbl = inpatient_dx,
                          countcol = "person_id",
                          stepname = paste0('Patients with ≥1 dx code associated with any inpatient stay'),
                          stepnum = 3L,
                          prev_log = attr_site
)

#' -----------------------------------------------------------------------------
#' Diagnosis date between 2020-2025

dx_2020_2025 <- dx_time_filter(conds = inpatient_dx,
                               min_date = as.Date('2020-01-01'),
                               max_date = as.Date('2025-12-31'))

attr_site <- count_bysite(tbl = dx_2020_2025,
                          countcol = "person_id",
                          stepname = paste0('Patients with a diagnosis date between 2020 and 2025'),
                          stepnum = 4L,
                          prev_log = attr_site
)

#' -----------------------------------------------------------------------------
#' Get patients with any (in-person?) visit >=6 months after diagnosis/index hospitalization

dx_time_w_index <- dx_2020_2025 %>%
  group_by(person_id) %>%
  mutate(index_date = min(as.Date(visit_start_date))) %>%
  ungroup() %>%
  filter(visit_start_date == index_date) %>%
  group_by(person_id, index_date) %>%
  summarise(index_date_end = max(as.Date(visit_end_date))) %>%
  ungroup() %>%
  add_site()

visit_6m_post <- post_dx_visit_filter(conds = dx_time_w_index,
                                      visit_concepts = in_person_visit_concepts,
                                      days_post_min = 180L,
                                      study_end_date = as.Date(study_end_date))

attr_site <- count_bysite(tbl = visit_6m_post,
                          countcol = "person_id",
                          stepname = paste0('Patients with any in-person visit ≥6 months after dx/index hospitalization'),
                          stepnum = 5L,
                          prev_log = attr_site
)

#' -----------------------------------------------------------------------------
#' Get patients with any (in-person?) visit >=12 months after diagnosis/index hospitalization  

visit_12m_post <- post_dx_visit_filter(conds = dx_time_w_index,
                                       visit_concepts = in_person_visit_concepts,
                                       days_post_min = 365L,
                                       study_end_date = as.Date(study_end_date))

attr_site <- count_bysite(tbl = visit_12m_post,
                          countcol = "person_id",
                          stepname = paste0('Patients with any in-person visit ≥12 months after dx/index hospitalization'),
                          stepnum = 6L,
                          prev_log = attr_site
)

#' -----------------------------------------------------------------------------
#' Final bridge cohort

bridge_cohort <- visit_12m_post %>%
  left_join(dx_time_w_index %>%
              select(-site), by=c('person_id','index_date'))

output_tbl(bridge_cohort, 'bridge_cohort')

#' -----------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Summarise attrition results
# ------------------------------------------------------------------------------

attrition_results <- ca_process(attrition_tbl=attr_site,
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                start_step_num = 1)

output_tbl(attrition_results,
           name='attrition_cohort')

#' -----------------------------------------------------------------------------
#' Classify dx_type
#' -----------------------------------------------------------------------------

dx_type_info <- dx_2020_2025 %>%
  inner_join(results_tbl('bridge_cohort') %>% distinct(person_id), by=c('person_id')) %>%
  group_by(person_id) %>%
  mutate(min_dx_date = min(as.Date(visit_start_date))) %>%
  ungroup() %>%
  filter(visit_start_date==min_dx_date) %>%
  mutate(has_dx_type=1L) %>%
  select(person_id, visit_start_date, dx_type, has_dx_type) %>%
  pivot_wider(names_from=c('dx_type'), values_from=c('has_dx_type'), values_fill=0) %>%
  mutate(index_dx_type = case_when(congenital_diaphragmatic_hernia+anorectal_malformation+Hirschsprung_disease+gastroschisis+esophageal_atresia == 1L &&
                                     congenital_diaphragmatic_hernia==1L ~ 'congenital_diaphragmatic_hernia',
                                   congenital_diaphragmatic_hernia+anorectal_malformation+Hirschsprung_disease+gastroschisis+esophageal_atresia == 1L &&
                                     anorectal_malformation==1L ~ 'anorectal_malformation',
                                   congenital_diaphragmatic_hernia+anorectal_malformation+Hirschsprung_disease+gastroschisis+esophageal_atresia == 1L &&
                                     Hirschsprung_disease==1L ~ 'Hirschsprung_disease',
                                   congenital_diaphragmatic_hernia+anorectal_malformation+Hirschsprung_disease+gastroschisis+esophageal_atresia == 1L &&
                                     gastroschisis==1L ~ 'gastroschisis',
                                   congenital_diaphragmatic_hernia+anorectal_malformation+Hirschsprung_disease+gastroschisis+esophageal_atresia == 1L &&
                                     esophageal_atresia==1L ~ 'esophageal_atresia',
                                   congenital_diaphragmatic_hernia+anorectal_malformation+Hirschsprung_disease+gastroschisis+esophageal_atresia > 1L ~ 'multiple_dx'
  )) %>%
  mutate(index_dx_type_hierarchy = case_when(congenital_diaphragmatic_hernia==1L ~ 'congenital_diaphragmatic_hernia',
                                             esophageal_atresia==1L ~ 'esophageal_atresia',
                                             anorectal_malformation==1L ~ 'anorectal_malformation',
                                             Hirschsprung_disease==1L ~ 'Hirschsprung_disease',
                                             gastroschisis==1L ~ 'gastroschisis',
                                             TRUE ~ 'none'
                                             ))

output_tbl(dx_type_info, 'bridge_cohort_w_dx_type')

#' -----------------------------------------------------------------------------
#' Get outcomes
#' -----------------------------------------------------------------------------


#' -----------------------------------------------------------------------------
#' Unplanned hospitalizations (preceded by an ED visit)

unplanned_hosps_180 <- find_all_unplanned_hosps(cohort = results_tbl('bridge_cohort'),
                                            ip_visit_concepts = ip_visit_concepts,
                                            days_post = 180L,
                                            study_end_date = as.Date(study_end_date))

unplanned_hosps_summary_180 <- summarize_unplanned_hosps(unplanned_hosps = unplanned_hosps_180,
                                                     cohort = results_tbl('bridge_cohort'))

output_tbl(unplanned_hosps_summary_180, 'cohort_w_unplanned_hosps_180')


unplanned_hosps_365 <- find_all_unplanned_hosps(cohort = results_tbl('bridge_cohort'),
                                                ip_visit_concepts = ip_visit_concepts,
                                                days_post = 365L,
                                                study_end_date = as.Date(study_end_date))

unplanned_hosps_summary_365 <- summarize_unplanned_hosps(unplanned_hosps = unplanned_hosps_365,
                                                         cohort = results_tbl('bridge_cohort'))

output_tbl(unplanned_hosps_summary_365, 'cohort_w_unplanned_hosps_365')

#' -----------------------------------------------------------------------------
#' Planned hospitalizations (without a preceding ED visit)

planned_hosps_180 <- find_and_summarize_planned_hosps(cohort = results_tbl('bridge_cohort'),
                                                  unplanned_hosps = unplanned_hosps_180,
                                                  ip_visit_concepts = ip_visit_concepts,
                                                  days_post = 180L,
                                                  study_end_date = as.Date(study_end_date))


output_tbl(planned_hosps_180, 'cohort_w_planned_hosps_180')


planned_hosps_365 <- find_and_summarize_planned_hosps(cohort = results_tbl('bridge_cohort'),
                                                      unplanned_hosps = unplanned_hosps_365,
                                                      ip_visit_concepts = ip_visit_concepts,
                                                      days_post = 365L,
                                                      study_end_date = as.Date(study_end_date))


output_tbl(planned_hosps_365, 'cohort_w_planned_hosps_365')

#' -----------------------------------------------------------------------------
#' ICU stays

icu_visits_180 <- get_icu_visits(cohort = results_tbl('bridge_cohort'),
                             days_post = 180L,
                             study_end_date = as.Date(study_end_date))

output_tbl(icu_visits_180, 'cohort_w_icu_visits_180', .chunk_size = 1000)



icu_visits_365 <- get_icu_visits(cohort = results_tbl('bridge_cohort'),
                                 days_post = 365L,
                                 study_end_date = as.Date(study_end_date))

output_tbl(icu_visits_365, 'cohort_w_icu_visits_365', .chunk_size = 1000)


#' -----------------------------------------------------------------------------
#' Demographic variables: age at index, sex, race/ethnicity, site

demog <- get_core_demographics(cohort = results_tbl('bridge_cohort') %>%
                                 select(-site))

output_tbl(demog, 'bridge_cohort_demographics')




#' -----------------------------------------------------------------------------
#' Put the cohort together
#' -----------------------------------------------------------------------------

cohort_final <- results_tbl('bridge_cohort_demographics') %>%
  left_join(bridge_cond_counts, by=c('person_id')) %>%
  left_join(results_tbl('bridge_cohort_w_dx_type') %>%
              rename(index_date = visit_start_date), by=c('person_id', 'index_date')) %>%
  left_join(results_tbl('cohort_w_unplanned_hosps_180') %>%
              rename(n_unique_unplanned_hosps_180 = n_unique_unplanned_hosps,
                     any_unplanned_hosp_180 = any_unplanned_hosp,
                     unplanned_visit_days_180 = unplanned_visit_days,
                     min_unplanned_hosp_date_180 = min_unplanned_hosp_date,
                     max_unplanned_hosp_date_180 = max_unplanned_hosp_date
                     ), by=c('person_id', 'index_date')) %>%
  left_join(results_tbl('cohort_w_planned_hosps_180') %>%
              rename(n_unique_planned_hosps_180 = n_unique_planned_hosps,
                     any_planned_hosp_180 = any_planned_hosp,
                     planned_visit_days_180 = planned_visit_days,
                     min_planned_hosp_date_180 = min_planned_hosp_date,
                     max_planned_hosp_date_180 = max_planned_hosp_date
              ), by=c('person_id', 'index_date')) %>%
  left_join(results_tbl('cohort_w_icu_visits_180') %>%
              rename(icu_visit_days_180 = icu_visit_days,
                     n_unique_icu_stays_180 = n_unique_icu_stays,
                     any_icu_180 = any_icu,
                     min_adt_date_180 = min_adt_date,
                     max_adt_date_180 = max_adt_date), by=c('person_id', 'index_date')) %>%
left_join(results_tbl('cohort_w_unplanned_hosps_365') %>%
            rename(n_unique_unplanned_hosps_365 = n_unique_unplanned_hosps,
                   any_unplanned_hosp_365 = any_unplanned_hosp,
                   unplanned_visit_days_365 = unplanned_visit_days,
                   min_unplanned_hosp_date_365 = min_unplanned_hosp_date,
                   max_unplanned_hosp_date_365 = max_unplanned_hosp_date
            ), by=c('person_id', 'index_date')) %>%
  left_join(results_tbl('cohort_w_planned_hosps_365') %>%
              rename(n_unique_planned_hosps_365 = n_unique_planned_hosps,
                     any_planned_hosp_365 = any_planned_hosp,
                     planned_visit_days_365 = planned_visit_days,
                     min_planned_hosp_date_365 = min_planned_hosp_date,
                     max_planned_hosp_date_365 = max_planned_hosp_date
              ), by=c('person_id', 'index_date')) %>%
  left_join(results_tbl('cohort_w_icu_visits_365') %>%
              rename(icu_visit_days_365 = icu_visit_days,
                     n_unique_icu_stays_365 = n_unique_icu_stays,
                     any_icu_365 = any_icu,
                     min_adt_date_365 = min_adt_date,
                     max_adt_date_365 = max_adt_date), by=c('person_id', 'index_date'))
  

output_tbl(cohort_final, 'bridge_cohort_w_outcomes')
















db_remove_table(db, dbplyr::in_schema(config('results_schema'), 'bridge_cohort'))


