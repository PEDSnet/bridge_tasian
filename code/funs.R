#' Function to count distinct patients per site
#' @param tbl table with at least the cols site | [countcol] 
#' @param countcol the (quoted) name of the column to count individual ids
#' @param stepname value to insert into a "step" column 
#' @param stepnum value to insert into a "stepnum" column
#' @return table with step_number | attrition_step | site | num_pts
count_bysite <- function(tbl,
                         countcol="person_id",
                         stepname,
                         stepnum,
                         prev_log=NULL){
  
  total<-tbl%>%mutate(id:=!!sym(countcol))%>% #to align if there are different ids to count
    summarise(num_pts=n_distinct(id))%>%
    mutate(site='total')%>%collect()
  site_total<-tbl %>%
    mutate(id:=!!sym(countcol))%>%
    group_by(site)%>%
    summarise(num_pts=n_distinct(id)) %>%
    ungroup() %>%collect()%>%
    dplyr::union_all(total) %>%
    mutate(attrition_step:=!!stepname,
           step_number=stepnum)
  if(!is.null(prev_log)){
    rslt<-prev_log%>%
      dplyr::union_all(site_total)
  }else{rslt<-site_total}
  
  return(rslt)
}


get_conds <- function(cohort,
                      codes,
                      study_start_date,
                      study_end_date,
                      min_days_after_birth,
                      max_days_after_birth,
                      cond_tbl = cdm_tbl('condition_occurrence'),
                      person_tbl = cdm_tbl('person')
) {
  
  conds <- cond_tbl %>%
    filter(as.Date(condition_start_date) >= study_start_date) %>%
    filter(as.Date(condition_start_date) <= study_end_date) %>%
    left_join(person_tbl %>% select(person_id, birth_date), by=c('person_id')) %>%
    #mutate(age = date_diff('day', as.Date(birth_date), as.Date(condition_start_date))/365.25) %>%
    filter(as.numeric(date_diff('day', as.Date(birth_date), as.Date(condition_start_date))) >= min_days_after_birth) %>%
    filter(as.numeric(date_diff('day', as.Date(birth_date), as.Date(condition_start_date))) <= max_days_after_birth)
  
  cond_info <- conds %>%
    inner_join(codes %>%
                 distinct(concept_id), by=c('condition_concept_id'='concept_id')) %>%
    distinct(condition_occurrence_id) %>%
    mutate(cond_concept_id_info=1L)
  
  cond_src_info <- conds %>%
    inner_join(codes %>%
                 distinct(concept_id), by=c('condition_source_concept_id'='concept_id')) %>%
    distinct(condition_occurrence_id) %>%
    mutate(cond_source_concept_id_info=1L)
  
  conds_all <- cond_info %>%
    full_join(cond_src_info, by=c('condition_occurrence_id')) %>%
    left_join(cond_tbl, by=c('condition_occurrence_id')) %>%
    mutate(condition_type=case_when(cond_concept_id_info==1L ~ condition_concept_id,
                                    is.na(cond_concept_id_info) &&
                                      cond_source_concept_id_info==1L ~ condition_source_concept_id)) %>%
    left_join(codes %>% distinct(concept_id, dx_type), by=c('condition_type'='concept_id')) %>%
    select(-cond_concept_id_info, -cond_source_concept_id_info) %>%
    inner_join(cohort %>% distinct(person_id), by=c('person_id'))
  
  return(conds_all)
}


#' Index date is being defined as the earliest inpatient stay meeting criteria
inpatient_filter <- function(conds = bridge_conds,
                             ip_visit_concepts,
                             visit_tbl = cdm_tbl('visit_occurrence')
) {
  
  ip_conds <- bridge_conds %>%
    left_join(visit_tbl %>%
                select(visit_occurrence_id, visit_concept_id, 
                       visit_start_date, visit_end_date), by=c('visit_occurrence_id')) %>%
    filter(visit_concept_id %in% ip_visit_concepts)
  
  return(ip_conds)
}


dx_time_filter <- function(conds = inpatient_dx,
                           min_date = as.Date('2020-01-01'),
                           max_date = as.Date('2025-12-31'),
                           min_days_after_birth,
                           max_days_after_birth,
                           person_tbl = cdm_tbl('person')
) {
  
  dx_time_filtered <- conds %>%
    group_by(person_id) %>%
    mutate(index_date = min(as.Date(visit_start_date))) %>%
    ungroup() %>%
    filter(visit_start_date == index_date) %>%
    group_by(person_id, index_date) %>%
    summarise(index_date_end = max(as.Date(visit_end_date))) %>%
    ungroup() %>%
    filter(as.Date(index_date) >= as.Date(min_date)) %>%
    filter(as.Date(index_date) <= as.Date(max_date)) %>%
    add_site() %>%
    left_join(cdm_tbl('person') %>%
                select(person_id, birth_date), by=c('person_id')) %>%
    filter(as.numeric(date_diff('day', as.Date(birth_date), as.Date(index_date))) >= min_days_after_birth) %>%
    filter(as.numeric(date_diff('day', as.Date(birth_date), as.Date(index_date))) <= max_days_after_birth) %>%
    select(-birth_date)
    #filter(as.Date(visit_start_date) >= as.Date(min_date)) %>%
    #filter(as.Date(visit_start_date) <= as.Date(max_date))
  
  return(dx_time_filtered)
}


#' Preserves unique person_id / hospitalization date combinations
post_dx_visit_filter <- function(conds = inpatient_dx,
                                 visit_concepts = in_person_visit_concepts,
                                 days_post_min = 180L,
                                 study_end_date,
                                 visit_tbl = cdm_tbl('visit_occurrence')
) {
  
  post_visit <- conds %>%
    distinct(person_id, index_date) %>%
    mutate(post_visit_min = date_add('day', days_post_min, as.Date(index_date))) %>%
    inner_join(visit_tbl %>%
                 filter(visit_concept_id %in% visit_concepts), by=c('person_id')) %>%
    filter(as.Date(visit_start_date) >= as.Date(post_visit_min)) %>%
    filter(as.Date(visit_start_date) <= as.Date(study_end_date)) %>%
    filter(!is.na(visit_start_date)) %>%
    distinct(person_id, index_date, post_visit_min, visit_start_date) %>%
    group_by(person_id, index_date, post_visit_min) %>%
    summarise(n_visits=n()) %>%
    ungroup() %>%
    mutate(has_post_visit=1L) %>%
    add_site()
  
  return(post_visit)
}





#' Get core demographics
get_core_demographics <- function(cohort,
                                  person_tbl = cdm_tbl('person')
) {
  
  demog_cohort <- cohort %>%
    left_join(person_tbl %>%
                select(person_id, birth_date, site,
                       gender_concept_id, race_concept_id, ethnicity_concept_id, language_concept_id), by=c('person_id')) %>%
    mutate(age = date_diff('day', as.Date(birth_date), as.Date(index_date))/365.25) %>%
    mutate(age_group = case_when(age < 1L ~ '00_to_<01',
                                 age < 5L ~ '01_to_<05',
                                 age < 9L ~ '05_to_<12',
                                 age < 18L ~ '12_to_<18',
                                 age >= 18L ~ '18_Plus'
    )) %>%
    mutate(year = year(as.Date(index_date))) %>%
    mutate(
      sex_cat=case_when(
        gender_concept_id==8507L ~ 'Male',
        gender_concept_id==8532L ~ 'Female',
        TRUE ~ 'Other/unknown/ambiguous')) %>%
    mutate(eth_cat=case_when(ethnicity_concept_id == 38003563L ~ 'Hispanic',
                             ethnicity_concept_id == 38003564L ~ 'Not_Hispanic',
                             ethnicity_concept_id %in% c(44814660L, 44814649L, 44814650L,
                                                         44814653L) ~ 'Refuse_Other_Unknown',
                             TRUE ~ 'Other/Unknown'
    )) %>%
    mutate(race_cat = case_when(race_concept_id == 8527L ~ 'White',
                                race_concept_id == 8516L ~ 'Black',
                                race_concept_id == 8515L ~ 'Asian',
                                race_concept_id == 8657L ~ 'American_Indian_Alaska_Native',
                                race_concept_id == 8557L ~ 'Native_Hawaiian_Other_PI',
                                race_concept_id == 38003615L ~ 'Middle_Eastern_North_African',
                                race_concept_id == 44814659L ~ 'Multiple_Race',
                                race_concept_id %in% c(44814650L, 44814653L,
                                                       44814649L, 44814660) ~ 'Unknown_Other_Refuse'
    )) %>%
    mutate(
      raceeth_cat=case_when(ethnicity_concept_id == 38003563L ~ 'Hispanic',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8527L ~ 'NH_White',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8516L ~ 'NH_Black/AA',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8515L ~ 'NH_Asian',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8657L ~ 'Multiple/Other/Unknown',
                            ethnicity_concept_id == 38003564L & race_concept_id == 8557L ~ 'Multiple/Other/Unknown',
                            ethnicity_concept_id == 38003564L & race_concept_id == 38003615L ~ 'Multiple/Other/Unknown',
                            ethnicity_concept_id == 38003564L & race_concept_id == 44814659L ~ 'Multiple/Other/Unknown',
                            ethnicity_concept_id == 38003564L & race_concept_id %in% c(44814650L, 44814653L,
                                                                                       44814649L, 44814660) ~ 'Multiple/Other/Unknown',
                            TRUE ~ 'Multiple/Other/Unknown'))
  
  return(demog_cohort)
}



find_all_unplanned_hosps <- function(cohort = results_tbl('bridge_cohort'),
                                     ip_visit_concepts = ip_visit_concepts,
                                     days_post,
                                     study_end_date,
                                     visit_tbl = cdm_tbl('visit_occurrence'),
                                     fact_relationship = cdm_tbl('fact_relationship')
) {
  
  visit_tbl <- visit_tbl %>%
    filter(visit_start_date <= as.Date(study_end_date))
  
  #' Take a look at the ED-to-inpatient visit_concept_id
  ed_to_ip_cid <- visit_tbl %>%
    filter(visit_concept_id==2000000048L) %>%
    distinct(visit_occurrence_id)
  
  #' Look at ED-to-inpatient visits as they're included in the fact-relationship table
  #' Only exists for CHOP, Nemours, Colorado and Nationwide
  occurs_before <- fact_relationship %>%
    filter(relationship_concept_name %in% c('Occurs before (SNOMED)')) %>%
    select(fact_id_1, fact_id_2) %>%
    left_join(visit_tbl %>%
                select(visit_occurrence_id, visit_start_date, visit_concept_id),
              by=c('fact_id_1'='visit_occurrence_id')) %>%
    rename(visit_occurrence_id_1 = fact_id_1,
           visit_start_date_1 = visit_start_date,
           visit_concept_id_1 = visit_concept_id) %>%
    left_join(visit_tbl %>%
                select(visit_occurrence_id, visit_start_date, visit_concept_id),
              by=c('fact_id_2'='visit_occurrence_id')) %>%
    rename(visit_occurrence_id_2 = fact_id_2,
           visit_start_date_2 = visit_start_date,
           visit_concept_id_2 = visit_concept_id) %>%
    filter(visit_concept_id_1==9203L) %>%
    filter(visit_concept_id_2 %in% ip_visit_concepts) %>%
    distinct(visit_occurrence_id_1, visit_occurrence_id_2)
  
  #' I think all of these records are included in the "occurs_before" output above
  occurs_after <- fact_relationship %>%
    filter(relationship_concept_name %in% c('Occurs after (SNOMED)')) %>%
    select(fact_id_1, fact_id_2) %>%
    left_join(visit_tbl %>%
                select(visit_occurrence_id, visit_start_date, visit_concept_id),
              by=c('fact_id_1'='visit_occurrence_id')) %>%
    rename(visit_occurrence_id_1 = fact_id_1,
           visit_start_date_1 = visit_start_date,
           visit_concept_id_1 = visit_concept_id) %>%
    left_join(visit_tbl %>%
                select(visit_occurrence_id, visit_start_date, visit_concept_id),
              by=c('fact_id_2'='visit_occurrence_id')) %>%
    rename(visit_occurrence_id_2 = fact_id_2,
           visit_start_date_2 = visit_start_date,
           visit_concept_id_2 = visit_concept_id) %>%
    filter(visit_concept_id_1 %in% ip_visit_concepts) %>%
    filter(visit_concept_id_2 == 9203L) %>%
    distinct(visit_occurrence_id_1, visit_occurrence_id_2) %>%
    rename(visit_occurrence_id_x=visit_occurrence_id_1,
           visit_occurrence_id_y=visit_occurrence_id_2) %>%
    rename(visit_occurrence_id_2=visit_occurrence_id_x,
           visit_occurrence_id_1=visit_occurrence_id_y)
  
  occurs_before_after <- occurs_before %>%
    full_join(occurs_after, by=c('visit_occurrence_id_1', 'visit_occurrence_id_2'))
  
  #' all_ed_to_ip
  all_ed_to_ip <- ed_to_ip_cid %>%
    full_join(occurs_before_after, by=c('visit_occurrence_id'='visit_occurrence_id_1')) %>%
    left_join(visit_tbl %>%
                select(visit_occurrence_id, person_id,
                       visit_start_date, visit_end_date,
                       visit_start_datetime, visit_end_datetime, visit_concept_id),
              by=c('visit_occurrence_id')) %>%
    rename(visit_occurrence_id_1 = visit_occurrence_id,
           visit_start_date_1 = visit_start_date,
           visit_end_date_1 = visit_end_date,
           visit_start_datetime_1 = visit_start_datetime,
           visit_end_datetime_1 = visit_end_datetime,
           visit_concept_id_1 = visit_concept_id) %>%
    left_join(visit_tbl %>%
                select(visit_occurrence_id,
                       visit_start_date, visit_end_date,
                       visit_start_datetime, visit_end_datetime, visit_concept_id),
              by=c('visit_occurrence_id_2'='visit_occurrence_id')) %>%
    rename(visit_start_date_2 = visit_start_date,
           visit_end_date_2 = visit_end_date,
           visit_start_datetime_2 = visit_start_datetime,
           visit_end_datetime_2 = visit_end_datetime,
           visit_concept_id_2 = visit_concept_id)
  
  #' cohort output
  cohort_w_outcome <- cohort %>%
    distinct(person_id, index_date, index_date_end) %>%
    left_join(all_ed_to_ip, by=c('person_id')) %>%
    mutate(date_diff = date_diff('day', as.Date(index_date_end), as.Date(visit_start_date_1))) %>%
    filter(date_diff > 0L) %>%
    filter(date_diff <= days_post)
  
  return(cohort_w_outcome)
}


summarize_unplanned_hosps <- function(unplanned_hosps,
                                      cohort = results_tbl('bridge_cohort')
    ) {
  
  unplanned_hosps <- unplanned_hosps %>%
    collect()
  
  #' Total visit duration
  cohort_total_visit_duration <- unplanned_hosps %>%
    distinct(person_id, index_date, visit_occurrence_id_1, visit_occurrence_id_2,
             visit_start_datetime_1, visit_end_datetime_1,
             visit_start_datetime_2, visit_end_datetime_2) %>%
    mutate(visit_duration_1 = as.numeric(visit_end_datetime_1 - visit_start_datetime_1, units='days')) %>%
    mutate(visit_duration_2 = as.numeric(visit_end_datetime_2 - visit_start_datetime_2, units='days')) %>%
    mutate(visit_duration_all = case_when(!is.na(visit_duration_2) ~ visit_duration_1 + visit_duration_2,
                                          TRUE ~ visit_duration_1)) %>%
    group_by(person_id, index_date) %>%
    summarise(total_unplanned_hosp_duration = sum(visit_duration_all),
              median_unplanned_hosp_duration = median(visit_duration_all, na.rm=TRUE)) %>%
    ungroup()
  
  #' cohort total visit days
  cohort_visit_days_1 <- unplanned_hosps %>%
    mutate(date = map2(visit_start_date_1, visit_end_date_1, ~ seq(.x, .y, by = "day"))) %>%
    distinct(person_id, index_date, date)
  
  cohort_visit_days_2 <- unplanned_hosps %>%
    filter(!is.na(visit_start_date_2)) %>%
    mutate(date = map2(visit_start_date_2, visit_end_date_2, ~ seq(.x, .y, by = "day"))) %>%
    distinct(person_id, index_date, date)
  
  cohort_visit_days_all <- cohort_visit_days_1 %>%
    full_join(cohort_visit_days_2, by=c('person_id', 'index_date', 'date')) %>%
    distinct(person_id, index_date, date) %>%
    group_by(person_id, index_date) %>%
    summarise(unplanned_visit_days = n()) %>%
    ungroup()
  
  #' cohort unique visits
  cohort_unique_visits <- unplanned_hosps %>%
    distinct(person_id, index_date, visit_start_date_1) %>%
    group_by(person_id, index_date) %>%
    summarise(n_unique_unplanned_hosps = n(),
              min_unplanned_hosp_date = min(visit_start_date_1),
              max_unplanned_hosp_date = max(visit_start_date_1)) %>%
    ungroup()
  
  #' cohort any outcome
  cohort_any_outcome <- unplanned_hosps %>%
    distinct(person_id, index_date) %>%
    mutate(any_unplanned_hosp=1L) %>%
    full_join(cohort %>%
                distinct(person_id, index_date) %>%
                collect(), by=c('person_id','index_date')) %>%
    mutate(any_unplanned_hosp = case_when(any_unplanned_hosp==1L ~ 1L,
                                          TRUE ~ 0L))
  
  
  #' final info
  cohort_output <- cohort %>%
    distinct(person_id, index_date) %>%
    collect() %>%
    left_join(cohort_total_visit_duration, by=c('person_id','index_date')) %>%
    left_join(cohort_unique_visits, by=c('person_id', 'index_date')) %>%
    left_join(cohort_any_outcome, by=c('person_id', 'index_date')) %>%
    left_join(cohort_visit_days_all, by=c('person_id', 'index_date')) %>%
    mutate(n_unique_unplanned_hosps = case_when(is.na(n_unique_unplanned_hosps) ~ 0L,
                                                TRUE ~ n_unique_unplanned_hosps)) %>%
    mutate(unplanned_visit_days = case_when(is.na(unplanned_visit_days) ~ 0L,
                                            TRUE ~ unplanned_visit_days))
  
  return(cohort_output)
  
}



find_and_summarize_planned_hosps <- function(cohort = results_tbl('bridge_cohort'),
                                   unplanned_hosps = unplanned_hosps,
                                   ip_visit_concepts = ip_visit_concepts,
                                   days_post,
                                   study_end_date,
                                   visit_tbl = cdm_tbl('visit_occurrence')
) {
  
  visit_tbl <- visit_tbl %>%
    filter(visit_start_date <= as.Date(study_end_date))
  
  unplanned_hosp_vid <- unplanned_hosps %>%
    distinct(visit_occurrence_id_1) %>%
    rename(visit_occurrence_id = visit_occurrence_id_1) %>%
    full_join(unplanned_hosps %>%
                distinct(visit_occurrence_id_2) %>%
                rename(visit_occurrence_id = visit_occurrence_id_2)) %>%
    distinct(visit_occurrence_id)
  
  planned_hosps <- cohort %>%
    select(person_id, index_date, index_date_end) %>%
    left_join(visit_tbl, by=c('person_id')) %>%
    filter(visit_concept_id %in% ip_visit_concepts) %>%
    anti_join(unplanned_hosp_vid, by=c('visit_occurrence_id')) %>%
    mutate(date_diff = date_diff('day', as.Date(index_date), as.Date(visit_start_date))) %>%
    filter(date_diff > 0L) %>%
    filter(date_diff <= days_post) %>%
    collect()
  
  #' Total visit duration
  cohort_total_visit_duration <- planned_hosps %>%
    distinct(person_id, index_date, visit_occurrence_id,
             visit_start_datetime, visit_end_datetime) %>%
    mutate(visit_duration = as.numeric(visit_end_datetime - visit_start_datetime, units='days')) %>%
    group_by(person_id, index_date) %>%
    summarise(total_planned_hosp_duration = sum(visit_duration),
              median_planned_hosp_duration = median(visit_duration, na.rm=TRUE)) %>%
    ungroup()

  #' cohort total visit days
  cohort_visit_days <- planned_hosps %>%
    filter(!(as.Date(visit_start_date) > as.Date(visit_end_date))) %>%
    mutate(date = map2(visit_start_date, visit_end_date, ~ seq(.x, .y, by = "day"))) %>%
    distinct(person_id, index_date, date) %>%
    group_by(person_id, index_date) %>%
    summarise(planned_visit_days = n()) %>%
    ungroup()
  
  #' cohort unique visits
  cohort_unique_visits <- planned_hosps %>%
    distinct(person_id, index_date, visit_start_date) %>%
    group_by(person_id, index_date) %>%
    summarise(n_unique_planned_hosps = n(),
              min_planned_hosp_date = min(visit_start_date),
              max_planned_hosp_date = max(visit_start_date)) %>%
    ungroup()
  
  #' cohort any outcome
  cohort_any_outcome <- planned_hosps %>%
    distinct(person_id, index_date) %>%
    mutate(any_planned_hosp=1L) %>%
    full_join(cohort %>%
                distinct(person_id, index_date) %>%
                collect(), by=c('person_id','index_date')) %>%
    mutate(any_planned_hosp = case_when(any_planned_hosp==1L ~ 1L,
                                        TRUE ~ 0L))
  
  
  #' final info
  cohort_output <- cohort %>%
    distinct(person_id, index_date) %>%
    collect() %>%
    left_join(cohort_unique_visits, by=c('person_id', 'index_date')) %>%
    left_join(cohort_any_outcome, by=c('person_id', 'index_date')) %>%
    left_join(cohort_visit_days, by=c('person_id', 'index_date')) %>%
    left_join(cohort_total_visit_duration, by=c('person_id', 'index_date')) %>%
    mutate(n_unique_planned_hosps = case_when(is.na(n_unique_planned_hosps) ~ 0L,
                                              TRUE ~ n_unique_planned_hosps)) %>%
    mutate(planned_visit_days = case_when(is.na(planned_visit_days) ~ 0L,
                                              TRUE ~ planned_visit_days))
  
  return(cohort_output)
}




get_icu_visits <- function(cohort = results_tbl('bridge_cohort'),
                           days_post,
                           study_end_date,
                           adt_tbl = cdm_tbl('adt_occurrence'),
                           visit_tbl = cdm_tbl('visit_occurrence')
) {
  
  visit_tbl <- visit_tbl %>%
    filter(visit_start_date <= as.Date(study_end_date))
  
  #' Note that this table contains duplicate rows! (Should be consolidated later on)
  cohort_icu <- cohort %>%
    inner_join(visit_tbl %>%
                 select(visit_occurrence_id, person_id,
                        visit_start_date, visit_end_date,
                        visit_start_datetime, visit_end_datetime), by=c('person_id')) %>%
    filter(date_diff('day', as.Date(index_date), as.Date(visit_start_date)) > 0L) %>%
    left_join(adt_tbl %>%
                select(visit_occurrence_id, adt_occurrence_id, service_concept_id,
                       adt_date), by=c('visit_occurrence_id')) %>%
    filter(service_concept_id %in% c(2000000078L, 2000000080L, 2000000079L)) %>%
    filter(date_diff('day', as.Date(index_date_end), as.Date(adt_date)) > 0L) %>%
    filter(date_diff('day', as.Date(index_date_end), as.Date(adt_date)) <= days_post) %>%
    collect()
  
  cohort_total_visit_duration <- cohort_icu %>%
    distinct(person_id, index_date, visit_occurrence_id, visit_start_datetime, visit_end_datetime) %>%
    mutate(visit_duration = as.numeric(visit_end_datetime - visit_start_datetime, units='days')) %>%
    group_by(person_id, index_date) %>%
    summarise(total_icu_visit_duration = sum(visit_duration),
              median_icu_visit_duration = median(visit_duration, na.rm=TRUE)) %>%
    ungroup()
    
  cohort_visit_days <- cohort_icu %>%
    mutate(date = map2(visit_start_date, visit_end_date, ~ seq(.x, .y, by = "day"))) %>%
    distinct(person_id, index_date, date) %>%
    group_by(person_id, index_date) %>%
    summarise(icu_visit_days = n()) %>%
    ungroup()
  
  cohort_unique_visits <- cohort_icu %>%
    distinct(person_id, index_date, adt_date) %>%
    group_by(person_id, index_date) %>%
    summarise(n_unique_icu_stays = n(),
              min_adt_date = min(adt_date),
              max_adt_date = max(adt_date)) %>%
    ungroup()
  
  cohort_any_outcome <- cohort_icu %>%
    distinct(person_id, index_date) %>%
    mutate(any_icu=1L) %>%
    full_join(cohort %>%
                distinct(person_id, index_date) %>%
                collect(), by=c('person_id','index_date')) %>%
    mutate(any_icu = case_when(any_icu==1L ~ 1L,
                               TRUE ~ 0L))
  
  
  cohort_icu_all <- cohort %>%
    distinct(person_id, index_date) %>%
    collect() %>%
    left_join(cohort_total_visit_duration, by=c('person_id','index_date')) %>%
    left_join(cohort_visit_days, by=c('person_id', 'index_date')) %>%
    left_join(cohort_unique_visits, by=c('person_id', 'index_date')) %>%
    left_join(cohort_any_outcome, by=c('person_id', 'index_date')) %>%
    mutate(icu_visit_days = case_when(is.na(icu_visit_days) ~ 0L,
                                      TRUE ~ icu_visit_days)) %>%
    mutate(n_unique_icu_stays = case_when(is.na(n_unique_icu_stays) ~ 0L,
                                          TRUE ~ n_unique_icu_stays))
  
  return(cohort_icu_all)
  
}


get_cond_counts <- function(conds = bridge_conds) {
  cond_counts <- conds %>%
    distinct(person_id, dx_type, condition_start_date) %>%
    group_by(person_id, dx_type) %>%
    summarise(n_condition_codes = n()) %>%
    ungroup() %>%
    pivot_wider(names_from=c('dx_type'), values_from=c('n_condition_codes')) %>%
    rename(hirschsprung_disease_conds=Hirschsprung_disease,
           esophageal_atresia_conds=esophageal_atresia,
           anorectal_malformation_conds=anorectal_malformation,
           gastroschisis_conds=gastroschisis,
           congenital_diaphragmatic_hernia_conds=congenital_diaphragmatic_hernia
    ) %>%
    mutate(two_plus_conds_within_dx_type = case_when(hirschsprung_disease_conds>=2L |
                                                       esophageal_atresia_conds>=2L |
                                                       anorectal_malformation_conds>=2L |
                                                       gastroschisis_conds>=2L |
                                                       congenital_diaphragmatic_hernia_conds>=2L ~ 1L,
                                                     TRUE ~ 0L
                                                       )) %>%
    mutate(three_plus_conds_within_dx_type = case_when(hirschsprung_disease_conds>=3L |
                                                       esophageal_atresia_conds>=3L |
                                                       anorectal_malformation_conds>=3L |
                                                       gastroschisis_conds>=3L |
                                                       congenital_diaphragmatic_hernia_conds>=3L ~ 1L,
                                                     TRUE ~ 0L
    ))
  
  
  return(cond_counts)
}



