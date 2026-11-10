library('tidyverse')
df_1 <- readRDS(here::here('data', 'df_1.rds'))
df_2 <- readRDS(here::here('data', 'df_2.rds'))
df_3 <- readRDS(here::here('data', 'df_3.rds')) %>% ungroup
df_4 <- df_3 %>% filter(!is.na(pm25_total_10y), !is.na(lden_10y), !is.na(ndvi500m_10y), 
                        !is.na(age), !is.na(sex), !is.na(site), !is.na(edu),
                        !is.na(ses_s), !is.na(civ), !is.na(swe), !is.na(smo), 
                        !is.na(pyr), !is.na(lpa), !is.na(alc), !is.na(bmi), !is.na(whr))

table1_helper <- function(x,y) {
  x %>% 
    summarise(n = n(),
              sex. = mean(sex == 'FEMALE', na.rm = T) * 100,
              age = mean(age, na.rm = T) + mean(df_1$age, na.rm = T),
              dia = mean(dia != 0, na.rm = T) * 100,
              bmi = mean(bmi, na.rm = T),
              whr = mean(sex == 'MALE' & whr > .9 |
                           sex == 'FEMALE' & whr > .85, na.rm = T) * 100,
              smo0 = mean(smo == 'NEVER', na.rm = T) * 100,
              smo1 = mean(smo == 'CURRENT', na.rm = T) * 100,
              smo2 = mean(smo == 'EX_SMOKER', na.rm = T) * 100,
              alc0 = mean(alc == 0, na.rm = T) * 100,
              alc1 = mean(alc == 1 | alc == 2, na.rm = T) * 100,
              alc2 = mean(alc == 3 | alc == 4, na.rm = T) * 100,
              lpa0 = mean(lpa == 1, na.rm = T) * 100,
              lpa1 = mean(lpa == 2, na.rm = T) * 100,
              lpa2 = mean(lpa == 3, na.rm = T) * 100,
              edu0 = mean(edu == 1, na.rm = T) * 100,
              edu1 = mean(edu == 2, na.rm = T) * 100,
              edu2 = mean(edu == 3, na.rm = T) * 100,
              ses = mean(ses, na.rm = T) * 100,
              swe = mean(swe == 'YES', na.rm = T) * 100,
              civ = mean(civ == 'MARRIED') * 100,
              eGFR = median(exp(eGFR), na.rm = T), # log-transformed mL/min/1.73 m2
              uACR = median(exp(u_acr), na.rm = T), # log-transformed mg/mmol
              uKIM1 = median(exp(u_kim1), na.rm = T), # log-transformed ug/mmol
              uDKK3 = median(exp(u_dkk3), na.rm = T), # log-transformed ug/mmol
              pKIM1 = median(exp(p_kim1), na.rm = T)) %>% # log-transformed arbitrary OLINK units
    pivot_longer(cols = everything(), names_to = 'var', values_to = y)
}

table1 <- list(
  table1_helper(df_4, 'all'),
  table1_helper(df_4 %>% filter(pm25_total_10y < 5), 'pm_low'),
  table1_helper(df_4 %>% filter(pm25_total_10y >= 5), 'pm_high'),
  table1_helper(df_4 %>% filter(lden_10y < 55), 'lden_low'),
  table1_helper(df_4 %>% filter(lden_10y >= 55), 'lden_high'),
  table1_helper(df_4 %>% filter(ndvi500m_10y < .5), 'ndvi_low'),
  table1_helper(df_4 %>% filter(ndvi500m_10y >= .5), 'ndvi_high'),
  table1_helper(df_4 %>% filter(exp(eGFR) < 90), 'eGFR_low'),
  table1_helper(df_4 %>% filter(exp(eGFR) >= 90), 'eGFR_high'),
  table1_helper(df_4 %>% filter(exp(u_acr) <= 3), 'uacr_low'),
  table1_helper(df_4 %>% filter(exp(u_acr) > 3), 'uacr_high'),
  df_3 %>% 
    summarise(across(c(sex, age, dia, bmi, whr, smo, alc, edu, ses, swe, civ, ses, eGFR, u_acr, u_kim1, u_dkk3, p_kim1),
                     \(x) mean(!is.na(x)))) %>% 
    rename('smo2' = 'smo',
           'alc2' = 'alc',
           'edu2' = 'edu',
           'uACR' = 'u_acr',
           'uKIM1' = 'u_kim1',
           'uDKK3' = 'u_dkk3',
           'pKIM1' = 'p_kim1') %>% 
    pivot_longer(cols = everything(), names_to = 'var', values_to = 'n_miss')) %>%
  reduce(left_join, by = 'var')

writexl::write_xlsx(table1, here::here(paste0(Sys.Date(), '_table1.xlsx')))

table2 <- 
  left_join(
    summarise(df_4 %>% select(pm25_total_10y, pm10_total_10y, nox_total_10y, lden_10y, ndvi500m_10y, ses) %>% ungroup, 
              across(everything(),
                     \(x) paste0(sprintf('%.2f', median(x, na.rm = T)), ' (',
                                 sprintf('%.2f', quantile(x, .25, na.rm = T)), ', ',
                                 sprintf('%.2f', quantile(x, .75, na.rm = T)), ')')
              ) %>% pivot_longer(cols = everything(), names_to = 'exposure', values_to = 'all')
    ),
    summarise(df_4 %>% select(site, pm25_total_10y, pm10_total_10y, nox_total_10y, lden_10y, ndvi500m_10y, ses) %>% group_by(site), 
              across(everything(),
                     \(x) paste0(sprintf('%.2f', median(x, na.rm = T)), ' (',
                                 sprintf('%.2f', quantile(x, .25, na.rm = T)), ', ',
                                 sprintf('%.2f', quantile(x, .75, na.rm = T)), ')')
              )) %>% pivot_longer(cols = -'site', names_to = 'exposure') %>% 
      pivot_wider(id_cols = 'exposure', names_from = 'site')
  ) %>% 
  bind_cols(
    df_4 %>% 
      select(pm25_total_10y, pm10_total_10y, nox_total_10y, lden_10y, ndvi500m_10y, ses) %>% 
      ungroup %>% 
      cor(use = 'pairwise') %>% 
      as.data.frame() %>% 
      mutate(across(everything(), \(x) sprintf('%.2f', x)))
  )

writexl::write_xlsx(table2, here::here(paste0(Sys.Date(), '_table2.xlsx')))

tableS1 <-
  df_2 %>% 
  filter(scapis_id %in% df_4$scapis_id) %>% 
  select(eGFR, u_acr, u_kim1, u_dkk3, p_kim1) %>% 
  cor(use = 'pairwise') %>% 
  as_tibble() %>% 
  mutate(across(everything(),
                \(x) round(x, 2)))

writexl::write_xlsx(tableS1, here::here(paste0(Sys.Date(), '_tableS1.xlsx')))
