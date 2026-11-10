library('tidyverse')

df_1 <- readRDS('/safe/data/Research projects/SCAPIS/Originaldata/SCAPIS_master_20230605.rds') %>% 
  transmute(scapis_id,
            sex = factor(sex), 
            site = case_when(str_sub(site, 1, 1) == 'S' ~ 'sth',
                             str_sub(site, 1, 1) == 'G' ~ 'got',
                             str_sub(site, 1, 1) == 'M' ~ 'mal',
                             str_sub(site, 1, 1) == 'L' ~ 'lin',
                             str_sub(site, 1, 2) == 'Up' ~ 'upp',
                             str_sub(site, 1, 2) == 'Um' ~ 'ume') %>% factor(), 
            year = str_sub(anthropometrycollectiondate, 1, 4) %>% 
              as.integer(), 
            age = ageatvisitone,
            edu = cqed001 %>% 
              na_if(-99) %>% 
              factor(levels = c(0, 1, 2, 3), ordered = F) %>% 
              recode('0' = '1'), 
            smo = derived_smoke_status %>% 
              na_if('UNKNOWN') %>% 
              factor(levels = c('NEVER', 'EX_SMOKER', 'CURRENT'), 
                     ordered = F),
            pyr = case_when(is.na(smo) ~ NA_real_,
                            smo == 'NEVER' ~ 0,
                            pack_yr_cig == -98 ~ NA_real_,
                            T ~ pack_yr_cig), 
            lpa = cqpa012 %>% 
              na_if(-99) %>% 
              factor(levels = c(0, 1, 2, 3, 4), ordered = F) %>% 
              recode('0' = '1', '4' = '3'),
            civ = cqli003 %>% 
              na_if('') %>% 
              na_if('NO_ANSWER') %>% 
              factor(levels = c('ALONE', 'DIVORCED', 'WIDOW', 'MARRIED'), 
                     ordered = F) %>% 
              recode_factor('ALONE' ='UNMARRIED' , 
                            'DIVORCED' = 'UNMARRIED',
                            'WIDOW' = 'UNMARRIED'),
            hrt = case_when(sex == 'MALE' ~ 0,
                            cqwh014 == 'DO_NOT_KNOW' ~ NA_real_,
                            cqwh014 == 'NO_ANSWER' ~ NA_real_,
                            cqwh014 == 'NO' ~ 0,
                            cqwh014 == 'YES' ~ 1) %>% 
              factor(levels = c(0, 1)),
            swe = cqli009 %>% 
              na_if('') %>% 
              na_if('NO_ANSWER') %>% 
              factor(levels = c('NO', 'YES')),
            bmi = bmi,
            bmi_cat = case_when(bmi < 25 ~ 1,
                            bmi < 30 ~ 2,
                            bmi >=30 ~ 3,
                            T ~ NA_real_) %>% 
              factor(ordered = F),
            whr = waist / hip,
            hpt = case_when(cqhe034 == 'YES' | cqhe035 == 'YES' ~ 1,
                            cqhe034 == 'NO' | cqhe034 == 'NO' ~ 0,
                            T ~ NA_real_) %>% 
              factor(),
            dia = case_when((cqhe038 == 'YES' | cqhe039 == 'YES') & cqhe040 %in% c('INSULIN', 'INSULIN_ORAL') ~ 2,
                            cqhe038 == 'YES' | cqhe039 == 'YES' ~ 1,
                            cqhe038 == 'NO' | cqhe039 == 'NO' ~ 0,
                            T ~ NA_real_) %>% 
              factor(),
            hlp = case_when(ldlformattedresult >= 4.9 ~ 1,
                            cqhe036 == 'YES' ~ 1,
                            cqhe037 == 'YES' ~ 1,
                            !is.na(ldlformattedresult) & 
                              cqhe036 %in% c('', 'NO_ANSWER') ~ NA_real_,
                            is.na(ldlformattedresult) &
                              is.na(cqhe036) ~ NA_real_,
                            T ~ 0) %>% 
              factor(),
            alc = cqah001 %>% 
              na_if(-99) %>% 
              factor(levels = c(0,1, 2, 3, 4), ordered = F),
            rural = str_sub(deso, 5, 5) == 'A',
            p_kim1 = 2^kim1
  )

df_egfr <- haven::read_dta('/safe/data/Research projects/SCAPIS/Originaldata/Locally derived datasets/SCAPIS_eGFR_230612.dta')
df_ses <- haven::read_dta('/safe/data/Research projects/SCAPIS/Originaldata/Locally derived datasets/SCAPIS_ses_231009.dta')

df_uacr <- read_delim(
  '/safe/data/Research projects/SCAPIS/Originaldata/PETITION-1073/SCAPIS1-DATA-PETITION-1073-20260120.csv',
  delim = ',') %>% 
  left_join(read_csv2('/safe/data/Research projects/SCAPIS/Originaldata/scapis_id_subject.csv') %>% 
              select('Subject', 'scapis_id'),
            by = 'Subject') %>% 
  transmute(scapis_id,
            u_alb = coalesce(u_albumin_LS232, u_albumin_LS81) %>% 
              if_else(. < 2, 2 / sqrt(2), .),
            u_crea = coalesce(u_creatinine_LS232, u_creatinine_LS81) %>% 
              if_else(. < .5, .5 / sqrt(2), .),
            u_dkk3 = coalesce(u_DKK_3_LS232, u_DKK_3_LS81) %>% 
              if_else(. < 60, 60 / sqrt(2), .),
            u_kim1 = u_KIM_1_LS232 %>% 
              if_else(. < 100, 100 / sqrt(2), .),
            site = case_when(str_sub(Site, 1, 1) == 'S' ~ 'sth',
                             str_sub(Site, 1, 1) == 'G' ~ 'got',
                             str_sub(Site, 1, 1) == 'M' ~ 'mal',
                             str_sub(Site, 1, 1) == 'L' ~ 'lin',
                             str_sub(Site, 1, 2) == 'Up' ~ 'upp',
                             str_sub(Site, 1, 2) == 'Um' ~ 'ume')) %>% 
  mutate(u_acr = u_alb / u_crea, # units: u_alb mg/L, u_crea mmol/L, ratio mg/mmol - no conversion needed
         u_kim1 = 0.001 * u_kim1 / u_crea, # units: u_kim1 pg/mL, u_crea mmol/L, ratio ug/mmol - conversion by 10^-3
         u_dkk3 = 0.001 * u_dkk3 / u_crea) # units: u_dkk3 pg/mL, u_crea mmol/L, ratio ug/mmol - conversion by 10^-3

df_expo <- readRDS('/safe/data/Research projects/SCAPIS/Originaldata/SCAPIS_master_20230605.rds') %>% 
  select(scapis_id, year = anthropometrycollectiondate, contains('pm25_total'), contains('pm10_total'), contains('nox_total'),
         contains('lden'), contains('ndvi500m'), -contains('rail'), -contains('air')) %>% 
  mutate(year = str_sub(year, 1, 4) %>% as.integer()) %>% 
  pivot_longer(-c(scapis_id, year), names_to = 'exposure', values_to = 'value') %>% 
  mutate(expo_year = str_sub(exposure, -9, -6) %>% as.integer(),
         exposure = str_sub(exposure, 0, -10) %>% str_remove('_\\b')) %>% 
  pivot_wider(id_cols = c('scapis_id', 'expo_year', 'year'), names_from = 'exposure', values_from = 'value') %>% 
  group_by(scapis_id) %>% 
  summarise(across(-c(expo_year, year),
                  .fns = list( '1y'  = ~ mean(.x[expo_year == year - 1]),
                               '5y'  = ~ mean(.x[expo_year < year & expo_year >= year - 5]),
                               '10y' = ~ mean(.x[expo_year < year & expo_year >= year -10])))) %>% 
  ungroup

df_2 <- df_1 %>% 
  left_join(df_ses, by = c('scapis_id')) %>% 
  left_join(df_uacr, by = c('scapis_id', 'site')) %>% 
  left_join(df_egfr, by = 'scapis_id') %>% 
  left_join(df_expo, by = 'scapis_id') %>% 
  mutate(ckd = case_when((eGFR < 30 & u_acr < 3) |
                           (eGFR < 45 & u_acr >= 3 & u_acr < 30) |
                           (eGFR < 60 & u_acr >= 30) ~ 'very high',
                         (eGFR < 45 & u_acr < 3) |
                           (eGFR < 60 & u_acr >= 3 & u_acr < 30) |
                           (u_acr >= 30) ~ 'high',
                         (eGFR < 60 & u_acr < 3) |
                           (u_acr >= 3) ~ 'moderate',
                         (eGFR > 60 & u_acr < 3) ~ 'low'),
         ckd_d = case_match(ckd, 
                            c('moderate', 'high', 'very high') ~ 1,
                            'low' ~ 0))

df_3 <- df_2 %>% 
  mutate(across(c(age, year), 
                ~ .x - mean(.x, na.rm = T)),
         across(c(ses, 
                  contains('pm25'), 
                  contains('pm10'), 
                  contains('nox'), 
                  contains('ndvi'), 
                  contains('lden'),
                  'ses'),
                .fns = list('s' = ~ (.x - mean(.x, na.rm = T)) / sd(.x, na.rm = T),
                            'l' = ~ log(.x)),
                .names = '{.col}_{.fn}'),
         across(c(contains('_q'), contains('_d'), -contains('dkk3')),
                ~ as.factor(.x) %>% relevel(ref = '1')),
         across(c('eGFR', 'u_acr', 'u_kim1', 'u_dkk3', 'p_kim1'),
                \(x) log(x))
         ) %>% 
  group_by(site) %>% 
  mutate(across(c(ses, 
                  contains('pm25'), 
                  contains('pm10'), 
                  contains('nox'), 
                  contains('ndvi'), 
                  contains('lden'),
                  -contains('rail'),
                  'ses'),
                .fns = list('q' = ~ ntile(.x, 4),
                            'd' = ~ ntile(.x, 2)),
                .names = '{.col}_{.fn}'),
         across(c(contains('_q'), contains('_d'), -contains('dkk3')),
                ~ as.factor(.x) %>% relevel(ref = '1'))
  ) %>% 
  ungroup()
           

saveRDS(df_1, here::here('data', 'df_1.rds'))
saveRDS(df_2, here::here('data', 'df_2.rds'))
saveRDS(df_3, here::here('data', 'df_3.rds'))

