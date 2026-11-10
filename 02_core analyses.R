library('tidyverse')
library('splines')
library('ggh4x')

df_1 <- readRDS(here::here('data', 'df_1.rds'))
df_2 <- readRDS(here::here('data', 'df_2.rds'))
df_3 <- readRDS(here::here('data', 'df_3.rds'))

df_4 <- df_3 %>% filter(!is.na(pm25_total_10y), !is.na(lden_10y), !is.na(ndvi500m_10y), 
                        !is.na(age), !is.na(sex), !is.na(site), !is.na(edu),
                        !is.na(ses_s), !is.na(civ), !is.na(swe), !is.na(smo), 
                        !is.na(pyr), !is.na(lpa), !is.na(alc), !is.na(bmi), !is.na(whr))


# Analyses model 0 -------------------------------------------------------------
model0_lm <- 
  bind_rows(
    expand.grid(
      outcome = c('u_acr', 'eGFR', 'p_kim1', 'u_dkk3'),
      exposure = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m'),
      time_window = c('1y', '5y', '10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>% 
      mutate(
        formula = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix,
                                ' + site'),
        lm = map(formula, \(x) lm(x, data = df_4) %>% broom::tidy(conf.int = T)),
        nobs = map(formula, \(x) lm(x, data = df_4) %>% nobs())
      ),
    expand.grid(
      outcome = c('u_kim1'),
      exposure = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m'),
      time_window = c('1y', '5y', '10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>% 
      mutate(
        formula = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix),
        lm = map(formula, \(x) lm(x, data = df_4) %>% broom::tidy(conf.int = T)),
        nobs = map(formula, \(x) lm(x, data = df_4) %>% nobs())
      )
  ) %>% 
  unnest(lm) %>%
  unnest(nobs) %>% 
  filter(term == paste0(exposure, '_', time_window, suffix))

# Analyses model 1 -------------------------------------------------------------
model1_lm <- 
  bind_rows(
    expand.grid(
      outcome = c('u_acr', 'eGFR', 'p_kim1', 'u_dkk3'),
      exposure = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m'),
      time_window = c('1y', '5y', '10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>% 
      mutate(
        formula = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix, 
                                ' + ns(age, 3) + sex + site'),
        lm = map(formula, \(x) lm(x, data = df_4) %>% broom::tidy(conf.int = T)),
        nobs = map(formula, \(x) lm(x, data = df_4) %>% nobs())
      ),
    expand.grid(
      outcome = c('u_kim1'),
      exposure = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m'),
      time_window = c('1y', '5y', '10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>% 
      mutate(
        formula = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix, 
                                ' + ns(age, 3) + sex'),
        lm = map(formula, \(x) lm(x, data = df_4) %>% broom::tidy(conf.int = T)),
        nobs = map(formula, \(x) lm(x, data = df_4) %>% nobs())
      )
  ) %>% 
  unnest(lm) %>%
  unnest(nobs) %>% 
  filter(term == paste0(exposure, '_', time_window, suffix))

# Analyses model 2 -------------------------------------------------------------
model2_lm <- 
  bind_rows(
    expand.grid(
      outcome = c('u_acr', 'eGFR', 'u_dkk3', 'p_kim1'),
      exposure = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m'),
      time_window = c('1y', '5y', '10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>% 
      mutate(
        formula = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix, 
                                ' + ns(age, 3) + sex + site',
                                ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        lm = map(formula, \(x) lm(x, data = df_4) %>% broom::tidy(conf.int = T)),
        nobs = map(formula, \(x) lm(x, data = df_4) %>% nobs())
      ),
    expand.grid(
      outcome = c('u_kim1'),
      exposure = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m'),
      time_window = c('1y', '5y', '10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>% 
      mutate(
        formula = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix, 
                                ' + ns(age, 3) + sex',
                                ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        lm = map(formula, \(x) lm(x, data = df_4) %>% broom::tidy(conf.int = T)),
        nobs = map(formula, \(x) lm(x, data = df_4) %>% nobs())
      )
  ) %>% 
  unnest(lm) %>%
  unnest(nobs) %>% 
  filter(term == paste0(exposure, '_', time_window, suffix))

# Site stratification ----------------------------------------------------------
model2_lm_site <- 
  bind_rows(
    expand.grid(
      outcome = c('eGFR', 'p_kim1', 'u_acr', 'u_dkk3'),
      exposure = c('pm25_total', 'lden', 'ndvi500m'),
      time_window = c('10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>%
      mutate(
        formula = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix, 
                         ':site + site',
                         ' + ns(age, 3) + sex',
                         ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        lm = map(formula, \(x) lm(x, data = df_4) %>% broom::tidy(conf.int = T))
      )
  ) %>% 
  unnest(lm) %>%
  filter(str_detect(term, '\\:')) %>% 
  separate(term, into = c('term', 'site'), sep = ':site')

model2_lm_site_inter <- 
  bind_rows(
    expand.grid(
      outcome = c('eGFR', 'u_acr', 'u_dkk3', 'p_kim1'),
      exposure = c('pm25_total', 'lden', 'ndvi500m'),
      time_window = c('10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>%
      mutate(
        formula1 = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix, 
                         ' + site',
                         ' + ns(age, 3) + sex',
                         ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        formula2 = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix, 
                          ':site + site',
                          ' + ns(age, 3) + sex',
                          ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        p_site = map2(formula1, formula2,
                      \(x,y) anova(lm(x, data = df_4),
                                 lm(y, data = df_4))[2, 'Pr(>F)'])
      )
  ) %>% 
  unnest(p_site)

# Two-exposure models ----------------------------------------------------------
model2_lm_multi <- 
  bind_rows(
    expand.grid(
      outcome = c('u_acr', 'eGFR', 'u_dkk3', 'p_kim1'),
      exposure1 = c('pm25_total', 'lden', 'ndvi500m'),
      exposure2 = c('pm25_total', 'lden', 'ndvi500m'),
      time_window = c('10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>% 
      filter(exposure1 != exposure2) %>% 
      mutate(
        formula = paste0(outcome, ' ~ ', exposure1, '_', time_window, suffix, 
                                ' + ', exposure2, '_', time_window, suffix, 
                                ' + ns(age, 3) + sex + site',
                                ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        lm = map(formula, \(x) lm(x, data = df_4) %>% broom::tidy(conf.int = T)),
        nobs = map(formula, \(x) lm(x, data = df_4) %>% nobs()),
        formula_inter = paste0(outcome, ' ~ ', exposure1, '_', time_window, suffix, 
                                ' * ', exposure2, '_', time_window, suffix, 
                                ' + ns(age, 3) + sex + site',
                                ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        p_inter = map(formula_inter, \(x) lm(x, data = df_4) %>% broom::tidy() %>% filter(str_detect(term, '\\:')) %>% pull('p.value'))
      ),
    expand.grid(
      outcome = c('u_kim1'),
      exposure1 = c('pm25_total', 'lden', 'ndvi500m'),
      exposure2 = c('pm25_total', 'lden', 'ndvi500m'),
      time_window = c('10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>% 
      filter(exposure1 != exposure2) %>% 
      mutate(
        formula = paste0(outcome, ' ~ ', exposure1, '_', time_window, suffix, 
                                ' + ', exposure2, '_', time_window, suffix, 
                                ' + ns(age, 3) + sex',
                                ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        lm = map(formula, \(x) lm(x, data = df_4) %>% broom::tidy(conf.int = T)),
        nobs = map(formula, \(x) lm(x, data = df_4) %>% nobs()),
        formula_inter = paste0(outcome, ' ~ ', exposure1, '_', time_window, suffix, 
                                ' * ', exposure2, '_', time_window, suffix, 
                                ' + ns(age, 3) + sex',
                                ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        p_inter = map(formula_inter, \(x) lm(x, data = df_4) %>% broom::tidy() %>% filter(str_detect(term, '\\:')) %>% pull('p.value'))
      )
  ) %>% 
  unnest(lm) %>%
  unnest(nobs) %>%
  unnest(p_inter) %>%
  filter(term == paste0(exposure1, '_', time_window, suffix) |
           term == paste0(exposure2, '_', time_window, suffix))

# Effect modification ----------------------------------------------------------
df_5 <- df_4 %>%
  mutate(age_gr = case_when(age < 55  - mean(df_1$age, na.rm = T) ~ '1', 
                            age < 60  - mean(df_1$age, na.rm = T) ~ '2',
                            age >= 60 - mean(df_1$age, na.rm = T) ~ '3') %>%
           factor(ordered = F) %>% 
           relevel(ref = '1'),
         sex = relevel(sex, ref = 'MALE'),
         edu = relevel(edu, ref = '1'),
         ses_d = factor(ses_s > 0) %>% relevel(ref = 'TRUE'),
         civ = relevel(civ, ref = 'MARRIED'),
         swe = relevel(swe, ref = 'YES'),
         smo = relevel(smo, ref = 'NEVER'),
         lpa_d = recode_factor(lpa, '3' = '2') %>% relevel(ref = '1'),
         alc_d = recode_factor(alc, '0' = '1', '2' = '1', '3' = '2', '4' = '2') %>% relevel(ref = '1'),
         bmi_cat = relevel(bmi_cat, ref = '1'),
         whr_d = factor((sex == 'MALE' & whr > 0.9) | (sex == 'FEMALE' & whr > 0.85)) %>% relevel(ref = 'FALSE'),
         dia_d = recode_factor(dia, '2' = '1') %>% relevel(ref = '0'),
         hpt = relevel(hpt, ref = '0'),
         u_acr3 = factor(u_acr > log(3)) %>% relevel(ref = 'FALSE')
  )

model2_lm_em <- 
  bind_rows(
    expand.grid(
      outcome = c('u_acr', 'eGFR', 'u_dkk3', 'p_kim1'),
      exposure = c('pm25_total', 'lden', 'ndvi500m'),
      em = c('age_gr', 'sex', 'edu', 'ses_d', 'civ', 'swe', 'smo', 'lpa_d', 'alc_d', 'bmi_cat', 'whr_d', 'dia_d', 'hpt', 'u_acr3'),
      time_window = c('10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>%
      filter(!(em == 'u_acr3' & outcome != 'eGFR')) %>% 
      mutate(
        formula = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix,  
                         ' : ', em, 
                         ' + ns(age, 3) + sex + site',
                         ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        lm = map(formula, \(x) lm(x, data = df_5) %>% broom::tidy(conf.int = T))) %>% 
      mutate(
        formula_inter = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix, 
                               ' * ', em, 
                               ' + ns(age, 3) + sex + site',
                               ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        p_inter = map(formula_inter, \(x) lm(x, data = df_5) %>% broom::tidy(conf.int = T) %>% filter() %>% filter(str_detect(term, '\\:')) %>% pull('p.value'))
      ),
    expand.grid(
      outcome = c('u_kim1'),
      exposure = c('pm25_total', 'lden', 'ndvi500m'),
      em = c('age_gr', 'sex', 'edu', 'ses_d', 'civ', 'swe', 'smo', 'lpa_d', 'alc_d', 'bmi_cat', 'whr_d', 'dia_d', 'hpt'),
      time_window = c('10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>% 
      mutate(
        formula = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix,  
                         ' : ', em, 
                         ' + ns(age, 3) + sex',
                         ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        lm = map(formula, \(x) lm(x, data = df_5) %>% broom::tidy(conf.int = T)),
        formula_inter = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix, 
                               ' * ', em, 
                               ' + ns(age, 3) + sex',
                               ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        p_inter = map(formula_inter, \(x) lm(x, data = df_5) %>% broom::tidy(conf.int = T) %>% filter() %>% filter(str_detect(term, '\\:')) %>% pull('p.value'))
      )
  ) %>% 
  unnest(lm) %>%
  filter(str_detect(term, paste0(exposure, '_', time_window, suffix))) %>% 
  group_by(outcome, exposure, em) %>% 
  mutate(p_inter = {
    p <- first(p_inter)
    c(NA_real_, p) }) %>% 
  ungroup()

# Exposure-response ------------------------------------------------------------
model2_lm_er <- 
  bind_rows(
    expand.grid(
      outcome = c('u_acr', 'eGFR', 'u_dkk3', 'p_kim1'),
      exposure = c('pm25_total', 'lden', 'ndvi500m'),
      time_window = c('1y', '5y', '10y')
    ) %>% 
      as_tibble() %>%
      mutate(
        formula = paste0(outcome, ' ~ ', exposure, '_', time_window, '_q',  
                                ' + ns(age, 3) + sex + site',
                                ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        lm = map(formula, \(x) lm(x, data = df_4) %>% broom::tidy(conf.int = T)),
        formula_trend = paste0(outcome, ' ~ as.integer(', exposure, '_', time_window, '_q)', 
                                      ' + ns(age, 3) + sex + site',
                                      ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        p_trend = map(formula_trend, \(x) lm(x, data = df_4) %>% 
                         broom::tidy() %>% 
                         filter(str_detect(term, 'as.integer')) %>% 
                         pull('p.value'))
      ),
    expand.grid(
      outcome = c('u_kim1'),
      exposure = c('pm25_total', 'lden', 'ndvi500m'),
      time_window = c('1y', '5y', '10y')
    ) %>% 
      as_tibble() %>%  
      mutate(
        formula = paste0(outcome, ' ~ ', exposure, '_', time_window, '_q',  
                                ' + ns(age, 3) + sex',
                                ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        lm = map(formula, \(x) lm(x, data = df_4) %>% broom::tidy(conf.int = T)),
        formula_trend = paste0(outcome, ' ~ as.integer(', exposure, '_', time_window, '_q)', 
                                      ' + ns(age, 3) + sex',
                                      ' + edu + ns(ses_s, 3) + civ + swe + smo + pyr + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        p_trend = map(formula_trend, \(x) lm(x, data = df_4) %>% 
                         broom::tidy() %>%
                         filter(str_detect(term, 'as.integer')) %>% 
                         pull('p.value'))
      )
  ) %>% 
  unnest(lm) %>%
  unnest(p_trend) %>% 
  filter(str_detect(term, paste0(exposure, '_', time_window)))

# Only non-smokers -------------------------------------------------------------
df_5 <- df_4 %>% filter(smo == 'NEVER')

model2_lm_non_smo <- 
  bind_rows(
    expand.grid(
      outcome = c('u_acr', 'eGFR', 'u_dkk3', 'p_kim1'),
      exposure = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m'),
      time_window = c('1y', '5y', '10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>% 
      mutate(
        formula = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix, 
                         ' + ns(age, 3) + sex + site',
                         ' + edu + ns(ses_s, 3) + civ + swe + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        lm = map(formula, \(x) lm(x, data = df_5) %>% broom::tidy(conf.int = T)),
        nobs = map(formula, \(x) lm(x, data = df_5) %>% nobs())
      ),
    expand.grid(
      outcome = c('u_kim1'),
      exposure = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m'),
      time_window = c('1y', '5y', '10y'),
      suffix = '_s'
    ) %>% 
      as_tibble() %>% 
      mutate(
        formula = paste0(outcome, ' ~ ', exposure, '_', time_window, suffix, 
                         ' + ns(age, 3) + sex',
                         ' + edu + ns(ses_s, 3) + civ + swe + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
        lm = map(formula, \(x) lm(x, data = df_5) %>% broom::tidy(conf.int = T)),
        nobs = map(formula, \(x) lm(x, data = df_5) %>% nobs())
      )
  ) %>% 
  unnest(lm) %>%
  unnest(nobs) %>% 
  filter(term == paste0(exposure, '_', time_window, suffix))

# Table ------------------------------------------------------------------------
sd_table <- df_2 %>%
  select(
    apply(expand.grid(
      c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m'),
      c('1y', '5y', '10y')), 1, paste, collapse = '_')
  ) %>% 
  summarise(across(everything(), \(x) sd(x, na.rm = T))) %>% 
  pivot_longer(cols = everything(), names_to = 'exposure_time', values_to = 'sd') %>% 
  mutate(int = case_when(str_detect(exposure_time, 'pm25') ~ 2,
                         str_detect(exposure_time, 'pm10') ~ 5,
                         str_detect(exposure_time, 'nox') ~ 10,
                         str_detect(exposure_time, 'lden') ~ 10,
                         str_detect(exposure_time, 'ndvi') ~ .15))

bind_rows(model0_lm %>% mutate(model = 'm0'),
          model1_lm %>% mutate(model = 'm1'),
          model2_lm %>% mutate(model = 'm2')) %>% 
  mutate(exposure = as.character(exposure),
         exposure_time = paste(exposure, time_window, sep = '_')) %>% 
  left_join(sd_table, by = 'exposure_time') %>% 
  mutate(across(c('estimate', 'conf.low', 'conf.high'), \(x) { (exp(x * int / sd) - 1) }),
         res = paste0(sprintf('%.2f (%.2f, %.2f)', 
                              estimate * 100,
                              conf.low * 100,
                              conf.high * 100))) %>% 
  writexl::write_xlsx(here::here(paste0(Sys.Date(), '_res_lm.xlsx')))

model2_lm_site %>% 
  left_join(model2_lm_site_inter %>% 
              select(exposure, outcome, time_window, p_site)) %>% 
  mutate(exposure = as.character(exposure),
         exposure_time = paste(exposure, time_window, sep = '_')) %>% 
  left_join(sd_table, by = 'exposure_time') %>%
  mutate(across(c('estimate', 'conf.low', 'conf.high'), \(x) { (exp(int * x / sd) - 1) }),
         res = paste0(sprintf('%.2f (%.2f, %.2f)',
                              estimate * 100,
                              conf.low * 100,
                              conf.high * 100))) %>% 
  writexl::write_xlsx(here::here(paste0(Sys.Date(), '_res_lm_site.xlsx')))

model2_lm_site_meta <- model2_lm_site %>% 
  left_join(model2_lm_site_inter %>% 
              select(exposure, outcome, time_window, p_site)) %>% 
  mutate(exposure = as.character(exposure),
         exposure_time = paste(exposure, time_window, sep = '_')) %>% 
  left_join(sd_table, by = 'exposure_time') %>% 
  transmute(outcome,
            exposure,
            time_window,
            yi = estimate,
            sei = std.error) %>% 
  group_by(outcome, exposure, time_window) %>% 
  nest() %>% 
  mutate(k = map_int(data, nrow)) %>% 
  mutate(model = map(data, ~ metafor::rma(
    yi = yi,
    sei = sei,
    data = .x,
    method = 'REML'
  ))) %>% 
  transmute(
    outcome, exposure, time_window,
    estimate  = map_dbl(model, ~as.numeric(coef(.x))),
    std.error = map_dbl(model, ~.x$se),
    lcl       = map_dbl(model, ~.x$ci.lb),
    ucl       = map_dbl(model, ~.x$ci.ub),
    p         = map_dbl(model, ~.x$pval),
    i2        = map_dbl(model, ~.x$I2)
  )
writexl::write_xlsx(model2_lm_site_meta, here::here(paste0(Sys.Date(), '_res_lm_site_meta.xlsx')))

model2_lm_multi %>% 
  bind_rows() %>% 
  mutate(exposure1 = as.character(exposure1),
         exposure2 = as.character(exposure2),
         exposure_time = paste(exposure1, time_window, sep = '_')) %>% 
  left_join(sd_table, by = 'exposure_time') %>% 
  mutate(across(c('estimate', 'conf.low', 'conf.high'), \(x) { (exp(int * x / sd) - 1) })) %>% 
  mutate(res = paste0(sprintf('%.2f', estimate * 100), ' (',
                      sprintf('%.2f', conf.low * 100), ', ',
                      sprintf('%.2f', conf.high * 100), ')')) %>% 
  writexl::write_xlsx(here::here(paste0(Sys.Date(), '_res_lm_multi.xlsx')))

model2_lm_em %>% 
  bind_rows() %>% 
  mutate(exposure = as.character(exposure),
         exposure_time = paste(exposure, time_window, sep = '_')) %>% 
  left_join(sd_table, by = 'exposure_time') %>% 
  mutate(across(c('estimate', 'conf.low', 'conf.high'), \(x) { (exp(int * x / sd) - 1) }),
         stratum = str_extract(term, '(?<=:).*')) %>% 
  mutate(res = paste0(sprintf('%.2f', estimate * 100), ' (',
                      sprintf('%.2f', conf.low * 100), ', ',
                      sprintf('%.2f', conf.high * 100), ')')) %>% 
  writexl::write_xlsx(here::here(paste0(Sys.Date(), '_res_lm_em.xlsx')))

model2_lm_er %>% 
  mutate(exposure = as.character(exposure),
         exposure_time = paste(exposure, time_window, sep = '_')) %>% 
  mutate(across(c('estimate', 'conf.low', 'conf.high'), \(x) { (exp(x) - 1) }),
         quartile = str_sub(term, -2, -1)) %>% 
  mutate(res = paste0(sprintf('%.2f', estimate * 100), ' (',
                      sprintf('%.2f', conf.low * 100), ', ',
                      sprintf('%.2f', conf.high * 100), ')')) %>% 
  writexl::write_xlsx(here::here(paste0(Sys.Date(), '_res_lm_er.xlsx')))

# Plots ------------------------------------------------------------------------
model2_lm %>% 
  mutate(exposure_time = paste0(exposure, '_', time_window)) %>% 
  left_join(sd_table) %>% 
  filter(time_window == '10y') %>% 
  mutate(
    across(c(estimate, conf.low, conf.high), \(x) exp(x * int / sd) - 1),
    association_label = sprintf("%.2f%% (%.2f, %.2f)", 100 * estimate, 100 * conf.low, 100 * conf.high),
    exposure = factor(exposure,
                      levels = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m') %>% rev(),
                      labels = c('PM2.5', 'PM10', 'NOx', 'Lden', 'NDVI') %>% rev(),
                      ordered = T),
    outcome = factor(outcome,
                     levels = c('eGFR', 'u_acr', 'u_kim1', 'u_dkk3', 'p_kim1'),
                     labels = c('eGFR', 'uACR', 'uKIM1', 'uDKK3', 'pKIM1'),
                     ordered = T),
    p_colour = case_when(p.value >= 0.1 ~ '0',
                         p.value >= 0.05 ~ '1',
                         p.value < 0.05 ~ '2')) %>%
  ggplot(aes(x = estimate, y = exposure, colour = p_colour)) +
  geom_vline(xintercept = 0, 
             colour = 'grey55', 
             linewidth = 0.5, 
             linetype = 'dotted') +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0,
                 linewidth = .6,
                 position = position_dodge(width = .5)) +
  geom_point(size = 2,
             position = position_dodge(width = .5)) +
  geom_text(aes(label = association_label, x = Inf),
            hjust = 1.03,
            size = 2.7) +
  facet_wrap(vars(outcome),
             scales = 'free_x') +
  scale_x_continuous(expand = expansion(mult = c(.08, 1.65)),
                     labels = scales::percent,
                     trans = 'log1p') +
  scale_colour_manual(values = c('2' = 'darkred', '1' = 'darkorange', '0' = 'grey20')) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = 'none',
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text.x = element_text(face = 'bold'),
    strip.text.y.left = element_text(face = 'bold'),
    panel.spacing.x = unit(.5, 'lines'),
    panel.grid.major.y = element_line(colour = 'grey94', linewidth = 4),
    panel.grid.major.x = element_line(colour = 'white', linewidth = .5),
    axis.text = element_text(colour = 'black'),
    axis.text.y.left = element_text(hjust = 0, margin = margin(r = -1, l = 2)),
    axis.text.y = element_text(hjust = 0),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(6, 12, 6, 6)
  )
ggsave(here::here('figures', paste0(Sys.Date(), '_fig1.svg')), width = 6, height = 2.5)
ggsave(here::here('figures', paste0(Sys.Date(), '_fig1.png')), width = 6, height = 2.5)

bind_rows(
  model0_lm %>% mutate(model = 0),
  model1_lm %>% mutate(model = 1),
  model2_lm %>% mutate(model = 2)
) %>% 
  mutate(exposure_time = paste0(exposure, '_', time_window)) %>% 
  left_join(sd_table) %>% 
  mutate(
    across(c(estimate, conf.low, conf.high), \(x) exp(x * int / sd) - 1),
    association_label = sprintf("%.2f%% (%.2f, %.2f)", 100 * estimate, 100 * conf.low, 100 * conf.high),
    exposure = factor(exposure,
                      levels = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m') %>% rev(),
                      labels = c('PM2.5', 'PM10', 'NOx', 'Lden', 'NDVI') %>% rev(),
                      ordered = T),
    outcome = factor(outcome,
                     levels = c('eGFR', 'u_acr', 'u_kim1', 'u_dkk3', 'p_kim1'),
                     labels = c('eGFR', 'uACR', 'uKIM1', 'uDKK3', 'pKIM1'),
                     ordered = T),
    model = factor(model,
                   levels = 0:2 %>% rev(),
                   labels = c('Model 0', 'Model 1', 'Model 2') %>% rev(),
                   ordered = T),
    time_window = factor(time_window,
                         levels = c('1y', '5y', '10y') %>% rev(),
                         labels = c('1-year mean', '5-year mean', '10-year mean') %>% rev(),
                         ordered = T),
    p_colour = case_when(p.value >= 0.1 ~ '0',
                         p.value >= 0.05 ~ '1',
                         p.value < 0.05 ~ '2')) %>%
  ggplot(aes(x = estimate, y = interaction(model, exposure, sep = '!'), colour = p_colour)) +
  geom_vline(xintercept = 0, 
             colour = 'grey55', 
             linewidth = 0.5, 
             linetype = 'dotted') +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0,
                 linewidth = .6) +
  geom_point(size = 2) +
  geom_text(aes(label = association_label, x = Inf),
            hjust = 1.03,
            size = 2.7) +
  facet_grid(time_window ~ outcome,
             scales = 'free_x',
             switch = 'y') +
  scale_y_discrete(guide = guide_axis_nested(delim = '!', colour = 'black')) +
  scale_x_continuous(expand = expansion(mult = c(.08, 1.65)),
                     labels = scales::percent,
                     trans = 'log1p') +
  scale_colour_manual(values = c('2' = 'darkred', '1' = 'darkorange', '0' = 'grey20')) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = 'none',
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text.x = element_text(face = 'bold'),
    strip.text.y.left = element_text(face = 'bold'),
    ggh4x.axis.nesttext.y = element_text(vjust = 0.5, face = 'bold'),
    panel.spacing.x = unit(.5, 'lines'),
    panel.grid.major.y = element_line(colour = 'grey94', linewidth = 4),
    panel.grid.major.x = element_line(colour = 'white', linewidth = .5),
    axis.text = element_text(colour = 'black'),
    axis.text.y.left = element_text(hjust = 0, margin = margin(r = -1, l = 2)),
    axis.text.y = element_text(hjust = 0),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(6, 12, 6, 6)
  )
ggsave(here::here('figures', paste0(Sys.Date(), '_figS1_models_time.svg')), width = 12, height = 7)
ggsave(here::here('figures', paste0(Sys.Date(), '_figS1_models_time.png')), width = 12, height = 7)

model2_lm_multi %>% 
  filter(str_detect(term, as.character(exposure1))) %>% 
  mutate(exposure_time = paste0(exposure1, '_', time_window)) %>% 
  left_join(sd_table) %>%
  mutate(
    across(c(estimate, conf.low, conf.high), \(x) exp(x * int / sd) - 1),
    p_label = case_when(p_inter < 0.001 ~ '<0.001',
                        is.na(p_inter) ~ '',
                        T ~ sprintf("%.3f", p_inter)),
    exposure1 = factor(exposure1,
                       levels = c('pm25_total', 'lden', 'ndvi500m') %>% rev(),
                       labels = c('PM2.5', 'Lden', 'NDVI') %>% rev(),
                       ordered = T),
    exposure2 = factor(exposure2,
                       levels = c('pm25_total', 'lden', 'ndvi500m') %>% rev(),
                       labels = c('PM2.5', 'Lden', 'NDVI') %>% rev(),
                       ordered = T),
    outcome = case_match(outcome,
                         'u_acr' ~ 'uACR',
                         'eGFR' ~ 'eGFR',
                         'p_kim1' ~ 'pKIM1',
                         'u_kim1' ~ 'uKIM1',
                         'u_dkk3' ~ 'uDKK3') %>% 
      factor(levels = c('eGFR', 'uACR', 'uKIM1', 'uDKK3', 'pKIM1'),
             ordered = T),
    p_colour = case_when(p.value >= 0.1 ~ '0',
                         p.value >= 0.05 ~ '1',
                         p.value < 0.05 ~ '2'),
    p_inter_colour = case_when(p_inter >= 0.1 ~ '0',
                               p_inter >= 0.05 ~ '1',
                               p_inter < 0.05 ~ '2')
  ) %>% 
  ggplot(aes(x = estimate, y = interaction(exposure2, exposure1, sep = '!'), colour = p_colour)) +
  geom_vline(xintercept = 0, 
             colour = 'grey55', 
             linewidth = 0.5, 
             linetype = 'dotted') +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0,
                 linewidth = .6,
                 position = position_dodge(width = .5)) +
  geom_point(size = 2,
             position = position_dodge(width = .5)) +
  geom_text(aes(label = p_label, x = Inf, colour = p_inter_colour),
            hjust = 1.1,
            size = 2.7) +
  facet_wrap(vars(outcome),
             scales = 'free_x') +
  scale_y_discrete(guide = guide_axis_nested(delim = '!', colour = 'black')) +
  scale_x_continuous(expand = expansion(mult = c(.08, .35)),
                     labels = scales::percent,
                     trans = 'log1p') +
  scale_colour_manual(values = c('2' = 'darkred', '1' = 'darkorange', '0' = 'grey20')) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = 'none',
    ggh4x.axis.nesttext.y = element_text(vjust = 0.5, face = 'bold'),
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text.x = element_text(face = 'bold'),
    strip.text.y.left = element_text(face = 'bold'),
    panel.spacing.x = unit(.5, 'lines'),
    panel.grid.major.y = element_line(colour = 'grey94', linewidth = 3),
    panel.grid.major.x = element_line(colour = 'white', linewidth = .5),
    axis.text = element_text(colour = 'black'),
    axis.text.y.left = element_text(hjust = 0, margin = margin(r = -1, l = 2)),
    axis.text.y = element_text(hjust = 0),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(6, 12, 6, 6)
  )
ggsave(here::here('figures', paste0(Sys.Date(), '_figS2_multi.svg')), width = 6, height = 2.5)
ggsave(here::here('figures', paste0(Sys.Date(), '_figS2_multi.png')), width = 6, height = 2.5)

model2_lm_site %>% 
  bind_rows(model2_lm_site_meta %>% 
              select(exposure, outcome, estimate, conf.low = lcl, conf.high = ucl) %>% 
              mutate(site = 'meta')) %>% 
  left_join(model2_lm_site_inter %>% 
              select(outcome, exposure, time_window, p_site)) %>% 
  mutate(exposure_time = paste0(exposure, '_', time_window),
         meta = site == 'meta') %>% 
  left_join(sd_table) %>% 
  mutate(
    across(c(estimate, conf.low, conf.high), \(x) exp(x * int / sd) - 1),
    interaction_label = sprintf("%.f2 (%.2f, %.2f)", estimate, conf.low, conf.high),
    p_label = case_when(site != 'meta' ~ '',
                        p_site < 0.001 ~ '<0.001',
                        is.na(p_site) ~ '',
                        T ~ sprintf("%.3f", p_site)),
    exposure = factor(exposure,
                      levels = c('pm25_total', 'lden', 'ndvi500m'),
                      labels = c('PM2.5', 'Lden', 'NDVI'),
                      ordered = T),
    outcome = factor(outcome,
                     levels = c('eGFR', 'u_acr', 'u_kim1', 'u_dkk3', 'p_kim1'),
                     labels = c('eGFR', 'uACR', 'uKIM1', 'uDKK3', 'pKIM1'),
                     ordered = T),
    site = factor(site,
                  levels = c('got',
                             'lin',
                             'mal',
                             'sth',
                             'ume',
                             'upp',
                             'meta') %>% rev(),
                  labels = c('Gothenburg',
                             'Link\u00f6ping',
                             'Malm\u00f6',
                             'Stockholm',
                             'Ume\u00e5',
                             'Uppsala',
                             'Meta-estimate') %>% rev(),
                  ordered = T),
    p_colour = case_when(p.value >= 0.1 ~ '0',
                         p.value >= 0.05 ~ '1',
                         p.value < 0.05 ~ '2'),
    p_site_colour = case_when(p_site >= 0.1 ~ '0',
                              p_site >= 0.05 ~ '1',
                              p_site < 0.05 ~ '2')) %>%
  ggplot(aes(x = estimate, y = site, colour = p_colour)) +
  geom_vline(xintercept = 0, 
             colour = 'grey55', 
             linewidth = 0.5, 
             linetype = 'dotted') +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0,
                 linewidth = .6,
                 position = position_dodge(width = .5)) +
  geom_point(aes(shape = meta),
             size = 2,
             position = position_dodge(width = .5)) +
  geom_text(aes(label = p_label, x = Inf, colour = p_site_colour),
            hjust = 1.1,
            size = 2.7) +
  facet_grid(exposure ~ outcome,
             scales = 'free_x',
             switch = 'y') +
  scale_x_continuous(expand = expansion(mult = c(.08, .35)),
                     labels = scales::percent,
                     trans = 'log1p') +
  scale_colour_manual(values = c('2' = 'darkred', '1' = 'darkorange', '0' = 'grey20')) +
  scale_shape_manual(values = c('TRUE' = 18, 'FALSE' = 16)) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = 'none',
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text.x = element_text(face = 'bold'),
    strip.text.y.left = element_text(face = 'bold'),
    panel.spacing.x = unit(.5, 'lines'),
    panel.grid.major.y = element_line(colour = 'grey94', linewidth = 3),
    panel.grid.major.x = element_line(colour = 'white', linewidth = .5),
    axis.text = element_text(colour = 'black'),
    axis.text.y.left = element_text(hjust = 0, margin = margin(r = -1, l = 2)),
    axis.text.y = element_text(hjust = 0),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(6, 12, 6, 6)
  )
ggsave(here::here('figures', paste0(Sys.Date(), '_figS3_site.svg')), width = 10, height = 2.75)
ggsave(here::here('figures', paste0(Sys.Date(), '_figS3_site.png')), width = 10, height = 2.75)

model2_lm_em %>% 
  mutate(exposure_time = paste0(exposure, '_', time_window)) %>% 
  left_join(sd_table) %>% 
  mutate(
    across(c(estimate, conf.low, conf.high), \(x) exp(x * int / sd) - 1),
    interaction_label = sprintf("%.f2 (%.2f, %.2f)", estimate, conf.low, conf.high),
    p_label = case_when(p_inter < 0.001 ~ '<0.001',
                        is.na(p_inter) ~ '',
                        T ~ sprintf("%.3f", p_inter)),
    exposure = factor(exposure,
                      levels = c('pm25_total', 'lden', 'ndvi500m'),
                      labels = c('PM2.5', 'Lden', 'NDVI'),
                      ordered = T),
    outcome = case_match(outcome,
                         'u_acr' ~ 'uACR',
                         'eGFR' ~ 'eGFR',
                         'p_kim1' ~ 'pKIM1',
                         'u_kim1' ~ 'uKIM1',
                         'u_dkk3' ~ 'uDKK3') %>% 
      factor(levels = c('eGFR', 'uACR', 'uKIM1', 'uDKK3', 'pKIM1'),
             ordered = T),
    stratum = str_extract(term, '(?<=:).*'),
    emlabel = factor(em,
                     levels = c('age_gr', 'sex', 'edu', 'ses_d', 'civ', 'swe', 'smo', 'lpa_d', 'alc_d', 'bmi_cat', 'whr_d', 'dia_d', 'hpt', 'u_acr3') %>% rev(),
                     labels = c('Age group', 'Sex', 'Education', 'SES', 'Civil status', 'Country of birth', 'Smoking', 'Physical activity', 'Alcohol', 'BMI', 'WHR', 'Diabetes', 'Hypertension', 'uACR') %>% rev(),
                     ordered = T),
    stratumlabel = factor(stratum,
                          levels = c('age_gr1',
                                     'age_gr2',
                                     'age_gr3', 
                                     'sexMALE',
                                     'sexFEMALE',
                                     'edu1',
                                     'edu2',
                                     'edu3',
                                     'ses_dTRUE',
                                     'ses_dFALSE',
                                     'civMARRIED',
                                     'civUNMARRIED',
                                     'sweYES',
                                     'sweNO',
                                     'smoNEVER',
                                     'smoEX_SMOKER',
                                     'smoCURRENT',
                                     'lpa_d1',
                                     'lpa_d2',
                                     'alc_d1',
                                     'alc_d2',
                                     'bmi_cat1',
                                     'bmi_cat2',
                                     'bmi_cat3',
                                     'whr_dFALSE',
                                     'whr_dTRUE',
                                     'dia_d0',
                                     'dia_d1',
                                     'hpt0',
                                     'hpt1',
                                     'u_acr3FALSE',
                                     'u_acr3TRUE'
                                     ) %>% rev(),
                          labels = c('50-54y', #age
                                     '55-59y',
                                     '60-64y',
                                     'Men', #sex
                                     'Women',
                                     'Compulsory', #edu
                                     'Secondary',
                                     'Tertiary',
                                     'Above median', #ses
                                     'Below median',
                                     'Married', #civ
                                     'Unmarried',
                                     'Sweden',  #swe
                                     'Not Sweden',
                                     'Never', #smo
                                     'Former',
                                     'Current',
                                     'Sedentary', #lpa
                                     'Regular',
                                     '\u22641 per week', #alc
                                     '>1 per week',
                                     '<25', #bmi
                                     '25-30',
                                     '\u226530',
                                     'Normal', #whr
                                     'Elevated',
                                     'No', #dia
                                     'Yes',
                                     'No', #hpt
                                     'Yes',
                                     '\u22643 mg/mmol', #uacr
                                     '>3 mg/mmol'
                                     ) %>% rev(),
                          ordered = T),
    p_colour = case_when(p.value >= 0.1 ~ '0',
                         p.value >= 0.05 ~ '1',
                         p.value < 0.05 ~ '2'),
    p_inter_colour = case_when(p_inter >= 0.1 ~ '0',
                               p_inter >= 0.05 ~ '1',
                               p_inter < 0.05 ~ '2')
  ) %>% 
  ggplot(aes(x = estimate, y = interaction(stratumlabel, emlabel, sep = '!'), colour = p_colour)) +
  geom_vline(xintercept = 0, 
             colour = 'grey55', 
             linewidth = 0.5, 
             linetype = 'dotted') +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0,
                 linewidth = .6,
                 position = position_dodge(width = .5)) +
  geom_point(size = 2,
             position = position_dodge(width = .5)) +
  geom_text(aes(label = p_label, x = Inf, colour = p_inter_colour),
            hjust = 1.1,
            size = 2.7) +
  facet_grid(exposure ~ outcome,
             scales = 'free_x',
             switch = 'y') +
  scale_y_discrete(guide = guide_axis_nested(delim = '!', colour = 'black')) +
  scale_x_continuous(expand = expansion(mult = c(.08, .35)),
                     labels = scales::percent,
                     trans = 'log1p') +
  scale_colour_manual(values = c('2' = 'darkred', '1' = 'darkorange', '0' = 'grey20')) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = 'none',
    ggh4x.axis.nesttext.y = element_text(vjust = 0.5, face = 'bold'),
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text.x = element_text(face = 'bold'),
    strip.text.y.left = element_text(face = 'bold'),
    panel.spacing.x = unit(.5, 'lines'),
    panel.grid.major.y = element_line(colour = 'grey94', linewidth = 3),
    panel.grid.major.x = element_line(colour = 'white', linewidth = .5),
    axis.text = element_text(colour = 'black'),
    axis.text.y.left = element_text(hjust = 0, margin = margin(r = -1, l = 2)),
    axis.text.y = element_text(hjust = 0),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(6, 12, 6, 6)
  )
ggsave(here::here('figures', paste0(Sys.Date(), '_figS4_em.svg')), width = 15, height = 10)
ggsave(here::here('figures', paste0(Sys.Date(), '_figS4_em.png')), width = 15, height = 10)

model2_lm_er %>% 
  mutate(exposure_time = paste0(exposure, '_', time_window)) %>% 
  mutate(
    across(c(estimate, conf.low, conf.high), \(x) exp(x) - 1),
    quartile = str_sub(term, -2, -1) %>%  
      factor(levels = paste0('q', 2:4) %>% rev(),
             labels = paste0('Q', 2:4) %>% rev(),
             ordered = T),
    p_label = case_when(quartile != 'Q4' ~ '',
                        p_trend < 0.001 ~ '<0.001',
                        T ~ sprintf("%.3f", p_trend)),
    exposure = factor(exposure,
                      levels = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m') %>% rev(),
                      labels = c('PM2.5', 'PM10', 'NOx', 'Lden', 'NDVI') %>% rev(),
                      ordered = T),
    outcome = factor(outcome,
                     levels = c('eGFR', 'u_acr', 'u_kim1', 'u_dkk3', 'p_kim1'),
                     labels = c('eGFR', 'uACR', 'uKIM1', 'uDKK3', 'pKIM1'),
                     ordered = T),
    time_window = factor(time_window,
                         levels = c('1y', '5y', '10y') %>% rev(),
                         labels = c('1-year mean', '5-year mean', '10-year mean') %>% rev(),
                         ordered = T),
    p_colour = case_when(p.value >= 0.1 ~ '0',
                         p.value >= 0.05 ~ '1',
                         p.value < 0.05 ~ '2'),
    p_trend_colour = case_when(p_trend >= 0.1 ~ '0',
                               p_trend >= 0.05 ~ '1',
                               p_trend < 0.05 ~ '2')
    ) %>%
  ggplot(aes(x = estimate, y = interaction(quartile, exposure, sep = '!'), colour = p_colour)) +
  geom_vline(xintercept = 0, 
             colour = 'grey55', 
             linewidth = 0.5, 
             linetype = 'dotted') +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0,
                 linewidth = .6) +
  geom_point(size = 2) +
  geom_text(aes(label = p_label, x = Inf, colour = p_trend_colour),
            hjust = 1.03,
            size = 2.7) +
  facet_grid(time_window ~ outcome,
             scales = 'free_x',
             switch = 'y') +
  scale_y_discrete(guide = guide_axis_nested(delim = '!', colour = 'black')) +
  scale_x_continuous(expand = expansion(mult = c(.08, 0.55)),
                     labels = scales::percent,
                     trans = 'log1p') +
  scale_colour_manual(values = c('2' = 'darkred', '1' = 'darkorange', '0' = 'grey20')) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = 'none',
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text.x = element_text(face = 'bold'),
    strip.text.y.left = element_text(face = 'bold'),
    ggh4x.axis.nesttext.y = element_text(vjust = 0.5, face = 'bold'),
    panel.spacing.x = unit(.5, 'lines'),
    panel.grid.major.y = element_line(colour = 'grey94', linewidth = 4),
    panel.grid.major.x = element_line(colour = 'white', linewidth = .5),
    axis.text = element_text(colour = 'black'),
    axis.text.y.left = element_text(hjust = 0, margin = margin(r = -1, l = 2)),
    axis.text.y = element_text(hjust = 0),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(6, 12, 6, 6)
  )
ggsave(here::here('figures', paste0(Sys.Date(), '_figS5_er.svg')), width = 12, height = 4.5)
ggsave(here::here('figures', paste0(Sys.Date(), '_figS5_er.png')), width = 12, height = 4.5)

model2_lm_non_smo %>% 
  mutate(exposure_time = paste0(exposure, '_', time_window)) %>% 
  left_join(sd_table) %>% 
  filter(time_window == '10y') %>% 
  mutate(
    across(c(estimate, conf.low, conf.high), \(x) exp(x * int / sd) - 1),
    association_label = sprintf("%.2f%% (%.2f, %.2f)", 100 * estimate, 100 * conf.low, 100 * conf.high),
    exposure = factor(exposure,
                      levels = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m') %>% rev(),
                      labels = c('PM2.5', 'PM10', 'NOx', 'Lden', 'NDVI') %>% rev(),
                      ordered = T),
    outcome = factor(outcome,
                     levels = c('eGFR', 'u_acr', 'u_kim1', 'u_dkk3', 'p_kim1'),
                     labels = c('eGFR', 'uACR', 'uKIM1', 'uDKK3', 'pKIM1'),
                     ordered = T),
    p_colour = case_when(p.value >= 0.1 ~ '0',
                         p.value >= 0.05 ~ '1',
                         p.value < 0.05 ~ '2')) %>%
  ggplot(aes(x = estimate, y = exposure, colour = p_colour)) +
  geom_vline(xintercept = 0, 
             colour = 'grey55', 
             linewidth = 0.5, 
             linetype = 'dotted') +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0,
                 linewidth = .6,
                 position = position_dodge(width = .5)) +
  geom_point(size = 2,
             position = position_dodge(width = .5)) +
  geom_text(aes(label = association_label, x = Inf),
            hjust = 1.03,
            size = 2.7) +
  facet_wrap(vars(outcome),
             scales = 'free_x') +
  scale_x_continuous(expand = expansion(mult = c(.08, 1.65)),
                     labels = scales::percent,
                     trans = 'log1p') +
  scale_colour_manual(values = c('2' = 'darkred', '1' = 'darkorange', '0' = 'grey20')) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = 'none',
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text.x = element_text(face = 'bold'),
    strip.text.y.left = element_text(face = 'bold'),
    panel.spacing.x = unit(.5, 'lines'),
    panel.grid.major.y = element_line(colour = 'grey94', linewidth = 4),
    panel.grid.major.x = element_line(colour = 'white', linewidth = .5),
    axis.text = element_text(colour = 'black'),
    axis.text.y.left = element_text(hjust = 0, margin = margin(r = -1, l = 2)),
    axis.text.y = element_text(hjust = 0),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(6, 12, 6, 6)
  )
ggsave(here::here('figures', paste0(Sys.Date(), '_fig_non_smo.svg')), width = 6, height = 2.5)
ggsave(here::here('figures', paste0(Sys.Date(), '_fig_non_smo.png')), width = 6, height = 2.5)