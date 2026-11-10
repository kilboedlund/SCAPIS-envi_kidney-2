library('tidyverse')
library('lme4')

model0_icben <- expand.grid(
  exposure = 'lden',
  time_window = c('1y', '5y', '10y'),
  mode = c('s', 'q'),
  model = 0,
  outcome = c('ckd_d', 'eGFR < 90', 'u_acr > 3')
) %>% 
  as_tibble() %>% 
  mutate(
    model_formulae = paste0(outcome, ' ~ ', exposure, '_', time_window, '_', mode, ' + (1|site)'),
    glmer = map(model_formulae, ~ lme4::glmer(.x, data = df_3, family = 'poisson')),
    tidy = map(glmer, ~broom.mixed::tidy(.x, conf.int = T) %>% 
                 filter(str_detect(term, 'lden')))
  )

model0_icben_res <- model0_icben %>% 
  select(-glmer) %>% 
  unnest(tidy)

model1_icben <- expand.grid(
  exposure = 'lden',
  time_window = c('1y', '5y', '10y'),
  mode = c('s', 'q'),
  model = 1,
  outcome = c('ckd_d', 'eGFR < 90', 'u_acr > 3')
) %>% 
  as_tibble() %>% 
  mutate(
    model_formulae = paste0(outcome, ' ~ ', exposure, '_', time_window, '_', mode, ' + (1|site)',
                            ' + age + sex'),
    glmer = map(model_formulae, ~ lme4::glmer(.x, data = df_3, family = 'poisson')),
    tidy = map(glmer, ~broom.mixed::tidy(.x, conf.int = T) %>% 
                 filter(str_detect(term, 'lden')))
  )

model1_icben_res <- model1_icben %>% 
  select(-glmer) %>% 
  unnest(tidy)

model2_icben <- expand.grid(
  exposure = 'lden',
  time_window = c('1y', '5y', '10y'),
  mode = c('s', 'q'),
  model = 2,
  outcome = c('ckd_d', 'eGFR < 90', 'u_acr > 3')
) %>% 
  as_tibble() %>% 
  mutate(
    model_formulae = paste0(outcome, ' ~ ', exposure, '_', time_window, '_', mode, ' + (1|site)',
                            ' + age + sex + edu + ses + civ + swe + smo + pyr + lpa + alc + bmi'),
    glmer = map(model_formulae, ~ lme4::glmer(.x, data = df_3, family = 'poisson')),
    tidy = map(glmer, ~broom.mixed::tidy(.x, conf.int = T) %>% 
                 filter(str_detect(term, 'lden')))
  )

model2_icben_res <- model2_icben %>% 
  select(-glmer) %>% 
  unnest(tidy)

model3_icben <- expand.grid(
  exposure = 'lden',
  time_window = c('10y'),
  mode = c('s', 'q'),
  model = 3,
  outcome = c('u_acr > 3')
) %>% 
  as_tibble() %>% 
  mutate(
    model_formulae = paste0(outcome, ' ~ ', exposure, '_', time_window, '_', mode, ' + site',
                            ' + age + sex + edu + ses + civ + swe + smo + pyr + lpa + alc + bmi + ',
                            'pm25_total_', time_window, '_', mode), # + ndvi500m_', time_window, '_s'),
    glm = map(model_formulae, ~ glm(.x, data = df_3, family = 'poisson')),
    tidy = map(glm, ~broom::tidy(.x, conf.int = T) %>% 
                 filter(str_detect(term, 'lden') | str_detect(term, 'pm25')))
  )

model3_icben_res <- model3_icben %>% 
  select(-glm) %>% 
  unnest(tidy) %>% 
  select(term, estimate, conf.low, conf.high, p.value)

icben_res <- bind_rows(
  #model0_icben_res,
  #model1_icben_res,
  #model2_icben_res,
  model3_icben_res
  ) %>% 
  mutate(exposure_time = paste(exposure, time_window, sep = '_')) %>% 
  left_join(df_2 %>% 
              select(contains('lden')) %>% 
              pivot_longer(cols = everything(), 
                           names_to = 'exposure_time',
                           values_to = 'lden') %>%
              group_by(exposure_time) %>% 
              summarise(sd = sd(lden, na.rm = T)),
            by = 'exposure_time') %>% 
  mutate(sd = case_match(mode, 
                         's' ~ sd, 
                         'q' ~ 1),
         pr = exp(estimate),
         lcl = exp(conf.low),
         ucl = exp(conf.high))

saveRDS(icben_res, here::here('icben_res.rds'))
icben_res <- readRDS(here::here('icben_res.rds'))

icben_res %>% 
  select(mode, model, outcome, term, pr, lcl, ucl) %>% 
  view
