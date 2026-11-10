library('tidyverse')
library('splines')

# Sensitivity analysis of the lower level of quantification --------------------
df_LLQ1 <- readRDS(here::here('data', 'df_3.rds')) %>% 
  select(-c(u_acr, u_alb, u_crea)) %>% 
  left_join(
    read_delim(
    '/safe/data/Research projects/SCAPIS/Originaldata/PETITION-1073/SCAPIS1-DATA-PETITION-1073-20260120.csv',
    delim = ',') %>% 
      left_join(read_csv2('/safe/data/Research projects/SCAPIS/Originaldata/scapis_id_subject.csv') %>% 
                  select('Subject', 'scapis_id'),
                by = 'Subject') %>% 
      transmute(scapis_id,
                u_alb = pmax(coalesce(u_albumin_LS232, u_albumin_LS81), 2 / sqrt(2)), # LLQ = 2
                u_crea = pmax(coalesce(u_creatinine_LS232, u_creatinine_LS81), .5 / sqrt(.5)), # LLQ = .5
                u_acr = u_alb / u_crea
      )
  )

model_LLQ1 <- 
  expand.grid(
    outcome = c('u_acr'),
    exposure = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'ndvi500m'),
    time_window = c('1y', '5y', '10y'),
    suffix = '_s'
  ) %>% 
  as_tibble() %>% 
  mutate(
    model_formulae = paste0('log(', outcome, ') ~ ', exposure, '_', time_window, suffix, 
                            ' + ns(age, 3) + sex + site',
                            ' + edu + ns(ses_s, 3) + civ + swe + smo + ns(pyr, 3) + lpa + alc + ns(bmi, 3) + ns(whr, 3)'),
    lm = map(model_formulae, ~ lm(.x, data = df_LLQ1))
  )

res_llq1 <- 
  model_LLQ1 %>% 
  mutate(model = 'llq1') %>% 
  mutate(exposure = as.character(exposure),
         exposure_time = paste(exposure, time_window, sep = '_'),
         n = map(lm, \(x) nobs(x)),
         lm = map(lm, \(x) broom::tidy(x, conf.int = T))) %>% 
  left_join(sd_table, by = 'exposure_time') %>% 
  unnest(lm) %>% 
  filter(map2_lgl(term, exposure, \(x,y) str_detect(x, y))) %>% 
  mutate(across(c('estimate', 'conf.low', 'conf.high'), \(x) { (exp(int * x / sd) - 1) })) %>% 
  mutate(res_llq1 = paste0(sprintf('%.2f', estimate * 100), ' (',
                      sprintf('%.2f', conf.low * 100), ', ',
                      sprintf('%.2f', conf.high * 100), ')')) %>% 
  left_join(readxl::read_xlsx(here::here('2026-07-27_lm.xlsx')) %>%
              filter(model == 'm2') %>% 
              select(outcome, exposure, time_window, suffix, term, int, res))

writexl::write_xlsx(res_llq1, here::here(paste0(Sys.Date(), '_res_llq1.xlsx')))

readRDS(here::here('data', 'df_3.rds')) %>% 
  filter(!is.na(pm25_total_10y), !is.na(lden_10y), !is.na(ndvi500m_10y), 
                        !is.na(age), !is.na(sex), !is.na(site), !is.na(edu),
                        !is.na(ses_s), !is.na(civ), !is.na(swe), !is.na(smo), 
                        !is.na(pyr), !is.na(lpa), !is.na(alc), !is.na(bmi), !is.na(whr)) %>% 
  #group_by(site) %>% 
  summarise(across(c('pm25_total_10y', 'pm10_total_10y', 'nox_total_10y', 'ndvi500m_10y', 'lden_10y'),
                   \(x) IQR(x))) #diff(quantile(x, c(.25, .75)))))

# Figure S6 with histograms ----------------------------------------------------
cowplot::plot_grid(
  cowplot::plot_grid(
    ggplot(df_4 %>% 
             select(scapis_id, pm25_total_10y, ndvi500m_10y, u_acr, eGFR), aes(x = pm25_total_10y)) +
      geom_histogram(binwidth = .5, fill = 'grey20') +
      geom_vline(aes(xintercept = median(pm25_total_10y)), linetype = 'dotted') +
      geom_errorbar(data = . %>% summarise(q25 = quantile(pm25_total_10y, .25),
                                           q75 = quantile(pm25_total_10y, .75)),
                    aes(xmin = q25, xmax = q75, y = 4500), 
                    inherit.aes = F,
                    orientation = 'y',
                    width = 500) +
      scale_y_continuous(limits = c(0, 5000), expand = c(0, 0)) +
      coord_cartesian(xlim = c(3, 12)) +
      ggtitle('PM2.5 (10y) [\u00b5g/m3]') +
      theme(panel.border = element_rect(fill = NA),
            axis.title = element_blank()),
    ggplot(df_4 %>% 
             select(scapis_id, lden_10y, u_acr, eGFR), 
           aes(x = lden_10y)) +
      geom_histogram(binwidth = 2, fill = 'grey20') +
      geom_vline(aes(xintercept = median(lden_10y)), linetype = 'dotted') +
      geom_errorbar(data = . %>% summarise(q25 = quantile(lden_10y, .25),
                                           q75 = quantile(lden_10y, .75)),
                    aes(xmin = q25, xmax = q75, y = 4500), 
                    inherit.aes = F,
                    orientation = 'y',
                    width = 500) +
      scale_y_continuous(limits = c(0, 5000), expand = c(0, 0)) +
      coord_cartesian(xlim = c(25, 80)) +
      ggtitle('Lden (10y) [dB]') +
      theme(panel.border = element_rect(fill = NA),
            axis.title = element_blank()),
    ggplot(df_4 %>% 
             select(scapis_id, pm25_total_10y, ndvi500m_10y, u_acr, eGFR), 
           aes(x = ndvi500m_10y)) +
      geom_histogram(binwidth = .025, fill = 'grey20') +
      geom_vline(aes(xintercept = median(ndvi500m_10y)), linetype = 'dotted') +
      geom_errorbar(data = . %>% summarise(q25 = quantile(ndvi500m_10y, .25),
                                            q75 = quantile(ndvi500m_10y, .75)),
                     aes(xmin = q25, xmax = q75, y = 4500), 
                    inherit.aes = F,
                    orientation = 'y',
                    width = 500) +
      scale_y_continuous(limits = c(0, 5000), expand = c(0, 0)) +
      coord_cartesian(xlim = c(0, 1)) +
      ggtitle('NDVI (10y)') +
      theme(panel.border = element_rect(fill = NA),
            axis.title = element_blank()),
    nrow = 1, ncol = 3),
    ggplot(df_4 %>% 
             select(scapis_id, site, pm25_total_10y), 
           aes(x = pm25_total_10y)) +
      geom_histogram(binwidth = .25, fill = 'grey20') +
      geom_vline(data = . %>% group_by(site) %>% summarise(m = median(pm25_total_10y)),
        aes(xintercept = m), linetype = 'dotted') +
      geom_errorbar(data = . %>% group_by(site) %>% summarise(q25 = quantile(pm25_total_10y, .25),
                                           q75 = quantile(pm25_total_10y, .75)),
                    aes(xmin = q25, xmax = q75, y = 1500), 
                    inherit.aes = F,
                    orientation = 'y',
                    width = 250) +
      scale_y_continuous(limits = c(0, 2000), expand = c(0, 0)) +
      coord_cartesian(xlim = c(3, 12)) +
      ggtitle('PM2.5 (10y) by site [\u00b5g/m3]') +
      facet_wrap(vars(site)) +
      theme(panel.border = element_rect(fill = NA),
            axis.title = element_blank()),
    cowplot::plot_grid(
      ggplot(df_4 %>% 
             select(scapis_id, pm25_total_10y, ndvi500m_10y, u_acr, eGFR) %>% 
             filter(!is.na(eGFR)), 
             aes(x = eGFR)) +
        geom_histogram(binwidth = .05, fill = 'grey20') +
        geom_vline(aes(xintercept = log(60))) +
        geom_vline(aes(xintercept = log(90))) +
        geom_vline(aes(xintercept = median(eGFR)), linetype = 'dotted') +
        geom_errorbar(data = . %>% summarise(q25 = quantile(eGFR, .25),
                                           q75 = quantile(eGFR, .75)),
                    aes(xmin = q25, xmax = q75, y = 4500), 
                    inherit.aes = F,
                    orientation = 'y',
                    width = 500) +
      scale_y_continuous(limits = c(0, 5000), expand = c(0, 0)) +
      coord_cartesian(xlim = c(3.5, 5)) +
      scale_x_continuous(breaks = c(log(20*2:7), log(90)), labels = c(20 * 2:7, 90)) +
      ggtitle('eGFR [mL/min/1.73 m2]') +
      theme(panel.border = element_rect(fill = NA),
            axis.title = element_blank()),
    ggplot(df_4 %>% 
             select(scapis_id, pm25_total_10y, ndvi500m_10y, u_acr, eGFR, u_alb, u_crea) %>% 
             filter(!is.na(u_acr)) %>% 
             mutate(below_llq = (u_alb < 2 | u_crea < .5)), 
           aes(x = u_acr, fill = below_llq)) +
      geom_histogram(binwidth = 1) +
      geom_vline(aes(xintercept = log(3))) +
      geom_vline(aes(xintercept = median(u_acr)), linetype = 'dotted') +
      geom_errorbar(data = . %>% summarise(q25 = quantile(u_acr, .25),
                                           q75 = quantile(u_acr, .75)),
                    aes(xmin = q25, xmax = q75, y = 4500), 
                    inherit.aes = F,
                    orientation = 'y',
                    width = 500) +
      scale_y_continuous(limits = c(0, 5000), expand = c(0, 0)) +
      scale_x_continuous(breaks = log(c(.1, 1, 3, 10, 100)), labels = c(.1, 1, 3, 10, 100)) +
      scale_fill_manual(limits = c(F, T), values = c('grey20', 'grey70')) +
      ggtitle('uACR [mg/mmol]') +
      theme(panel.border = element_rect(fill = NA),
            axis.title = element_blank(),
            legend.position = 'none'),
    nrow = 1, ncol = 2
    ),
  nrow = 3, ncol = 1,
  rel_heights = c(.3, .4, .3)
  )
ggsave(here::here('figures', paste0(Sys.Date(), '_fig_S6.png')), width = 12, height = 8)
