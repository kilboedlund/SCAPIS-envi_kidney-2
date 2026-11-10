library('splines')

model2_lm <- expand.grid(
  outcome = c('eGFR', 'u_acr', 'u_kim1'),
  exposure = c('pm25_total', 'pm10_total', 'nox_total', 'lden', 'lden_rail', 'ndvi500m'),
  time_window = '10y',
  suffix = '_s'
) %>% 
  as_tibble() %>% 
  mutate(
    model_formulae = paste0('log(', outcome, ') ~ ns(', exposure, '_', time_window, suffix, ', 3)',
                            ' + age + sex + site',
                            ' + edu + ses + civ + swe + smo + pyr + lpa + alc + bmi'),
    lm = map(model_formulae, ~ lm(.x, data = df_3))
  )

plot_df <- model2_lm %>% 
  filter(outcome == 'u_acr') %>% 
  mutate(exp_var = paste0(exposure, '_', time_window, suffix),
         pred = map2(`lm`, exp_var, \(mod, exp_var) {
    x <- df_3[exp_var]
    nd <- data.frame(
      seq(
        quantile(x, 0.05, na.rm = T),
        quantile(x, 0.95, na.rm = T),
        length.out = 100
      )
    )
    names(nd) <- exp_var    
    nd <- nd %>% 
      mutate(age = median(df_3$age, na.rm = T),
             sex = factor('FEMALE', levels = c('MALE', 'FEMALE')),
             site = 'mal',
             edu = factor(1, levels = c(1,2,3)),
             ses = 0.25,
             civ = factor('MARRIED', levels = c('MARRIED', 'ALONE', 'DIVORCED', 'WIDOW')),
             swe = factor('YES', levels = c('YES', 'NO')),
             smo = factor('CURRENT', levels = c('NEVER', 'EX_SMOKER', 'CURRENT')),
             pyr = 0,
             lpa = factor(1, levels = c(1,2,3)),
             alc = factor(2, levels = 0:4),
             bmi = factor(1, levels = c(1, 2, 3))
      )
    ref <- nd[1, ]
    ref[[exp_var]] <- quantile(x, .5, na.rm = T)
    
    p <- predict(mod, newdata = nd, se.fit = T)
    p_ref <- predict(mod, newdata = ref, se.fit = T)
    
    nd <- nd %>% 
      mutate(
        fit = p$fit - as.numeric(p_ref$fit),
        lower = fit - 1.96 * p$se.fit,
        upper = fit + 1.96 * p$se.fit
      ) %>% 
      rename('E' = exp_var)
    
    return(nd)
  })) %>% 
  select(exp_var, pred) %>% 
  unnest(pred)



ggplot(plot_df %>% left_join(
  df_2 %>% 
    summarise(across(unique(str_sub(plot_df$exp_var, 1, -3)),
                     .fns = list(mean = \(x) mean(x, na.rm = T),
                                 sd = \(x) sd(x, na.rm = T)),
                     .names = '{col}-{fn}')) %>% 
    pivot_longer(cols = everything(),
                 names_to = 'exp_var') %>% 
    separate(exp_var, into = c('exp_var', 'name'), sep = '-') %>% 
    pivot_wider(id_cols = 'exp_var') %>% 
    mutate(exp_var = paste0(exp_var, '_s'))),
  aes(x = E * sd + mean, y = fit, ymin = lower, ymax = upper)) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  geom_line() +
  geom_ribbon(alpha = .1) +
  facet_wrap(vars(exp_var, ), scales = 'free')

