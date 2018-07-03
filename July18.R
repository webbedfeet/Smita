##%######################################################%##
#                                                          #
####            Analysis of bone biomarkers             ####
#                                                          #
##%######################################################%##

## See e-mail from 6/27/18

d <- readRDS('data/RawDataJuly18.rds')
d %>% group_by(MAP2K_Status) %>%
  summarise_if(is_numeric, funs(q1 = quantile(., 0.25, na.rm=T),
                                q3 = quantile(., 0.75, na.rm=T),
                                med = median(., na.rm=T))) %>%
  gather(variable, value, -MAP2K_Status) %>%
  separate(variable, c('variable', 'stats'), sep='_') %>%
  spread(stats, value) %>%
  mutate(out = glue::glue('{med} ({q1}, {q3})')) %>%
  select(MAP2K_Status, variable, out) %>%
  ungroup() %>%
  spread(MAP2K_Status, out) -> d_summaries


# Differences in bone turnover markers by group

d %>% group_by(MAP2K_Status) %>%
  select_if(is.numeric) %>%
  ungroup() %>%
  gather(variable, value, -MAP2K_Status) %>%
  nest(-variable) %>%
  mutate(p.value = map_dbl(data, ~broom::tidy(wilcox.test(value ~ MAP2K_Status, data=.))$p.value)) %>%
  select(-data) %>% unnest() %>% left_join(d_summaries) %>%
  select(variable, Positive, Negative, p.value) -> d_comparison

d %>% group_by(MAP2K_Status) %>%
  select(`Age at Diag`,BSALP:DYPD) %>%
  ungroup() %>%
  gather(variable, value, -MAP2K_Status, -`Age at Diag`) %>%
  nest(-variable) %>%
  mutate(age.adj.p.values = map_dbl(data, ~broom::tidy(lm(value ~ rms::rcs(`Age at Diag`) + MAP2K_Status, data = .))[5,5])) %>%
  select(-data) -> blah

d_comparison <- d_comparison %>% left_join(blah)

# Association of bone turnover markers with skeletal burden
library(cowplot)
d %>%
  select(`Skeletal Burden (TV)`, BSALP:DYPD) %>%
  gather(variable, value, -`Skeletal Burden (TV)`) %>%
  mutate(variable = factor(variable, levels = names(d)[16:21] )) %>%
  ggplot(aes(`Skeletal Burden (TV)`, value))+geom_point()+
  geom_smooth()+geom_smooth(method = 'lm', color='green')+facet_wrap(~variable, scales = 'free_y')+
  labs(y = '')

d %>% select(`Skeletal Burden (TV)`, BSALP:DYPD) %>%
  gather(variable, value, -`Skeletal Burden (TV)`) %>%
  nest(-variable) %>%
  mutate(correlation = map_dbl(data,~cor(., use = 'pair', method = 'spearman')[1,2])) %>%
  select(-data) %>% unnest() %>%
  set_names(c('Variable', 'Correlation with Skeletal Burden')) -> d_association

openxlsx::write.xlsx(list('Comparison' = d_comparison, 'Association' = d_association), file = 'July18numeric.xlsx')
