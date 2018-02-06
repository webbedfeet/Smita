ProjTemplate::reload()
dat <- readRDS(file.path('data','forTable.rds'))
names(dat) <- make.names(names(dat))
# Table 1 -------------------------------------------------------------------------------------

bin_ind <- which(summarize_all(dat, max, na.rm=T)==1)
bin_vars <- names(dat)[bin_ind]
cts_vars <- setdiff(names(dat), bin_vars) %>% setdiff(c('Status'))

cts_summ <- function(d){
  out <- d %>% group_by(Status) %>%
    summarize(meds = median(value, na.rm=T)) %>%
    ungroup() %>%
    spread(Status, meds)
  return(out)
}
bin_summ <- function(d){
  out <- d %>% group_by(Status) %>%
    summarise(percs = 100*mean(value, na.rm=T)) %>%
    ungroup() %>%
    spread(Status, percs)
  return(out)
}

dat %>% mutate(Status = as.factor(Status)) %>%
  gather(variable, value, -Status) %>%
  nest(-variable) -> bl
bl %>% filter(variable %in% cts_vars) %>%
  mutate(summs = map(data, cts_summ)) %>%
  mutate(pval = map_dbl(data, ~wilcox.test(value~Status, data=., exact=F)$p.value)) %>%
  select(-data) %>%
  unnest() %>%
  select(variable, Positive, Negative, pval) -> cts_summary
bl %>% filter(variable %in% bin_vars) %>%
  mutate(summs = map(data, bin_summ)) %>%
  mutate(pval = map_dbl(data, ~fisher.test(.$value, .$Status)$p.value)) %>%
  select(-data) %>%
  unnest() %>%
  select(variable, Positive, Negative, pval) -> bin_summary
summs <- bind_rows(cts_summary, bin_summary) %>%
  filter(variable != 'Skeletal.Burden')

row_order <- c('Classic',
               'Extra.osseous',
               'Polyostotic',
               'Age.at.Diag',
               'Disease.duration',
               'Average.Pain',
               'Skeletal.Burden..TV.',
               'Gender',
               'Extremity',
               'Right..Left',
               'Skin.findings',
               'Sensory.loss',
               'Motor.exam',
               'TA')
summs[match(row_order, summs$variable),] %>% write_csv('data/Table1.csv', na="")
