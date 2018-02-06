ProjTemplate::reload()
dat <- readRDS(file.path('data', 'RawData.rds'))

# Convert characters to binary factors --------------------------------------------------------
dat[dat=='NA'] <- NA
dat <- dat %>%
  mutate_at(vars(`Age at Diag`, `Average Pain`,`Disease duration`,`Skeletal Burden (TV)`, TA),
            function(x) as.numeric(as.character(x))) %>%
  mutate_at(vars(Classic, `Extra-osseous`, Polyostotic), function(x) ifelse(x == 'Yes', 1,0)) %>%
  mutate(`Right/ Left` = ifelse(`Right/ Left` == 'Right', 1, 0),
         Extremity = ifelse(Extremity == 'Lower', 1,0),
         `Skin findings` = ifelse(str_detect(`Skin findings`,'^vascular'),1, 0),
         `Sensory loss` = ifelse(`Sensory loss` == 'present', 1, 0),
         `Motor exam` = ifelse(`Motor exam` == 'normal', 0, 1),
         Gender = ifelse(Gender == 'F', 1, 0))

saveRDS(dat, file = file.path('data','forTable.rds'), compress=T)

