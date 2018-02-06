## Data ingestion from Excel file

ProjTemplate::reload()

x <- xlsx_cells(file.path('data','Raw data Phenotype_Full.xlsx'), sheets=1)
tbl_names = x %>% filter(row==1, col %in% 3:16) %>% pull(character)


tbl1 <- x %>% filter(row %in% 2:9, col %in% 3:16) %>%
  mutate(output = case_when(data_type=='character'~character,
                            data_type=='numeric'~as.character(numeric))) %>%
  mutate(Names = rep(tbl_names,8)) %>%
  select(Names, output, row) %>%
  spread(Names, output) %>%
  select(-row)
tbl1[tbl1=='NA'] <- NA
tbl1[tbl1==''] <-  NA

tbl2 <- x %>% filter(col %in% 3:16,row %in% 16:22) %>%
  mutate(output = case_when(data_type=='character'~character,
                            data_type=='numeric'~as.character(numeric))) %>%
  mutate(Names = rep(tbl_names,7)) %>%
  select(Names, output, row) %>%
  spread(Names, output) %>%
  select(-row)
tbl2[tbl2==''] <- NA
tbl2[tbl2=='NA'] <- NA

dat <- bind_rows(list('Positive'=tbl1,'Negative'=tbl2), .id='Status')

saveRDS(dat, file=file.path('data','RawData.rds'), compress=TRUE)

