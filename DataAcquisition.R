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


# New data with bone turnover markers ---------------------------------------------------------

# devtools::install_github('nacnudus/unpivotr')
library(unpivotr)

d <- readxl::read_xlsx('data/Raw data Phenotype_Full_July18.xlsx', sheet=1) %>%
  filter(!is.na(`MAP2K1-positive`)) %>%
  select(-`X__1`) %>%
  mutate(MAP2K_Status = ifelse(toupper(`MAP2K1-positive`)==`MAP2K1-positive`, 'Negative','Positive')) %>%
  rename(ID = `MAP2K1-positive`)
saveRDS(d, file = file.path('data', 'RawDataJuly18.rds'), compress = T)
