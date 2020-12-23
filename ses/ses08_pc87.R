# Based on "ses_data_guide.dir":
# C:\Cloud\Box Sync\INCAP SES Harmonization\RSPH Research\Raw and old data\human capital\data_Apr06\SAS\ses
# pcall contains variables from popdemogstack from all census years as well as dummy variables
# familia : defined by unique combination of census, comuni, familia



pc87 <- left_join(pcall %>%
                     dplyr::distinct(census,comuni,familia,.keep_all=TRUE) %>% 
                     dplyr::filter(census==1987) %>% 
                     dplyr::select(-id_uni),
                   
                   
                   popdemogind %>%
                     dplyr::select(census,comuni,familia,id_uni),
                  
                  # perf %>% 
                  #   dplyr::select(census,comuni,familia,id_uni),
                   
                   by=c("census","comuni","familia")) %>% 
  dplyr::filter(id_uni %in% pc6775$id_uni)
  # dplyr::filter(id_uni %in% c0411z87$id_uni)
  

 
# QC : Comparison with acumulado/c0411z ----------

c0411z87 <- read_dta(paste0(path_census,"/acumulado/data/stata_clean/c0411z.dta")) %>%
  dplyr::select(id_uni,master,ev1987,pr1987,en1987) %>%
  dplyr::filter(ev1987==1,master==1)

table(c0411z87$pr1987,c0411z87$en1987,dnn = list("pr","en"))
# Present (Yes/No) vs Included (Yes)
## c0411z87[c0411z87$pr1987==1,] == popdemogind[popdemogind$census==1987,]!!