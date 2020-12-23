# Based on "ses_data_guide.dir":
# C:\Cloud\Box Sync\INCAP SES Harmonization\RSPH Research\Raw and old data\human capital\data_Apr06\SAS\ses
# pcall contains variables from popdemogstack from all census years as well as dummy variables
# familia : defined by unique combination of census, comuni, familia


pc02 <- left_join(pcall %>%
                    dplyr::distinct(census,comuni,familia,.keep_all=TRUE) %>%
                    dplyr::filter(census==2002) %>%
                    dplyr::select(-id_uni),


                  popdemogind %>%
                    dplyr::select(census,comuni,familia,id_uni),

                  by=c("census","comuni","familia")) %>%
  dplyr::filter(id_uni %in% pc6775$id_uni)


# QC : Comparison with acumulado/c0411z ----------

c0411z02 <- read_dta(paste0(path_census,"/acumulado/data/stata_clean/c0411z.dta")) %>%
  dplyr::select(id_uni,master,ev2002,pr2002,en2002) %>%
  dplyr::filter(ev2002==1,master==1)

table(c0411z02$pr2002,c0411z02$en2002,dnn = list("pr","en"))
# Present (Yes/No) vs Included (Yes)
table(c0411z02[c0411z02$pr2002==1,]$id_uni %in% popdemogind[popdemogind$census==2002,]$id_uni)
