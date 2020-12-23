

ses_masters <- readRDS(paste0(path_incap_ses_dfa,"/ses_masters.RDS"))
gates <- read_dta(paste0(path_gtml_redcap_data,"/Gates_data_2019-07-12.dta"))
alive2018 <- gates %>% 
  dplyr::filter(edo_vida2018 %in% c(1,4,6)) %>% 
  dplyr::select(iduni) %>% 
  pull()


source(paste0(path_incap_repo,"/structural/classify_urban_rural.R"))
# STATUS IN 2018 -------
status2018 <- ses_masters %>% 
  dplyr::select(id_uni,ends_with("_1"),srcses_67,srcses_75,-pcall6775_1) %>% 
  dplyr::filter(id_uni %in% alive2018) %>%
  
  left_join(ur %>%
              dplyr::select(iduni,urbano_rural,urbano_rural2018,urbano_rural2015) %>%
              mutate(urbano_rural = factor(urbano_rural,levels=c(0:2),labels=c("urban","rural","other"))),
            by=c("id_uni"="iduni")) %>% 
  mutate(missing2018 = case_when(is.na(pcall2018_1) & id_uni %in% alive2018 ~ 1,
                                 is.na(pcall2018_1) ~ 0,
                                 !is.na(pcall2018_1) & urbano_rural2018 == 2 ~ -2,
                                 TRUE ~ -1),
         pcall1967_1 = case_when(srcses_67 == 0 ~ NA_real_,
                                 TRUE ~ pcall1967_1),
         pcall1975_1 = case_when(srcses_75 == 0 ~ NA_real_,
                                 TRUE ~ pcall1975_1)
  ) %>% 
  dplyr::select(-srcses_67,-srcses_75) %>% 
  dplyr::filter(missing2018 %in% c(-1,1)) %>% 
  
  mutate(status = case_when(!is.na(pcall2018_1) ~ "Available (n = 1265)",
                            TRUE ~ "Data unavailable (n = 739)")) %>% 
  rename_at(vars(ends_with("_1")),.funs=function(x) str_replace(string = x,"_1","")) %>% 
  pivot_longer(cols = starts_with("pcall"),names_to = "year",values_to = "pcall") %>% 
  dplyr::mutate(year = str_replace(year,"pcall","") %>% as.numeric()) %>% 
  dplyr::filter(!is.na(pcall)) %>%
  group_by(status,year) %>% 
  mutate(mean_pc = mean(pcall,na.rm=TRUE)) %>% 
  ungroup() %>% 
  
  ggplot(data=.,aes(x=year,y=pcall)) +
  geom_point(aes(x=year, y = mean_pc,shape=status),size=5) + 
  geom_smooth(method = "lm",formula = y ~ poly(x, 3),aes(linetype=status),col="grey60",se=FALSE) +
  # stat_smooth(k=2) +
  theme_bw() +
  ggtitle("Status in 2017-18 (n = 2004)") +
  theme(legend.position = "bottom",
        title = element_text(size=15),
        legend.text = element_text(size = 12),
        axis.text = element_text(size=12)) +
  scale_linetype_manual(name = "Status",values=c(1,2)) +
  scale_shape_manual(name = "Status",values=c(17,19)) +
  ylab("Asset index (z-scores)") +
  xlab("Year")

# T-TEST ---------
ttest_out = ses_masters %>% 
  dplyr::select(id_uni,ends_with("_1"),srcses_67,srcses_75,-pcall6775_1) %>% 
  dplyr::filter(id_uni %in% alive2018) %>%
  
  left_join(ur %>%
              dplyr::select(iduni,urbano_rural,urbano_rural2018,urbano_rural2015) %>%
              mutate(urbano_rural = factor(urbano_rural,levels=c(0:2),labels=c("urban","rural","other"))),
            by=c("id_uni"="iduni")) %>% 
  mutate(missing2018 = case_when(is.na(pcall2018_1) & id_uni %in% alive2018 ~ 1,
                                 is.na(pcall2018_1) ~ 0,
                                 !is.na(pcall2018_1) & urbano_rural2018 == 2 ~ -2,
                                 TRUE ~ -1),
         pcall1967_1 = case_when(srcses_67 == 0 ~ NA_real_,
                                 TRUE ~ pcall1967_1),
         pcall1975_1 = case_when(srcses_75 == 0 ~ NA_real_,
                                 TRUE ~ pcall1975_1)
  ) %>% 
  dplyr::select(-srcses_67,-srcses_75) %>% 
  dplyr::filter(missing2018 %in% c(-1,1)) %>% 
  mutate(missing2018 = case_when(missing2018 == -1 ~ "Available",
                                 TRUE ~ "Missing")) %>% 
  dplyr::summarize(

    n1967 = paste0("Available: ",sum(!is.na(.[missing2018 == "Available",]$pcall1967_1)),
                   "\n Data unavailable: ",sum(!is.na(.[missing2018 == "Missing",]$pcall1967_1)),
                   "\n",
                   case_when(t.test(pcall1967_1 ~ missing2018,na.action = na.omit)$p.value < 0.01 ~ "p < 0.01",
                             TRUE ~ paste0("\n p = ", round(t.test(pcall1967_1 ~ missing2018,
                                                                   na.action = na.omit)$p.value,2)))),
    
    n1975 = paste0("Available: ",sum(!is.na(.[missing2018 == "Available",]$pcall1975_1)),
                   "\n Data unavailable: ",sum(!is.na(.[missing2018 == "Missing",]$pcall1975_1)),
                   "\n",
                   case_when(t.test(pcall1975_1 ~ missing2018,na.action = na.omit)$p.value < 0.01 ~ "p < 0.01",
                             TRUE ~ paste0("p = ", round(t.test(pcall1975_1 ~ missing2018,
                                                                na.action = na.omit)$p.value,2)))),
    
    n1987 = paste0("Available: ",sum(!is.na(.[missing2018 == "Available",]$pcall1987_1)),
                   "\n Data unavailable: ",sum(!is.na(.[missing2018 == "Missing",]$pcall1987_1)),
                   "\n",
                   case_when(t.test(pcall1987_1 ~ missing2018,na.action = na.omit)$p.value < 0.01 ~ "p < 0.01",
                             TRUE ~ paste0("p = ", round(t.test(pcall1987_1 ~ missing2018,
                                                                na.action = na.omit)$p.value,2)))),
    
    n2002 = paste0("Available: ",sum(!is.na(.[missing2018 == "Available",]$pcall2002_1)),
                   "\n Data unavailable: ",sum(!is.na(.[missing2018 == "Missing",]$pcall2002_1)),
                   "\n",
                   case_when(t.test(pcall2002_1 ~ missing2018,na.action = na.omit)$p.value < 0.01 ~ "p < 0.01",
                             TRUE ~ paste0("p = ", round(t.test(pcall2002_1 ~ missing2018,
                                                                na.action = na.omit)$p.value,2)))),
    
    n2016 = paste0("Available: ",sum(!is.na(.[missing2018 == "Available",]$pcall2016_1)),
                   "\n Data unavailable: ",sum(!is.na(.[missing2018 == "Missing",]$pcall2016_1)),
                   "\n",
                   case_when(t.test(pcall2016_1 ~ missing2018,na.action = na.omit)$p.value < 0.01 ~ "p < 0.01",
                             TRUE ~ paste0("p = ", round(t.test(pcall2016_1 ~ missing2018,
                                                                na.action = na.omit)$p.value,2))))                           
    
  ) %>% 
  pivot_longer(cols = everything(),names_to = "year",values_to = "test") %>% 
  mutate(year = str_replace(year,"n","") %>% as.numeric())

# PLOT -------

status2018 +
  geom_text(data=ttest_out, mapping = aes(x = year,y=0,label=test),size = 3.2)