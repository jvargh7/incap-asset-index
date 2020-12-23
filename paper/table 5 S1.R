source(paste0(path_incap_repo,"/ses/table 3.R"))

u15 = cor.test(method = "spearman", pca_df_urban2015$pcall,pca_df_urban2015$pcu15)$estimate
r15 = cor.test(method = "spearman", pca_df_rural2015$pcall,pca_df_rural2015$pcr15)$estimate
u18 = cor.test(method = "spearman", pca_df_urban2018$pcall,pca_df_urban2018$pcu18)$estimate
r18 = cor.test(method = "spearman", pca_df_rural2018$pcall,pca_df_rural2018$pcr18)$estimate

index_levels = c("UR0","UR1967","UR1975","UR1987","UR2002","UR2016","UR2018",
                 "R2016","U2016","R2018","U2018"
)

index_labels = c("Harmonized","1967","1975","1987","2002","2016","2018",
                 "Rural 2016","Urban 2016","Rural 2018","Urban 2018")


ses_spearman_cs %>% 
  dplyr::filter(Var2 == "all") %>%
  mutate(index = paste0("UR",Var1),
         r = as.numeric(r)) %>% 
  dplyr::select(index,Var2,r) %>% 
  
  bind_rows(.,
            
            data.frame(index = c("R2016","U2016",
                                 "R2018","U2018"),
                       Var2 = rep("all",times=4),
                       r = c(r15,u15,r18,u18) %>% round(.,2))
            
  ) %>% 
  
  mutate(index = factor(index,levels=index_levels,labels=index_labels),
         Var2 = factor(Var2,var2_levels,var2_labels)) %>% 
  ggplot(data=.,aes(x=index,y=Var2,fill=r)) +
  geom_tile() +
  
  geom_text(aes(label=round(r,2))) + 
  theme_bw()+
  scale_fill_gradient2(low = "red", high = "lightgreen", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  
  theme(axis.text.x = element_text(size=12,angle = 270, hjust = 0),
        axis.text.y = element_text(size=12,angle = 0, hjust = 0)
  ) +
  xlab("") + ylab("")