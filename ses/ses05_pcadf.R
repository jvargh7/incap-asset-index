
# pca_df exploratory -----
pca05_df <- pca05_df  %>%
  dplyr::filter(census %in% years)

pca05_df[is.na(pca05_df)] <- 0

pca05_df %>% 
  dplyr::filter(census %in% c(1965:1999)) %>% 
  compareGroups(census~.-id_uni -familia -comuni,data=.)  %>% 
  createTable(show.n=TRUE) %>% 
  export2md(.)

pca05_df %>% 
  dplyr::filter(census %in% c(2002:2018)) %>% 
  compareGroups(census~.-id_uni -familia -comuni,data=.)  %>% 
  createTable(show.n=TRUE) %>% 
  export2md(.)



## Running PCA -----


pca05_obj = pca05_df   %>%
  dplyr::select(-id_uni,-census,-comuni,-familia)  %>% 
  dplyr::select(radio, tocad, 
                coser,
                frig,
                tv,
                bike,
                moto,
                auto,
                sitio90,
                tenen_7,
                # vivien_4,
                cuartv07,
                piso_high,
                techo_high,
                pared_high,
                cocin_34,
                poele_high,
                letrin_yes,
                luz,
                abasag_high) %>% 
  temp_pca(.,scale_term)


pca05_obj$rotation %>% 
  data.frame() %>% 
  mutate(row = rownames(.)) %>% 
  mutate_at(vars(starts_with("PC")),~round(.,2)) %>% 
  write.csv(.,paste0(path_incap_repo,"/ses/pca05_df_rotation.csv"))

## After adding components -------------

# pcall[,c("pcall", paste0("pcall_",c(2,3)))] <- pca_obj$x[,1:3]

pca05_df[,c("pcall", paste0("pcall_",c(2,3)))] <- pca05_obj$x[,1:3]*-1
# pcall <- pca05_df

ggplot(data=pca05_df,aes(x=pcall)) +geom_histogram(bins=300)

ggplot(data=pca05_df,aes(sample=pcall))+
  geom_qq(distribution=qunif) +
  geom_qq_line(distribution=qunif)


tab1 <- pca05_df %>% 
  dplyr::filter(census %in% c(1965:1999)) %>% 
  dplyr::select(census,luz:abasag_high,
                
                starts_with("pcall")) %>% 
  compareGroups(census~.,data=.)  %>% 
  createTable(show.n=TRUE)

tab2 <- pca05_df %>% 
  dplyr::filter(census %in% c(2002:2018)) %>% 
  dplyr::select(census,luz:abasag_high,
                starts_with("pcall")) %>% 
  compareGroups(census~.,data=.)  %>% 
  createTable(show.n=TRUE)

cbind(tab1,tab2) %>% 
  export2md(.)

