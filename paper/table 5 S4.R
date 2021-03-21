path_census <- paste0(path_incap_ses_box,"/BDGT_oriente/Census")



popdemogind <- read_dta(paste0(path_incap_ses_box,
                               "/RSPH Research/Organized data from old waves/Programs/Human capital programs/FNB/Dev_change/",
                               "popdemogind.dta"))
popdemoind67 <- read_dta(paste0(path_incap_ses_box,
                                "/RSPH Research/Organized data from old waves/Programs/Human capital programs/FNB/Dev_change/",
                                "popdemogind67.dta"))

years = c(0,1967,1975,1987,2002,2016,2018)

source(paste0(path_incap_repo,"/ses/ses01_pc04.R"))
source(paste0(path_incap_repo,"/ses/ses02_pc16.R"))
source(paste0(path_incap_repo,"/ses/ses03_pc18.R"))
source(paste0(path_incap_repo,"/ses/ses04_pcall update.R"))
source(paste0(path_incap_repo,"/ses/ses05_pcadf.R"))

temp_pca_df = pca05_df 

c_items <- "cuartv07"
d_items <- c(
  "radio",
  "tocad",
  "coser",
  "frig",
  "tv",
  "bike",
  "moto",
  "auto",
  "sitio90",
  "tenen_7",
  # vivien_4,
  "piso_high",
  "techo_high",
  "pared_high",
  "cocin_34",
  "poele_high",
  "letrin_yes",
  "luz",
  "abasag_high")
# Polychoric ------
mixedcor_output <- (temp_pca_df %>% 
                      dplyr::select(-id_uni,-census,-comuni,-familia,
                                    -pcall,-pcall_2,-pcall_3) %>%
                      psych::mixedCor(data=.,
                                      c= c_items,
                                      d = d_items,
                                      use="pairwise",
                                      method="pearson",
                                      correct = 0))
mixedcor_mat <- mixedcor_output$rho

x = temp_pca_df %>% 
  dplyr::select(-id_uni,-census,-comuni,-familia,
                -pcall,-pcall_2,-pcall_3) %>% as.matrix()


# PCA + Polychoric ------------

output_wealth_pca <- prcomp(mixedcor_mat)
l = output_wealth_pca$rotation[,]
f = x %*% l
temp_pca_df[,c("pca_pc", paste0("pca_pc_",c(2,3)))] <- f[,1:3]


r1 = paste0(
  # sprintf(abs(cor.test(temp_pca_df$pca_pc,temp_pca_df$pcall)$estimate),fmt = "%0.2f")
  sprintf(abs(cor(temp_pca_df$pca_pc,temp_pca_df$pcall,method = "spearman")),fmt = "%0.2f")
)

psych_pca <- psych::principal(r = mixedcor_mat, nfactors = 3, rotate = "none") # works if you actually give it the matrix
psych_pca$scores <- psych::factor.scores(x,psych_pca,method = "components")   #find the scores from the response data set with the p3 pca solution
# psych::biplot.psych(psych_pca)
r1a <- paste0(
  sprintf(abs(cor(psych_pca$scores$scores[,1],temp_pca_df$pcall,method = "spearman")),fmt = "%0.2f")
)

# Factor Analysis + Pearson  ------------
rotation_method = "varimax"
fm_method = "minres"
nfactors = 1


pearsoncor_output <- temp_pca_df %>% 
  dplyr::select(one_of(c(c_items,d_items))) %>% 
  # dplyr::select(-id_uni,-census,-comuni,-familia,
  #               -pcall,-pcall_2,-pcall_3) %>%
  cor(.,use="pairwise.complete.obs")

output_wealth_efa2 <- psych::fa(x,
                               n.obs = nrow(temp_pca_df),
                               nfactors = nfactors,fm=fm_method,rotate=rotation_method)



output_wealth_efa2$scores <- psych::factor.scores(x,output_wealth_efa2,method = "components")   #find the scores from the response data set with the p3 pca solution
# psych::biplot.psych(psych_pca)
r2a <- paste0(
  sprintf(abs(cor(output_wealth_efa2$scores$scores[,1],temp_pca_df$pcall,method = "spearman")),fmt = "%0.2f")
)



# Factor Analysis + Polychoric ------------
rotation_method = "varimax"
fm_method = "minres"
nfactors = 1
output_wealth_efa3 <- psych::fa(mixedcor_mat,
                               n.obs = nrow(temp_pca_df),
                               nfactors = nfactors,fm=fm_method,rotate=rotation_method)
l = output_wealth_efa3$loadings[,]
f = x %*% l 

temp_pca_df[,c("fa_pc")] <- f


r3 = paste0(
  # sprintf(abs(cor.test(temp_pca_df$fa_pc,temp_pca_df$pcall)$estimate),fmt = "%0.2f")
  sprintf(abs(cor(temp_pca_df$fa_pc,temp_pca_df$pcall,method = "spearman")),fmt = "%0.2f")
)

output_wealth_efa3$scores <- psych::factor.scores(x,output_wealth_efa3,method = "components")   #find the scores from the response data set with the p3 pca solution
# psych::biplot.psych(psych_pca)
r3a <- paste0(
  sprintf(abs(cor(output_wealth_efa3$scores$scores[,1],temp_pca_df$pcall,method = "spearman")),fmt = "%0.2f")
)




# Multiple Correspondence Analysis --------------
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/
library(MASS)
mca_out <- temp_pca_df %>% 
  dplyr::select(one_of(c(c_items,d_items))) %>%
  mutate(cuartv07 = case_when(cuartv07 > 0.75 ~ 1,
                              TRUE ~ 0)) %>% 
  mutate_all(~as.factor(.)) %>% 
  mca(.)

temp_pca_df[,"mca"] <- mca_out$rs[,1]
r4 = paste0(
  # sprintf(abs(cor.test(temp_pca_df$mds,temp_pca_df$pcall)$estimate),fmt = "%0.2f")
  sprintf(abs(cor(temp_pca_df$mca,temp_pca_df$pcall,method = "spearman")),fmt = "%0.2f")
)

# Fit statistics
psych::alpha(temp_pca_df %>% dplyr::select(one_of(c(c_items,d_items))))
psych::KMO(temp_pca_df %>% dplyr::select(one_of(c(c_items,d_items))))
