options(stringsAsFactors = F)

#Wessolek-MVG
wessolek_mvg_tab10 <- read.csv("data-raw/wessolek_MVG_tab10.csv")
wessolek_mvg_tab10$mpar <- 1-1/wessolek_mvg_tab10$n
# wessolek_mvg_tab10$PV <- wessolek_mvg_tab10$ths*100
# wessolek_mvg_tab10$FC <- with(wessolek_mvg_tab10, brook90r::MvG.swc(10^1.8, alpha,n,ths, thr,m))*100
# wessolek_mvg_tab10$PWP <- with(wessolek_mvg_tab10, brook90r::MvG.swc(10^4.2, alpha,n,ths, thr,m))*100
# wessolek_mvg_tab10$AWC <- with(wessolek_mvg_tab10,FC-PWP)
# wessolek_mvg_tab10$AC <- with(wessolek_mvg_tab10,PV-FC)
wessolek_mvg_tab10$ksat <- wessolek_mvg_tab10$ksat*10
names(wessolek_mvg_tab10)[5] <- "npar"
wessolek_mvg_tab10
# # DIN4220
# din4220_tabA1 <- read.csv("H:/RProjects/PTF-Validierung/RESULTS1/PTF_tables&functions/DIN_RengerTexTRD_MvG_final.csv", stringsAsFactors=F)
# din4220_tabA1 <- din4220_tabA1[,c(1:5,7,9:12,17,18)]
# names(din4220_tabA1) <- c("tex.KA5","bd.KA5","PV","AC","AWC","FC","PWP","alpha","nparm", "thr", "tort", "ksat")
# din4220_tabA1$ths <- din4220_tabA1$PV/100
#
# hypres tab
# hypres_tab4 <- read.csv("data-raw/HypresKlassPTF.csv", stringsAsFactor=F)
# names(hypres_tab4) <- c("tex.hypres", "topsoil", "ths", "thr", "alpha", "n","m","ksat", "tort")
# hypres_tab4$topsoil <- as.logical(hypres_tab4$topsoil)
# #hypres_tab4$PV <- hypres_tab4$ths*100
# #hypres_tab4$FC <- with(hypres_tab4, brook90r::MvG.swc(10^1.8, alpha,n,ths, thr,m))*100
# #hypres_tab4$PWP <- with(hypres_tab4, brook90r::MvG.swc(10^4.2, alpha,n,ths, thr,m))*100
# # hypres_tab4$AWC <- with(hypres_tab4,FC-PWP)
# # hypres_tab4$AC <- with(hypres_tab4,PV-FC)
# hypres_tab4 <- rbind(hypres_tab4, hypres_tab4[hypres_tab4$tex.hypres=="Org",])
# hypres_tab4$topsoil[11] <- TRUE

hydpar_forestfloor <- data.frame(ths = 0.848, thr = 0, alpha = 98, npar = 1.191,
                         mpar=0.1603694, ksat = 98000,tort = 0.5, stringsAsFactors = F)


# # hypres tab
hypres_tab4 <- read.csv("H:/RProjects/PTF-Validierung/RESULTS1/PTF_tables&functions/HypresKlassPTF.csv", stringsAsFactor=F)
names(hypres_tab4) <- c("tex.hypres", "topsoil", "ths", "thr", "alpha", "npar","mpar","ksat", "tort")
hypres_tab4$topsoil <- as.logical(hypres_tab4$topsoil)
# hypres_tab4$PV <- hypres_tab4$ths*100
# hypres_tab4$FC <- with(hypres_tab4, brook90r::MvG.swc(10^1.8, alpha,n,ths, thr,m))*100
# hypres_tab4$PWP <- with(hypres_tab4, brook90r::MvG.swc(10^4.2, alpha,n,ths, thr,m))*100
# hypres_tab4$AWC <- with(hypres_tab4,FC-PWP)
# hypres_tab4$AC <- with(hypres_tab4,PV-FC)
hypres_tab4 <- rbind(hypres_tab4, hypres_tab4[hypres_tab4$tex.hypres=="Org",])
hypres_tab4$topsoil[11] <- TRUE
row.names(hypres_tab4) <- NULL
#
# # teepe-table
# teepe_tables123 <- read.csv("data-raw/TeepePTF.csv", stringsAsFactors=F)
# str(teepe_tables123)
# names(teepe_tables123) <- c("bd.teepe", "tex.teepe", "AC", "AWC", "PWP", "mean_oc", "AC_surcharge",
#                             "AWC_surcharge","PWP_surcharge","ths", "n","alpha","thr")
# teepe_tables123$m <- 1-1/teepe_tables123$n
# teepe_tables123$thr <- teepe_tables123$thr/100
#devtools::use_data(teepe_tables123, hypres_tab4,wessolek_mvg_tab10,din4220_tabA1, internal =T, overwrite = T)

load("data-raw/vign_dat.rda") #mrun_dt, b90res
#speichert den Dataframe als internes Objekt, welches nicht exportiert wird. ANsprechen mit brook90r:::wess_mvg_tex
usethis::use_data(mrun_res, b90res, wessolek_mvg_tab10,hydpar_forestfloor, hypres_tab4, internal = T, overwrite =T)

