####   ANALYSIS ####
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plm)
library(car)
library(lmtest)

####  GW ####
#Load Data
library(readxl)
data <- read_excel("C:/Users/Lenovo/Downloads/Data Gabungan.xlsx")

#Multikolinearitas
m0 <- lm(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
           SanitasiLayak+Pekerja+PMDN+PMA, data=data)
vif(m0)

gf1=plm(AirMinumLayak~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
          SanitasiLayak+Pekerja+PMDN+PMA, data=data,model="within",index=c("Provinsi","Tahun"))
gc1=plm(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
          SanitasiLayak+Pekerja+PMDN+PMA, data=data,model="pooling",index=c("Provinsi","Tahun"))
gr1=plm(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
          SanitasiLayak+Pekerja+PMDN+PMA, data=data,model="random",index=c("Provinsi","Tahun"))

#Uji Chow
pooltest(gc1,gf1) #H0 = common

#Uji Hausman
phtest(gf1,gr1) #H0 = random

#Uji Breusch Pagan
plmtest(gf1,effect="twoways",type="bp") #H0 ditolak berarti ada efek dua arah
plmtest(gf1,effect="individual",type="bp") #H0 ditolak berarti ada efek individu
plmtest(gf1,effect="time",type="bp") #H0 ditolak berarti ada efek waktu

#Pembentukan model
g1a=plm(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
          SanitasiLayak+Pekerja+PMDN+PMA, data=data, model="within",index=c("Provinsi","Tahun"),effect='twoways')
g1a

#Homoskedastisitas Residual
bptest(g1a, studentize =FALSE)

#Diagnostic
#Normalitas residual
ks.test(g1a$residuals, "pnorm", 
        mean=mean(g1a$residuals), 
        sd=sd(g1a$residuals))

#Autokorelasi Residual
pbgtest(g1a) #H0 : tidak ada korelasi serial pada komponen gla

summary(g1a)

###########################

data$Provinsi = toupper(data$Provinsi)
list(data$Provinsi)
# Menggunakan dplyr untuk memanipulasi data frame
library(dplyr)
data <- data %>%
  mutate(Provinsi = case_when(
    Provinsi == "D I YOGYAKARTA" ~ "DI YOGYAKARTA",
    Provinsi == "KEP. BANGKA BELITUNG" ~ "KEPULAUAN BANGKA BELITUNG",
    Provinsi == "KEP. RIAU" ~ "KEPULAUAN RIAU",
    TRUE ~ Provinsi
  ))
nrow(data)
unique(data$Provinsi)

library(jsonlite)
df_long <- fromJSON("https://raw.githubusercontent.com/yusufsyaifudin/wilayah-indonesia/master/data/list_of_area/provinces.json")
unique(df_long$name)
df_long$Provinsi = df_long$name
data_gabungan <- merge(df_long, data, by = "Provinsi")
data_gab = data_gabungan[,-c(2:4)]
#Data Baru
library(GWmodel)
coordinates(data_gab)=c(2:3)
class(data_gab)


nrow(data_gab)


#Pembobot Terbaik
#adaptive bisquare
bwd.GWPR.bisquare.ad <- bw.gwr(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
                                 SanitasiLayak+Pekerja+PMDN+PMA, data = data_gab, approach = "CV", kernel = "bisquare", adaptive=T)
hasil.GWPR.bisquare.ad <- gwr.basic(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
                                      SanitasiLayak+Pekerja+PMDN+PMA
                                    , data = data_gab, bw = bwd.GWPR.bisquare.ad, kernel = "bisquare", adaptive=T)

#adaptive gaussian
bwd.GWPR.gaussian.ad <- bw.gwr(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
                                 SanitasiLayak+Pekerja+PMDN+PMA
                               , data = data_gab, approach = "CV", kernel = "gaussian", adaptive=T)
hasil.GWPR.gaussian.ad <- gwr.basic(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
                                      SanitasiLayak+Pekerja+PMDN+PMA
                                    , data = data_gab, bw = bwd.GWPR.gaussian.ad, kernel = "gaussian", adaptive=T)

#adaptive exponential
bwd.GWPR.exponential.ad <- bw.gwr(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
                                    SanitasiLayak+Pekerja+PMDN+PMA
                                  , data = data_gab, approach = "CV", kernel = "exponential", adaptive=T)
hasil.GWPR.exponential.ad <- gwr.basic(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
                                         SanitasiLayak+Pekerja+PMDN+PMA
                                       , data = data_gab, bw = bwd.GWPR.exponential.ad, kernel = "exponential", adaptive=T)

#fixed bisquare
bwd.GWPR.bisquare.fix <- bw.gwr(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
                                  SanitasiLayak+Pekerja+PMDN+PMA
                                , data = data_gab, approach = "CV", kernel = "bisquare", adaptive=F)
hasil.GWPR.bisquare.fix <- gwr.basic(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
                                       SanitasiLayak+Pekerja+PMDN+PMA
                                     , data = data_gab, bw = bwd.GWPR.bisquare.fix, kernel = "bisquare", adaptive=F)

#fixed gaussian
bwd.GWPR.gaussian.fix <- bw.gwr(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
                                  SanitasiLayak+Pekerja+PMDN+PMA
                                , data = data_gab, approach = "CV", kernel = "gaussian", adaptive=F)
hasil.GWPR.gaussian.fix <- gwr.basic(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
                                       SanitasiLayak+Pekerja+PMDN+PMA
                                        , data = data_gab, bw = bwd.GWPR.gaussian.fix, kernel = "gaussian", adaptive=F)

#fixed exponential
bwd.GWPR.exponential.fix <- bw.gwr(AirMinumLayak ~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+SanitasiLayak+Pekerja+PMDN+PMA, data = data_gab, approach = "CV", kernel = "exponential", adaptive=F)
hasil.GWPR.exponential.fix <- gwr.basic(AirMinumLayak~ AirBersihDisalurkan+JumlahPenduduk+PDRB_Per_Kapita+
                                          SanitasiLayak+Pekerja+PMDN+PMA, data = data_gab, bw = bwd.GWPR.exponential.fix, kernel = "exponential", adaptive=F)



AIC <- c(hasil.GWPR.bisquare.ad$GW.diagnostic$AIC,
         hasil.GWPR.gaussian.ad$GW.diagnostic$AIC,
         hasil.GWPR.exponential.ad$GW.diagnostic$AIC,
         hasil.GWPR.bisquare.fix$GW.diagnostic$AIC,
         hasil.GWPR.gaussian.fix$GW.diagnostic$AIC,
         hasil.GWPR.exponential.fix$GW.diagnostic$AIC)
R2 <- c(hasil.GWPR.bisquare.ad$GW.diagnostic$gw.R2,
        hasil.GWPR.gaussian.ad$GW.diagnostic$gw.R2,
        hasil.GWPR.exponential.ad$GW.diagnostic$gw.R2,
        hasil.GWPR.bisquare.fix$GW.diagnostic$gw.R2,
        hasil.GWPR.gaussian.fix$GW.diagnostic$gw.R2,
        hasil.GWPR.exponential.fix$GW.diagnostic$gw.R2)

R2adj <- c(hasil.GWPR.bisquare.ad$GW.diagnostic$gwR2.adj,
           hasil.GWPR.gaussian.ad$GW.diagnostic$gwR2.adj,
           hasil.GWPR.exponential.ad$GW.diagnostic$gwR2.adj,
           hasil.GWPR.bisquare.fix$GW.diagnostic$gwR2.adj,
           hasil.GWPR.gaussian.fix$GW.diagnostic$gwR2.adj,
           hasil.GWPR.exponential.fix$GW.diagnostic$gwR2.adj)
RSS <- c(hasil.GWPR.bisquare.ad$GW.diagnostic$RSS.gw,
         hasil.GWPR.gaussian.ad$GW.diagnostic$RSS.gw,
         hasil.GWPR.exponential.ad$GW.diagnostic$RSS.gw,
         hasil.GWPR.bisquare.fix$GW.diagnostic$RSS.gw,
         hasil.GWPR.gaussian.fix$GW.diagnostic$RSS.gw,
         hasil.GWPR.exponential.fix$GW.diagnostic$RSS.gw)

tabel <- cbind(RSS,R2,R2adj,AIC)
rownames(tabel) <- c("Adaptive Bisquare", "Adaptive Gaussian",
                     "Adaptive Exponential", "Fixed Bisquare",
                     "Fixed Gaussian", "Fixed Exponential")
data.frame(tabel)


comparison <- data.frame(R2=c(hasil.GWPR.exponential.fix$GW.diagnostic$gw.R2,summary(g1a)$r.squared[1]),
                         R2adj=c(hasil.GWPR.exponential.fix$GW.diagnostic$gwR2.adj,summary(g1a)$r.squared[2]),
                         RSS=c(hasil.GWPR.exponential.fix$GW.diagnostic$RSS.gw,sum(summary(g1a)$residuals^2)))
rownames(comparison) <- c("GWPR", "Regresi Panel")
comparison

parameter_GWPR <- data.frame(
  Provinsi = data_gab$Provinsi,
  hasil.GWPR.gaussian.fix$SDF[,c(1,2,3,4,5,6,7,8,30)]
)
parameter_GWPR

input = hasil.GWPR.gaussian.fix$SDF
input = parameter_GWPR[,c(1:9)]
srow <- seq(from = 1, to = nrow(input), by = 4)
s_data <- input[srow, ]
s_data
nrow(s_data)

pval <- gwr.t.adjust(hasil.GWPR.gaussian.fix)$results$p
pvalue= data.frame("Provinsi"=data_gab$Provinsi,ifelse(pval<0.05, "Signifikan", "Tidak"))
selected_rows <- seq(from = 1, to = nrow(pvalue), by = 4)
selected_data <- pvalue[selected_rows, ]
selected_data

