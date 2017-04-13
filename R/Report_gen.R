#' ---
#' title: Laporaan ancaman
#' author: LinHut
#' ---

# Prints the period of the report:
cat("Dari tanggal:", as.character(analysis_period$from_date), "- sampai tanggal:", as.character(analysis_period$to_date))

#' ## Tingkat ancaman per sektor:
#' 
#' Jam patroli berkurang berarti kurang dari 4 jam patroli.
par(mfrow=c(2,2), mar=c(1,1,1,1), oma=c(2,0,0,0))
plot(sectors,
     col=col_pal_thrt(sectors$df, breaks=c(0,2,5,15,100)),
     main='Pembukaan lahan')
plot(glad_period, col=rgb(1,0.3,0,0.8), add=TRUE, legend=FALSE)
plot(sectors,
     col=col_pal_thrt(sectors$encr_effort, breaks=c(0,1,10,20,100), no_survey = sectors$no_survey),
     main='Perambahan')
plot(sectors,
     col=col_pal_thrt(sectors$logg_effort, breaks=c(0,1,10,20,100), no_survey = sectors$no_survey),
     main='Pembalakan illegal')
plot(sectors,
     col=col_pal_thrt(sectors$hunt_effort, breaks=c(0,1,10,20,100), no_survey = sectors$no_survey),
     main='Perburuan')

plot_reset()
legend("bottom", 
       fill = c(brewer.pal(4, "Blues"), "grey"), 
       legend = c('Rendah', 'Sedang', 'Tinggi', 'Extrim', "Patroli tidak cukup"), 
       horiz=TRUE, text.width =  0.1, bty="n")


#' ## Tingkat ancaman total per sektor:
par(mfrow=c(1,1), mar=c(1,1,1,1), oma=c(2,0,0,0))
plot(sectors,
     col=col_pal_total_thrt(sectors$total_thrt),
     main='Total')
plot_reset()
legend("bottom", fill = brewer.pal(4, "Blues"), legend = c('Rendah', 'Sedang', 'Tinggi', 'Extrim'), horiz=TRUE, bty="n")


#' ## Ringkasan sektor
#'
#' Pembukaan GLAD berarti persentasi yang dibukan per tahun. 

sectors_summary<-sectors@data %>%
  mutate(Area = round(Area), df = round(df,1)) %>%
  select(Sector:df) %>%
  rename(Luasan = Area, Pembukaan_GLAD = df)
knitr::kable(sectors_summary)

#' ## Ringkasan patroli

patrol_summary<-sectors@data %>%
  rename(jam_patroli = effort_hours, 
         rambah = encr,
         balak = logg,
         buru = hunt,
         rambah_100jam = encr_effort,
         balak_100jam = logg_effort,
         buru_100jam = hunt_effort) %>%
  mutate(jam_patroli = round(jam_patroli),
         rambah_100jam = round(rambah_100jam,1),
         balak_100jam = round(balak_100jam,1),
         buru_100jam = round(buru_100jam,1)
  ) %>%
  select(Sector,jam_patroli, rambah:buru_100jam)
  
knitr::kable(patrol_summary)

#' ## Status ancaman

threat_summary<-sectors@data %>%
  rename(jam_patroli = effort_hours,
         pembukaan = df_thrt, 
         perambahan = encr_thrt,
         pembalakan = logg_thrt,
         perburuan = hunt_thrt,
         total = total_thrt) %>%
  mutate(jam_patroli = round(jam_patroli)) %>%
  select(Sector,jam_patroli, pembukaan,perambahan:total)

knitr::kable(threat_summary)


