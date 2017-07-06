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
text(sectors_centroid, labels = sectors$Code, cex = 0.5)
plot_reset()
legend("bottom", fill = brewer.pal(4, "Blues"), legend = c('Rendah', 'Sedang', 'Tinggi', 'Extrim'), horiz=TRUE, bty="n")

#' ## Ringkasan kantor lapangan
#' ### Jumlah tingkat ancaman untuk masing masing kantor lapangan.
knitr::kable(sector_threat)

#' ### Indikator kinerja per tingkat ancaman untuk masing masing kantor lapangan.
#' 
#' hari berarti berapa hari patroli diperlu untuk suatu sektor di tingkat ancaman yang tersebut.  
#' periode berarti berapa hari rata rata di antara patroli dalam setiap sektor di tingkat
#' ancaman yang tersebut.
knitr::kable(sector_effort)


sectors@data<-sectors@data[order(sectors@data$Kantor, sectors@data$Code),]

#' ## Ringkasan sektor
#'
#' Pembukaan GLAD berarti persentasi yang dibukan per tahun. 

sectors_summary<-sectors@data %>%
  filter(Type != "Pos") %>%
  mutate(Area = round(Area), df = round(df,1)) %>%
  select(Kantor, Code, Type, Prioritas, Area, df) %>%
  rename(Luasan = Area, Pembukaan_GLAD = df)
knitr::kable(sectors_summary)

#' ## Ringkasan patroli

patrol_summary<-sectors@data %>%
  filter(Type != "Pos") %>%
  rename(jam_patroli = effort_hours,
         jumlah_patroli = n_surveys,
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
  select(Kantor, Code,jam_patroli, jumlah_patroli, rambah:buru_100jam)
  
knitr::kable(patrol_summary)

#' ## Status ancaman

threat_summary<-sectors@data %>%
  filter(Type != "Pos") %>%
  rename(pembukaan = df_thrt, 
         perambahan = encr_thrt,
         pembalakan = logg_thrt,
         perburuan = hunt_thrt,
         total = total_thrt) %>%
  select(Kantor, Code, pembukaan,perambahan,pembalakan,perburuan,total)

knitr::kable(threat_summary)

#' ## Penilaian indikator kinerja
#'
#' Indikator dan nilai kerja ini dihitung untuk periode yang tersebut.  
#' Periode patroli berarti jumlah hari rata rata di antara setiap kali patroli.  
#' ik = indikator kinerja; nilai = yg. aktual


kpi<-sectors@data %>%
  filter(Type != "Pos") %>%
  rename(ancaman = total_thrt,
         periode_patroli = periode,
         ik_jumlah = hari,
         ik_jam = jam,
         nilai_jumlah = n_surveys,
         nilai_jam = effort_hours) %>%
  mutate(ik_jumlah = round(ik_jumlah),
         ik_jam = round(ik_jam),
         nilai_jam = round(nilai_jam)) %>%
  mutate(nilai_jumlah_persen = round((nilai_jumlah / ik_jumlah) *100),
         nilai_jam_persen = round((nilai_jam / ik_jam) *100)) %>%
  select(Kantor,
         Code,
         ancaman,
         ik_jumlah,
         nilai_jumlah,
         nilai_jumlah_persen,
         ik_jam,
         nilai_jam,
         nilai_jam_persen)

knitr::kable(kpi)