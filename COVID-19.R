setwd("C:/Users/user/Dropbox/DRAFT BUKU KE-5 - SAINS DATA GEOSPASIAL/WebScrap")

install.packages(c("tidyverse", "rvest"))

library(tidyverse)
library(rvest)

h <- read_html("https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Indonesia")
class(h)
h
html_text(h)
tab <- h %>% html_nodes("table")
tab

#Hanya mengambil tabel yang dibutuhkan
tab[[4]] #Tabel 4 yang menyimpan informasi yang kita butuhkan
tab <- tab[[4]] %>% html_table(header = TRUE, fill = TRUE)
class(tab)

#Memberi nama kolom
tab <- tab %>% setNames(c("Provinsi", "Positif", "Sembuh", "Meninggal", "PDP", "Website", "Kosong"))
head(tab)
view(tab)

#Menghapus baris dan kolom
data.covid <- tab[-c(1,36,37,38), ] #Menghapus baris 1, 36-38
data.covid$Kosong <- NULL #Menghapus kolom "Kosong"
data.covid$Website <- NULL
head(data.covid)
class(data.covid)

#Mengubah factor menjadi numeric
data.covid[, c(2:5)] <- sapply(data.covid[, c(2:5)], as.numeric)

#Simpan sebagai CSV
write.csv(data.covid,"C:/Users/user/Dropbox/DRAFT BUKU KE-5 - SAINS DATA GEOSPASIAL/WebScrap/data-covid19.csv", row.names = FALSE)

library(ggplot2)
qplot(data.covid$Positif, geom="density")
rate <-(data.covid$Meninggal/data.covid$Positif*100)
hist(rate)

#Mengubah dataframe
library(reshape)
data.covid.new <- melt(data.covid, id=c("Provinsi"))
colnames(data.covid.new)[2] <- "Klasifikasi"
ggplot(data.covid.new) + geom_col(aes(x = Provinsi, y = value, fill = Klasifikasi))
ggplot(data.covid.new) + geom_col(aes(x = reorder (Provinsi, -value), y = value, fill = Klasifikasi)) +
  theme(axis.text.x = element_text(size = 9, angle = 45, vjust = 0.5))+xlab("Provinsi") + ylab("Jumlah") 


