# GEREKLİ PAKETLER (ilk çalıştırmadan önce bir kez kurun):
# install.packages(c(
#   "readxl", "dplyr", "tidyr", "stringr", "readr",
#   "ggplot2", "scales", "forcats", "ggrepel",
#   "RColorBrewer", "fmsb", "corrplot", "patchwork",
#   "gridExtra", "GGally"
# ))

# ============================================================
# AŞAMA 1: Veri Okuma, Temizleme ve Düzenleme
# ============================================================
#
# Bu script şunları yapar:
#   1. Ham Excel dosyalarını okur
#   2. Geniş (wide) formattan uzun (long) tidy formata dönüştürür
#   3. En son yılın verisini seçer (2024)
#   4. Nüfusa bölünmesi gereken değişkenleri normalleştirir
#   5. Temizlenmiş veriyi birleştirerek tek bir ana tablo oluşturur
#   6. Temiz veriyi CSV olarak kaydeder

# ============================================================

# ---- 0. GEREKLİ PAKETLERİ YÜKLE ----
# Eğer bu paketler kurulu değilse, önce şunu çalıştırın:
# install.packages(c("readxl", "dplyr", "tidyr", "stringr"))

library(readxl)   # Excel dosyalarını okumak için
library(dplyr)    # Veri manipülasyonu için (filter, mutate, select, vb.)
library(tidyr)    # Veriyi tidy formata dönüştürmek için (pivot_longer)
library(stringr)  # Metin işlemleri için (str_trim)

# ---- 1. KLASÖR YOLLARINI TANIMLA ----

veri_klasoru <- "data/raw"

cikti_klasoru <- "output"

# ---- 2. YARDIMCI FONKSİYON: Ham Excel'i Tidy Formata Dönüştür ----
#
# Tüm Excel dosyaları aynı yapıya sahip:
#   - Satır 1 (index 1): Değişken adı (örn. "GSYH (bin TL)")
#   - Satır 4 (index 4): İl isimleri (sütun başlıkları)
#   - Satır 5-8 (index 5-8): Yıllara göre veriler (2021, 2022, 2023, 2024)
#
# Bu fonksiyon bu yapıyı okuyup temiz bir data.frame döndürür.

excel_oku_temizle <- function(dosya_yolu, degisken_adi) {
  
  # Excel'i başlıksız oku (header=FALSE), tüm satırları görmek için
  ham_veri <- read_excel(dosya_yolu, col_names = FALSE)
  
  # İl isimlerinin bulunduğu satırı bul (Türkiye'yi içeren satır)
  # Bu genellikle 4. satır (R'da index 4)
  il_satiri <- which(apply(ham_veri, 1, function(x) any(x == "Türkiye", na.rm = TRUE)))

  # İl isimlerini al (Türkiye dahil - onu sonra çıkaracağız)
  il_isimleri <- as.character(ham_veri[il_satiri, ])
  
  # Yıl sütununu bul (yıl değerlerinin olduğu sütun - genellikle 3. sütun, index 3)
  # Veri satırları il satırından sonra başlar
  veri_baslangic <- il_satiri + 1
  veri_bitis <- nrow(ham_veri)
  
  # Yıl sütununu bul: NA olmayan sayısal değer içeren sütun
  yil_sutunu <- NA
  for (s in 1:ncol(ham_veri)) {
    degerler <- ham_veri[veri_baslangic:veri_bitis, s]
    sayisal <- suppressWarnings(as.numeric(unlist(degerler)))
    if (any(!is.na(sayisal) & sayisal > 2000 & sayisal < 2030)) {
      yil_sutunu <- s
      break
    }
  }
  
  # Veri satırlarını al
  veri_satirlari <- ham_veri[veri_baslangic:veri_bitis, ]
  
  # Yılları çıkar
  if (!is.na(yil_sutunu)) {
    yillar <- as.numeric(unlist(veri_satirlari[, yil_sutunu]))
  } else {
    # Yıl sütunu yoksa 2021'den başlayarak sıralı yıllar ata
    yillar <- seq(2021, length.out = nrow(veri_satirlari))
    cat("  NOT: Yıl sütunu otomatik atandı.\n")
  }
  
  # Sayısal veri sütunlarını belirle
  # İl isimleri satırında değer olan sütunları bul
  veri_sutunlari <- which(!is.na(il_isimleri) & il_isimleri != "NA")
  
  # Türkiye'yi çıkar (sadece il verileri istiyoruz)
  turkiye_sutunu <- which(il_isimleri == "Türkiye")
  veri_sutunlari <- veri_sutunlari[veri_sutunlari != turkiye_sutunu]
  
  # Sonuç tablosunu oluştur
  sonuc_listesi <- list()
  
  for (i in seq_along(yillar)) {
    yil <- yillar[i]
    if (is.na(yil)) next
    
    satir <- veri_satirlari[i, ]
    
    for (s in veri_sutunlari) {
      il_adi <- str_trim(as.character(il_isimleri[s]))
      deger_ham <- as.character(satir[[s]])
      
      # Sayısal değere dönüştür (nokta binlik ayırıcı olabilir)
      deger_ham <- gsub("\\.", "", deger_ham)   # Noktaları kaldır (1.000 -> 1000)
      deger_ham <- gsub(",", ".", deger_ham)    # Virgülü noktaya çevir (1,5 -> 1.5)
      deger <- suppressWarnings(as.numeric(deger_ham))
      
      sonuc_listesi[[length(sonuc_listesi) + 1]] <- data.frame(
        il = il_adi,
        yil = yil,
        degisken = degisken_adi,
        deger = deger,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Listeyi tek bir data.frame'e birleştir
  if (length(sonuc_listesi) == 0) return(NULL)
  
  sonuc <- bind_rows(sonuc_listesi)
  
  return(sonuc)
}

# ---- 3. TÜM EXCEL DOSYALARINI OKU ----

# Sağlık kategorisi değişkenleri
bebek_olum <- excel_oku_temizle(
  file.path(veri_klasoru, "bebek_olum_hizi.xlsx"),
  "bebek_olum_hizi"
)

cocuk_olum <- excel_oku_temizle(
  file.path(veri_klasoru, "cocuk_olum_hizi.xlsx"),
  "cocuk_olum_hizi"
)

hastane_sayi <- excel_oku_temizle(
  file.path(veri_klasoru, "hastane_sayisi.xlsx"),
  "hastane_sayisi"
)

hastane_yatak <- excel_oku_temizle(
  file.path(veri_klasoru, "hastane_yatak_sayisi.xlsx"),
  "hastane_yatak_sayisi"
)

hekim_sayi <- excel_oku_temizle(
  file.path(veri_klasoru, "toplam_hekim_sayisi.xlsx"),
  "toplam_hekim_sayisi"
)

# Eğitim kategorisi değişkenleri
derslik_ogrenci <- excel_oku_temizle(
  file.path(veri_klasoru, "derslik_basina_ogrenci.xlsx"),
  "derslik_basina_ogrenci"
)

ogretmen_ogrenci <- excel_oku_temizle(
  file.path(veri_klasoru, "ogretmen_basina_ogrenci.xlsx"),
  "ogretmen_basina_ogrenci"
)

# Kültür kategorisi değişkenleri
muze_sayi <- excel_oku_temizle(
  file.path(veri_klasoru, "bakanliga_bagli_muze_sayisi.xlsx"),
  "muze_sayisi"
)

kutuphane_sayi <- excel_oku_temizle(
  file.path(veri_klasoru, "halk_kutuphanesi_sayisi.xlsx"),
  "kutuphane_sayisi"
)

sinema_sayi <- excel_oku_temizle(
  file.path(veri_klasoru, "sinema_sayisi.xlsx"),
  "sinema_sayisi"
)

tiyatro_sayi <- excel_oku_temizle(
  file.path(veri_klasoru, "tiyatro_sayisi.xlsx"),
  "tiyatro_sayisi"
)

# Ekonomi kategorisi değişkenleri
gsyh <- excel_oku_temizle(
  file.path(veri_klasoru, "GSYH.xlsx"),
  "GSYH"
)

isgucune_katilma <- excel_oku_temizle(
  file.path(veri_klasoru, "isgucune_katilma.xlsx"),
  "isgucune_katilma_orani"
)

issizlik <- excel_oku_temizle(
  file.path(veri_klasoru, "issizlik_orani.xlsx"),
  "issizlik_orani"
)

istihdam <- excel_oku_temizle(
  file.path(veri_klasoru, "istihdam_orani.xlsx"),
  "istihdam_orani"
)

girisim_sayi <- excel_oku_temizle(
  file.path(veri_klasoru, "toplam_girisim_sayisi.xlsx"),
  "girisim_sayisi"
)

ihracat <- excel_oku_temizle(
  file.path(veri_klasoru, "toplam_ihracat.xlsx"),
  "ihracat"
)

# Nüfus verisi (normalizasyon için)
nufus <- excel_oku_temizle(
  file.path("data", "toplam_nufus.xlsx"),
  "nufus"
)

# ---- 4. TÜM VERİLERİ BİRLEŞTİR ----

# Tüm data.frame'leri bir listeye koy
tum_veriler <- list(
  bebek_olum, cocuk_olum, hastane_sayi, hastane_yatak, hekim_sayi,
  derslik_ogrenci, ogretmen_ogrenci,
  muze_sayi, kutuphane_sayi, sinema_sayi, tiyatro_sayi,
  gsyh, isgucune_katilma, issizlik, istihdam, girisim_sayi, ihracat,
  nufus
)

# NULL olanları filtrele ve hepsini birleştir
tum_veriler <- tum_veriler[!sapply(tum_veriler, is.null)]
birlesik_veri <- bind_rows(tum_veriler)

# ---- 5. TÜM YILLARIN ORTALAMASINI AL ----

son_yil_veri <- birlesik_veri %>%
  group_by(degisken, il) %>%
  summarise(deger = mean(deger, na.rm = TRUE), .groups = "drop")

# ---- 6. NÜFUSA GÖRE NORMALİZASYON ----
#
# Nüfusa bölünecek değişkenler:
#   - muze_sayisi        (kültür)
#   - GSYH               (ekonomi) 
#   - kutuphane_sayisi   (kültür)
#   - hastane_sayisi     (sağlık)
#   - sinema_sayisi      (kültür)
#   - tiyatro_sayisi     (kültür)
#   - girisim_sayisi     (ekonomi)
#   - ihracat            (ekonomi)

# Nüfus verisini ayır (son yılı kullan)
nufus_veri <- son_yil_veri %>%
  filter(degisken == "nufus") %>%
  select(il, nufus = deger)

# Nüfusa bölünmesi gereken değişkenlerin listesi
nufusa_bolunecekler <- c(
  "muze_sayisi",
  "GSYH",
  "kutuphane_sayisi",
  "hastane_sayisi",
  "sinema_sayisi",
  "tiyatro_sayisi",
  "girisim_sayisi",
  "ihracat"
)

# Nüfus normalizasyonunu uygula
son_yil_normalize <- son_yil_veri %>%
  filter(degisken != "nufus") %>%  # Nüfusu ana tablodan çıkar
  left_join(nufus_veri, by = "il") %>%  # Nüfus sütununu ekle
  mutate(
    # Eğer değişken nüfusa bölünecekler listesindeyse böl, değilse orijinal değeri kullan
    deger_normalize = if_else(
      degisken %in% nufusa_bolunecekler,
      deger / nufus,  # Kişi başına değer
      deger           # Orijinal değer (oran, hız, vb.)
    )
  )

# ---- 7. GENİŞ (WIDE) FORMATA DÖN ----
#
# Şu an "uzun" (long) format var: her satır bir il-değişken çifti
# Analiz için "geniş" (wide) format daha uygun: her satır bir il,
# her sütun bir değişken

# pivot_wider: uzun → geniş format
veri_genis <- son_yil_normalize %>%
  select(il, degisken, deger_normalize) %>%
  pivot_wider(
    names_from = degisken,    # Değişken adları sütun başlığı olur
    values_from = deger_normalize  # Değerler hücrelere yerleşir
  )

# ---- 8. BÖLGE BİLGİSİNİ EKLE ----

# İl-bölge eşleştirme tablosunu oku
bolge_veri <- read_excel(
  file.path("data", "turkiye_iller_bolgeler.xlsx")
)

# Sütun adlarını temizle
colnames(bolge_veri) <- c("il_no", "il", "bolge")

# İl adlarını temizle (boşlukları kaldır)
bolge_veri <- bolge_veri %>%
  mutate(il = str_trim(il)) %>%
  select(il, bolge)

# Ana tabloya bölge bilgisini ekle
veri_tam <- veri_genis %>%
  left_join(bolge_veri, by = "il")

# Bölgeyi tablonun başına taşı (okunması kolay olsun)
veri_tam <- veri_tam %>%
  select(il, bolge, everything())

# Temel istatistikler

# ---- 10. TEMİZ VERİYİ KAYDET ----

# Ana temiz veriyi kaydet
write.csv(
  veri_tam,
  file = file.path(cikti_klasoru, "veri_temiz.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Uzun format versiyonunu da kaydet (farklı analizler için işe yarayabilir)
write.csv(
  son_yil_normalize,
  file = file.path(cikti_klasoru, "veri_uzun_format.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ============================================================
#   AŞAMA 2: YAŞANABİLİRLİK ENDEKSİ HESAPLAMA  
# ============================================================

library(dplyr)   
library(tidyr)    
library(readr)   

cikti_klasoru <- "output"

veri <- veri_tam

# Değişkenlerin kategorilere göre gruplanması

saglik_degiskenleri <- c(
  "bebek_olum_hizi",        
  "cocuk_olum_hizi",        
  "hastane_sayisi",         
  "hastane_yatak_sayisi",   
  "toplam_hekim_sayisi"     
)

egitim_degiskenleri <- c(
  "derslik_basina_ogrenci",   
  "ogretmen_basina_ogrenci"   
)

kultur_degiskenleri <- c(
  "muze_sayisi",        
  "kutuphane_sayisi",     
  "sinema_sayisi",        
  "tiyatro_sayisi"        
)

ekonomi_degiskenleri <- c(
  "GSYH",                      
  "isgucune_katilma_orani",     
  "issizlik_orani",             
  "istihdam_orani",            
  "girisim_sayisi",             
  "ihracat"                    
)

# Reverse min-max normalization yapılacak değişkenler: Ters Değişkenler 
ters_degiskenler <- c(
  "bebek_olum_hizi",         
  "cocuk_olum_hizi",         
  "derslik_basina_ogrenci",  
  "ogretmen_basina_ogrenci", 
  "issizlik_orani"           
)

# min-max Normalizasyonu Fonksiyonu

min_max_normalize <- function(x, ters = FALSE) {
  # Eksik değerleri yok say
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  
  # Eğer min == max (tüm değerler aynı), 0.5 döndür
  if (max_x == min_x) {
    return(rep(0.5, length(x)))
  }
  
  if (ters) {
    return((max_x - x) / (max_x - min_x))
  } else {
    return((x - min_x) / (max_x - min_x))
  }
}

# Normalize edilmiş değerler 
veri_norm <- veri %>%
  select(il, bolge) 

# Tüm değişkenleri sırayla normalize et
tum_degiskenler <- c(saglik_degiskenleri, egitim_degiskenleri,
                     kultur_degiskenleri, ekonomi_degiskenleri)

for (degisken in tum_degiskenler) {
  
  # Ters normalizasyon gerekli mi?
  ters_mi <- degisken %in% ters_degiskenler
  
  # Değerleri al ve normalize et
  ham_degerler <- veri[[degisken]]
  normalize_degerler <- min_max_normalize(ham_degerler, ters = ters_mi)
  
  # Normalize tablosuna ekle (yeni sütun adı: degisken_norm)
  veri_norm[[paste0(degisken, "_norm")]] <- normalize_degerler
  
}

# Kategori Skoru (Eşit ağırlıklı)

# Normalize edilmiş sütun adlarını oluşturan yardımcı fonksiyon
norm_sutunlar <- function(degisken_listesi) {
  paste0(degisken_listesi, "_norm")
}

# Kategori skoru hesaplama yardımcı fonksiyonu
kategori_skoru_hesapla <- function(veri_norm_df, degiskenler) {
  mevcut <- norm_sutunlar(degiskenler)
  rowMeans(veri_norm_df[, mevcut, drop = FALSE], na.rm = TRUE)
}

# Kategori Skorları
saglik_ham   <- kategori_skoru_hesapla(veri_norm, saglik_degiskenleri)
egitim_ham   <- kategori_skoru_hesapla(veri_norm, egitim_degiskenleri)
kultur_ham   <- kategori_skoru_hesapla(veri_norm, kultur_degiskenleri)
ekonomi_ham  <- kategori_skoru_hesapla(veri_norm, ekonomi_degiskenleri)

# Normalize edilmiş Kategori Skorları 
saglik_skoru  <- min_max_normalize(saglik_ham)
egitim_skoru  <- min_max_normalize(egitim_ham)
kultur_skoru  <- min_max_normalize(kultur_ham)
ekonomi_skoru <- min_max_normalize(ekonomi_ham)

# Yaşanabilirlik Endeksi Hesaplama

# Ham endeks (kategori ortalamalarının ortalaması)
endeks_ham <- (saglik_skoru + egitim_skoru + kultur_skoru + ekonomi_skoru) / 4

# Normalize edilmiş Yaşanabilirlik Endeksi
yasanabilirlik_endeksi <- min_max_normalize(endeks_ham)

# Sonuç Tablosu Oluşturma

sonuc <- data.frame(
  il             = veri$il,
  bolge          = veri$bolge,
  saglik_skoru   = round(saglik_skoru,  4),
  egitim_skoru   = round(egitim_skoru,  4),
  kultur_skoru   = round(kultur_skoru,  4),
  ekonomi_skoru  = round(ekonomi_skoru, 4),
  yasanabilirlik_endeksi    = round(yasanabilirlik_endeksi, 4),
  stringsAsFactors = FALSE
)

# Yaşanabilirlik endeksine göre sırala (en yüksek = en yaşanabilir)
sonuc <- sonuc %>%
  arrange(desc(yasanabilirlik_endeksi)) %>%
  mutate(siralama = row_number())  # Sıralama sütunu ekle

# Sonucu öne taşı: sıralama, il, bölge, endeks, kategoriler
sonuc <- sonuc %>%
  select(siralama, il, bolge, yasanabilirlik_endeksi,
         saglik_skoru, egitim_skoru, kultur_skoru, ekonomi_skoru)

# Coğrafi Bölge Bazlı Özet Tablosu (görseller için)
bolge_ozet <- sonuc %>%
  group_by(bolge) %>%
  summarise(
    il_sayisi       = n(),
    ort_endeks      = round(mean(yasanabilirlik_endeksi), 4),
    en_yuksek       = round(max(yasanabilirlik_endeksi), 4),
    en_dusuk        = round(min(yasanabilirlik_endeksi), 4),
    ort_saglik      = round(mean(saglik_skoru), 4),
    ort_egitim      = round(mean(egitim_skoru), 4),
    ort_kultur      = round(mean(kultur_skoru), 4),
    ort_ekonomi     = round(mean(ekonomi_skoru), 4),
    .groups = "drop"
  ) %>%
  arrange(desc(ort_endeks))

# Ana sonuç tablosu
write.csv(
  sonuc,
  file = file.path(cikti_klasoru, "yasanabilirlik_endeksi.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Bölge özet tablosu
write.csv(
  bolge_ozet,
  file = file.path(cikti_klasoru, "bolge_ozet.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Normalize edilmiş değişken verisi (görselleştirme için)
veri_norm_tam <- cbind(
  veri_norm,
  saglik_skoru   = round(saglik_skoru, 4),
  egitim_skoru   = round(egitim_skoru, 4),
  kultur_skoru   = round(kultur_skoru, 4),
  ekonomi_skoru  = round(ekonomi_skoru, 4),
  yasanabilirlik_endeksi    = round(yasanabilirlik_endeksi, 4)
)

write.csv(
  veri_norm_tam,
  file = file.path(cikti_klasoru, "veri_normalize.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ============================================================
#     AŞAMA 3: GÖRSELLEŞTİRME VE RAPORLAMA       
# ============================================================
#
# Bu aşama şunları yapar:
#   1-2.   En iyi/kötü 10 il yatay bar chart + ısı haritası
#   3-7.   Coğrafi bölge kutu grafikleri (5 metrik)
#   8.     Bölge × kategori dikey bar chart
#   9.     Tüm 81 il yatay bar chart (bölge rengi)
#   10.    Pie chart: endeks + 4 kategori en iyi 3 il
#   11.    Her bölge top-3 dikey bar chart
#   12.    Tahmin vs gerçek karşılaştırma tablosu
#
# GEREKLİ PAKETLER:
#   ggplot2, dplyr, tidyr, forcats, scales, RColorBrewer,
#   ggrepel, patchwork, gridExtra
# ============================================================

#install.packages(c("patchwork", "ggrepel", "forcats"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(scales)
library(RColorBrewer)
library(ggrepel)
library(patchwork)
library(gridExtra)

# ============================================================
# 0. RENK PALETLERİ VE TEMA AYARLARI
# ============================================================

# Metrik renkleri (her zaman sabit)
renk_endeks   <- "#E67E22"   # turuncu  – yaşanabilirlik endeksi
renk_saglik   <- "#E74C3C"   # kırmızı  – sağlık
renk_egitim   <- "#2980B9"   # mavi     – eğitim
renk_kultur   <- "#8E44AD"   # mor      – kültür
renk_ekonomi  <- "#27AE60"   # yeşil    – ekonomi

metrik_renkleri <- c(
  "yasanabilirlik_endeksi"   = renk_endeks,
  "saglik_skoru"  = renk_saglik,
  "egitim_skoru"  = renk_egitim,
  "kultur_skoru"  = renk_kultur,
  "ekonomi_skoru" = renk_ekonomi
)

metrik_etiketleri <- c(
  "yasanabilirlik_endeksi"   = "Yaşanabilirlik Endeksi",
  "saglik_skoru"  = "Sağlık",
  "egitim_skoru"  = "Eğitim",
  "kultur_skoru"  = "Kültür",
  "ekonomi_skoru" = "Ekonomi"
)

# Coğrafi bölge renkleri (her zaman sabit)
bolge_renkleri <- c(
  "Marmara"              = "#1F77B4",   # çelik mavi
  "Ege"                  = "#17BECF",   # turkuaz
  "Akdeniz"              = "#FF7F0E",   # açık turuncu
  "Karadeniz"            = "#2CA02C",   # orman yeşili
  "İç Anadolu"           = "#9467BD",   # leylak
  "Doğu Anadolu"         = "#8C564B",   # kiremit kahve
  "Güneydoğu Anadolu"    = "#D62728"    # kırmızı
)

# Ortak tema
tema_proje <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, hjust = 0.5,
                                    margin = margin(b = 8)),
    plot.subtitle    = element_text(size = 11, hjust = 0.5, color = "grey40",
                                    margin = margin(b = 10)),
    plot.caption     = element_text(size = 9, color = "grey55", hjust = 1),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 10),
    legend.title     = element_text(size = 11, face = "bold"),
    legend.text      = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Grafik kaydetme yardımcı fonksiyonu
grafik_kaydet <- function(grafik, dosya_adi, genislik = 12, yukseklik = 7) {
  png_yolu <- file.path(cikti_klasoru,
                        paste0(dosya_adi, ".png"))
  pdf_yolu <- file.path(cikti_klasoru,
                        paste0(dosya_adi, ".pdf"))
  
  ggsave(png_yolu, plot = grafik,
         width = genislik, height = yukseklik,
         dpi = 150, units = "in", bg = "white")
  
  ggsave(pdf_yolu, plot = grafik,
         width = genislik, height = yukseklik,
         units = "in")
}

# ============================================================
# GRAFİK 1: En İyi 10 – En Kötü 10 İl (Yatay Bar Chart)
# ============================================================

top10    <- sonuc %>% arrange(desc(yasanabilirlik_endeksi)) %>% head(10) %>%
  mutate(grup = "En İyi 10")
bottom10 <- sonuc %>% arrange(yasanabilirlik_endeksi) %>% head(10) %>%
  mutate(grup = "En Kötü 10")

g1_veri <- bind_rows(top10, bottom10) %>%
  mutate(il = fct_reorder(il, yasanabilirlik_endeksi))

g1_sol <- ggplot(filter(g1_veri, grup == "En İyi 10"),
                 aes(x = yasanabilirlik_endeksi, y = il)) +
  geom_col(fill = renk_endeks, alpha = 0.85, width = 0.7) +
  geom_text(aes(label = round(yasanabilirlik_endeksi, 3)),
            hjust = -0.1, size = 3.5, color = "grey25") +
  scale_x_continuous(limits = c(0, 1.08), labels = number_format(accuracy = 0.01)) +
  labs(title = "En İyi 10 İl", x = "Yaşanabilirlik Endeksi", y = NULL) +
  tema_proje +
  theme(panel.grid.major.y = element_blank())

g1_sag <- ggplot(filter(g1_veri, grup == "En Kötü 10"),
                 aes(x = yasanabilirlik_endeksi, y = il)) +
  geom_col(fill = "grey55", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = round(yasanabilirlik_endeksi, 3)),
            hjust = -0.1, size = 3.5, color = "grey25") +
  scale_x_continuous(limits = c(0, 1.08), labels = number_format(accuracy = 0.01)) +
  labs(title = "En Kötü 10 İl", x = "Yaşanabilirlik Endeksi", y = NULL) +
  tema_proje +
  theme(panel.grid.major.y = element_blank())

g1 <- g1_sol + g1_sag +
  plot_annotation(
    title    = "Türkiye İlleri Yaşanabilirlik Endeksi – En İyi ve En Kötü 10 İl",
    caption  = "Kaynak: TÜİK 2021-2024",
    theme    = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

grafik_kaydet(g1, "01_en_iyi_en_kotu_10_il", genislik = 14, yukseklik = 7)

# ============================================================
# GRAFİK 2: En Yaşanabilir 10 İl İçin Isı Haritası
# ============================================================
g2_veri <- sonuc %>%
  arrange(desc(yasanabilirlik_endeksi)) %>%
  head(10) %>%
  select(il, yasanabilirlik_endeksi, saglik_skoru, egitim_skoru,
         kultur_skoru, ekonomi_skoru) %>%
  pivot_longer(-il, names_to = "metrik", values_to = "deger") %>%
  mutate(
    il     = factor(il, levels = rev(sonuc %>%
                                       arrange(desc(yasanabilirlik_endeksi)) %>%
                                       head(10) %>% pull(il))),
    metrik = factor(metrik,
                    levels = names(metrik_etiketleri),
                    labels = metrik_etiketleri)
  )

g2 <- ggplot(g2_veri, aes(x = metrik, y = il, fill = deger)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = round(deger, 3)), size = 3.8, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#FFF3E0", high = renk_endeks,
                      limits = c(0, 1), name = "Skor") +
  labs(
    title   = "En Yaşanabilir 10 İl – Metrik Bazlı Skorlar",
    subtitle = "Değerler Min-Max normalizasyonu ile [0,1] aralığına ölçeklendirilmiştir",
    x = NULL, y = NULL,
    caption = "Kaynak: TÜİK 2021-2024"
  ) +
  tema_proje +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

grafik_kaydet(g2, "02_isi_haritasi_top10", genislik = 11, yukseklik = 7)

# ============================================================
# GRAFİKLER 3-7: Coğrafi Bölge Kutu Grafikleri
# ============================================================

kutu_grafigi_ciz <- function(metrik_sutun, baslik, renk, dosya_adi) {
  cat("  ", baslik, "...\n")
  
  veri <- sonuc %>%
    mutate(bolge = factor(bolge, levels = names(bolge_renkleri)))
  
  g <- ggplot(veri, aes(x = fct_reorder(bolge, .data[[metrik_sutun]], .fun = median),
                        y = .data[[metrik_sutun]], fill = bolge)) +
    geom_boxplot(alpha = 0.75, outlier.shape = 21,
                 outlier.fill = "white", outlier.color = "grey40",
                 outlier.size = 2, width = 0.6) +
    geom_jitter(aes(color = bolge), width = 0.15, alpha = 0.4, size = 1.5) +
    scale_fill_manual(values  = bolge_renkleri, guide = "none") +
    scale_color_manual(values = bolge_renkleri, guide = "none") +
    scale_y_continuous(limits = c(0, 1), labels = number_format(accuracy = 0.01)) +
    coord_flip() +
    labs(
      title   = baslik,
      subtitle = "Coğrafi bölge bazlı dağılım (medyana göre sıralı)",
      x = NULL, y = "Skor [0–1]",
      caption = "Kaynak: TÜİK 2021-2024"
    ) +
    tema_proje
  
  grafik_kaydet(g, dosya_adi, genislik = 11, yukseklik = 7)
  invisible(g)
}

cat("Grafikler 3-7: Coğrafi bölge kutu grafikleri...\n")

kutu_grafigi_ciz("yasanabilirlik_endeksi",   "Yaşanabilirlik Endeksi – Bölge Bazlı Dağılım",
                 renk_endeks,  "03_kutu_yasanabilirlik")
kutu_grafigi_ciz("saglik_skoru",  "Sağlık Skoru – Bölge Bazlı Dağılım",
                 renk_saglik,  "04_kutu_saglik")
kutu_grafigi_ciz("egitim_skoru",  "Eğitim Skoru – Bölge Bazlı Dağılım",
                 renk_egitim,  "05_kutu_egitim")
kutu_grafigi_ciz("kultur_skoru",  "Kültür Skoru – Bölge Bazlı Dağılım",
                 renk_kultur,  "06_kutu_kultur")
kutu_grafigi_ciz("ekonomi_skoru", "Ekonomi Skoru – Bölge Bazlı Dağılım",
                 renk_ekonomi, "07_kutu_ekonomi")

# ============================================================
# GRAFİK 8: Bölge × Kategori Dikey Bar Chart
# ============================================================

g8_veri <- sonuc %>%
  group_by(bolge) %>%
  summarise(
    `Yaşanabilirlik\nEndeksi` = mean(yasanabilirlik_endeksi,   na.rm = TRUE),
    Sağlık   = mean(saglik_skoru,  na.rm = TRUE),
    Eğitim   = mean(egitim_skoru,  na.rm = TRUE),
    Kültür   = mean(kultur_skoru,  na.rm = TRUE),
    Ekonomi  = mean(ekonomi_skoru, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  pivot_longer(-bolge, names_to = "kategori", values_to = "ort_skor") %>%
  mutate(
    bolge    = factor(bolge, levels = names(bolge_renkleri)),
    kategori = factor(kategori,
                      levels = c("Yaşanabilirlik\nEndeksi",
                                 "Sağlık", "Eğitim", "Kültür", "Ekonomi"))
  )

kategori_renkleri_g8 <- c(
  "Yaşanabilirlik\nEndeksi" = renk_endeks,
  "Sağlık"                  = renk_saglik,
  "Eğitim"                  = renk_egitim,
  "Kültür"                  = renk_kultur,
  "Ekonomi"                 = renk_ekonomi
)

g8 <- ggplot(g8_veri, aes(x = bolge, y = ort_skor, fill = kategori)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.88) +
  geom_text(aes(label = round(ort_skor, 2)),
            position = position_dodge(width = 0.75),
            vjust = -0.4, size = 3, color = "grey25") +
  scale_fill_manual(values = kategori_renkleri_g8, name = "Kategori") +
  scale_y_continuous(limits = c(0, 1.05),
                     labels = number_format(accuracy = 0.01)) +
  labs(
    title   = "Coğrafi Bölgelerin Kategori Bazlı Ortalama Skorları",
    subtitle = "Her bölge için 4 kategori ortalaması",
    x = NULL, y = "Ortalama Skor [0–1]",
    caption = "Kaynak: TÜİK 2021-2024"
  ) +
  tema_proje +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

grafik_kaydet(g8, "08_bolge_kategori_bar", genislik = 13, yukseklik = 8)

# ============================================================
# GRAFİK 9: Tüm 81 İl Yatay Bar Chart (Bölge Rengi)
# ============================================================

g9_veri <- sonuc %>%
  arrange(desc(yasanabilirlik_endeksi)) %>%
  mutate(
    il    = factor(il, levels = rev(il)),
    bolge = factor(bolge, levels = names(bolge_renkleri))
  )

g9 <- ggplot(g9_veri, aes(x = yasanabilirlik_endeksi, y = il, fill = bolge)) +
  geom_col(alpha = 0.82, width = 0.75) +
  geom_text(aes(label = round(yasanabilirlik_endeksi, 2)),
            hjust = -0.1, size = 2.5, color = "grey30") +
  scale_fill_manual(values = bolge_renkleri, name = "Coğrafi Bölge") +
  scale_x_continuous(limits = c(0, 1.12),
                     labels = number_format(accuracy = 0.01)) +
  labs(
    title   = "Türkiye'de Tüm İllerin Yaşanabilirlik Endeksi",
    subtitle = "81 il, büyükten küçüğe sıralı; renkler coğrafi bölgeyi göstermektedir",
    x = "Yaşanabilirlik Endeksi [0–1]", y = NULL,
    caption = "Kaynak: TÜİK 2021-2024"
  ) +
  tema_proje +
  theme(
    axis.text.y     = element_text(size = 7),
    panel.grid.major.y = element_blank(),
    legend.position = "right"
  )

grafik_kaydet(g9, "09_tum_iller_bar", genislik = 12, yukseklik = 18)

# ============================================================
# GRAFİK 10: Endeks + 4 Kategori En İyi 3 İl (Pie Charts)
# ============================================================

pie_ciz <- function(metrik_sutun, baslik, renk) {
  veri <- sonuc %>%
    arrange(desc(.data[[metrik_sutun]])) %>%
    head(3) %>%
    mutate(
      il    = factor(il, levels = rev(il)),
      etiket = paste0(il, "\n", round(.data[[metrik_sutun]], 3))
    )
  
  # Pasta renkleri: ana rengin 3 tonu
  pasta_renkleri <- colorRampPalette(c(renk, "white"))(5)[1:3]
  
  ggplot(veri, aes(x = "", y = .data[[metrik_sutun]], fill = il)) +
    geom_col(width = 1, color = "white", linewidth = 0.8) +
    geom_text(aes(label = etiket),
              position = position_stack(vjust = 0.5),
              size = 3.5, color = "white", fontface = "bold") +
    coord_polar(theta = "y") +
    scale_fill_manual(values = setNames(pasta_renkleri, levels(veri$il)),
                      guide = "none") +
    labs(title = baslik) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 12,
                                    hjust = 0.5, margin = margin(b = 5)))
}

p_endeks  <- pie_ciz("yasanabilirlik_endeksi",   "Yaşanabilirlik\nEndeksi", renk_endeks)
p_saglik  <- pie_ciz("saglik_skoru",  "Sağlık",                  renk_saglik)
p_egitim  <- pie_ciz("egitim_skoru",  "Eğitim",                  renk_egitim)
p_kultur  <- pie_ciz("kultur_skoru",  "Kültür",                  renk_kultur)
p_ekonomi <- pie_ciz("ekonomi_skoru", "Ekonomi",                 renk_ekonomi)

g10 <- (p_endeks | p_saglik | p_egitim | p_kultur | p_ekonomi) +
  plot_annotation(
    title   = "Her Metrik İçin En İyi 3 İl",
    caption = "Kaynak: TÜİK 2021-2024",
    theme   = theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))
  )

grafik_kaydet(g10, "10_pie_en_iyi_3_il", genislik = 16, yukseklik = 5)

# ============================================================
# GRAFİK 11: Her Bölge Top-3 Dikey Bar Chart
# ============================================================

g11_veri <- sonuc %>%
  group_by(bolge) %>%
  arrange(desc(yasanabilirlik_endeksi)) %>%
  slice_head(n = 3) %>%
  mutate(sira = row_number(),
         il_etiket = paste0(sira, ". ", il)) %>%
  ungroup() %>%
  mutate(bolge = factor(bolge, levels = names(bolge_renkleri)))

g11 <- ggplot(g11_veri,
              aes(x = reorder(il_etiket, yasanabilirlik_endeksi),
                  y = yasanabilirlik_endeksi, fill = bolge)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_text(aes(label = round(yasanabilirlik_endeksi, 3)),
            vjust = -0.4, size = 3.2, color = "grey25") +
  facet_wrap(~ bolge, scales = "free_x", ncol = 4) +
  scale_fill_manual(values = bolge_renkleri, guide = "none") +
  scale_y_continuous(limits = c(0, 1.1),
                     labels = number_format(accuracy = 0.01)) +
  labs(
    title   = "Her Coğrafi Bölgenin En Yaşanabilir İlk 3 İli",
    subtitle = "Yaşanabilirlik endeksine göre sıralı",
    x = NULL, y = "Yaşanabilirlik Endeksi [0–1]",
    caption = "Kaynak: TÜİK 2021-2024"
  ) +
  tema_proje +
  theme(
    axis.text.x  = element_text(angle = 15, hjust = 1, size = 9),
    strip.text   = element_text(face = "bold", size = 10)
  )

grafik_kaydet(g11, "11_bolge_top3_bar", genislik = 15, yukseklik = 9)

# ============================================================
# GRAFİK 12: Tahmin vs Gerçek Karşılaştırma
# ============================================================

# Tahminler
tahminler <- tribble(
  ~bolge,                ~tahmin_1,   ~tahmin_2,    ~tahmin_3,
  "Marmara",             "Edirne",    "Bursa",      "İstanbul",
  "Ege",                 "Muğla",     "İzmir",      "Aydın",
  "Akdeniz",             "Mersin",    "Antalya",    "Adana",
  "Karadeniz",           "Samsun",    "Zonguldak",  "Kastamonu",
  "İç Anadolu",          "Ankara",    "Eskişehir",  "Nevşehir",
  "Doğu Anadolu",        "Van",       "Malatya",    "Erzurum",
  "Güneydoğu Anadolu",   "Gaziantep", "Mardin",     "Diyarbakır"
)

# Gerçek top-3
gercek_top3 <- sonuc %>%
  group_by(bolge) %>%
  arrange(desc(yasanabilirlik_endeksi)) %>%
  slice_head(n = 3) %>%
  mutate(sira = row_number()) %>%
  ungroup() %>%
  select(bolge, sira, il, yasanabilirlik_endeksi) %>%
  pivot_wider(id_cols = bolge,
              names_from  = sira,
              values_from = c(il, yasanabilirlik_endeksi),
              names_glue  = "{.value}_{sira}")

# Birleştir ve eşleşmeleri bul
karsilastirma <- tahminler %>%
  left_join(gercek_top3, by = "bolge") %>%
  mutate(
    eslesme_1    = tahmin_1 %in% c(il_1, il_2, il_3),
    eslesme_2    = tahmin_2 %in% c(il_1, il_2, il_3),
    eslesme_3    = tahmin_3 %in% c(il_1, il_2, il_3),
    toplam_dogru = eslesme_1 + eslesme_2 + eslesme_3
  )

# Her hücre: il adı + eşleşme durumu
tablo_veri <- karsilastirma %>%
  mutate(bolge = factor(bolge, levels = names(bolge_renkleri))) %>%
  transmute(
    bolge,
    `1. Tahmin`  = tahmin_1,
    `1. Gerçek`  = il_1,
    `2. Tahmin`  = tahmin_2,
    `2. Gerçek`  = il_2,
    `3. Tahmin`  = tahmin_3,
    `3. Gerçek`  = il_3,
    `Doğru`      = paste0(toplam_dogru, " / 3"),
    e1 = eslesme_1,
    e2 = eslesme_2,
    e3 = eslesme_3
  ) %>%
  arrange(bolge)

# Uzun forma çevir
tablo_uzun <- tablo_veri %>%
  mutate(
    satir = as.integer(bolge)
  ) %>%
  pivot_longer(
    cols      = c(`1. Tahmin`, `1. Gerçek`,
                  `2. Tahmin`, `2. Gerçek`,
                  `3. Tahmin`, `3. Gerçek`,
                  `Doğru`),
    names_to  = "sutun",
    values_to = "deger"
  ) %>%
  mutate(
    sutun = factor(sutun, levels = c("1. Tahmin", "1. Gerçek",
                                     "2. Tahmin", "2. Gerçek",
                                     "3. Tahmin", "3. Gerçek",
                                     "Doğru")),
    sutun_no = as.integer(sutun),
    # Tahmin hücrelerinin arka plan rengi
    arkaplan = case_when(
      sutun == "1. Tahmin" & e1 ~ "#D5F5E3",   # eşleşen → açık yeşil
      sutun == "1. Tahmin" & !e1 ~ "#FADBD8",  # eşleşmeyen → açık kırmızı
      sutun == "2. Tahmin" & e2 ~ "#D5F5E3",
      sutun == "2. Tahmin" & !e2 ~ "#FADBD8",
      sutun == "3. Tahmin" & e3 ~ "#D5F5E3",
      sutun == "3. Tahmin" & !e3 ~ "#FADBD8",
      sutun == "Doğru"           ~ "#EBF5FB",   # özet → açık mavi
      TRUE                       ~ "grey96"
    ),
    metin_renk = case_when(
      sutun %in% c("1. Tahmin", "2. Tahmin", "3. Tahmin") &
        (e1 | e2 | e3) ~ "#1E8449",
      sutun %in% c("1. Tahmin", "2. Tahmin", "3. Tahmin") ~ "#922B21",
      sutun == "Doğru"  ~ "#1A5276",
      TRUE              ~ "grey20"
    )
  )

g12 <- ggplot(tablo_uzun,
              aes(x = sutun_no, y = -satir)) +
  # Arka plan kutucukları
  geom_tile(aes(fill = arkaplan), color = "white", linewidth = 1.2,
            width = 0.95, height = 0.85) +
  scale_fill_identity() +
  # Bölge adı (en sol sütun — y ekseninde)
  geom_text(data = tablo_veri %>%
              mutate(satir  = as.integer(bolge),
                     b_renk = bolge_renkleri[as.character(bolge)]),
            aes(x = 0, y = -satir, label = as.character(bolge)),
            color    = bolge_renkleri[as.character(
              tablo_veri %>%
                arrange(as.integer(factor(bolge,
                                          levels = names(bolge_renkleri)))) %>%
                pull(bolge))],
            fontface = "bold", size = 3.8, hjust = 1,
            inherit.aes = FALSE) +
  # Hücre metinleri
  geom_text(aes(label = deger, color = metin_renk),
            size = 3.5, fontface = "bold") +
  scale_color_identity() +
  # Sütun başlıkları
  geom_text(data = data.frame(
    sutun_no = 1:7,
    label    = c("1. Tahmin", "1. Gerçek",
                 "2. Tahmin", "2. Gerçek",
                 "3. Tahmin", "3. Gerçek",
                 "Doğru")),
    aes(x = sutun_no, y = 0.6, label = label),
    fontface = "bold", size = 3.8, color = "grey20",
    inherit.aes = FALSE) +
  # Ayırıcı çizgiler (tahmin–gerçek grupları arası)
  geom_vline(xintercept = c(2.5, 4.5), linetype = "dashed",
             color = "grey70", linewidth = 0.6) +
  scale_x_continuous(limits = c(-1.5, 7.5)) +
  scale_y_continuous(limits = c(-7.6, 1)) +
  labs(
    title   = "Tahmin vs Gerçek: Bölge Bazlı En Yaşanabilir İlk 3 İl",
    subtitle = paste0("Yeşil arka plan = eşleşen tahmin  |  ",
                      "Kırmızı arka plan = eşleşmeyen tahmin"),
    caption = "Kaynak: TÜİK 2021-2024"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 15,
                                 hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11, hjust = 0.5,
                                 color = "grey40", margin = margin(b = 10)),
    plot.caption  = element_text(size = 9, color = "grey55",
                                 hjust = 1, margin = margin(t = 8)),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(20, 30, 15, 80)
  )

grafik_kaydet(g12, "12_tahmin_vs_gercek_tablo", genislik = 14, yukseklik = 7)
