#wd oluşturma
setwd("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn")

#MS Excel dosyalarını okumak için readxl
library(readxl)

#İl ve bağlı olduğu bölge listesi
il_listesi <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/il_listesi.xls")
#head(il_listesi)  

#iller ve nüfusları, nüfus verisi kişi başına düşen değerleri hesaplamak için kullanılacak
nufus <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/population.xls")
#head(nufus)


#kültürel imkanlar data setleri
#toplam sinema sayısı
toplam_sinema <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Culture/cinema.xls")
#head(toplam_sinema)  

#toplam kütüphane sayısı
toplam_kutuphane <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Culture/library.xls")

#toplam müze sayısı
toplam_muze <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Culture/museum.xls")

#toplam tiyatro sayısı
toplam_tiyatro <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Culture/theatre.xls")


#sağlık data setleri
#toplam hastane sayısı
toplam_hastane <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Health/hospital.xls")
#head(toplam_hastane)

#Bin kişi başına düşen toplam hekim sayısı
hekim <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Health/doctor.xls")

#Yüzbin kişi başına toplam hastane yatak sayısı
hastane_yatak <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Health/hospital-bed.xls")
#head(hastane_yatak)

#bebek ölüm hızı
bebek_olum <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Health/baby-death.xls")

#5 yaş altı çocuk ölüm oranları
cocuk_olum <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Health/child-death.xls")


#eğitim data setleri
#ilkokullarda sınıf başına düşen öğrenci sayısı
cocuk_sınıf <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/education/student-per-class.xls")

#ilkokullarda öğretmen başına düşen öğrenci sayısı
cocuk_ogretmen <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/education/student-per-teacher.xls")


#ekonomik data setleri
#istihdam oranı
istihdam <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Economy/employment.xls")

#girişimci sayısı 
girisimci <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Economy/enterprising.xls")

#işsizlik oranı
issizlik <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Economy/unemployment.xls")

#ihracat 
ihracat <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Economy/export.xls")

#GSYH
gsyh <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Economy/GSYH.xls")

#kişi başı GSYH 
gsyh_per <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Economy/GSYH-per.xls")


#güvenlik data setleri
#toplam trafik kazaları  
trafik_kaza <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Safety/traffic-accidents.xls")

#Suç işlenen ile göre ceza infaz kurumuna giren hükümlü sayısı
hukumlu <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Safety/crime.xls")

#toplam bağımlı çocuk  
bagimli_cocuk <- read_excel("/home/nurettin/Desktop/burcu/emu660-spring2026-BurcuuOzmenn/assets/TUIK/Safety/child-addiction.xls")
