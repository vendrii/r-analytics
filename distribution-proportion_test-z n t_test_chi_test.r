setwd("#")
library(readxl)
tugas = read_excel("Tugas data kuesioner.xlsx")


#Converting character to number
library(dplyr)
tugas = tugas %>%
  mutate(weighted_sikap = case_when(tugas$Sikap %in% "Setuju" ~ 1,
                                    tugas$Sikap %in% "Tidak Setuju" ~ 2,
                                    tugas$Sikap %in% "abstain" ~ 3))


#B.1 (Pakenya yang binomial [yang keputusan sukses/gagal])
#inputing variable x with x only "Tidak Setuju"
x = tugas[tugas$weighted_sikap==2,]

#length x of Tidak Setuju
x = NROW(x)
n = NROW(tugas)

#Result B.1
prop.test(x, n, p = 0.05, alternative = "less",correct = FALSE)

#B.2
library(BSDA)
zsum.test(mean.x=mean(Kepuasan), sigma.x=sd(Kepuasan), n.x=NROW(Kepuasan),
          alternative = "greater", mu=80,
          conf.level=0.95)

#B.3
#Convert Alphabet to Numeric
ipkcd = tugas %>%
  filter(Prodi %in% c("C","D")) %>%
  select(Prodi, IPK) %>%
  group_by(Prodi, IPK)

t.test(ipkcd$IPK~ipkcd$Prodi, var.equal = TRUE)

#C.1 persentase mahasiswa tidak setuju di kelima prodi sama besar
# Create only Tidak Setuju
# Stage 1 = filtering data Tidak Setuju
names(tugas)
tso = tugas %>%
  filter(Sikap == "Tidak Setuju") %>%
  select(Prodi, Sikap) %>%
  group_by(Prodi,Sikap)
tso
tso = table(tso)
tso = as.data.frame(tso)
# Stage 2 = Aggregating data cause group by can't cumulative the data\
tso = tso %>%
  select(Prodi, Freq) %>%
  group_by(Prodi) %>%
  summarise(value1 = sum(Freq))
tso
# Create table exclude Tidak Setuju i.e abstain & Setuju
# Stage 1 = filtering data Setuju and abstain
aso = tugas %>%
  filter(Sikap %in% c("Setuju","abstain")) %>%
  select(Prodi, Sikap) %>%
  group_by(Prodi, Sikap)
aso = table(aso)
aso = as.data.frame(aso)
# Stage 2 = Aggregating data cause group by can't cumulative the data
aso = aso %>%
  select(Prodi,Freq) %>%
  group_by(Prodi) %>%
  summarise(value2 = sum(Freq))
aso
# Combining table for running prop.test
combi = data.frame(tso$value1,aso$value2)
combi = as.matrix(combi)
combi
# Finding resut for C.1
prop.test(combi)

#C.2
# Taking data
all = tugas %>%
  select(Prodi,Sikap) %>%
  group_by(Prodi,Sikap)
#convert table
all = table(all)
all = as.matrix(all)
all
chisq.test(all)

#C.3
library(car)
attach(tugas)
leveneTest(IPK~Prodi,data = tugas)
aov.1way = aov(IPK~Prodi, data = tugas)
summary(aov.1way)

#C.4
library(car)
attach(tugas)
leveneTest(Kepuasan~Prodi,data = tugas)
aov.1way = aov(Kepuasan~Prodi, data = tugas)
summary(aov.1way)
