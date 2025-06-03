#Downloadet data 6/2-2025 
DNK <- read_excel("/Users/philipstrommen/Desktop/Bachelor/DNK.xlsx")

#Filtrering af data
DNK_filtered <- DNK %>%
  filter(Season %in% c("2022/2023", "2023/2024", "2024/2025"))

#UAFHÆNGIGHED-test
#Observeret kampe med resultater
# 1) Opret lumpede kolonner i DNK_filtered
DNK_filtered$HG_lumped <- ifelse(DNK_filtered$HG >= 5, "5+", as.character(DNK_filtered$HG))
DNK_filtered$AG_lumped <- ifelse(DNK_filtered$AG >= 5, "5+", as.character(DNK_filtered$AG))

# Gør dem til faktorer, så rækkefølgen bliver pæn (0,1,2,3,4,5+):
DNK_filtered$HG_lumped <- factor(DNK_filtered$HG_lumped, 
                                 levels = c("0","1","2","3","4","5+"))
DNK_filtered$AG_lumped <- factor(DNK_filtered$AG_lumped, 
                                 levels = c("0","1","2","3","4","5+"))

# 2) Lav kontingenstabel af lumpede kolonner
cont_tbl_lumped <- table(DNK_filtered$HG_lumped, DNK_filtered$AG_lumped)

# 3) Kør chi^2-test på den lumpede tabel
test_result_lumped <- chisq.test(cont_tbl_lumped, correct = F)
test_result_lumped  # Udskriv resultatet af testen

# 4) Konvertér til data frame og omdøb kolonner
df_cont_lumped <- as.data.frame(cont_tbl_lumped)
colnames(df_cont_lumped) <- c("HG", "AG", "Antal")

# Se den nye data frame
df_cont_lumped

#Se det forventede antal resultater
test_result_lumped$expected
