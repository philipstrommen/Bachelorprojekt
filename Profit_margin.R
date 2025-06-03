#Profitmargin
SL <- read_excel("/Users/philipstrommen/Desktop/Bachelor/DNK_17_3.xlsx")
SL<-SL %>% filter(Season == "2022/2023" | Season == "2023/2024" | Season == "2024/2025")

# Beregn profit-margin for hver række
SL$profit_margin <- (1/SL[[11]]) * 100 + (1/SL[[12]]) * 100 + (1/SL[[13]]) * 100

# Beregn gennemsnitlig cut
cut_calc <- (mean(SL$profit_margin) - 100) / 100
cut_calc
