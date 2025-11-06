# 載入必要的套件
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# 讀取數據
Table_1 <- read_excel("C:/Users/26385/OneDrive/桌面/Table 130-06806_tc_1.xlsx")
Table_2 <- read_excel("C:/Users/26385/OneDrive/桌面/Table 130-06806_tc.xlsx")

# 整理 Table_1 (家庭收入中位數)
Table_1_long <- Table_1 %>%
  pivot_longer(cols = starts_with("20"),  # 將年度列展開
               names_to = "Year",         # 新列名：年份
               values_to = "Household_Income") %>%  # 新列名：家庭收入中位數
  rename(District = `地區從事經濟活動家庭住戶每月入息中位數(港元)`)

# 整理 Table_2 (個人收入中位數)
Table_2_long <- Table_2 %>%
  pivot_longer(cols = starts_with("20"),  # 將年度列展開
               names_to = "Year",         # 新列名：年份
               values_to = "Personal_Income") %>%  # 新列名：個人收入中位數
  rename(District = `地區住戶每月入息中位數(港元)`)

# 合併數據
merged_data <- Table_1_long %>%
  inner_join(Table_2_long, by = c("District", "Year"))

# 繪製圖像
# 1. 家庭收入中位數變化折線圖
p1 <- ggplot(Table_1_long, aes(x = Year, y = Household_Income, group = District, color = District)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "各區每月家庭收入中位數變化 (2020-2023)", 
       x = "年份", 
       y = "家庭收入中位數 (港元)", 
       color = "地區") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. 個人收入中位數變化折線圖
p2 <- ggplot(Table_2_long, aes(x = Year, y = Personal_Income, group = District, color = District)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "各區每月個人收入中位數變化 (2020-2023)", 
       x = "年份", 
       y = "個人收入中位數 (港元)", 
       color = "地區") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 繪製個人與家庭收入中位數變化對比圖 (明確圖例)
p3 <- ggplot(merged_data, aes(x = Year)) +
  geom_line(aes(y = Household_Income, group = District, color = District, linetype = "家庭收入"), size = 1) +
  geom_line(aes(y = Personal_Income, group = District, color = District, linetype = "個人收入"), size = 1) +
  labs(title = "各區個人與家庭收入中位數變化對比 (2020-2023)",
       x = "年份",
       y = "收入中位數 (港元)",
       color = "地區",
       linetype = "收入類型") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 顯示圖像
print(p1)
print(p2)
print(p3)