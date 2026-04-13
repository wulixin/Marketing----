
# data_load.R
library(readxl)
library(dplyr)
library(tidyr)

# 设置文件路径
file_path <- "/Users/wulixin/Downloads/每日板块信息.xlsx"


# 1. 读取“指数表现”
df_index_perf <- read_excel(file_path, sheet = "指数表现") %>%
  rename(
    code = `指数代码`,
    name = `指数名称`,
    close = `收盘`,
    daily_change = `日涨跌`,
    daily_pct = `日涨跌幅（%）`,
    ytd_change = `今年以来涨跌`,
    ytd_pct = `今年以来涨跌幅（%）`,
    volume_diff_abs = `成交额较昨日增减（亿元）`,
    volume_diff_pct = `成交额较昨日增减（%）`
  ) %>%
  mutate(
    category = "指数表现"
  )



# 2. 读取“指数估值”
df_index_valuation <- read_excel(file_path, sheet = "指数估值") %>%
  rename(
    name = `指数名称）`,
    static_pe = `静态市盈率`,
    rolling_pe = `滚动市盈率`,
    pb = `市静率`,
    dividend_yield = `股息率`,
    last_year_static_pe = `去年静态市盈率`,
    last_year_rolling_pe = `去年滚动市盈率`,
    last_year_pb = `去年市静率`
  ) %>%
  mutate(category = "指数估值")

# 3. 读取“指数贡献”
df_index_contribution <- read_excel(file_path, sheet = "指数贡献") %>%
  rename(
    index_name = `贡献指数名称`,
    stock_name = `股票名称`,
    close_price = `收盘价`,
    daily_pct = `日涨跌幅(%)`,
    contribution_points = `贡献点数`
  ) %>%
  mutate(category = "指数贡献")


# 4. 读取“每日板块信息”
df_sector_info <- read_excel(file_path, sheet = "每日板块信息") %>%
  rename(
    index_name = `指数名称`,
    sector_name = `板块名称`,
    daily_pct = `日涨跌幅（%）`
  ) %>%
  # 根据“沪市最强表现”等标签，创建一个“表现类型”列
  mutate(
    performance_type = case_when(
      str_detect(index_name, "最强表现") ~ "最强",
      str_detect(index_name, "最弱表现") ~ "最弱",
      TRUE ~ "未知"
    ),
    market = case_when(
      str_detect(index_name, "沪市") ~ "沪市",
      str_detect(index_name, "深市") ~ "深市",
      str_detect(index_name, "港市") ~ "港市",
      TRUE ~ "其他"
    )
  ) %>%
  mutate(category = "每日板块信息")

# ，添加以下清洗步骤：
df_sector_info <- df_sector_info %>%
  mutate(
    # 移除可能存在的 "%" 符号，并转换为数值（小数形式）
    daily_pct = str_remove_all(daily_pct, "%") %>% 
      as.numeric() %>% 
      # 如果原始数据是 "5.23" 表示 5.23%，则除以 100；如果是 "0.0523" 则不用
      # 根据你的实际数据判断是否需要 /100
      { if (max(., na.rm = TRUE) > 1) ./100 else . }
  )

library(foreign)
# 保存为 RData 文件
save(df_index_perf, df_index_valuation, df_index_contribution, df_sector_info, 
     file = "/Users/wulixin/Downloads/processed_data.RData")

# 转换数据框类型
# 转换数据框类型


