# ==============================================================================
# 退市风险机会挖掘系统 — R语言分析代码
# 数据来源：另类数据挖掘之退市风险.xlsx + 辅助画像标签数据
# ==============================================================================

# 1. 加载必要的包 ---------------------------------------------------------------
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plotly")) install.packages("plotly")
if (!require("highcharter")) install.packages("highcharter")
if (!require("DT")) install.packages("DT")
if (!require("stringr")) install.packages("stringr")
if (!require("jsonlite")) install.packages("jsonlite")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(highcharter)
library(DT)
library(stringr)
library(jsonlite)

# 2. 数据读取 -------------------------------------------------------------------
data_dir <- "."

# 2.1 退市风险公告数据（5个Sheet）
sheet_names <- excel_sheets(file.path(data_dir, "另类数据挖掘之退市风险.xlsx"))
board_labels <- c("沪市A股", "深市A股主板", "创业板", "北交所", "科创板")
names(board_labels) <- sheet_names

delisting_all <- lapply(sheet_names, function(sn) {
  df <- read_excel(file.path(data_dir, "另类数据挖掘之退市风险.xlsx"), sheet = sn)
  df$板块 <- board_labels[sn]
  return(df)
})
delisting_df <- bind_rows(delisting_all)

# 2.2 简称变更数据
name_change <- read_excel(file.path(data_dir, "简称变更.xlsx"))

# 2.3 PE/PB/ROE估值数据
pe_data <- read.delim(file.path(data_dir, "2026年PE市盈率PB市净率ROE投资风格.txt"),
                       header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# 2.4 行情数据
prices <- read.csv(file.path(data_dir, "dataprices_20260515.csv"), stringsAsFactors = FALSE)

# 2.5 股票画像标签
stock_label <- read.csv(file.path(data_dir, "df_all_type.csv"), stringsAsFactors = FALSE)

# 2.6 异常波动标签
abnormal <- read.delim(file.path(data_dir, "2027年个股异常波动标签画像.txt"),
                        header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# 3. 数据预处理 -----------------------------------------------------------------

# 3.1 公告类型分类标注
delisting_df <- delisting_df %>%
  mutate(
    is_zhaimao = str_detect(公告类型, "撤销|摘帽"),
    is_shishi  = str_detect(公告类型, "实施"),
    is_fengxian = str_detect(公告类型, "风险提示|风险警示"),
    is_ST = str_detect(名称, "\\*?ST"),
    代码 = str_pad(as.character(代码), width = 6, side = "left", pad = "0")
  )

# 3.2 按代码聚合，提取每只股票的关键信息
code_agg <- delisting_df %>%
  group_by(代码, 板块) %>%
  summarise(
    名称 = last(名称),
    公告数 = n(),
    摘帽次数 = sum(is_zhaimao),
    实施次数 = sum(is_shishi),
    风险提示次数 = sum(is_fengxian),
    最新公告日期 = max(公告日期),
    最新公告类型 = last(公告类型),
    最新公告标题 = last(公告标题),
    最早公告日期 = min(公告日期),
    .groups = "drop"
  )

# 3.3 风险状态分类
code_agg <- code_agg %>%
  mutate(
    风险状态 = case_when(
      摘帽次数 > 0 & 实施次数 == 0 ~ "摘帽成功",
      摘帽次数 > 0 & 实施次数 > 0 ~ "摘帽申请中",
      实施次数 > 0 ~ "退市风险实施中",
      风险提示次数 > 0 ~ "风险提示中",
      TRUE ~ "其他公告"
    )
  )

# 3.4 关联行情数据
prices$symbol <- str_replace(prices$ts_code, "\\.(SZ|SH|BJ)$", "")
price_cols <- prices %>% select(symbol, close, pct_chg, vol, amount)

code_agg <- code_agg %>%
  left_join(price_cols, by = c("代码" = "symbol"))

# 3.5 关联PE/PB/ROE
pe_data$代码 <- str_pad(as.character(pe_data$证券代码), width = 6, side = "left", pad = "0")
pe_cols <- pe_data %>% select(代码, 个股股息率, 个股市净率, 个股滚动市盈率, 个股静态市盈率, 投资风格)

code_agg <- code_agg %>%
  left_join(pe_cols, by = "代码")

# 3.6 关联股票画像标签
stock_label$sym <- str_pad(as.character(stock_label$symbol), width = 6, side = "left", pad = "0")
label_cols <- stock_label %>% select(sym, ShiZhi, industry_1, industry_4, market, act_name, act_ent_type, NTpye1, NTpye9, list_date)

code_agg <- code_agg %>%
  left_join(label_cols, by = c("代码" = "sym"))

# 3.7 关联异常波动标签
abnormal$sym <- str_replace(abnormal$ts_code, "\\.(SZ|SH|BJ)$", "")
ab_cols <- abnormal %>% select(sym, 游资偏好度, Pianhao)

code_agg <- code_agg %>%
  left_join(ab_cols, by = c("代码" = "sym"))

# 4. 三大板块（科创板/北交所/创业板）分析 ----------------------------------------

focus_df <- code_agg %>% filter(板块 %in% c("科创板", "北交所", "创业板"))

# 4.1 摘帽机会识别
zhaimao_df <- focus_df %>%
  filter(风险状态 %in% c("摘帽成功", "摘帽申请中")) %>%
  arrange(板块, desc(摘帽次数))

cat("=== 摘帽机会公司 ===\n")
print(zhaimao_df %>% select(代码, 名称, 板块, 风险状态, close, pct_chg))

# 4.2 退市风险高危识别
risk_df <- focus_df %>%
  filter(风险状态 %in% c("退市风险实施中", "风险提示中")) %>%
  arrange(板块, desc(实施次数))

cat("\n=== 退市风险高危公司 ===\n")
print(risk_df %>% select(代码, 名称, 板块, 风险状态, close, pct_chg))

# 5. 可视化分析 -----------------------------------------------------------------

# 5.1 各板块风险状态分布
p1 <- ggplot(code_agg, aes(x = 板块, fill = 风险状态)) +
  geom_bar(position = "stack") +
  labs(title = "各板块退市风险状态分布", x = "板块", y = "公司数") +
  scale_fill_manual(values = c(
    "摘帽成功" = "#27ae60",
    "摘帽申请中" = "#f39c12",
    "退市风险实施中" = "#e74c3c",
    "风险提示中" = "#e67e22",
    "其他公告" = "#95a5a6"
  )) +
  theme_minimal(base_family = "STHeiti") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("plot_board_status.png", p1, width = 10, height = 6)

# 5.2 摘帽成功率按板块
zhaimao_rate <- code_agg %>%
  group_by(板块) %>%
  summarise(
    总数 = n(),
    摘帽数 = sum(风险状态 %in% c("摘帽成功", "摘帽申请中")),
    摘帽率 = round(摘帽数 / 总数 * 100, 1)
  )

p2 <- ggplot(zhaimao_rate, aes(x = 板块, y = 摘帽率, fill = 板块)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(摘帽率, "%")), vjust = -0.5) +
  labs(title = "各板块摘帽成功率", x = "", y = "摘帽率(%)") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_family = "STHeiti")

ggsave("plot_zhaimao_rate.png", p2, width = 8, height = 5)

# 5.3 退市公司行业分布（三大板块）
p3 <- focus_df %>%
  filter(!is.na(industry_1)) %>%
  ggplot(aes(x = industry_1, fill = 风险状态)) +
  geom_bar(position = "stack") +
  coord_flip() +
  labs(title = "科创板/北交所/创业板退市公司行业分布",
       x = "行业", y = "公司数") +
  scale_fill_manual(values = c(
    "摘帽成功" = "#27ae60",
    "摘帽申请中" = "#f39c12",
    "退市风险实施中" = "#e74c3c",
    "风险提示中" = "#e67e22",
    "其他公告" = "#95a5a6"
  )) +
  theme_minimal(base_family = "STHeiti")

ggsave("plot_industry_dist.png", p3, width = 10, height = 6)

# 5.4 公告时间线分析
timeline_data <- delisting_df %>%
  filter(板块 %in% c("科创板", "北交所", "创业板")) %>%
  mutate(公告月份 = format(as.Date(公告日期), "%Y-%m")) %>%
  count(板块, 公告月份, is_zhaimao) %>%
  mutate(类型 = ifelse(is_zhaimao, "摘帽相关", "其他退市公告"))

p4 <- ggplot(timeline_data, aes(x = 公告月份, y = n, fill = 类型)) +
  geom_col(position = "stack") +
  facet_wrap(~板块, scales = "free_y") +
  labs(title = "三大板块退市公告月度分布", x = "月份", y = "公告数") +
  scale_fill_manual(values = c("摘帽相关" = "#27ae60", "其他退市公告" = "#e74c3c")) +
  theme_minimal(base_family = "STHeiti") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plot_timeline.png", p4, width = 12, height = 6)

# 6. 交互式可视化（plotly/highcharter）------------------------------------------

# 6.1 交互式散点图：滚动PE vs 收盘价（按风险状态着色）
p5 <- plot_ly(
  data = focus_df %>% filter(!is.na(close) & !is.na(个股市净率)),
  x = ~close,
  y = ~个股市净率,
  color = ~风险状态,
  text = ~paste0(名称, " (", 代码, ")<br>板块:", 板块, "<br>收盘:", close),
  type = "scatter",
  mode = "markers",
  sizes = c(5, 20)
) %>%
  layout(
    title = "退市风险公司：收盘价 vs 市净率",
    xaxis = list(title = "收盘价(元)"),
    yaxis = list(title = "市净率")
  )

htmlwidgets::saveWidget(p5, "plot_scatter_pb.html")

# 6.2 Highcharts 板块公告数
hc1 <- hchart(
  code_agg %>% count(板块, 风险状态) %>% spread(风险状态, n, fill = 0),
  type = "column",
  hcaes(x = 板块)
) %>%
  hc_title(text = "各板块退市风险状态分布") %>%
  hc_colors(c("#27ae60", "#f39c12", "#e74c3c", "#e67e22", "#95a5a6"))

htmlwidgets::saveWidget(hc1, "plot_hc_board.html")

# 7. 导出分析结果 ---------------------------------------------------------------

# 7.1 导出整合数据为JSON
result_json <- toJSON(list(
  summary = list(
    total_stocks = nrow(code_agg),
    total_announcements = nrow(delisting_df),
    board_counts = code_agg %>% count(板块) %>% deframe(),
    status_counts = code_agg %>% count(风险状态) %>% deframe()
  ),
  focus_stocks = focus_df %>% select(-is_zhaimao, -is_shishi, -is_fengxian, -is_ST)
), auto_unbox = TRUE, force = TRUE)

writeLines(result_json, "delisting_analysis_result.json")

# 7.2 导出Excel结果
writexl::write_xlsx(
  list(
    全市场退市风险 = code_agg,
    科创板北交所创业板 = focus_df,
    摘帽机会 = zhaimao_df,
    退市风险高危 = risk_df
  ),
  path = "退市风险分析结果.xlsx"
)

cat("\n=== 分析完成！输出文件 ===\n")
cat("1. plot_board_status.png — 各板块风险状态分布\n")
cat("2. plot_zhaimao_rate.png — 摘帽成功率\n")
cat("3. plot_industry_dist.png — 行业分布\n")
cat("4. plot_timeline.png — 月度公告时间线\n")
cat("5. plot_scatter_pb.html — 交互式散点图\n")
cat("6. 退市风险分析结果.xlsx — 完整分析Excel\n")
cat("7. delisting_analysis_result.json — JSON格式结果\n")
