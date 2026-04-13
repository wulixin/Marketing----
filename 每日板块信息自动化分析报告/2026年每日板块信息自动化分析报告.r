
## 🚀 第四步：自动化运行
  

# run_analysis.R
# 1. 数据加载
source("/Users/wulixin/Downloads/data_load.R")

# 2. 数据分析
source("/Users/wulixin/Downloads/analysis.R")

# 3. 生成报告
rmarkdown::render("/Users/wulixin/Downloads/report.Rmd", output_file = paste0("daily_report_", Sys.Date(), ".html"))

message("✅ 报告已成功生成！")

30 9 * * 1-5 /Users/wulixin/Downloads/run_analysis.R