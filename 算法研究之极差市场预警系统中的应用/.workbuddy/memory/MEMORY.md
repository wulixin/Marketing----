# 工作记忆 - 极差市场预警系统项目

## 项目概述
构建极差(Range Spread)市场预警系统，监测A股科技赛道与消费/金融赛道的极端分化。
- 5组配对（科技大牛股vs传统牛股）：寒武纪vs茅台、海光vs上海机场、中际旭创vs招行、工业富联vs海天味业、宁德时代vs中信证券
- 核心算法：Z-Score标准化 → 极差计算 → 布林带(2σ)预警 → 均值回归信号
- 交付物：Python代码 + R代码 + HTML仪表板(ECharts) + 分析文档

## 技术栈
- Python 3.13: range_spread_monitor.py (tushare 1.4.29)
- R: range_spread_monitor.R (依赖tidyverse, zoo, jsonlite, Tushare)
- HTML: range_spread_dashboard.html (ECharts 5.5, 暗色主题)
- 数据源：Tushare Pro API (真实3年行情数据, 2023-05~2026-05, 约726个交易日)

## Tushare配置
- Token: fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55
- Python: `ts.pro_api(token)` → `pro.daily(ts_code='600519.SH', start_date='20230516')`
- R: `pro <- pro_api(token=TUSHARE_TOKEN)` → `pro(api_name='daily', ts_code='600519.SH', start_date='20230516')`
- 上海后缀: .SH (如600519.SH, 688256.SH)
- 深圳后缀: .SZ (如300308.SZ, 300750.SZ)

## 股票代码对照
- 贵州茅台 600519.SH | 寒武纪 688256.SH
- 海光信息 688041.SH | 上海机场 600009.SH
- 中际旭创 300308.SZ | 招商银行 600036.SH (注：之前错误写成600036，招行正确代码就是600036)
- 工业富联 601138.SH | 海天味业 603288.SH
- 宁德时代 300750.SZ | 中信证券 600030.SH

## 文件清单
- `range_spread_monitor.py` — Python版完整系统(Tushare真实数据)
- `range_spread_monitor.R` — R版完整系统(Tushare真实数据)
- `range_spread_dashboard.html` — 交互式HTML仪表板
- `极差统计分析技术应用.md` — 算法原理与扩展应用文档
- `output/` — 数据输出目录(CSV/JSON)

## 关键决策
- 滚动窗口=60日(约1个季度)，布林带=2σ
- 数据源从模拟数据升级为Tushare Pro真实3年行情
- 仪表板使用fetch加载JSON(需HTTP服务器: `python3 -m http.server 8090`)
- R的Tushare包名为大写T `Tushare`，函数为 `pro_api(token=)`

## 最新运行结果(v2.0, 2026-05-16) — 配对顺序已调整为科技股vs传统股
- 中际旭创vs招商银行：综合极差4.31，极端偏高⚠️，百分位92.43%
- 寒武纪vs贵州茅台：综合极差+1.97，偏高预警（原"茅台vs寒武纪"已对调）
- 海光信息vs上海机场：综合极差2.29，偏高预警
- 工业富联vs海天味业：综合极差2.29，偏高预警
- 宁德时代vs中信证券：综合极差0.32，正常区间
