# ==============================================================================
# 极差市场预警系统 v2.0 - R版 (Tushare + CSI行业PE)
# Range Spread Market Early Warning System
#
# v2.0新增:
#   - 行业市盈率极差 (PE Spread): 基于中证行业分类的静态PE差值
#   - 动态PE极差: Tushare daily_basic获取个股PE_TTM
#   - 综合极差: 0.6×价格极差 + 0.4×PE极差
#
# 配对组合:
# 1. 寒武纪(688256.SH) vs 贵州茅台(600519.SH)  — 半导体(PE 117.8) vs 白酒(PE 20.2)
# 2. 海光信息(688041.SH) vs 上海机场(600009.SH) — 半导体(PE 117.8) vs 航空运输(PE 26.3)
# 3. 中际旭创(300308.SZ) vs 招商银行(600036.SH) — 通信系统设备(PE 96.7) vs 银行(PE 6.7)
# 4. 工业富联(601138.SH) vs 海天味业(603288.SH) — 消费电子组件(PE 41.8) vs 食品(PE 22.3)
# 5. 宁德时代(300750.SZ) vs 中信证券(600030.SH) — 锂(PE 101.3) vs 证券(PE 15.5)
# ==============================================================================

# 加载包
required_packages <- c("tidyverse", "zoo", "jsonlite", "Tushare")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
    library(pkg, character.only = TRUE)
  }
}

# ==============================================================================
# Tushare API配置
# ==============================================================================

TUSHARE_TOKEN <- 'fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55'

# 初始化Tushare连接
pro <- pro_api(token = TUSHARE_TOKEN)

# 5组配对定义
PAIRS_CONFIG <- list(
  list(high_name = "寒武纪", high_code = "688256.SH", low_name = "贵州茅台", low_code = "600519.SH"),
  list(high_name = "海光信息", high_code = "688041.SH", low_name = "上海机场", low_code = "600009.SH"),
  list(high_name = "中际旭创", high_code = "300308.SZ", low_name = "招商银行", low_code = "600036.SH"),
  list(high_name = "工业富联", high_code = "601138.SH", low_name = "海天味业", low_code = "603288.SH"),
  list(high_name = "宁德时代", high_code = "300750.SZ", low_name = "中信证券", low_code = "600030.SH")
)

# 回测日期范围 (3年)
START_DATE <- "20230516"
END_DATE   <- "20260516"

# ==============================================================================
# 1. 数据获取模块 — 通过Tushare获取真实行情数据
# ==============================================================================

fetch_stock_data <- function(ts_code, start_date = START_DATE, end_date = END_DATE) {
  # 通过Tushare获取个股日线行情
  # ts_code: Tushare代码 (如 '600519.SH', '300308.SZ')
  # 返回: tibble with columns [date, open, high, low, close, vol, amount]
  df <- pro(api_name = "daily", ts_code = ts_code,
            start_date = start_date, end_date = end_date)
  
  if (is.null(df) || nrow(df) == 0) {
    stop(paste("无法获取", ts_code, "的数据，请检查代码是否正确"))
  }
  
  # Tushare返回的是倒序(最新在前)，需要正序排列
  df <- df %>% arrange(trade_date)
  
  # 转换日期格式
  df$date <- as.Date(df$trade_date, format = "%Y%m%d")
  
  # 选择并重命名列
  df %>% select(date, open, high, low, close, pre_close, change, pct_chg, vol, amount)
}


generate_market_data <- function() {
  # 获取5组配对的真实行情数据 + PE_TTM (Tushare)
  # 返回: list(df, pairs_config)
  cat("  正在从Tushare获取数据...\n")
  
  all_data <- list()
  pairs_config <- list()
  
  for (pair in PAIRS_CONFIG) {
    high_name <- pair$high_name
    high_code <- pair$high_code
    low_name  <- pair$low_name
    low_code  <- pair$low_code
    
    # 获取高位股数据
    cat(paste0("    获取 ", high_name, "(", high_code, ")...\n"))
    df_high <- pro(api_name = "daily", ts_code = high_code,
                   start_date = START_DATE, end_date = END_DATE)
    if (is.null(df_high) || nrow(df_high) == 0) {
      stop(paste("无法获取", high_name, "(", high_code, ") 的数据"))
    }
    df_high <- df_high %>% arrange(trade_date)
    df_high$date <- as.Date(df_high$trade_date, format = "%Y%m%d")
    high_col <- paste0(high_name, "_", substr(high_code, 1, 6))
    
    # 获取低位股数据
    cat(paste0("    获取 ", low_name, "(", low_code, ")...\n"))
    df_low <- pro(api_name = "daily", ts_code = low_code,
                  start_date = START_DATE, end_date = END_DATE)
    if (is.null(df_low) || nrow(df_low) == 0) {
      stop(paste("无法获取", low_name, "(", low_code, ") 的数据"))
    }
    df_low <- df_low %>% arrange(trade_date)
    df_low$date <- as.Date(df_low$trade_date, format = "%Y%m%d")
    low_col <- paste0(low_name, "_", substr(low_code, 1, 6))
    
    all_data[[high_col]] <- df_high %>% select(date, close) %>% rename(!!high_col := close)
    all_data[[low_col]]  <- df_low %>% select(date, close) %>% rename(!!low_col := close)
    
    # 获取高位股PE_TTM (合并到价格数据中)
    cat(paste0("    获取 ", high_name, " PE_TTM...\n"))
    df_high_pe <- tryCatch({
      pro(api_name = "daily_basic", ts_code = high_code,
          start_date = START_DATE, end_date = END_DATE,
          fields = "ts_code,trade_date,pe_ttm,turnover_rate")
    }, error = function(e) NULL)
    if (!is.null(df_high_pe) && nrow(df_high_pe) > 0) {
      df_high_pe <- df_high_pe %>% arrange(trade_date)
      df_high_pe$date <- as.Date(df_high_pe$trade_date, format = "%Y%m%d")
      pe_col <- paste0(high_name, "_", substr(high_code, 1, 6), "_PE")
      all_data[[pe_col]] <- df_high_pe %>% select(date, pe_ttm) %>% rename(!!pe_col := pe_ttm)
    }
    
    # 获取低位股PE_TTM
    cat(paste0("    获取 ", low_name, " PE_TTM...\n"))
    df_low_pe <- tryCatch({
      pro(api_name = "daily_basic", ts_code = low_code,
          start_date = START_DATE, end_date = END_DATE,
          fields = "ts_code,trade_date,pe_ttm,turnover_rate")
    }, error = function(e) NULL)
    if (!is.null(df_low_pe) && nrow(df_low_pe) > 0) {
      df_low_pe <- df_low_pe %>% arrange(trade_date)
      df_low_pe$date <- as.Date(df_low_pe$trade_date, format = "%Y%m%d")
      pe_col <- paste0(low_name, "_", substr(low_code, 1, 6), "_PE")
      all_data[[pe_col]] <- df_low_pe %>% select(date, pe_ttm) %>% rename(!!pe_col := pe_ttm)
    }
    
    pairs_config[[length(pairs_config) + 1]] <- list(
      high_name = high_name,
      high_code = substr(high_code, 1, 6),
      low_name  = low_name,
      low_code  = substr(low_code, 1, 6)
    )
  }
  
  # 合并所有数据 (按日期对齐, 使用full_join)
  cat("    合并数据并处理缺失值...\n")
  df <- all_data[[1]]
  for (i in 2:length(all_data)) {
    df <- full_join(df, all_data[[i]], by = "date")
  }
  
  df <- df %>% arrange(date)
  
  # 前向填充缺失值 (停牌期间使用前一日值)
  for (col in names(df)) {
    if (col != "date") {
      df[[col]] <- zoo::na.locf(df[[col]], na.rm = FALSE)
    }
  }
  
  # 去掉开盘可能的NaN (只在非PE列上过滤)
  price_cols <- names(df)[!grepl("_PE$|_TR$", names(df)) & names(df) != "date"]
  df <- df %>% filter(complete.cases(across(all_of(price_cols))))
  # 对PE列前向填充
  for (col in names(df)) {
    if (grepl("_PE$|_TR$", col)) {
      df[[col]] <- zoo::na.locf(df[[col]], na.rm = FALSE)
    }
  }
  
  list(df = df, pairs_config = pairs_config)
}

# ==============================================================================
# 2. 极差计算模块
# ==============================================================================

calc_zscore <- function(x, window = 60) {
  roll_mean <- rollapply(x, window, mean, fill = NA, align = "right")
  roll_sd <- rollapply(x, window, sd, fill = NA, align = "right")
  (x - roll_mean) / roll_sd
}

calc_range_spread <- function(df, high_col, low_col, window = 60) {
  high_prices <- df[[high_col]]
  low_prices  <- df[[low_col]]
  
  # 累计收益率
  cum_ret_high <- high_prices / high_prices[1]
  cum_ret_low  <- low_prices / low_prices[1]
  
  # Z-Score标准化
  z_high <- calc_zscore(cum_ret_high, window)
  z_low  <- calc_zscore(cum_ret_low, window)
  
  # 极差
  spread <- z_high - z_low
  
  # 布林带
  spread_mean <- rollapply(spread, window, mean, fill = NA, align = "right")
  spread_sd   <- rollapply(spread, window, sd, fill = NA, align = "right")
  upper_band  <- spread_mean + 2.0 * spread_sd
  lower_band  <- spread_mean - 2.0 * spread_sd
  
  # 预警信号
  signal <- ifelse(spread > upper_band, 1, ifelse(spread < lower_band, -1, 0))
  
  tibble(
    date = df$date,
    cum_ret_high = cum_ret_high,
    cum_ret_low = cum_ret_low,
    z_high = z_high,
    z_low = z_low,
    spread = spread,
    spread_mean = spread_mean,
    upper_band = upper_band,
    lower_band = lower_band,
    signal = signal
  )
}

calc_pe_spread <- function(df, high_pe_col, low_pe_col, window = 60) {
  # 计算动态PE极差指标
  pe_high <- df[[high_pe_col]]
  pe_low  <- df[[low_pe_col]]
  
  # PE比值
  pe_ratio <- pe_high / ifelse(pe_low == 0, NA, pe_low)
  
  # Z-Score标准化PE
  z_pe_high <- calc_zscore(pe_high, window)
  z_pe_low  <- calc_zscore(pe_low, window)
  
  # PE极差
  pe_spread <- z_pe_high - z_pe_low
  
  # 布林带
  pe_spread_mean <- rollapply(pe_spread, window, mean, fill = NA, align = "right")
  pe_spread_sd   <- rollapply(pe_spread, window, sd, fill = NA, align = "right")
  pe_upper <- pe_spread_mean + 2.0 * pe_spread_sd
  pe_lower <- pe_spread_mean - 2.0 * pe_spread_sd
  
  pe_signal <- ifelse(pe_spread > pe_upper, 1, ifelse(pe_spread < pe_lower, -1, 0))
  
  tibble(
    date = df$date,
    pe_high = pe_high,
    pe_low = pe_low,
    pe_ratio = pe_ratio,
    z_pe_high = z_pe_high,
    z_pe_low = z_pe_low,
    pe_spread = pe_spread,
    pe_spread_mean = pe_spread_mean,
    pe_upper = pe_upper,
    pe_lower = pe_lower,
    pe_signal = pe_signal
  )
}

calc_composite_spread <- function(price_spread_df, pe_spread_df, alpha = 0.6) {
  # 综合极差 = alpha * 价格极差 + (1-alpha) * PE极差
  composite <- alpha * price_spread_df$spread + (1 - alpha) * pe_spread_df$pe_spread
  
  comp_mean <- rollapply(composite, 60, mean, fill = NA, align = "right")
  comp_sd   <- rollapply(composite, 60, sd, fill = NA, align = "right")
  comp_upper <- comp_mean + 2.0 * comp_sd
  comp_lower <- comp_mean - 2.0 * comp_sd
  comp_signal <- ifelse(composite > comp_upper, 1, ifelse(composite < comp_lower, -1, 0))
  
  tibble(
    date = price_spread_df$date,
    composite = composite,
    composite_mean = comp_mean,
    composite_upper = comp_upper,
    composite_lower = comp_lower,
    composite_signal = comp_signal
  )
}

calc_all_pairs <- function(df, pairs_config, window = 60) {
  results <- list()
  for (pair in pairs_config) {
    high_col <- paste0(pair$high_name, "_", pair$high_code)
    low_col  <- paste0(pair$low_name, "_", pair$low_code)
    pair_key <- paste0(pair$high_name, " vs ", pair$low_name)
    
    # 价格极差
    price_spread <- calc_range_spread(df, high_col, low_col, window)
    
    # PE极差
    high_pe_col <- paste0(high_col, "_PE")
    low_pe_col  <- paste0(low_col, "_PE")
    
    has_pe <- high_pe_col %in% names(df) && low_pe_col %in% names(df)
    
    if (has_pe) {
      pe_spread <- calc_pe_spread(df, high_pe_col, low_pe_col, window)
      composite <- calc_composite_spread(price_spread, pe_spread)
    } else {
      pe_spread <- NULL
      composite <- NULL
    }
    
    results[[pair_key]] <- list(
      price_spread = price_spread,
      pe_spread = pe_spread,
      composite = composite,
      has_pe = has_pe
    )
  }
  results
}

# ==============================================================================
# 3. 预警信号模块
# ==============================================================================

generate_alerts <- function(results, df, pairs_config) {
  alerts <- list()
  for (i in seq_along(pairs_config)) {
    pair <- pairs_config[[i]]
    pair_key <- paste0(pair$high_name, " vs ", pair$low_name)
    spread_data <- results[[pair_key]]
    
    n <- nrow(spread_data)
    current_spread <- spread_data$spread[n]
    current_upper  <- spread_data$upper_band[n]
    current_lower  <- spread_data$lower_band[n]
    current_signal <- spread_data$signal[n]
    
    high_col <- paste0(pair$high_name, "_", pair$high_code)
    low_col  <- paste0(pair$low_name, "_", pair$low_code)
    high_price <- df[[high_col]][nrow(df)]
    low_price  <- df[[low_col]][nrow(df)]
    
    # 极差偏离度
    deviation <- if (current_upper != 0) {
      (current_spread - current_upper) / abs(current_upper) * 100
    } else 0
    
    # 预警等级
    if (current_signal == 1) {
      alert_level <- "极端偏高"
      advice <- paste0(pair$high_name, "(", pair$high_code, ")过热风险极高，",
                       pair$low_name, "(", pair$low_code, ")超跌反弹概率大")
    } else if (current_signal == -1) {
      alert_level <- "极端偏低"
      advice <- paste0(pair$low_name, "(", pair$low_code, ")相对走强，",
                       pair$high_name, "(", pair$high_code, ")可能回调")
    } else {
      alert_level <- "正常区间"
      advice <- "极差处于正常波动范围，暂无极端信号"
    }
    
    # 历史极值百分位
    spread_max <- max(spread_data$spread, na.rm = TRUE)
    spread_min <- min(spread_data$spread, na.rm = TRUE)
    spread_pct <- if (spread_max != spread_min) {
      (current_spread - spread_min) / (spread_max - spread_min) * 100
    } else 50
    
    alerts[[i]] <- list(
      pair = pair_key,
      high_stock = paste0(pair$high_name, "(", pair$high_code, ")"),
      low_stock = paste0(pair$low_name, "(", pair$low_code, ")"),
      high_price = round(high_price, 2),
      low_price = round(low_price, 2),
      current_spread = round(current_spread, 4),
      upper_band = round(current_upper, 4),
      lower_band = round(current_lower, 4),
      deviation_pct = round(deviation, 2),
      spread_pct = round(spread_pct, 2),
      alert_level = alert_level,
      advice = advice,
      signal = current_signal
    )
  }
  alerts
}

# ==============================================================================
# 4. 统计分析模块
# ==============================================================================

statistical_analysis <- function(results, pairs_config) {
  stats_list <- list()
  for (pair in pairs_config) {
    pair_key <- paste0(pair$high_name, " vs ", pair$low_name)
    spread <- results[[pair_key]]$spread
    spread <- spread[!is.na(spread)]
    
    # 基础统计
    mean_val <- mean(spread)
    std_val  <- sd(spread)
    skew_val <- (sum((spread - mean_val)^3) / length(spread)) / std_val^3
    kurt_val <- (sum((spread - mean_val)^4) / length(spread)) / std_val^4 - 3
    
    # 极值
    max_val <- max(spread)
    min_val <- min(spread)
    max_idx <- which.max(spread)
    min_idx <- which.min(spread)
    max_date <- as.character(results[[pair_key]]$date[max_idx])
    min_date <- as.character(results[[pair_key]]$date[min_idx])
    
    # 均值回归 - 半衰期 (AR(1))
    deviation <- spread - rollapply(spread, 60, mean, fill = NA, align = "right")
    deviation <- deviation[!is.na(deviation)]
    half_life <- NA
    if (length(deviation) > 2) {
      ar_fit <- tryCatch(arima(deviation, order = c(1, 0, 0)), error = function(e) NULL)
      if (!is.null(ar_fit)) {
        ar_coef <- coef(ar_fit)[1]
        if (ar_coef > 0 && ar_coef < 1) {
          half_life <- -log(2) / log(ar_coef)
        }
      }
    }
    
    # 穿越均值次数
    spread_sign <- sign(spread - mean(spread))
    cross_count <- sum(diff(spread_sign) != 0, na.rm = TRUE)
    
    # 当前百分位
    current_pct <- if (max_val != min_val) {
      (spread[length(spread)] - min_val) / (max_val - min_val) * 100
    } else 50
    
    stats_list[[pair_key]] <- tibble(
      pair = pair_key,
      mean = round(mean_val, 4),
      std = round(std_val, 4),
      skewness = round(skew_val, 4),
      kurtosis = round(kurt_val, 4),
      max = round(max_val, 4),
      min = round(min_val, 4),
      max_date = max_date,
      min_date = min_date,
      half_life_days = round(half_life, 1),
      cross_mean_count = cross_count,
      current_pct = round(current_pct, 2)
    )
  }
  bind_rows(stats_list)
}

# ==============================================================================
# 5. 导出模块
# ==============================================================================

export_for_dashboard <- function(df, results, alerts, stats_df, output_dir = "output") {
  dir.create(output_dir, showWarnings = FALSE)
  
  # 导出CSV
  write_csv(df, file.path(output_dir, "price_data_r.csv"))
  
  for (pair_key in names(results)) {
    safe_name <- gsub(" vs ", "_vs_", pair_key)
    write_csv(results[[pair_key]], file.path(output_dir, paste0("spread_r_", safe_name, ".csv")))
  }
  
  # 导出预警JSON
  alerts_json <- toJSON(alerts, auto_unbox = TRUE, pretty = TRUE)
  writeLines(alerts_json, file.path(output_dir, "alerts_r.json"))
  
  # 导出统计JSON
  stats_json <- toJSON(stats_df, auto_unbox = TRUE, pretty = TRUE)
  writeLines(stats_json, file.path(output_dir, "stats_r.json"))
  
  cat(paste0("  数据已导出至 ", output_dir, " 目录\n"))
}

# ==============================================================================
# 6. 主程序
# ==============================================================================

main <- function() {
  cat(strrep("=", 70), "\n")
  cat("  极差市场预警系统 — R版 (Tushare真实数据)\n")
  cat("  Range Spread Market Early Warning System\n")
  cat(strrep("=", 70), "\n\n")
  
  # 获取真实数据
  cat("[1/5] 从Tushare获取真实行情数据...\n")
  data <- generate_market_data()
  df <- data$df
  pairs_config <- data$pairs_config
  cat(sprintf("  数据范围: %s ~ %s\n", as.character(min(df$date)), as.character(max(df$date))))
  cat(sprintf("  交易日数: %d\n", nrow(df)))
  cat(sprintf("  配对数量: %d\n", length(pairs_config)))
  
  # 打印最新价格
  cat("\n  最新收盘价:\n")
  for (pair in pairs_config) {
    high_col <- paste0(pair$high_name, "_", pair$high_code)
    low_col  <- paste0(pair$low_name, "_", pair$low_code)
    cat(sprintf("    %s: %.2f  |  %s: %.2f\n",
                pair$high_name, df[[high_col]][nrow(df)],
                pair$low_name, df[[low_col]][nrow(df)]))
  }
  
  # 计算极差
  cat("\n[2/5] 计算极差指标...\n")
  results <- calc_all_pairs(df, pairs_config, window = 60)
  for (pair_key in names(results)) {
    n <- nrow(results[[pair_key]])
    current <- results[[pair_key]]$spread[n]
    sig <- results[[pair_key]]$signal[n]
    sig_text <- if (sig == 1) "偏高⚠️" else if (sig == -1) "偏低" else "正常"
    cat(sprintf("  %s: 当前极差=%.4f  信号=%s\n", pair_key, current, sig_text))
  }
  
  # 生成预警
  cat("\n[3/5] 生成预警信号...\n")
  alerts <- generate_alerts(results, df, pairs_config)
  for (a in alerts) {
    emoji <- if (a$alert_level == "极端偏高") "🔴" else if (a$alert_level == "极端偏低") "🟢" else "⚪"
    cat(sprintf("  %s %s: %s | 极差=%.4f | 偏离=%.2f%%\n",
                emoji, a$pair, a$alert_level, a$current_spread, a$deviation_pct))
  }
  
  # 统计分析
  cat("\n[4/5] 统计特征分析...\n")
  stats_df <- statistical_analysis(results, pairs_config)
  print(stats_df[, c("pair", "mean", "std", "skewness", "half_life_days", "current_pct")])
  
  # 导出数据
  cat("\n[5/5] 导出数据...\n")
  base_dir <- getwd()
  output_dir <- file.path(base_dir, "output")
  export_for_dashboard(df, results, alerts, stats_df, output_dir)
  
  cat("\n", strrep("=", 70), "\n")
  cat("  运行完成！数据来源: Tushare真实行情\n")
  cat(strrep("=", 70), "\n")
  
  invisible(list(df = df, results = results, alerts = alerts, stats_df = stats_df))
}

# 执行
result <- main()
