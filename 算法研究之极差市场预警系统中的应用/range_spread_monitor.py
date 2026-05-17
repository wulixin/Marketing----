#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
极差市场预警系统 v2.0 - Python版 (Tushare + CSI行业PE)
Range Spread Market Early Warning System

v2.0新增:
  - 行业市盈率极差 (PE Spread): 基于中证行业分类的静态PE差值
  - 估值极差 (Valuation Spread): 结合股价极差与PE极差的综合指标
  - PE均值回归加速度: 行业PE偏离历史均值的速度

配对组合:
1. 寒武纪(688256.SH) vs 贵州茅台(600519.SH)  — 半导体(PE 117.8) vs 白酒(PE 20.2)
2. 海光信息(688041.SH) vs 上海机场(600009.SH) — 半导体(PE 117.8) vs 航空运输(PE 26.3)
3. 中际旭创(300308.SZ) vs 招商银行(600036.SH) — 通信设备(PE 88.1) vs 银行(PE 6.7)
4. 工业富联(601138.SH) vs 海天味业(603288.SH) — 消费电子组件(PE 41.8) vs 食品(PE 22.3)
5. 宁德时代(300750.SZ) vs 中信证券(600030.SH) — 锂(PE 101.3) vs 证券(PE 15.5)
"""

import numpy as np
import pandas as pd
import tushare as ts
import json
import os
from datetime import datetime, timedelta
import warnings
warnings.filterwarnings('ignore')

# ============================================================================
# 配置
# ============================================================================
TUSHARE_TOKEN = 'fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55'
START_DATE = '20230516'
END_DATE = '20260516'

PAIRS_CONFIG = [
    ('寒武纪', '688256.SH', '贵州茅台', '600519.SH'),
    ('海光信息', '688041.SH', '上海机场', '600009.SH'),
    ('中际旭创', '300308.SZ', '招商银行', '600036.SH'),
    ('工业富联', '601138.SH', '海天味业', '603288.SH'),
    ('宁德时代', '300750.SZ', '中信证券', '600030.SH'),
]

# 中证行业PE映射 (来自csi20260515.xls)
CSI_PE_MAP = {
    # 行业代码: (行业名称, 最新静态PE, 近1月均PE, 近3月均PE, 近6月均PE, 近1年均PE)
    '4530':       ('半导体', 117.78, 115.11, 108.05, 104.69, 92.74),
    '45303010':   ('半导体材料', 107.88, 107.70, 105.67, 101.10, 86.35),
    '45303020':   ('半导体设备', 111.57, 102.19, 96.52, 92.56, 77.69),
    '30101010':   ('白酒', 20.20, 18.39, 17.54, 17.72, 18.23),
    '4010':       ('银行', 6.69, 6.99, 6.96, 7.15, 7.20),
    '401010':     ('商业银行', 6.69, 6.99, 6.96, 7.09, 7.37),
    '40301010':   ('证券公司', 15.45, 19.47, 21.43, 23.19, 23.92),
    '4510':       ('计算机', 86.26, 87.09, 86.67, 89.88, 88.52),
    '502010':     ('通信设备', 88.10, 96.09, 87.85, 83.78, 64.37),
    '50201020':   ('通信系统设备', 96.67, 110.20, 100.90, 95.67, 70.64),
    '3010':       ('食品饮料烟草', 22.26, 21.12, 20.54, 20.84, 21.38),
    '30103040':   ('休闲食品', 23.22, 22.50, 22.10, 22.90, 23.49),
    '45201030':   ('消费电子组件', 41.83, 46.08, 43.83, 44.10, 39.73),
    '15203030':   ('锂', 101.28, 98.56, 93.27, 87.45, 72.50),
    '20701030':   ('航空运输', 26.27, 28.15, 29.34, 30.12, 31.50),
    '452020':     ('电子元件', 75.62, 87.34, 80.74, 78.56, 64.36),
}

# 每组配对对应的中证行业PE (高位股行业代码, 低位股行业代码)
PAIR_PE_MAP = {
    '寒武纪 vs 贵州茅台': ('4530', '30101010'),         # 半导体 vs 白酒
    '海光信息 vs 上海机场': ('4530', '20701030'),         # 半导体 vs 航空运输
    '中际旭创 vs 招商银行': ('50201020', '4010'),         # 通信系统设备 vs 银行
    '工业富联 vs 海天味业': ('45201030', '3010'),         # 消费电子组件 vs 食品饮料
    '宁德时代 vs 中信证券': ('15203030', '40301010'),     # 锂 vs 证券公司
}


# ============================================================================
# 1. 数据获取模块
# ============================================================================

def generate_market_data():
    """获取5组配对的真实行情数据 + 个股动态PE"""
    pro = ts.pro_api(TUSHARE_TOKEN)
    all_data = {}
    pairs_config = []
    
    print("  正在从Tushare获取数据...")
    
    for high_name, high_code, low_name, low_code in PAIRS_CONFIG:
        # 获取高位股日线
        print(f"    获取 {high_name}({high_code})...")
        df_high = pro.daily(ts_code=high_code, start_date=START_DATE, end_date=END_DATE)
        df_high = df_high.sort_values('trade_date').reset_index(drop=True)
        df_high['trade_date'] = pd.to_datetime(df_high['trade_date'], format='%Y%m%d')
        
        # 获取高位股每日指标(含PE)
        try:
            df_high_basic = pro.daily_basic(ts_code=high_code, start_date=START_DATE, end_date=END_DATE,
                                            fields='ts_code,trade_date,pe_ttm,pb,turnover_rate')
            df_high_basic = df_high_basic.sort_values('trade_date').reset_index(drop=True)
            df_high_basic['trade_date'] = pd.to_datetime(df_high_basic['trade_date'], format='%Y%m%d')
        except:
            df_high_basic = None
        
        # 获取低位股日线
        print(f"    获取 {low_name}({low_code})...")
        df_low = pro.daily(ts_code=low_code, start_date=START_DATE, end_date=END_DATE)
        df_low = df_low.sort_values('trade_date').reset_index(drop=True)
        df_low['trade_date'] = pd.to_datetime(df_low['trade_date'], format='%Y%m%d')
        
        # 获取低位股每日指标(含PE)
        try:
            df_low_basic = pro.daily_basic(ts_code=low_code, start_date=START_DATE, end_date=END_DATE,
                                           fields='ts_code,trade_date,pe_ttm,pb,turnover_rate')
            df_low_basic = df_low_basic.sort_values('trade_date').reset_index(drop=True)
            df_low_basic['trade_date'] = pd.to_datetime(df_low_basic['trade_date'], format='%Y%m%d')
        except:
            df_low_basic = None
        
        high_col = f'{high_name}_{high_code[:6]}'
        low_col = f'{low_name}_{low_code[:6]}'
        
        # 价格数据
        all_data[high_col] = df_high[['trade_date', 'close']].rename(columns={'close': high_col})
        all_data[low_col] = df_low[['trade_date', 'close']].rename(columns={'close': low_col})
        
        # PE_TTM数据
        if df_high_basic is not None and 'pe_ttm' in df_high_basic.columns:
            pe_col = f'{high_name}_{high_code[:6]}_PE'
            all_data[pe_col] = df_high_basic[['trade_date', 'pe_ttm']].rename(columns={'pe_ttm': pe_col})
        
        if df_low_basic is not None and 'pe_ttm' in df_low_basic.columns:
            pe_col = f'{low_name}_{low_code[:6]}_PE'
            all_data[pe_col] = df_low_basic[['trade_date', 'pe_ttm']].rename(columns={'pe_ttm': pe_col})
        
        # 换手率数据
        if df_high_basic is not None and 'turnover_rate' in df_high_basic.columns:
            tr_col = f'{high_name}_{high_code[:6]}_TR'
            all_data[tr_col] = df_high_basic[['trade_date', 'turnover_rate']].rename(columns={'turnover_rate': tr_col})
        
        if df_low_basic is not None and 'turnover_rate' in df_low_basic.columns:
            tr_col = f'{low_name}_{low_code[:6]}_TR'
            all_data[tr_col] = df_low_basic[['trade_date', 'turnover_rate']].rename(columns={'turnover_rate': tr_col})
        
        pairs_config.append((high_name, high_code[:6], low_name, low_code[:6]))
    
    # 合并所有数据
    print("    合并数据并处理缺失值...")
    df = None
    for col_name, col_df in all_data.items():
        if df is None:
            df = col_df
        else:
            df = df.merge(col_df, on='trade_date', how='outer')
    
    df = df.sort_values('trade_date').reset_index(drop=True)
    df = df.rename(columns={'trade_date': 'date'})
    
    # 前向填充
    for col in df.columns:
        if col != 'date':
            df[col] = df[col].ffill()
    
    df = df.dropna(subset=[c for c in df.columns if c != 'date' and not c.endswith('_PE') and not c.endswith('_TR')]).reset_index(drop=True)
    # 对PE/TR列也前向填充
    for col in df.columns:
        if col.endswith('_PE') or col.endswith('_TR'):
            df[col] = df[col].ffill()
    
    df.set_index('date', inplace=True)
    
    return df, pairs_config


# ============================================================================
# 2. 极差计算模块 (价格 + PE)
# ============================================================================

def calc_zscore(series, window=60):
    """滚动Z-Score标准化"""
    rolling_mean = series.rolling(window=window).mean()
    rolling_std = series.rolling(window=window).std()
    return (series - rolling_mean) / rolling_std


def calc_range_spread(df, high_col, low_col, window=60):
    """计算价格极差指标"""
    cum_ret_high = df[high_col] / df[high_col].iloc[0]
    cum_ret_low = df[low_col] / df[low_col].iloc[0]
    
    z_high = calc_zscore(cum_ret_high, window)
    z_low = calc_zscore(cum_ret_low, window)
    
    spread = z_high - z_low
    
    spread_mean = spread.rolling(window=window).mean()
    spread_std = spread.rolling(window=window).std()
    upper_band = spread_mean + 2.0 * spread_std
    lower_band = spread_mean - 2.0 * spread_std
    
    signal = pd.Series(0, index=df.index)
    signal[spread > upper_band] = 1
    signal[spread < lower_band] = -1
    
    return pd.DataFrame({
        'cum_ret_high': cum_ret_high,
        'cum_ret_low': cum_ret_low,
        'z_high': z_high,
        'z_low': z_low,
        'spread': spread,
        'spread_mean': spread_mean,
        'upper_band': upper_band,
        'lower_band': lower_band,
        'signal': signal
    })


def calc_pe_spread(df, high_pe_col, low_pe_col, window=60):
    """
    计算动态PE极差指标
    PE Spread = Z(高位股PE_TTM) - Z(低位股PE_TTM)
    
    当PE极差扩大：高位股估值泡沫化 vs 低位股估值被极度压缩
    """
    pe_high = df[high_pe_col]
    pe_low = df[low_pe_col]
    
    # PE比值 (直接衡量估值差异)
    pe_ratio = pe_high / pe_low.replace(0, np.nan)
    
    # Z-Score标准化PE
    z_pe_high = calc_zscore(pe_high, window)
    z_pe_low = calc_zscore(pe_low, window)
    
    # PE极差
    pe_spread = z_pe_high - z_pe_low
    
    # PE极差布林带
    pe_spread_mean = pe_spread.rolling(window=window).mean()
    pe_spread_std = pe_spread.rolling(window=window).std()
    pe_upper = pe_spread_mean + 2.0 * pe_spread_std
    pe_lower = pe_spread_mean - 2.0 * pe_spread_std
    
    # PE信号
    pe_signal = pd.Series(0, index=df.index)
    pe_signal[pe_spread > pe_upper] = 1   # 估值极度分化
    pe_signal[pe_spread < pe_lower] = -1  # 估值趋于收敛
    
    return pd.DataFrame({
        'pe_high': pe_high,
        'pe_low': pe_low,
        'pe_ratio': pe_ratio,
        'z_pe_high': z_pe_high,
        'z_pe_low': z_pe_low,
        'pe_spread': pe_spread,
        'pe_spread_mean': pe_spread_mean,
        'pe_upper': pe_upper,
        'pe_lower': pe_lower,
        'pe_signal': pe_signal
    })


def calc_composite_spread(price_spread_df, pe_spread_df, alpha=0.6):
    """
    综合极差 = α × 价格极差 + (1-α) × PE极差
    α=0.6: 价格权重60%, PE权重40%
    
    综合极差同时考虑价格走势分化与估值分化，信号更可靠
    """
    composite = alpha * price_spread_df['spread'] + (1 - alpha) * pe_spread_df['pe_spread']
    
    comp_mean = composite.rolling(window=60).mean()
    comp_std = composite.rolling(window=60).std()
    comp_upper = comp_mean + 2.0 * comp_std
    comp_lower = comp_mean - 2.0 * comp_std
    
    comp_signal = pd.Series(0, index=composite.index)
    comp_signal[composite > comp_upper] = 1
    comp_signal[composite < comp_lower] = -1
    
    return pd.DataFrame({
        'composite': composite,
        'composite_mean': comp_mean,
        'composite_upper': comp_upper,
        'composite_lower': comp_lower,
        'composite_signal': comp_signal
    })


def calc_all_pairs(df, pairs_config, window=60):
    """计算所有配对的价格极差 + PE极差 + 综合极差"""
    results = {}
    for high_name, high_code, low_name, low_code in pairs_config:
        high_col = f'{high_name}_{high_code}'
        low_col = f'{low_name}_{low_code}'
        pair_key = f'{high_name} vs {low_name}'
        
        # 价格极差
        price_spread = calc_range_spread(df, high_col, low_col, window)
        
        # PE极差
        high_pe_col = f'{high_name}_{high_code}_PE'
        low_pe_col = f'{low_name}_{low_code}_PE'
        
        if high_pe_col in df.columns and low_pe_col in df.columns:
            pe_spread = calc_pe_spread(df, high_pe_col, low_pe_col, window)
            composite = calc_composite_spread(price_spread, pe_spread)
            has_pe = True
        else:
            pe_spread = None
            composite = None
            has_pe = False
        
        results[pair_key] = {
            'price_spread': price_spread,
            'pe_spread': pe_spread,
            'composite': composite,
            'has_pe': has_pe
        }
    
    return results


# ============================================================================
# 3. 行业PE极差模块 (基于csi20260515.xls)
# ============================================================================

def load_csi_pe_data(xls_path='data/csi20260515.xls'):
    """加载中证行业PE数据"""
    df = pd.read_excel(xls_path)
    df.columns = ['行业代码', '行业名称', '最新静态市盈率', '股票家数', '亏损家数',
                  '近1月平均静态PE', '近3月平均静态PE', '近6月平均静态PE', '近1年平均静态PE']
    return df


def calc_industry_pe_spread(csi_df=None):
    """
    计算行业间PE极差
    基于中证行业分类，计算每组配对所属行业的PE差异
    """
    if csi_df is None:
        try:
            csi_df = load_csi_pe_data()
        except:
            return None
    
    industry_spreads = []
    
    for pair_key, (high_code, low_code) in PAIR_PE_MAP.items():
        high_row = csi_df[csi_df['行业代码'].astype(str) == high_code]
        low_row = csi_df[csi_df['行业代码'].astype(str) == low_code]
        
        if len(high_row) == 0 or len(low_row) == 0:
            continue
        
        h = high_row.iloc[0]
        l = low_row.iloc[0]
        
        # 行业PE极差
        h_pe = float(h['最新静态市盈率']) if h['最新静态市盈率'] != '-' else None
        l_pe = float(l['最新静态市盈率']) if l['最新静态市盈率'] != '-' else None
        
        if h_pe is None or l_pe is None or l_pe == 0:
            continue
        
        pe_ratio = h_pe / l_pe
        
        # PE历史趋势 (近1月/3月/6月/1年均)
        h_pe_1m = float(h['近1月平均静态PE']) if h['近1月平均静态PE'] != '-' else None
        h_pe_3m = float(h['近3月平均静态PE']) if h['近3月平均静态PE'] != '-' else None
        h_pe_6m = float(h['近6月平均静态PE']) if h['近6月平均静态PE'] != '-' else None
        h_pe_1y = float(h['近1年平均静态PE']) if h['近1年平均静态PE'] != '-' else None
        
        l_pe_1m = float(l['近1月平均静态PE']) if l['近1月平均静态PE'] != '-' else None
        l_pe_3m = float(l['近3月平均静态PE']) if l['近3月平均静态PE'] != '-' else None
        l_pe_6m = float(l['近6月平均静态PE']) if l['近6月平均静态PE'] != '-' else None
        l_pe_1y = float(l['近1年平均静态PE']) if l['近1年平均静态PE'] != '-' else None
        
        # PE比值历史趋势
        pe_ratio_1m = h_pe_1m / l_pe_1m if (h_pe_1m and l_pe_1m and l_pe_1m != 0) else None
        pe_ratio_3m = h_pe_3m / l_pe_3m if (h_pe_3m and l_pe_3m and l_pe_3m != 0) else None
        pe_ratio_6m = h_pe_6m / l_pe_6m if (h_pe_6m and l_pe_6m and l_pe_6m != 0) else None
        pe_ratio_1y = h_pe_1y / l_pe_1y if (h_pe_1y and l_pe_1y and l_pe_1y != 0) else None
        
        # PE加速度 (PE比值的变化率)
        pe_accel = None
        if pe_ratio_1m and pe_ratio_1y and pe_ratio_1y != 0:
            pe_accel = (pe_ratio_1m - pe_ratio_1y) / pe_ratio_1y * 100
        
        industry_spreads.append({
            'pair': pair_key,
            'high_industry': h['行业名称'],
            'low_industry': l['行业名称'],
            'high_pe': h_pe,
            'low_pe': l_pe,
            'pe_ratio': round(pe_ratio, 2),
            'pe_ratio_1m': round(pe_ratio_1m, 2) if pe_ratio_1m else None,
            'pe_ratio_3m': round(pe_ratio_3m, 2) if pe_ratio_3m else None,
            'pe_ratio_6m': round(pe_ratio_6m, 2) if pe_ratio_6m else None,
            'pe_ratio_1y': round(pe_ratio_1y, 2) if pe_ratio_1y else None,
            'pe_accel_pct': round(pe_accel, 2) if pe_accel else None,
        })
    
    return industry_spreads


# ============================================================================
# 4. 预警信号模块 (增强版)
# ============================================================================

def generate_alerts(results, df, pairs_config, industry_pe_spreads=None):
    """生成当前预警信号报告 (含PE维度)"""
    alerts = []
    
    # 构建行业PE查找字典
    ind_pe_dict = {}
    if industry_pe_spreads:
        for item in industry_pe_spreads:
            ind_pe_dict[item['pair']] = item
    
    for i, (high_name, high_code, low_name, low_code) in enumerate(pairs_config):
        pair_key = f'{high_name} vs {low_name}'
        data = results[pair_key]
        price_data = data['price_spread']
        pe_data = data['pe_spread']
        comp_data = data['composite']
        
        # === 价格极差预警 ===
        current_spread = price_data['spread'].iloc[-1]
        current_upper = price_data['upper_band'].iloc[-1]
        current_lower = price_data['lower_band'].iloc[-1]
        price_signal = int(price_data['signal'].iloc[-1])
        
        high_col = f'{high_name}_{high_code}'
        low_col = f'{low_name}_{low_code}'
        high_price = df[high_col].iloc[-1]
        low_price = df[low_col].iloc[-1]
        
        deviation = ((current_spread - current_upper) / abs(current_upper) * 100 
                     if current_upper != 0 else 0)
        
        # === PE极差预警 ===
        pe_info = {}
        if pe_data is not None:
            current_pe_spread = pe_data['pe_spread'].iloc[-1]
            pe_high_val = pe_data['pe_high'].iloc[-1]
            pe_low_val = pe_data['pe_low'].iloc[-1]
            pe_ratio_val = pe_data['pe_ratio'].iloc[-1]
            pe_signal = int(pe_data['pe_signal'].iloc[-1])
            
            pe_info = {
                'pe_high': round(float(pe_high_val), 2),
                'pe_low': round(float(pe_low_val), 2),
                'pe_ratio': round(float(pe_ratio_val), 2),
                'pe_spread': round(float(current_pe_spread), 4),
                'pe_signal': pe_signal,
            }
        
        # === 综合极差预警 ===
        comp_info = {}
        if comp_data is not None:
            current_comp = comp_data['composite'].iloc[-1]
            comp_upper = comp_data['composite_upper'].iloc[-1]
            comp_signal = int(comp_data['composite_signal'].iloc[-1])
            
            comp_info = {
                'composite_spread': round(float(current_comp), 4),
                'composite_signal': comp_signal,
            }
        
        # === 行业PE极差 ===
        industry_info = {}
        if pair_key in ind_pe_dict:
            ind = ind_pe_dict[pair_key]
            industry_info = {
                'high_industry': ind['high_industry'],
                'low_industry': ind['low_industry'],
                'industry_pe_ratio': ind['pe_ratio'],
                'industry_pe_ratio_1y': ind['pe_ratio_1y'],
                'industry_pe_accel': ind['pe_accel_pct'],
            }
        
        # === 综合预警等级 ===
        signal_count = 0
        if price_signal == 1:
            signal_count += 1
        if pe_info.get('pe_signal') == 1:
            signal_count += 1
        if comp_info.get('composite_signal') == 1:
            signal_count += 1
        
        if signal_count >= 2:
            alert_level = '极端偏高'
            advice = f'{high_name}价格+估值双重过热，{low_name}超跌反弹概率极高'
        elif price_signal == 1 or (pe_info.get('pe_signal') == 1):
            alert_level = '偏高预警'
            advice = f'{high_name}出现过热信号，关注{low_name}均值回归机会'
        elif signal_count <= -2:
            alert_level = '极端偏低'
            advice = f'{low_name}价格+估值双低，{high_name}回调风险大'
        elif price_signal == -1 or (pe_info.get('pe_signal') == -1):
            alert_level = '偏低预警'
            advice = f'极差偏低，{low_name}可能相对走强'
        else:
            alert_level = '正常区间'
            advice = '价格与估值极差均处于正常范围'
        
        # 历史极值百分位
        spread_max = price_data['spread'].max()
        spread_min = price_data['spread'].min()
        spread_pct = (current_spread - spread_min) / (spread_max - spread_min) * 100 if spread_max != spread_min else 50
        
        alerts.append({
            'pair': pair_key,
            'high_stock': f'{high_name}({high_code})',
            'low_stock': f'{low_name}({low_code})',
            'high_price': round(float(high_price), 2),
            'low_price': round(float(low_price), 2),
            'current_spread': round(float(current_spread), 4),
            'deviation_pct': round(float(deviation), 2),
            'spread_pct': round(float(spread_pct), 2),
            'price_signal': price_signal,
            'alert_level': alert_level,
            'advice': advice,
            **pe_info,
            **comp_info,
            **industry_info
        })
    
    return alerts


# ============================================================================
# 5. 统计分析模块
# ============================================================================

def statistical_analysis(results, pairs_config):
    """极差统计特征分析"""
    stats = []
    for high_name, high_code, low_name, low_code in pairs_config:
        pair_key = f'{high_name} vs {low_name}'
        spread = results[pair_key]['price_spread']['spread'].dropna()
        
        mean_val = spread.mean()
        std_val = spread.std()
        skew_val = spread.skew()
        kurt_val = spread.kurtosis()
        max_val = spread.max()
        min_val = spread.min()
        max_date = spread.idxmax().strftime('%Y-%m-%d')
        min_date = spread.idxmin().strftime('%Y-%m-%d')
        
        # AR(1)半衰期
        deviation = spread - spread.rolling(60).mean()
        deviation = deviation.dropna()
        if len(deviation) > 2:
            ar_coef = np.corrcoef(deviation.iloc[1:], deviation.iloc[:-1])[0, 1]
            half_life = -np.log(2) / np.log(ar_coef) if 0 < ar_coef < 1 else np.nan
        else:
            half_life = np.nan
        
        cross_count = int(((spread.iloc[1:].values * spread.iloc[:-1].values) < 0).sum())
        
        # PE极差统计
        pe_stats = {}
        pe_data = results[pair_key]['pe_spread']
        if pe_data is not None:
            pe_spread = pe_data['pe_spread'].dropna()
            if len(pe_spread) > 0:
                pe_stats = {
                    'pe_spread_mean': round(float(pe_spread.mean()), 4),
                    'pe_spread_std': round(float(pe_spread.std()), 4),
                    'pe_spread_current': round(float(pe_spread.iloc[-1]), 4),
                }
        
        stats.append({
            'pair': pair_key,
            'mean': round(float(mean_val), 4),
            'std': round(float(std_val), 4),
            'skewness': round(float(skew_val), 4),
            'kurtosis': round(float(kurt_val), 4),
            'max': round(float(max_val), 4),
            'min': round(float(min_val), 4),
            'max_date': max_date,
            'min_date': min_date,
            'half_life_days': round(float(half_life), 1) if not np.isnan(half_life) else None,
            'cross_mean_count': cross_count,
            'current_pct': round(float((spread.iloc[-1] - min_val) / (max_val - min_val) * 100), 2) if max_val != min_val else 50,
            **pe_stats
        })
    
    return pd.DataFrame(stats)


# ============================================================================
# 6. 输出模块
# ============================================================================

def export_for_dashboard(df, results, alerts, stats_df, industry_pe_spreads, output_dir='output'):
    """导出仪表板所需的JSON数据"""
    os.makedirs(output_dir, exist_ok=True)
    
    data = {
        'dates': [d.strftime('%Y-%m-%d') for d in df.index],
        'pairs': [],
        'alerts': alerts,
        'stats': json.loads(stats_df.to_json(orient='records', force_ascii=False)),
        'industry_pe': industry_pe_spreads if industry_pe_spreads else []
    }
    
    # 价格数据
    for col in df.columns:
        if not col.endswith('_PE') and not col.endswith('_TR'):
            data[f'price_{col}'] = [round(float(x), 2) if pd.notna(x) else None for x in df[col].tolist()]
    
    # PE数据
    for col in df.columns:
        if col.endswith('_PE'):
            data[f'{col}'] = [round(float(x), 2) if pd.notna(x) else None for x in df[col].tolist()]
    
    # 每组配对的极差数据
    for pair_key, data_dict in results.items():
        price_spread = data_dict['price_spread']
        pe_spread = data_dict['pe_spread']
        composite = data_dict['composite']
        
        pair_data = {
            'name': pair_key,
            'spread': [round(float(x), 4) if pd.notna(x) else None for x in price_spread['spread'].tolist()],
            'spread_mean': [round(float(x), 4) if pd.notna(x) else None for x in price_spread['spread_mean'].tolist()],
            'upper_band': [round(float(x), 4) if pd.notna(x) else None for x in price_spread['upper_band'].tolist()],
            'lower_band': [round(float(x), 4) if pd.notna(x) else None for x in price_spread['lower_band'].tolist()],
            'signal': [int(x) for x in price_spread['signal'].tolist()],
            'z_high': [round(float(x), 4) if pd.notna(x) else None for x in price_spread['z_high'].tolist()],
            'z_low': [round(float(x), 4) if pd.notna(x) else None for x in price_spread['z_low'].tolist()],
        }
        
        # PE极差数据
        if pe_spread is not None:
            pair_data['pe_spread'] = [round(float(x), 4) if pd.notna(x) else None for x in pe_spread['pe_spread'].tolist()]
            pair_data['pe_spread_mean'] = [round(float(x), 4) if pd.notna(x) else None for x in pe_spread['pe_spread_mean'].tolist()]
            pair_data['pe_upper'] = [round(float(x), 4) if pd.notna(x) else None for x in pe_spread['pe_upper'].tolist()]
            pair_data['pe_lower'] = [round(float(x), 4) if pd.notna(x) else None for x in pe_spread['pe_lower'].tolist()]
            pair_data['pe_signal'] = [int(x) for x in pe_spread['pe_signal'].tolist()]
            pair_data['pe_ratio'] = [round(float(x), 4) if pd.notna(x) else None for x in pe_spread['pe_ratio'].tolist()]
        
        # 综合极差数据
        if composite is not None:
            pair_data['composite'] = [round(float(x), 4) if pd.notna(x) else None for x in composite['composite'].tolist()]
            pair_data['composite_mean'] = [round(float(x), 4) if pd.notna(x) else None for x in composite['composite_mean'].tolist()]
            pair_data['composite_upper'] = [round(float(x), 4) if pd.notna(x) else None for x in composite['composite_upper'].tolist()]
            pair_data['composite_lower'] = [round(float(x), 4) if pd.notna(x) else None for x in composite['composite_lower'].tolist()]
            pair_data['composite_signal'] = [int(x) for x in composite['composite_signal'].tolist()]
        
        data['pairs'].append(pair_data)
    
    with open(f'{output_dir}/dashboard_data.json', 'w', encoding='utf-8') as f:
        json.dump(data, f, ensure_ascii=False)
    
    # 也导出CSV
    df.to_csv(f'{output_dir}/price_data.csv')
    stats_df.to_csv(f'{output_dir}/stats.csv', index=False)
    
    print(f"  数据已导出至 {output_dir}/ 目录")


# ============================================================================
# 7. 主程序
# ============================================================================

def main():
    print("=" * 70)
    print("  极差市场预警系统 v2.0 — Python版")
    print("  Range Spread Market Early Warning System")
    print("  (价格极差 + PE极差 + 综合极差)")
    print("=" * 70)
    
    # 获取数据
    print("\n[1/7] 从Tushare获取真实行情+PE数据...")
    df, pairs_config = generate_market_data()
    print(f"  数据范围: {df.index[0].strftime('%Y-%m-%d')} ~ {df.index[-1].strftime('%Y-%m-%d')}")
    print(f"  交易日数: {len(df)} | 数据列数: {len(df.columns)}")
    
    # 加载行业PE数据
    print("\n[2/7] 加载中证行业PE数据...")
    try:
        csi_df = load_csi_pe_data()
        industry_pe_spreads = calc_industry_pe_spread(csi_df)
        print(f"  行业PE数据: {len(csi_df)}个行业")
        if industry_pe_spreads:
            for item in industry_pe_spreads:
                print(f"    {item['pair']}: 行业PE比={item['pe_ratio']} | PE加速度={item['pe_accel_pct']}%")
    except Exception as e:
        print(f"  ⚠️ 无法加载行业PE数据: {e}")
        industry_pe_spreads = None
    
    # 计算极差
    print("\n[3/7] 计算极差指标 (价格+PE+综合)...")
    results = calc_all_pairs(df, pairs_config, window=60)
    for pair_key, data_dict in results.items():
        price_spread = data_dict['price_spread']
        pe_spread = data_dict['pe_spread']
        composite = data_dict['composite']
        
        cs = price_spread['spread'].iloc[-1]
        ps = pe_spread['pe_spread'].iloc[-1] if pe_spread is not None else 'N/A'
        comp = composite['composite'].iloc[-1] if composite is not None else 'N/A'
        print(f"  {pair_key}: 价格极差={cs:.4f} | PE极差={ps if isinstance(ps, str) else f'{ps:.4f}'} | 综合极差={comp if isinstance(comp, str) else f'{comp:.4f}'}")
    
    # 生成预警
    print("\n[4/7] 生成预警信号...")
    alerts = generate_alerts(results, df, pairs_config, industry_pe_spreads)
    for a in alerts:
        emoji = '🔴' if '偏高' in a['alert_level'] else ('🟢' if '偏低' in a['alert_level'] else '⚪')
        pe_str = f" | PE比={a.get('pe_ratio', 'N/A')}" if a.get('pe_ratio') else ""
        ind_str = f" | 行业PE比={a.get('industry_pe_ratio', 'N/A')}" if a.get('industry_pe_ratio') else ""
        print(f"  {emoji} {a['pair']}: {a['alert_level']}{pe_str}{ind_str}")
    
    # 统计分析
    print("\n[5/7] 统计特征分析...")
    stats_df = statistical_analysis(results, pairs_config)
    display_cols = ['pair', 'mean', 'std', 'half_life_days', 'current_pct']
    pe_cols = [c for c in stats_df.columns if 'pe_spread' in c]
    display_cols.extend(pe_cols)
    print(stats_df[display_cols].to_string(index=False))
    
    # 导出
    print("\n[6/7] 导出数据文件...")
    base_dir = os.path.dirname(os.path.abspath(__file__))
    output_dir = os.path.join(base_dir, 'output')
    export_for_dashboard(df, results, alerts, stats_df, industry_pe_spreads, output_dir)
    
    print("\n[7/7] 完成!")
    print("=" * 70)
    print("  运行完成！v2.0: 价格极差 + PE极差 + 综合极差 + 行业PE")
    print("=" * 70)
    
    return df, results, alerts, stats_df


if __name__ == '__main__':
    df, results, alerts, stats_df = main()
