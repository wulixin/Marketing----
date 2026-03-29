#!/usr/bin/env python3

# -*- coding: utf-8 -*-

"""

A股上市公司市值管理与重组可能性分析框架 V2.0

增加市盈率(PE)和行业细分维度


分析维度：

1. 借壳上市可能性

2. 引入战略投资者可能性  

3. 控股权变更可能性

4. 市值管理需求迫切度

5. 新增：行业景气度与估值分析

"""


import pandas as pd

import numpy as np

from datetime import datetime

import warnings

warnings.filterwarnings('ignore')


# ============================================================

# 区域定义

# ============================================================


REGION_DEFINITIONS = {

    "大西北": ["新疆", "西藏", "甘肃", "青海", "宁夏"],

    "大东北": ["辽宁", "吉林", "黑龙江"],

    "云贵川": ["云南", "贵州", "广西", "四川"]

}


PROVINCE_TO_REGION = {}

for region, provinces in REGION_DEFINITIONS.items():

    for province in provinces:

        PROVINCE_TO_REGION[province] = region


# ============================================================

# 市值分类阈值（亿元）

# ============================================================


MARKET_CAP_THRESHOLDS = {

    "迷你市值": (0, 30),

    "小市值": (30, 100),

    "中市值": (100, 500),

    "大市值": (500, float('inf'))

}


def classify_market_cap(market_cap):

    if pd.isna(market_cap):

        return "未知"

    for label, (low, high) in MARKET_CAP_THRESHOLDS.items():

        if low <= market_cap < high:

            return label

    return "未知"


# ============================================================

# 市盈率分类与解读

# ============================================================


def classify_pe(pe, industry_pe=None):

    """

    市盈率分类与投资价值解读

    

    PE解读逻辑：

    - PE < 0: 亏损，需要特殊处理

    - PE 0-15: 低估值，可能是价值股或夕阳行业

    - PE 15-30: 合理估值

    - PE 30-50: 高估值，成长预期

    - PE > 50: 超高估值或概念炒作

    """

    if pd.isna(pe) or pe == '-':

        return "无数据", "缺少市盈率数据"

    

    try:

        pe = float(pe)

    except:

        return "无数据", "市盈率数据异常"

    

    if pe < 0:

        return "亏损", "公司亏损，重组压力大"

    elif pe == 0:

        return "零估值", "EPS接近零，风险较高"

    elif pe < 10:

        return "深度低估", "估值极低，可能是周期底部或夕阳行业"

    elif pe < 20:

        return "低估值", "估值偏低，价值投资标的"

    elif pe < 30:

        return "合理估值", "估值处于合理区间"

    elif pe < 50:

        return "高估值", "市场给予成长溢价"

    elif pe < 100:

        return "超高估值", "高成长预期或概念炒作"

    else:

        return "极度高估", "估值泡沫或概念炒作严重"


def pe_relative_analysis(pe, industry_pe):

    """

    相对市盈率分析

    比较公司PE与行业平均PE的关系

    """

    if pd.isna(pe) or pe == '-' or pd.isna(industry_pe) or industry_pe == '-':

        return "无法比较"

    

    try:

        pe = float(pe)

        industry_pe = float(industry_pe)

    except:

        return "无法比较"

    

    if industry_pe <= 0:

        return "行业亏损无法比较"

    

    ratio = pe / industry_pe

    

    if ratio < 0.7:

        return "显著低于行业，可能被低估"

    elif ratio < 0.9:

        return "略低于行业"

    elif ratio < 1.1:

        return "与行业持平"

    elif ratio < 1.3:

        return "略高于行业"

    else:

        return "显著高于行业，估值偏高"


# ============================================================

# 行业分类与特征定义（基于中证行业分类）

# ============================================================


INDUSTRY_CHARACTERISTICS = {

    # 一级行业

    "能源": {"cycle": "周期", "重组": "中等", "国企占比": "高", "备注": "油价波动大，国企改革重点"},

    "原材料": {"cycle": "周期", "重组": "高", "国企占比": "中", "备注": "产能过剩行业，重组需求强"},

    "工业": {"cycle": "混合", "重组": "高", "国企占比": "高", "备注": "国企改革重点领域"},

    "可选消费": {"cycle": "非周期", "重组": "中", "国企占比": "低", "备注": "市场化程度高"},

    "主要消费": {"cycle": "非周期", "重组": "低", "国企占比": "低", "备注": "防御性较强"},

    "医药卫生": {"cycle": "非周期", "重组": "高", "国企占比": "低", "备注": "集采压力下整合加速"},

    "金融": {"cycle": "周期", "重组": "中", "国企占比": "高", "备注": "监管严格，并购受限"},

    "信息技术": {"cycle": "成长", "重组": "高", "国企占比": "低", "备注": "高估值高成长，并购活跃"},

    "通信服务": {"cycle": "成长", "重组": "中", "国企占比": "中", "备注": "电信改革持续"},

    "公用事业": {"cycle": "防御", "重组": "中", "国企占比": "高", "备注": "国企改革重点"},

    "房地产": {"cycle": "周期", "重组": "极高", "国企占比": "中", "备注": "行业深度调整，重组并购高发"},

    

    # 重点二级行业细分

    "航空航天与国防": {"cycle": "成长", "重组": "高", "国企占比": "极高", "备注": "军工改革重点"},

    "建筑装饰": {"cycle": "周期", "重组": "高", "国企占比": "高", "备注": "基建投资相关，国企集中"},

    "电力设备": {"cycle": "成长", "重组": "高", "国企占比": "中", "备注": "新能源赛道，并购活跃"},

    "机械制造": {"cycle": "周期", "重组": "中", "国企占比": "中", "备注": "传统制造业转型"},

    "环保": {"cycle": "成长", "重组": "高", "国企占比": "中", "备注": "政策驱动，并购整合"},

    "半导体": {"cycle": "成长", "重组": "极高", "国企占比": "中", "备注": "国产替代主线，并购重组频繁"},

    "软件开发": {"cycle": "成长", "重组": "高", "国企占比": "低", "备注": "高估值科技股"},

    "房地产开发": {"cycle": "周期", "重组": "极高", "国企占比": "中", "备注": "行业出清，央国企并购民企"},

}


def get_industry_feature(industry_name):

    """获取行业特征"""

    # 精确匹配

    if industry_name in INDUSTRY_CHARACTERISTICS:

        return INDUSTRY_CHARACTERISTICS[industry_name]

    

    # 模糊匹配

    for key, value in INDUSTRY_CHARACTERISTICS.items():

        if key in industry_name or industry_name in key:

            return value

    

    return {"cycle": "未知", "重组": "中", "国企占比": "未知", "备注": ""}


# ============================================================

# 行业PE基准（从用户数据提取）

# ============================================================


INDUSTRY_PE_BENCHMARK = {

    # 一级行业PE基准（最新静态市盈率）

    "能源": 14.85,

    "原材料": 32.96,

    "工业": 27.30,

    "可选消费": 23.09,

    "主要消费": 20.34,

    "医药卫生": 28.62,

    "金融": 8.41,

    "信息技术": 62.52,

    "通信服务": 44.53,

    "公用事业": 21.11,

    "房地产": 25.49,

    

    # 重点细分行业

    "煤炭": 14.80,

    "钢铁": 28.44,

    "有色金属": 32.85,

    "化工": 33.62,

    "航空航天与国防": 77.81,

    "建筑装饰": 10.19,

    "电力设备": 41.94,

    "机械制造": 37.94,

    "环保": 29.23,

    "半导体": 99.47,

    "软件开发": 85.05,

    "银行": 6.96,

    "证券": 21.54,

    "房地产开发": 23.54,

    "白酒": 16.80,

    "中药": 21.79,

}


def get_industry_pe(industry_name):

    """获取行业PE基准"""

    if industry_name in INDUSTRY_PE_BENCHMARK:

        return INDUSTRY_PE_BENCHMARK[industry_name]

    

    for key, value in INDUSTRY_PE_BENCHMARK.items():

        if key in industry_name or industry_name in key:

            return value

    

    return None


# ============================================================

# 核心分析规则（增强版）

# ============================================================


def analyze_shell_listing_probability(row):

    """

    借壳上市可能性分析 V2.0

    新增：PE因素、行业因素

    """

    score = 0

    reasons = []

    

    province = row.get('province', '')

    region = PROVINCE_TO_REGION.get(province, '其他')

    price = row.get('price', 0)

    market_cap_class = row.get('market_cap_class', '')

    is_soe = row.get('is_soe', False)

    is_st = row.get('is_st', False)

    pe = row.get('pe', None)

    industry = row.get('industry', '')

    

    # 区域加分

    if region in ["大西北", "大东北", "云贵川"]:

        score += 2

        reasons.append(f"位于{region}地区")

    

    # 低价股加分

    if price > 0 and price < 10:

        score += 2

        reasons.append(f"低价股({price:.2f}元)")

    elif price > 0 and price < 20:

        score += 1

        reasons.append(f"中低价股({price:.2f}元)")

    

    # 市值加分

    if market_cap_class in ["迷你市值", "小市值"]:

        score += 3

        reasons.append(f"{market_cap_class}易被借壳")

    

    # 民企加分

    if not is_soe:

        score += 2

        reasons.append("民企决策灵活")

    

    # ST股加分

    if is_st:

        score += 3

        reasons.append("ST股重组压力大")

    

    # ========== 新增：PE因素 ==========

    pe_class, pe_reason = classify_pe(pe)

    if pe_class == "亏损":

        score += 2

        reasons.append("亏损企业重组需求强")

    elif pe_class == "深度低估":

        score += 1

        reasons.append("估值极低具备壳价值")

    

    # ========== 新增：行业因素 ==========

    industry_feature = get_industry_feature(industry)

    if industry_feature.get("重组") == "极高":

        score += 2

        reasons.append(f"{industry}行业重组高发")

    elif industry_feature.get("重组") == "高":

        score += 1

        reasons.append(f"{industry}行业整合需求强")

    

    probability = "高" if score >= 10 else "中" if score >= 6 else "低"

    

    return {

        "score": score,

        "probability": probability,

        "reasons": "；".join(reasons) if reasons else "无明显特征"

    }


def analyze_strategic_investor_probability(row):

    """

    引入战略投资者可能性分析 V2.0

    新增：PE因素、行业因素

    """

    score = 0

    reasons = []

    

    province = row.get('province', '')

    region = PROVINCE_TO_REGION.get(province, '其他')

    price = row.get('price', 0)

    market_cap_class = row.get('market_cap_class', '')

    is_soe = row.get('is_soe', False)

    pe = row.get('pe', None)

    industry = row.get('industry', '')

    

    # 区域加分

    if region in ["大西北", "大东北", "云贵川"]:

        score += 1

        reasons.append(f"位于{region}地区")

    

    # 中高价股加分

    if price >= 10:

        score += 1

        reasons.append(f"股价适中({price:.2f}元)")

    

    # 大中市值加分

    if market_cap_class in ["大市值", "中市值"]:

        score += 3

        reasons.append(f"{market_cap_class}吸引战略投资")

    

    # 国企加分

    if is_soe:

        score += 3

        reasons.append("国企混改需求")

    

    # ========== 新增：PE因素 ==========

    industry_pe = get_industry_pe(industry)

    pe_class, _ = classify_pe(pe)

    pe_relative = pe_relative_analysis(pe, industry_pe)

    

    # 估值合理或偏低更吸引战略投资者

    if pe_class in ["低估值", "深度低估"]:

        score += 2

        reasons.append("估值具备吸引力")

    elif pe_class == "合理估值":

        score += 1

        reasons.append("估值合理")

    

    # 相对估值分析

    if "低于行业" in pe_relative:

        score += 1

        reasons.append("相对行业低估")

    

    # ========== 新增：行业因素 ==========

    industry_feature = get_industry_feature(industry)

    if industry_feature.get("国企占比") in ["高", "极高"]:

        score += 1

        reasons.append(f"{industry}国企改革重点")

    if industry_feature.get("cycle") == "周期":

        score += 1

        reasons.append("周期行业整合需求")

    

    probability = "高" if score >= 7 else "中" if score >= 5 else "低"

    

    return {

        "score": score,

        "probability": probability,

        "reasons": "；".join(reasons) if reasons else "无明显特征"

    }


def analyze_control_change_probability(row):

    """

    控股权变更可能性分析 V2.0

    """

    score = 0

    reasons = []

    

    market_cap_class = row.get('market_cap_class', '')

    is_soe = row.get('is_soe', False)

    is_st = row.get('is_st', False)

    price = row.get('price', 0)

    pe = row.get('pe', None)

    industry = row.get('industry', '')

    

    # 小市值

    if market_cap_class in ["迷你市值", "小市值"]:

        score += 2

        reasons.append(f"{market_cap_class}易发生控制权变更")

    

    # 民企

    if not is_soe:

        score += 1

        reasons.append("民企控制权更易变更")

    

    # ST压力

    if is_st:

        score += 3

        reasons.append("ST股保壳压力大")

    

    # 低价股

    if price > 0 and price < 10:

        score += 1

        reasons.append("低价股收购成本低")

    

    # ========== 新增：PE因素 ==========

    pe_class, _ = classify_pe(pe)

    if pe_class == "亏损":

        score += 2

        reasons.append("亏损企业控制权易变更")

    

    # ========== 新增：行业因素 ==========

    industry_feature = get_industry_feature(industry)

    if industry_feature.get("重组") in ["极高", "高"]:

        score += 1

        reasons.append(f"{industry}行业整合活跃")

    

    probability = "高" if score >= 6 else "中" if score >= 4 else "低"

    

    return {

        "score": score,

        "probability": probability,

        "reasons": "；".join(reasons) if reasons else "无明显特征"

    }


def analyze_market_management_urgency(row):

    """

    市值管理需求迫切度分析 V2.0

    """

    score = 0

    reasons = []

    

    province = row.get('province', '')

    region = PROVINCE_TO_REGION.get(province, '其他')

    price = row.get('price', 0)

    market_cap_class = row.get('market_cap_class', '')

    is_st = row.get('is_st', False)

    volume_ratio = row.get('volume_ratio', 1)

    pe = row.get('pe', None)

    industry = row.get('industry', '')

    

    # 区域需求

    if region in ["大西北", "大东北", "云贵川"]:

        score += 1

        reasons.append(f"{region}地区发展需求")

    

    # 低价股

    if price > 0 and price < 5:

        score += 3

        reasons.append("超低价股急需市值管理")

    elif price > 0 and price < 10:

        score += 2

        reasons.append("低价股市值管理需求强")

    

    # 小市值

    if market_cap_class in ["迷你市值", "小市值"]:

        score += 2

        reasons.append(f"{market_cap_class}管理需求强")

    

    # ST压力

    if is_st:

        score += 3

        reasons.append("ST股市值管理迫切")

    

    # 成交量萎缩

    if volume_ratio < 0.5:

        score += 1

        reasons.append("成交量萎缩需激活")

    

    # ========== 新增：PE因素 ==========

    pe_class, _ = classify_pe(pe)

    industry_pe = get_industry_pe(industry)

    pe_relative = pe_relative_analysis(pe, industry_pe)

    

    if pe_class == "亏损":

        score += 2

        reasons.append("亏损企业市值管理迫切")

    elif pe_class == "深度低估":

        score += 1

        reasons.append("估值过低需价值发现")

    

    if "显著低于行业" in pe_relative:

        score += 1

        reasons.append("相对低估需价值回归")

    

    # ========== 新增：行业因素 ==========

    industry_feature = get_industry_feature(industry)

    if industry_feature.get("cycle") == "周期":

        score += 1

        reasons.append("周期行业波动大")

    

    urgency = "迫切" if score >= 9 else "较强" if score >= 5 else "一般"

    

    return {

        "score": score,

        "urgency": urgency,

        "reasons": "；".join(reasons) if reasons else "无明显需求"

    }


def analyze_valuation_quality(row):

    """

    新增：估值质量分析

    综合评估公司的估值水平与投资价值

    """

    pe = row.get('pe', None)

    pb = row.get('pb', None)  # 市净率

    industry = row.get('industry', '')

    price = row.get('price', 0)

    

    results = {}

    

    # PE分析

    pe_class, pe_reason = classify_pe(pe)

    results['pe_class'] = pe_class

    results['pe_reason'] = pe_reason

    

    # 相对估值分析

    industry_pe = get_industry_pe(industry)

    results['industry_pe'] = industry_pe

    results['pe_relative'] = pe_relative_analysis(pe, industry_pe)

    

    # 行业特征

    industry_feature = get_industry_feature(industry)

    results['industry_cycle'] = industry_feature.get('cycle', '未知')

    results['industry_reorg'] = industry_feature.get('重组', '中')

    results['industry_note'] = industry_feature.get('备注', '')

    

    # 综合估值评级

    if pe_class in ["深度低估", "低估值"] and "低于行业" in results['pe_relative']:

        results['valuation_rating'] = "显著低估"

        results['investment_suggestion'] = "价值投资标的，具备安全边际"

    elif pe_class in ["合理估值", "低估值"]:

        results['valuation_rating'] = "估值合理"

        results['investment_suggestion'] = "估值处于合理区间"

    elif pe_class in ["高估值", "超高估值"]:

        results['valuation_rating'] = "估值偏高"

        results['investment_suggestion'] = "需要关注成长性验证"

    elif pe_class == "亏损":

        results['valuation_rating'] = "亏损企业"

        results['investment_suggestion'] = "关注扭亏或重组可能性"

    else:

        results['valuation_rating'] = "数据不足"

        results['investment_suggestion'] = "需要更多数据分析"

    

    return results


# ============================================================

# 综合评分函数（增强版）

# ============================================================


def comprehensive_analysis(df):

    """对整个数据框进行综合分析 V2.0"""

    

    results = []

    

    for idx, row in df.iterrows():

        market_cap = row.get('total_mv', 0)

        if market_cap:

            market_cap_bn = market_cap / 10000

        else:

            market_cap_bn = 0

        

        market_cap_class = classify_market_cap(market_cap_bn)

        

        analysis_row = {

            'ts_code': row.get('ts_code', ''),

            'name': row.get('name', ''),

            'province': row.get('province', ''),

            'region': PROVINCE_TO_REGION.get(row.get('province', ''), '其他'),

            'industry': row.get('industry', ''),

            'price': row.get('close', 0) or row.get('price', 0),

            'pe': row.get('pe', None),

            'pb': row.get('pb', None),

            'market_cap_bn': round(market_cap_bn, 2),

            'market_cap_class': market_cap_class,

            'is_soe': row.get('is_soe', False),

            'is_st': 'ST' in str(row.get('name', '')),

            'volume_ratio': row.get('volume_ratio', 1)

        }

        

        # 各维度分析

        shell = analyze_shell_listing_probability(analysis_row)

        strategic = analyze_strategic_investor_probability(analysis_row)

        control = analyze_control_change_probability(analysis_row)

        management = analyze_market_management_urgency(analysis_row)

        valuation = analyze_valuation_quality(analysis_row)

        

        result = {

            **analysis_row,

            '借壳上市概率': shell['probability'],

            '借壳上市得分': shell['score'],

            '借壳上市原因': shell['reasons'],

            '战略投资者概率': strategic['probability'],

            '战略投资者得分': strategic['score'],

            '战略投资者原因': strategic['reasons'],

            '控股权变更概率': control['probability'],

            '控股权变更得分': control['score'],

            '控股权变更原因': control['reasons'],

            '市值管理迫切度': management['urgency'],

            '市值管理得分': management['score'],

            '市值管理原因': management['reasons'],

            # 新增估值分析

            'PE分类': valuation['pe_class'],

            'PE解读': valuation['pe_reason'],

            '行业PE': valuation['industry_pe'],

            '相对估值': valuation['pe_relative'],

            '估值评级': valuation['valuation_rating'],

            '投资建议': valuation['investment_suggestion'],

            '行业周期': valuation['industry_cycle'],

            '行业重组活跃度': valuation['industry_reorg'],

            '综合得分': shell['score'] + strategic['score'] + control['score'] + management['score']

        }

        

        results.append(result)

    

    return pd.DataFrame(results)


# ============================================================

# 输出规则说明文档

# ============================================================


ANALYSIS_RULES_V2 = """

# A股上市公司市值管理与重组可能性分析规则 V2.0


## 一、新增维度：市盈率(PE)分析


### 1.1 PE分类标准


| PE分类 | PE范围 | 解读 |

|--------|--------|------|

| 亏损 | PE < 0 | 公司亏损，重组压力大 |

| 零估值 | PE = 0 | EPS接近零，风险较高 |

| 深度低估 | 0 < PE < 10 | 估值极低，周期底部或夕阳行业 |

| 低估值 | 10 ≤ PE < 20 | 估值偏低，价值投资标的 |

| 合理估值 | 20 ≤ PE < 30 | 估值处于合理区间 |

| 高估值 | 30 ≤ PE < 50 | 市场给予成长溢价 |

| 超高估值 | 50 ≤ PE < 100 | 高成长预期或概念炒作 |

| 极度高估 | PE ≥ 100 | 估值泡沫或概念炒作严重 |


### 1.2 相对PE分析


比较公司PE与行业平均PE：


| 比值范围 | 解读 |

|---------|------|

| PE/行业PE < 0.7 | 显著低于行业，可能被低估 |

| 0.7 ≤ 比值 < 0.9 | 略低于行业 |

| 0.9 ≤ 比值 < 1.1 | 与行业持平 |

| 1.1 ≤ 比值 < 1.3 | 略高于行业 |

| 比值 ≥ 1.3 | 显著高于行业，估值偏高 |


## 二、新增维度：行业细分特征


### 2.1 行业周期属性


| 属性 | 说明 | 代表行业 |

|------|------|---------|

| 周期 | 与经济周期强相关 | 能源、原材料、房地产、金融 |

| 非周期 | 需求稳定 | 主要消费、医药 |

| 成长 | 高增长预期 | 信息技术、半导体、通信 |

| 防御 | 稳定现金流 | 公用事业 |

| 混合 | 兼具周期与成长 | 工业、可选消费 |


### 2.2 行业重组活跃度


| 活跃度 | 说明 | 代表行业 |

|--------|------|---------|

| 极高 | 行业整合加速，并购重组频繁 | 房地产、半导体 |

| 高 | 行业集中度提升，整合需求强 | 原材料、工业、医药、IT |

| 中 | 正常水平 | 消费、通信、公用事业 |

| 低 | 行业稳定，重组较少 | 主要消费 |


### 2.3 行业PE基准（2024年3月）


| 行业 | 静态PE | 特征 |

|------|--------|------|

| 金融 | 8.41 | 最低估值板块 |

| 能源 | 14.85 | 低估值周期股 |

| 主要消费 | 20.34 | 防御性板块 |

| 公用事业 | 21.11 | 稳定现金流 |

| 可选消费 | 23.09 | 合理估值 |

| 房地产 | 25.49 | 困境行业 |

| 医药卫生 | 28.62 | 成长与防御兼备 |

| 工业 | 27.30 | 国企改革重点 |

| 原材料 | 32.96 | 周期板块 |

| 通信服务 | 44.53 | 成长板块 |

| 信息技术 | 62.52 | 高估值成长 |

| 半导体 | 99.47 | 极高估值赛道 |


## 三、规则应用示例


### 场景：某半导体公司分析


假设：

- 行业：半导体

- PE：85倍

- 行业PE：99.47倍

- 市值：80亿（小市值）

- 股价：45元

- 实控人：民企


分析：

1. **PE分类**：超高估值（85倍）

2. **相对估值**：85/99.47=0.85，略低于行业，估值合理

3. **行业特征**：成长行业，重组活跃度极高

4. **借壳概率**：中（行业活跃度高但PE高）

5. **战略投资者概率**：高（成长行业，国企占比中等）

6. **市值管理**：一般（PE高，行业景气）


### 场景：某房地产公司分析


假设：

- 行业：房地产开发

- PE：亏损

- 市值：25亿（迷你市值）

- 股价：3.5元

- 实控人：民企


分析：

1. **PE分类**：亏损企业

2. **行业特征**：周期行业，重组活跃度极高

3. **借壳概率**：高（亏损+迷你市值+行业整合+民企）

4. **战略投资者概率**：中

5. **控股权变更**：高

6. **市值管理**：迫切

"""


if __name__ == "__main__":

    print("A股上市公司市值管理分析框架 V2.0 已加载")

    print(ANALYSIS_RULES_V2)