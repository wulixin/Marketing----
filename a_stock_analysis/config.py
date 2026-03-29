#!/usr/bin/env python3

# -*- coding: utf-8 -*-

"""

A股上市公司市值管理与重组可能性分析框架

基于区域、国企/民企、股价、市值等多维度特征


分析维度：

1. 借壳上市可能性

2. 引入战略投资者可能性  

3. 控股权变更可能性

4. 市值管理需求迫切度

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


# 省份到区域的映射

PROVINCE_TO_REGION = {}

for region, provinces in REGION_DEFINITIONS.items():

    for province in provinces:

        PROVINCE_TO_REGION[province] = region


# ============================================================

# 市值分类阈值（亿元）

# ============================================================


MARKET_CAP_THRESHOLDS = {

    "迷你市值": (0, 30),      # 0-30亿

    "小市值": (30, 100),      # 30-100亿

    "中市值": (100, 500),     # 100-500亿

    "大市值": (500, float('inf'))  # 500亿以上

}


def classify_market_cap(market_cap):

    """根据市值大小分类"""

    if pd.isna(market_cap):

        return "未知"

    for label, (low, high) in MARKET_CAP_THRESHOLDS.items():

        if low <= market_cap < high:

            return label

    return "未知"


# ============================================================

# 核心分析规则

# ============================================================


def analyze_shell_listing_probability(row):

    """

    借壳上市可能性分析

    

    高概率条件组合：

    - 位于大西北 + 股价<10元 + (小市值或迷你市值) + 民企

    - 位于大东北 + 类似条件

    - 位于云贵川 + 类似条件

    """

    score = 0

    reasons = []

    

    province = row.get('province', '')

    region = PROVINCE_TO_REGION.get(province, '其他')

    price = row.get('price', 0)

    market_cap_class = row.get('market_cap_class', '')

    is_soe = row.get('is_soe', False)

    is_st = row.get('is_st', False)

    

    # 区域加分（边远地区重组需求更强）

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

    

    # 市值加分（小市值更容易被借壳）

    if market_cap_class in ["迷你市值", "小市值"]:

        score += 3

        reasons.append(f"{market_cap_class}易被借壳")

    

    # 民企加分（民企决策更灵活）

    if not is_soe:

        score += 2

        reasons.append("民企决策灵活")

    

    # ST股加分（重组压力更大）

    if is_st:

        score += 3

        reasons.append("ST股重组压力大")

    

    probability = "高" if score >= 8 else "中" if score >= 5 else "低"

    

    return {

        "score": score,

        "probability": probability,

        "reasons": "；".join(reasons) if reasons else "无明显特征"

    }


def analyze_strategic_investor_probability(row):

    """

    引入战略投资者可能性分析

    

    高概率条件组合：

    - 位于大西北 + 股价>10元 + 大市值 + 国企

    - 央企入驻、产业整合需求

    """

    score = 0

    reasons = []

    

    province = row.get('province', '')

    region = PROVINCE_TO_REGION.get(province, '其他')

    price = row.get('price', 0)

    market_cap_class = row.get('market_cap_class', '')

    is_soe = row.get('is_soe', False)

    

    # 区域加分

    if region in ["大西北", "大东北", "云贵川"]:

        score += 1

        reasons.append(f"位于{region}地区")

    

    # 中高价股加分

    if price >= 10:

        score += 1

        reasons.append(f"股价适中({price:.2f}元)")

    

    # 大中市值加分（战略投资者更关注）

    if market_cap_class in ["大市值", "中市值"]:

        score += 3

        reasons.append(f"{market_cap_class}吸引战略投资")

    

    # 国企加分（引入战略投资者是国企改革方向）

    if is_soe:

        score += 3

        reasons.append("国企混改需求")

    

    probability = "高" if score >= 6 else "中" if score >= 4 else "低"

    

    return {

        "score": score,

        "probability": probability,

        "reasons": "；".join(reasons) if reasons else "无明显特征"

    }


def analyze_control_change_probability(row):

    """

    控股权变更可能性分析

    

    高概率条件：

    - 股权分散 + 小市值 + 经营困难

    - 民企传承问题 + ST压力

    """

    score = 0

    reasons = []

    

    market_cap_class = row.get('market_cap_class', '')

    is_soe = row.get('is_soe', False)

    is_st = row.get('is_st', False)

    price = row.get('price', 0)

    

    # 小市值易发生控权变更

    if market_cap_class in ["迷你市值", "小市值"]:

        score += 2

        reasons.append(f"{market_cap_class}易发生控制权变更")

    

    # 民企更易发生

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

    

    probability = "高" if score >= 5 else "中" if score >= 3 else "低"

    

    return {

        "score": score,

        "probability": probability,

        "reasons": "；".join(reasons) if reasons else "无明显特征"

    }


def analyze_market_management_urgency(row):

    """

    市值管理需求迫切度分析

    

    迫切条件：

    - 股价长期低迷 + 成交量萎缩

    - ST或经营困难

    - 区域发展需求

    """

    score = 0

    reasons = []

    

    province = row.get('province', '')

    region = PROVINCE_TO_REGION.get(province, '其他')

    price = row.get('price', 0)

    market_cap_class = row.get('market_cap_class', '')

    is_st = row.get('is_st', False)

    volume_ratio = row.get('volume_ratio', 1)  # 相对成交量

    

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

    

    urgency = "迫切" if score >= 7 else "较强" if score >= 4 else "一般"

    

    return {

        "score": score,

        "urgency": urgency,

        "reasons": "；".join(reasons) if reasons else "无明显需求"

    }


# ============================================================

# 综合评分函数

# ============================================================


def comprehensive_analysis(df):

    """对整个数据框进行综合分析"""

    

    results = []

    

    for idx, row in df.iterrows():

        # 市值分类

        market_cap = row.get('total_mv', 0)

        if market_cap:

            market_cap_bn = market_cap / 10000  # 转换为亿元

        else:

            market_cap_bn = 0

        

        market_cap_class = classify_market_cap(market_cap_bn)

        

        # 构建分析行

        analysis_row = {

            'ts_code': row.get('ts_code', ''),

            'name': row.get('name', ''),

            'province': row.get('province', ''),

            'region': PROVINCE_TO_REGION.get(row.get('province', ''), '其他'),

            'price': row.get('close', 0) or row.get('price', 0),

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

            '综合得分': shell['score'] + strategic['score'] + control['score'] + management['score']

        }

        

        results.append(result)

    

    return pd.DataFrame(results)


# ============================================================

# 输出规则说明文档

# ============================================================


ANALYSIS_RULES = """

# A股上市公司市值管理与重组可能性分析规则


## 一、区域定义


| 区域 | 包含省份 |

|------|----------|

| 大西北 | 新疆、西藏、甘肃、青海、宁夏 |

| 大东北 | 辽宁、吉林、黑龙江 |

| 云贵川 | 云南、贵州、广西、四川 |


## 二、市值分类


| 分类 | 市值范围（亿元） |

|------|------------------|

| 迷你市值 | 0-30亿 |

| 小市值 | 30-100亿 |

| 中市值 | 100-500亿 |

| 大市值 | 500亿以上 |


## 三、分析维度规则


### 3.1 借壳上市可能性


**高概率条件组合**：

- 位于大西北/大东北/云贵川 + 股价<10元 + 小市值/迷你市值 + 民企

- ST股 + 低价 + 小市值


**评分规则**：

- 边远地区 +2分

- 低价股（<10元）+2分，中低价（<20元）+1分

- 小市值/迷你市值 +3分

- 民企 +2分

- ST股 +3分

- ≥8分高概率，≥5分中概率，否则低概率


### 3.2 引入战略投资者可能性


**高概率条件组合**：

- 位于大西北 + 股价>10元 + 大市值 + 国企

- 央企入驻、产业整合需求


**评分规则**：

- 边远地区 +1分

- 股价≥10元 +1分

- 大中市值 +3分

- 国企 +3分

- ≥6分高概率，≥4分中概率，否则低概率


### 3.3 控股权变更可能性


**高概率条件**：

- 股权分散 + 小市值 + 经营困难

- 民企传承问题 + ST压力


**评分规则**：

- 小市值 +2分

- 民企 +1分

- ST股 +3分

- 低价股 +1分

- ≥5分高概率，≥3分中概率，否则低概率


### 3.4 市值管理需求迫切度


**迫切条件**：

- 股价长期低迷 + 成交量萎缩

- ST或经营困难

- 区域发展需求


**评分规则**：

- 边远地区 +1分

- 超低价（<5元）+3分，低价（<10元）+2分

- 小市值 +2分

- ST股 +3分

- 成交量萎缩 +1分

- ≥7分迫切，≥4分较强，否则一般


## 四、典型场景案例


### 场景1：大西北民企小市值

- 公司位于新疆，股价8元，市值25亿，民企

- 借壳上市概率：**高**（区域+低价+小市值+民企）

- 市值管理迫切度：**迫切**


### 场景2：大西北国企大市值

- 公司位于甘肃，股价15元，市值600亿，国企

- 战略投资者概率：**高**（区域+大市值+国企）

- 央企入驻可能性大


### 场景3：东北ST小市值

- 公司位于辽宁，ST股，股价4元，市值20亿

- 控股权变更概率：**高**（ST+低价+小市值）

- 借壳上市概率：**高**

"""


if __name__ == "__main__":

    print("A股上市公司市值管理分析框架已加载")

    print(ANALYSIS_RULES)