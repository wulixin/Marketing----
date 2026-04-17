"""
每日板块信息 Dashboard 更新脚本
用法: python3 update_dashboard.py
功能: 读取同目录下的 每日板块信息.xlsx，重新生成 每日板块信息_Dashboard.html
"""
import json
import os
import re
import sys

import pandas as pd

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    excel_path = os.path.join(script_dir, '每日板块信息.xlsx')

    if not os.path.exists(excel_path):
        print(f"❌ 找不到数据文件: {excel_path}")
        print("   请确保 每日板块信息.xlsx 与本脚本在同一目录下")
        sys.exit(1)

    # 读取数据
    xlsx = pd.ExcelFile(excel_path)
    df_index = pd.read_excel(xlsx, sheet_name='指数表现')
    df_valuation = pd.read_excel(xlsx, sheet_name='指数估值').rename(columns={'指数名称）': '指数名称'})
    df_contrib = pd.read_excel(xlsx, sheet_name='指数贡献')
    df_sector = pd.read_excel(xlsx, sheet_name='每日板块信息')

    # 提取最强/最弱表现数据
    if '指数名称' in df_sector.columns:
        df_best = df_sector[df_sector['指数名称'] == '沪市最强表现'].head(10)
        df_worst = df_sector[df_sector['指数名称'] == '沪市最弱表现'].head(10)
    else:
        df_best, df_worst = pd.DataFrame(), pd.DataFrame()

    data_json = {
        'index': df_index.to_dict(orient='records'),
        'valuation': df_valuation.to_dict(orient='records'),
        'contrib': df_contrib.to_dict(orient='records'),
        'sector': df_sector.to_dict(orient='records'),
        'best': df_best.to_dict(orient='records'),
        'worst': df_worst.to_dict(orient='records'),
    }
    data_str = json.dumps(data_json, ensure_ascii=False, default=str)

    # HTML 模板
    html_template = os.path.join(script_dir, '_dashboard_template.html')
    output_path = os.path.join(script_dir, '每日板块信息_Dashboard.html')

    # 读取模板或直接生成
    template = None
    if os.path.exists(html_template):
        with open(html_template, 'r', encoding='utf-8') as f:
            template = f.read()

    if template and '{DATA_PLACEHOLDER}' in template:
        html = template.replace('{DATA_PLACEHOLDER}', data_str)
    else:
        # 无模板时，读取已有的 HTML 并替换数据
        if os.path.exists(output_path):
            with open(output_path, 'r', encoding='utf-8') as f:
                existing = f.read()
            # 替换 const DATA = ... 部分
            # 使用更健壮的正则：匹配从 "const DATA = {" 到对应的结束分号
            pattern = r'(const DATA = )\{.*?\};'
            replacement = rf'\1{data_str};'
            html = re.sub(pattern, replacement, existing, flags=re.DOTALL)
        else:
            print("❌ 找不到模板文件，请先运行生成脚本创建初始 Dashboard")
            sys.exit(1)

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(html)

    print(f"✅ Dashboard 已更新: {output_path}")
    print(f"   数据来源: {excel_path}")
    print(f"   指数数量: {len(df_index)}, 板块数量: {len(df_sector)}")

if __name__ == '__main__':
    main()
