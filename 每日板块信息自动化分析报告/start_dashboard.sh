#!/bin/bash

# 每日板块信息可视化面板启动脚本


cd "$(dirname "$0")"


echo "======================================"

echo "  每日板块信息可视化面板"

echo "======================================"

echo ""

echo "正在启动 Streamlit 服务..."

echo ""

echo "使用说明："

echo "  - 面板将在浏览器中自动打开"

echo "  - 如未自动打开，请点击终端中的链接"

echo "  - 按 Ctrl+C 停止服务"

echo ""


streamlit run dashboard.py --server.headless=true