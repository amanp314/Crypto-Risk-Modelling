# Cryptocurrency Risk Modelling

**Quantitative analysis of cryptocurrency market risk using econometric and machine learning approaches.**

This project evaluates whether traditional financial risk models used in institutional finance remain effective when applied to cryptocurrency markets. The study compares traditional models with alternative approaches to assess their ability to capture the extreme volatility and structural characteristics of digital assets.

The analysis focuses on **Bitcoin and Ethereum**, benchmarked against traditional market indices (**S&P 500 and Dow Jones Industrial Average**) using historical market data from **2020–2024**.

## Key Findings

- Cryptocurrencies exhibit **significantly higher volatility** than traditional equity indices.  
- **Ethereum shows the highest volatility**, while the **S&P 500 remains the most stable**.  
- Traditional risk models such as **VaR frequently underestimate extreme market losses**.  
- **GARCH models capture volatility clustering but struggle with sudden market shocks** common in crypto markets.  
- Alternative modelling approaches demonstrate **greater flexibility in capturing nonlinear market behaviour**.

## Approach

The project implements a quantitative workflow including:

- financial data collection and preprocessing  
- econometric modelling (**VaR, GARCH**)  
- alternative predictive modelling techniques  
- model comparison using statistical performance metrics such as **RMSE**

## Tools

**Languages:** R, Python  
**Libraries:** tidyverse, rugarch, PerformanceAnalytics, caret, tidytext  
**Software:** RStudio, Power BI, Excel

---
