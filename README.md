# VatOfFinance
R package ：Some useful functions about calculate vat of financial products.

提供了一些计算金融商品转让增值税相关的R函数。

# 安装

如果在Windows系统安装出现错误，解决方法有两种：

## 方法1

``` r
options(download.file.method = "wininet") 
devtools::install_github('wuliubingbing/VatOfFinance')
library(VatOfFinance)
data(records)
cal_multiply_records(records)
```
## 方法2

下载所有文件，在本地build R包。

## 问题和建议反馈

反馈至 bilibili私信：五柳冰冰
或邮件 wuliubingbing@foxmail.com
