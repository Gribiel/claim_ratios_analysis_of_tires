#! -*- coding: utf-8 -*-
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(DT)
data_1 = read.csv("data/data_1.csv", header = TRUE, sep = ",", fileEncoding = 'GB2312')
data_2 = read.csv("data/data_2.csv", header = TRUE, sep = ",", fileEncoding = 'GB2312')
shinyUI(fluidPage(
    title = '理赔胎数据分析平台',
    tags$head(tags$style(HTML("
                            .shiny-text-output {
                            background-color:#fff;
                            }
                            "))),
    
    h1(span("理赔胎数据分析平台", style = "font-weight: 300"), 
       style = "font-family: 'Source Han Serif SC';
     color: #fff; text-align: center;
     background-image: url('texturebg.png');
     padding: 20px"),
    br(),
    
    sidebarLayout(
         sidebarPanel(
          fileInput('file1', '导入理赔数据', buttonLabel = '上传...',
                     placeholder = '未上传文件', 
                     accept = c('text/csv', 'text/comma-separated-values,text/plain')),
          fileInput('file2', '导入销售数据', buttonLabel = '上传...',
                     placeholder = '未上传文件', 
                     accept = c('text/csv', 'text/comma-separated-values,text/plain')),
          dateInput('dateInput1', '选择查询开始生产日期: yyyy-mm-dd', value = Sys.Date() - 3000),
          dateInput('dateInput2', '选择查询结束生产日期: yyyy-mm-dd', value = Sys.Date() + 1),
          dateInput('dateInput3', '选择查询开始理赔日期: yyyy-mm-dd', value = Sys.Date() - 3000),
          dateInput('dateInput4', '选择查询结束理赔日期: yyyy-mm-dd', value = Sys.Date() + 1),
          dateInput('dateInput5', '选择查询开始销售日期: yyyy-mm-dd', value = Sys.Date() - 3000),
          dateInput('dateInput6', '选择查询结束销售日期: yyyy-mm-dd', value = Sys.Date() + 1),
          selectizeInput('selectizeInput1', '选择查询规格', 
                         choices = data_1$tire_size,  selected = "富力通 12.00R20 S-3015 20PR"),
          selectizeInput('selectizeInput2', '选择生产厂', 
                         choices = c('合肥', '番禺'), selected = "合肥"),
          selectizeInput('selectizeInput3', '选择查询病象', 
                         choices = data_2$损坏原因, selected = "趾口裂"),
          p(
            class = "text-muted",
            h4(paste("NOTE： 本 平 台 所 有 分 析 均 建 立 在 数 据 准 确 的 基 础 之 上， 所 上 传 的 理",
                      "赔 数 据 应 包 括 年、 月、 轮胎品牌、 轮 胎 规 格、 轮 胎 花 纹、 轮 胎 层 级、",
                      "生 产 厂、 生 产 月、 DOT 年 号 （包 括 但 不 限 于） 等 数 据， 所 上 传 的 销 售",
                      "数 据 应 包 括 销 售 年、 销 售 月、 轮 胎 品 牌、 轮 胎 规 格、 轮 胎 花 纹、 轮 胎",
                      "层 级、 生 产 厂、 生 产 月、 DOT 年 号 （包 括 但 不 限 于） 等 数 据。")),
            h5(paste("如 有 问 题， 请 联 系： liuyunpeng@wanlitire.cn."))
          )
         ),
          mainPanel(
            actionButton("go1", h5("总理赔率趋势"), class = "btn btn-secondary"),
            actionButton("go2", h5("有内胎理赔率趋势"), class = "btn btn-secondary"),
            actionButton("go3", h5("无内胎理赔率趋势"), class = "btn btn-secondary"),
            actionButton("go4", h5("单规格理赔率趋势"), class = "btn btn-secondary"),
            actionButton("go5", h5("矿山胎理赔率趋势"), class = "btn btn-secondary"),
            actionButton("go6", h5("中短途理赔率趋势"), class = "btn btn-secondary"),
            actionButton("go7", h5("中长途理赔率趋势"), class = "btn btn-secondary"),
            actionButton("go8", h5("单规格不同病象理赔率趋势"), class = "btn btn-secondary"),
            DT::dataTableOutput('table1'), plotOutput('plot1', width = '100%', height = '600px'),
            DT::dataTableOutput('table2'), plotOutput('plot2', width = '100%', height = '600px'),
            DT::dataTableOutput('table3'), plotOutput('plot3', width = '100%', height = '600px'),
            DT::dataTableOutput('table4'), plotOutput('plot4', width = '100%', height = '600px'),
            DT::dataTableOutput('table5'), plotOutput('plot5', width = '100%', height = '600px'),
            DT::dataTableOutput('table6'), plotOutput('plot6', width = '100%', height = '600px'),
            DT::dataTableOutput('table7'), plotOutput('plot7', width = '100%', height = '600px'),
            DT::dataTableOutput('table8'), plotOutput('plot8', width = '100%', height = '600px')
))))