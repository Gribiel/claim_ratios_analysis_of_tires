#! -*- coding: utf-8 -*-
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

shinyUI(pageWithSidebar(
  headerPanel("理赔胎数据分析平台"),
  sidebarPanel(
    fileInput('file1', '导入理赔数据', buttonLabel = '上传...',
              placeholder = '没有文件', 
              accept = c('text/csv', 'text/comma-separated-values,text/plain')),
    fileInput('file2', '导入理赔数据', buttonLabel = '上传...',
              placeholder = '没有文件', 
              accept = c('text/csv', 'text/comma-separated-values,text/plain')),
    tags$hr(),
    checkboxInput('header', '第一行为列名', TRUE),
    radioButtons('sep', '分隔符',
                 c(逗号 = ','))
  ),
  mainPanel(
    plotOutput('plot')
  )
))