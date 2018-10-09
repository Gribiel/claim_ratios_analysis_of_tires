#! -*- coding: utf-8 -*-
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
if (!require(tidyr)) { install.packages("tidyr") }
if (!require(plyr)) { install.packages("plyr") }
if (!require(zoo)) { install.packages("zoo") }
if (!require(lubridate)) { install.packages("lubridate") }
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(ggplot2)) { install.packages("ggplot2") }
library(shiny)
library(tidyr)
library(plyr)
library(zoo)
library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggthemes)
options(shiny.maxRequestSize = 100*1024^2)
shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
    if (is.null(input$file1))     
      return(NULL)
    if (is.null(input$file2)) 
      return(NULL)
    file1 <- input$file1
    file2 <- input$file2
    
    claim_data <- read.csv(file1$datapath, header = input$header, sep = input$sep)
    sales_data <- read.csv(file2$datapath, header = input$header, sep = input$sep)
   
    claim_data = claim_data[,c('年', '月', '轮胎品牌', '轮胎规格', '轮胎花纹',
                               '轮胎层级', '生产月', 'DOT年号', '生产厂')]
    claim_data = unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
    claim_data = unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
    claim_data$生产日期 = paste("20", claim_data$生产日期, sep = "")
    claim_data = unite(claim_data,
                       轮胎规格,
                       轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                       sep = " ",remove = TRUE)
    claim_data = claim_data[claim_data$生产厂 == '合肥',] 
    claim_data$轮胎规格 = paste(claim_data$轮胎规格, "PR", sep = "")
    claim_data$理赔日期 = as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
    claim_data$生产日期 = as.yearmon(as.character(claim_data$生产日期), "%Y%m")
    claim_data$理赔日期 = as.Date(claim_data$理赔日期)
    claim_data$生产日期 = as.Date(claim_data$生产日期)
    int = interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
    claim_data$经过月 = time_length(int, "month")
    
    sales_data = sales_data[,c('销售年', '销售月', '轮胎品牌', '轮胎规格',
                               '轮胎花纹', '轮胎层级', '生产厂', '生产月', 'DOT年号')]
    sales_data = unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
    sales_data = unite(sales_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
    sales_data$生产日期 = paste("20", sales_data$生产日期, sep = "")
    sales_data = unite(sales_data,
                       轮胎规格, 
                       轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                       sep = " ",remove = TRUE)
    sales_data$轮胎规格 = paste(sales_data$轮胎规格, "PR", sep = "")
    sales_data$销售日期 = as.yearmon(as.character(sales_data$销售日期), "%Y%m")
    sales_data$生产日期 = as.yearmon(as.character(sales_data$生产日期), "%Y%m")
    sales_data$销售日期 = as.Date(sales_data$销售日期)
    sales_data$生产日期 = as.Date(sales_data$生产日期)
    int = interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
    sales_data$经过月 = time_length(int, "month")
    
    m_tt = c("6.50|7.00|7.50|8.25|9.00|10.00|11.00|12.00")
    x_tt = grep(pattern = m_tt, x = claim_data$轮胎规格, value = TRUE)
    y_tt = grep(pattern = m_tt, x = sales_data$轮胎规格, value = TRUE)
    myvars1_tt = claim_data$轮胎规格 %in% x_tt
    myvars2_tt = sales_data$轮胎规格 %in% y_tt
    dat1_tt = claim_data[myvars1_tt, ]
    dat2_tt = sales_data[myvars2_tt, ]
    dat1_1_tt = dat1_tt[, c("生产日期","经过月")]
    dat1_2_tt = plyr::count(dat1_1_tt, names(dat1_1_tt))
    names(dat1_2_tt)[3] = "理赔数量"
    dat1_3_tt = dat1_2_tt[order(dat1_2_tt$生产日期, dat1_2_tt$经过月),]
    dat2_1_tt = dat2_tt[, c("生产日期","经过月")]
    dat2_2_tt = plyr::count(dat2_1_tt, names(dat2_1_tt))
    names(dat2_2_tt)[3] = "销售数量"
    dat2_3_tt = dat2_2_tt[order(dat2_2_tt$生产日期, dat2_2_tt$经过月),]
    merge_data_tt = merge(dat1_3_tt, dat2_3_tt, by = c("经过月", "生产日期"), all = TRUE)
    merge_data_tt$理赔数量[is.na(merge_data_tt$理赔数量)] = 0
    merge_data_tt$销售数量[is.na(merge_data_tt$销售数量)] = 0
    new_merge_data_tt = merge_data_tt[merge_data_tt$经过月 >= 0,]
    new_merge_data_tt = new_merge_data_tt[order(new_merge_data_tt$生产日期,new_merge_data_tt$经过月),]
    t_tt = data.table(new_merge_data_tt)
    new_merge_data1_tt = t_tt[,cumsum(销售数量), by = 生产日期]
    new_merge_data2_tt = t_tt[,cumsum(理赔数量), by = 生产日期]
    names(new_merge_data1_tt)[2] = "销售数量"
    names(new_merge_data2_tt)[2] = "理赔数量"
    newdata_tt = cbind(new_merge_data1_tt, new_merge_data2_tt, new_merge_data_tt$经过月)
    names(newdata_tt)[5] = "经过月"
    newdata_tt = newdata_tt[,c("生产日期", "经过月", "销售数量", "理赔数量")]
    newdata_tt = transform(newdata_tt, 理赔率 = 理赔数量 / 销售数量 * 100)
    newdata_tt$理赔率 = round(newdata_tt$理赔率, 2)
    newdata_tt$生产日期 = format(newdata_tt$生产日期, format = "%Y%m")
    newdata_tt$生产日期 = as.character(newdata_tt$生产日期)
    newdata_tt = newdata_tt[newdata_tt$理赔率 <= 10,]
    ggplot(newdata_tt, aes(经过月, 理赔率, group = 生产日期, color = 生产日期)) +
      geom_line(size = 0.5) +
      geom_point(size = 0.8) +
      scale_y_continuous(breaks = seq(0,max(newdata_tt$理赔率) + 0.1,0.1), 
                         limits = c(0,max(newdata_tt$理赔率) + 0.1)) +
      scale_x_continuous(breaks = seq(0,max(newdata_tt$经过月),1),
                         limits = c(0,max(newdata_tt$经过月))) +
      labs(title = "有内胎产品理赔率趋势图",
           x = "经过月", y = "理赔率/%") +
      theme(title = element_text(family = "GB1", size = 12, color = "red",
                                 face = "italic", lineheight = 0.2),                     
            plot.title = element_text(hjust = 0.5))  
  })
})
