#! -*- coding: utf-8 -*-
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyr)
library(plyr)
library(zoo)
library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(utils)
options(shiny.maxRequestSize = 100*1024^2)

shinyServer(function(input, output) {
  v1 <- reactiveValues(doTable = FALSE)
  v2 <- reactiveValues(doPlot = FALSE)
  v3 <- reactiveValues(doTable = FALSE)
  v4 <- reactiveValues(doPlot = FALSE)
  v5 <- reactiveValues(doTable = FALSE)
  v6 <- reactiveValues(doPlot = FALSE)
  v7 <- reactiveValues(doTable = FALSE)
  v8 <- reactiveValues(doPlot = FALSE)
  v9 <- reactiveValues(doTable = FALSE)
  v10 <- reactiveValues(doPlot = FALSE)
  v11 <- reactiveValues(doTable = FALSE)
  v12 <- reactiveValues(doPlot = FALSE)
  v13 <- reactiveValues(doTable = FALSE)
  v14 <- reactiveValues(doPlot = FALSE)
  v15 <- reactiveValues(doTable = FALSE)
  v16 <- reactiveValues(doPlot = FALSE)
  
  observeEvent(input$go1, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v1$doTable <- input$go1
    v2$doPlot <- input$go1
  })
  
  observeEvent(input$go2, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v3$doTable <- input$go2
    v4$doPlot <- input$go2
  })
  
  observeEvent(input$go3, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v5$doTable <- input$go3
    v6$doPlot <- input$go3
  })
  
  observeEvent(input$go4, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v7$doTable <- input$go4
    v8$doPlot <- input$go4
  })
  
  observeEvent(input$go5, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v9$doTable <- input$go5
    v10$doPlot <- input$go5
  })
  
  observeEvent(input$go6, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v11$doTable <- input$go6
    v12$doPlot <- input$go6
  })
  
  observeEvent(input$go7, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v13$doTable <- input$go7
    v14$doPlot <- input$go7
  })
  
  observeEvent(input$go8, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v15$doTable <- input$go8
    v16$doPlot <- input$go8
  })
  
  
  
  output$table1 = DT::renderDataTable(DT::datatable(options = list(
    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 5),{
      
      if (v1$doTable == FALSE) return()
      
      if (is.null(input$file1))     
        return(NULL)
      if (is.null(input$file2)) 
        return(NULL)
      isolate({
        claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
        sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
        startdate1 = input$dateInput1
        enddate1 = input$dateInput2
        startdate2 = input$dateInput3
        enddate2 = input$dateInput4
        startdate3 = input$dateInput5
        enddate3 = input$dateInput6
        claim_data <- claim_data[,c("年", "月", "生产厂", "生产月", "DOT年号")]
        claim_data <- unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
        claim_data <- unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
        claim_data$生产日期 <- paste("20", claim_data$生产日期, sep = "")
        claim_data <- claim_data[claim_data$生产厂 == input$selectizeInput2,]
        claim_data$理赔日期 <- as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
        claim_data$生产日期 <- as.yearmon(as.character(claim_data$生产日期), "%Y%m")
        claim_data$理赔日期 = as.Date(claim_data$理赔日期)
        claim_data$生产日期 = as.Date(claim_data$生产日期)
        claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                        claim_data$生产日期 <= enddate1 &
                                        claim_data$理赔日期 >= startdate2 & 
                                        claim_data$理赔日期 <= enddate2),]
        
        int <- interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
        claim_data$经过月 <- time_length(int, "month")
        
        sales_data <- sales_data[,c("销售年", "销售月", "生产厂", "生产月", "DOT年号")]
        sales_data <- unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
        sales_data <- unite(sales_data, 生产日期, DOT年号, 生产月, sep = "",remove = TRUE)
        sales_data$生产日期 <- paste("20", sales_data$生产日期, sep = "")
        sales_data <- sales_data[sales_data$生产厂 == input$selectizeInput2,]
        sales_data$销售日期 <- as.yearmon(as.character(sales_data$销售日期), "%Y%m")
        sales_data$生产日期 <- as.yearmon(as.character(sales_data$生产日期), "%Y%m")
        sales_data$销售日期 = as.Date(sales_data$销售日期)
        sales_data$生产日期 = as.Date(sales_data$生产日期)
        sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                        sales_data$生产日期 <= enddate1 &
                                        sales_data$销售日期 >= startdate3 & 
                                        sales_data$销售日期 <= enddate3),]
        
        int <- interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
        sales_data$经过月 <- time_length(int, "month")
        
        dat1 <- claim_data[, c("生产日期", "经过月")]
        dat1_1 <- plyr::count(dat1, names(dat1))
        names(dat1_1)[3] <- "理赔数量"
        dat1_1 <- dat1_1[,c("生产日期", "经过月", "理赔数量")]
        dat1_1 <- dat1_1[order(dat1_1$生产日期,dat1_1$经过月),]
        
        dat2 <- sales_data[, c("生产日期", "经过月")]
        dat2_1 <- plyr::count(dat2, names(dat2))
        names(dat2_1)[3] <- "销售数量"
        dat2_1 <- dat2_1[,c("生产日期", "经过月", "销售数量")]
        dat2_1 <- dat2_1[order(dat2_1$生产日期, dat2_1$经过月),]
        
        a <- merge(dat1_1, dat2_1 ,by = c("经过月", "生产日期"), all = TRUE)
        a$理赔数量[is.na(a$理赔数量)] <- 0
        a$销售数量[is.na(a$销售数量)] <- 0
        data <- a[, c("生产日期", "经过月", "销售数量", "理赔数量")]
        data <- data[order(data$生产日期,data$经过月),]
        t = data.table(data)
        data1 <- t[,cumsum(销售数量), by = 生产日期]
        data2 <- t[,cumsum(理赔数量), by = 生产日期]
        names(data1)[2] <- "销售数量"
        names(data2)[2] <- "理赔数量"
        newdata <- cbind(data1, data2, data$经过月)
        names(newdata)[5] <- "经过月"
        newdata <- newdata[,c( "生产日期", "经过月", "销售数量", "理赔数量")]
        
        newdata <- transform(newdata, 理赔率 = (理赔数量 / 销售数量)*100)
        newdata$理赔率 <- round(newdata$理赔率, 2)
        newdata$生产日期 <- format(newdata$生产日期, format = "%Y%m")
        newdata$生产日期 = as.character(newdata$生产日期)
        newdata = newdata[newdata$理赔率 >= 0 & newdata$经过月 >= 0 & newdata$理赔率 <= 10,]
        newdata = newdata %>% group_by(newdata$生产日期) %>% mutate(count=n())
        newdata = newdata[newdata$count != 1,]
        newdata = subset(newdata, select = -count)
        newdata = newdata[,-6]
      })
    }))
  
  output$table2 = DT::renderDataTable(DT::datatable(options = list(
    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 5), {
    
    if (v3$doTable == FALSE) return()
    
    if (is.null(input$file1))     
      return(NULL)
    if (is.null(input$file2)) 
      return(NULL)
    isolate({
    claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
    sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
    startdate1 = input$dateInput1
    enddate1 = input$dateInput2
    startdate2 = input$dateInput3
    enddate2 = input$dateInput4
    startdate3 = input$dateInput5
    enddate3 = input$dateInput6
    
    claim_data = claim_data[,c('年', '月', '轮胎品牌', '轮胎规格', '轮胎花纹',
                               '轮胎层级', '生产月', 'DOT年号', '生产厂')]
    claim_data = unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
    claim_data = unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
    claim_data$生产日期 = paste("20", claim_data$生产日期, sep = "")
    claim_data = unite(claim_data,
                       轮胎规格,
                       轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                       sep = " ",remove = TRUE)
    claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,] 
    claim_data$轮胎规格 = paste(claim_data$轮胎规格, "PR", sep = "")
    claim_data$理赔日期 = as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
    claim_data$生产日期 = as.yearmon(as.character(claim_data$生产日期), "%Y%m")
    claim_data$理赔日期 = as.Date(claim_data$理赔日期)
    claim_data$生产日期 = as.Date(claim_data$生产日期)
    claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                  claim_data$生产日期 <= enddate1 & 
                                  claim_data$理赔日期 >= startdate2 & 
                                  claim_data$理赔日期 <= enddate2),]
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
    sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,] 
    sales_data$轮胎规格 = paste(sales_data$轮胎规格, "PR", sep = "")
    sales_data$销售日期 = as.yearmon(as.character(sales_data$销售日期), "%Y%m")
    sales_data$生产日期 = as.yearmon(as.character(sales_data$生产日期), "%Y%m")
    sales_data$销售日期 = as.Date(sales_data$销售日期)
    sales_data$生产日期 = as.Date(sales_data$生产日期)
    sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                  sales_data$生产日期 <= enddate1 &
                                  sales_data$销售日期 >= startdate3 & 
                                  sales_data$销售日期 <= enddate3),]
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
    newdata_tt = newdata_tt[newdata_tt$经过月 >= 0 & newdata_tt$理赔率 >= 0 & newdata_tt$理赔率 <= 10,]
    newdata_tt = newdata_tt %>% group_by(newdata_tt$生产日期) %>% mutate(count=n())
    newdata_tt = newdata_tt[newdata_tt$count != 1,]
    newdata_tt = subset(newdata_tt, select = -count)
    newdata_tt = newdata_tt[,-6]
    })
  }))
  
  output$table3 = DT::renderDataTable(DT::datatable(options = list(
    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 5),{
    
    if (v5$doTable == FALSE) return()
      
    if (is.null(input$file1))     
      return(NULL)
    if (is.null(input$file2)) 
      return(NULL)
    
      isolate({
    claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
    sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
    startdate1 = input$dateInput1
    enddate1 = input$dateInput2
    startdate2 = input$dateInput3
    enddate2 = input$dateInput4
    startdate3 = input$dateInput5
    enddate3 = input$dateInput6
    
    claim_data = claim_data[,c('年', '月', '轮胎品牌', '轮胎规格', '轮胎花纹',
                               '轮胎层级', '生产月', 'DOT年号', '生产厂')]
    claim_data = unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
    claim_data = unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
    claim_data$生产日期 = paste("20", claim_data$生产日期, sep = "")
    claim_data = unite(claim_data,
                       轮胎规格,
                       轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                       sep = " ",remove = TRUE)
    claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,] 
    claim_data$轮胎规格 = paste(claim_data$轮胎规格, "PR", sep = "")
    claim_data$理赔日期 = as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
    claim_data$生产日期 = as.yearmon(as.character(claim_data$生产日期), "%Y%m")
    claim_data$理赔日期 = as.Date(claim_data$理赔日期)
    claim_data$生产日期 = as.Date(claim_data$生产日期)
    claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                    claim_data$生产日期 <= enddate1 & 
                                    claim_data$理赔日期 >= startdate2 & 
                                    claim_data$理赔日期 <= enddate2),]
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
    sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,]
    sales_data$轮胎规格 = paste(sales_data$轮胎规格, "PR", sep = "")
    sales_data$销售日期 = as.yearmon(as.character(sales_data$销售日期), "%Y%m")
    sales_data$生产日期 = as.yearmon(as.character(sales_data$生产日期), "%Y%m")
    sales_data$销售日期 = as.Date(sales_data$销售日期)
    sales_data$生产日期 = as.Date(sales_data$生产日期)
    sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                    sales_data$生产日期 <= enddate1 &
                                    sales_data$销售日期 >= startdate3 & 
                                    sales_data$销售日期 <= enddate3),]
    int = interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
    sales_data$经过月 = time_length(int, "month")
    m_tl = c("155|165|175|185|195|205|215|225|235|245|255|265|275|285|295|31|385|445|
             425|17.5|19.5|22.5|24.5")
    x_tl = grep(pattern = m_tl, x = claim_data$轮胎规格, value = TRUE)
    y_tl = grep(pattern = m_tl, x = sales_data$轮胎规格, value = TRUE)
    myvars1_tl = claim_data$轮胎规格 %in% x_tl
    myvars2_tl = sales_data$轮胎规格 %in% y_tl
    dat1_tl = claim_data[myvars1_tl, ]
    dat2_tl = sales_data[myvars2_tl, ]
    dat1_1_tl = dat1_tl[, c("生产日期","经过月")]
    dat1_2_tl = plyr::count(dat1_1_tl, names(dat1_1_tl))
    names(dat1_2_tl)[3] = "理赔数量"
    dat1_3_tl = dat1_2_tl[order(dat1_2_tl$生产日期, dat1_2_tl$经过月),]
    dat2_1_tl = dat2_tl[, c("生产日期","经过月")]
    dat2_2_tl = plyr::count(dat2_1_tl, names(dat2_1_tl))
    names(dat2_2_tl)[3] = "销售数量"
    dat2_3_tl = dat2_2_tl[order(dat2_2_tl$生产日期, dat2_2_tl$经过月),]
    merge_data_tl = merge(dat1_3_tl, dat2_3_tl, by = c("经过月", "生产日期"), all = TRUE)
    merge_data_tl$理赔数量[is.na(merge_data_tl$理赔数量)] = 0
    merge_data_tl$销售数量[is.na(merge_data_tl$销售数量)] = 0
    new_merge_data_tl = merge_data_tl[merge_data_tl$经过月 >= 0,]
    new_merge_data_tl = new_merge_data_tl[order(new_merge_data_tl$生产日期,new_merge_data_tl$经过月),]
    t_tl = data.table(new_merge_data_tl)
    new_merge_data1_tl = t_tl[,cumsum(销售数量), by = 生产日期]
    new_merge_data2_tl = t_tl[,cumsum(理赔数量), by = 生产日期]
    names(new_merge_data1_tl)[2] = "销售数量"
    names(new_merge_data2_tl)[2] = "理赔数量"
    newdata_tl = cbind(new_merge_data1_tl, new_merge_data2_tl,        
                       new_merge_data_tl$经过月)
    names(newdata_tl)[5] = "经过月"
    newdata_tl = newdata_tl[,c("生产日期", "经过月", "销售数量", "理赔数量")]
    newdata_tl = transform(newdata_tl, 理赔率 = 理赔数量 / 销售数量 * 100)
    newdata_tl$理赔率 = round(newdata_tl$理赔率, 2)
    newdata_tl$生产日期 = format(newdata_tl$生产日期, format = "%Y%m")
    newdata_tl$生产日期 = as.character(newdata_tl$生产日期)
    newdata_tl = newdata_tl[newdata_tl$经过月 >= 0 & newdata_tl$理赔率 >= 0 & newdata_tl$理赔率 <= 10,]
    newdata_tl = newdata_tl %>% group_by(newdata_tl$生产日期) %>% mutate(count=n())
    newdata_tl = newdata_tl[newdata_tl$count != 1,]
    newdata_tl = subset(newdata_tl, select = -count)
    newdata_tl = newdata_tl[,-6]
      })
  }))
  
  output$table4 = DT::renderDataTable(DT::datatable(options = list(
    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 5),{
      
      if (v7$doTable == FALSE) return()
      
      if (is.null(input$file1))     
        return(NULL)
      if (is.null(input$file2)) 
        return(NULL)
      
      isolate({
      claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
      sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
      startdate1 = input$dateInput1
      enddate1 = input$dateInput2
      startdate2 = input$dateInput3
      enddate2 = input$dateInput4
      startdate3 = input$dateInput5
      enddate3 = input$dateInput6
      
      claim_data <- claim_data[,c("年", "月", "轮胎品牌", "轮胎规格", "轮胎花纹", 
                                  "轮胎层级", "生产厂", "生产月", "DOT年号")]
      claim_data <- unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
      claim_data <- unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
      claim_data$生产日期 <- paste("20", claim_data$生产日期, sep = "")
      claim_data <- unite(claim_data, 
                          轮胎规格,
                          轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                          sep = " ",remove = TRUE)
      claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,]
      claim_data <- claim_data[claim_data$生产厂 == '合肥',] 
      claim_data$轮胎规格 <- paste(claim_data$轮胎规格, "PR", sep = "")
      claim_data$理赔日期 <- as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
      claim_data$生产日期 <- as.yearmon(as.character(claim_data$生产日期), "%Y%m")
      claim_data$理赔日期 <- as.Date(claim_data$理赔日期)
      claim_data$生产日期 <- as.Date(claim_data$生产日期)
      claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                      claim_data$生产日期 <= enddate1 & 
                                      claim_data$理赔日期 >= startdate2 & 
                                      claim_data$理赔日期 <= enddate2),]
      int <- interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
      claim_data$经过月 <- time_length(int, "month")
      
      sales_data <- sales_data[,c("销售年", "销售月", "轮胎品牌", "轮胎规格", "轮胎花纹", 
                                  "轮胎层级", "生产厂", "生产月", "DOT年号")]
      sales_data <- unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
      sales_data <- unite(sales_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
      sales_data$生产日期 <- paste("20", sales_data$生产日期, sep = "")
      sales_data <- unite(sales_data, 
                          轮胎规格, 
                          轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                          sep = " ",remove = TRUE)
      sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,]
      sales_data$轮胎规格 <- paste(sales_data$轮胎规格, "PR", sep = "")
      sales_data$销售日期 <- as.yearmon(as.character(sales_data$销售日期), "%Y%m")
      sales_data$生产日期 <- as.yearmon(as.character(sales_data$生产日期), "%Y%m")
      sales_data$销售日期 <- as.Date(sales_data$销售日期)
      sales_data$生产日期 <- as.Date(sales_data$生产日期)
      sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                      sales_data$生产日期 <= enddate1 &
                                      sales_data$销售日期 >= startdate3 & 
                                      sales_data$销售日期 <= enddate3),]
      int <- interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
      sales_data$经过月 <- time_length(int, "month")
      
      dat1 <- claim_data[claim_data$轮胎规格 == input$selectizeInput1,]
      dat2 <- sales_data[sales_data$轮胎规格 == input$selectizeInput1,]
      dat1 <- dat1[, c("生产日期", "轮胎规格", "经过月")]
      dat1_1 <- plyr::count(dat1, names(dat1))
      names(dat1_1)[4] <- "理赔数量"
      dat1_1 <- dat1_1[,c("轮胎规格", "生产日期", "经过月", "理赔数量")]
      dat1_1 <- dat1_1[order(dat1_1$生产日期,dat1_1$经过月),]
      
      dat2 <- dat2[, c("生产日期", "轮胎规格", "经过月")]
      dat2_1 <- plyr::count(dat2, names(dat2))
      names(dat2_1)[4] <- "销售数量"
      dat2_1 <- dat2_1[,c("轮胎规格", "生产日期", "经过月", "销售数量")]
      dat2_1 <- dat2_1[order(dat2_1$生产日期, dat2_1$经过月),]
      
      a <- merge(dat1_1, dat2_1 ,by = c("经过月", "生产日期"), all = TRUE)
      a$理赔数量[is.na(a$理赔数量)] <- 0
      a$销售数量[is.na(a$销售数量)] <- 0
      a$轮胎规格.x[is.na(a$轮胎规格.x)] <- input$selectizeInput1
      a$轮胎规格.y[is.na(a$轮胎规格.y)] <- input$selectizeInput1
      data <- a[, c("轮胎规格.x", "生产日期", "经过月", "销售数量", "理赔数量")]
      data <- data[order(data$生产日期,data$经过月),]
      t = data.table(data)
      data1 <- t[,cumsum(销售数量), by = 生产日期]
      data2 <- t[,cumsum(理赔数量), by = 生产日期]
      names(data1)[2] <- "销售数量"
      names(data2)[2] <- "理赔数量"
      newdata <- cbind(data1, data2, data$经过月)
      names(newdata)[5] <- "经过月"
      newdata <- newdata[,c( "生产日期", "经过月", "销售数量", "理赔数量")]
      
      newdata <- transform(newdata, 理赔率 = 理赔数量 / 销售数量*100)
      newdata$理赔率 <- round(newdata$理赔率, 2)
      newdata$生产日期 <- format(newdata$生产日期, format = "%Y%m")
      newdata$生产日期 = as.character(newdata$生产日期)
      newdata = newdata[newdata$经过月 >= 0 & newdata$理赔率 >= 0  & newdata$理赔率 <= 10,]
      newdata = newdata %>% group_by(newdata$生产日期) %>% mutate(count=n())
      newdata = newdata[newdata$count != 1,]
      newdata = subset(newdata, select = -count)
      newdata = newdata[,-6]
      })
    }))
  
  output$table5 = DT::renderDataTable(DT::datatable(options = list(
    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 5),{
      
      if (v9$doTable == FALSE) return()
      
      if (is.null(input$file1))     
        return(NULL)
      if (is.null(input$file2)) 
        return(NULL)
      isolate({
        claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
        sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
        startdate1 = input$dateInput1
        enddate1 = input$dateInput2
        startdate2 = input$dateInput3
        enddate2 = input$dateInput4
        startdate3 = input$dateInput5
        enddate3 = input$dateInput6
        
        claim_data = claim_data[,c('年', '月', '轮胎花纹', '生产月', 'DOT年号', '生产厂')]
        claim_data = unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
        claim_data = unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
        claim_data$生产日期 = paste("20", claim_data$生产日期, sep = "")

        claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,]
        claim_data$理赔日期 = as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
        claim_data$生产日期 = as.yearmon(as.character(claim_data$生产日期), "%Y%m")
        claim_data$理赔日期 = as.Date(claim_data$理赔日期)
        claim_data$生产日期 = as.Date(claim_data$生产日期)
        claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                        claim_data$生产日期 <= enddate1 & 
                                        claim_data$理赔日期 >= startdate2 & 
                                        claim_data$理赔日期 <= enddate2),]
        int = interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
        claim_data$经过月 = time_length(int, "month")
        
        sales_data = sales_data[,c('销售年', '销售月', '轮胎花纹', '生产厂', '生产月', 'DOT年号')]
        sales_data = unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
        sales_data = unite(sales_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
        sales_data$生产日期 = paste("20", sales_data$生产日期, sep = "")

        sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,]
        sales_data$销售日期 = as.yearmon(as.character(sales_data$销售日期), "%Y%m")
        sales_data$生产日期 = as.yearmon(as.character(sales_data$生产日期), "%Y%m")
        sales_data$销售日期 = as.Date(sales_data$销售日期)
        sales_data$生产日期 = as.Date(sales_data$生产日期)
        sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                        sales_data$生产日期 <= enddate1 &
                                        sales_data$销售日期 >= startdate3 & 
                                        sales_data$销售日期 <= enddate3),]
        int = interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
        sales_data$经过月 = time_length(int, "month")
        m_tt = c("P")
        x_tt = grep(pattern = m_tt, x = claim_data$轮胎花纹, value = TRUE)
        y_tt = grep(pattern = m_tt, x = sales_data$轮胎花纹, value = TRUE)
        myvars1_tt = claim_data$轮胎花纹 %in% x_tt
        myvars2_tt = sales_data$轮胎花纹 %in% y_tt
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
        newdata_tt = newdata_tt[newdata_tt$经过月 >= 0 & newdata_tt$理赔率 >= 0 & newdata_tt$理赔率 <= 10,]
        newdata_tt = newdata_tt %>% group_by(newdata_tt$生产日期) %>% mutate(count=n())
        newdata_tt = newdata_tt[newdata_tt$count != 1,]
        newdata_tt = subset(newdata_tt, select = -count)
        newdata_tt = newdata_tt[,-6]
      })
      }))
  
  output$table6 = DT::renderDataTable(DT::datatable(options = list(
    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 5),{
      
      if (v11$doTable == FALSE) return()
      
      if (is.null(input$file1))     
        return(NULL)
      if (is.null(input$file2)) 
        return(NULL)
      isolate({
        claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
        sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
        startdate1 = input$dateInput1
        enddate1 = input$dateInput2
        startdate2 = input$dateInput3
        enddate2 = input$dateInput4
        startdate3 = input$dateInput5
        enddate3 = input$dateInput6
        
        claim_data = claim_data[,c('年', '月', '轮胎花纹', '生产月', 'DOT年号', '生产厂')]
        claim_data = unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
        claim_data = unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
        claim_data$生产日期 = paste("20", claim_data$生产日期, sep = "")

        claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,]
        claim_data$理赔日期 = as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
        claim_data$生产日期 = as.yearmon(as.character(claim_data$生产日期), "%Y%m")
        claim_data$理赔日期 = as.Date(claim_data$理赔日期)
        claim_data$生产日期 = as.Date(claim_data$生产日期)
        claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                        claim_data$生产日期 <= enddate1 & 
                                        claim_data$理赔日期 >= startdate2 & 
                                        claim_data$理赔日期 <= enddate2),]
        int = interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
        claim_data$经过月 = time_length(int, "month")
        
        sales_data = sales_data[,c('销售年', '销售月','轮胎花纹', '生产厂', '生产月', 'DOT年号')]
        sales_data = unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
        sales_data = unite(sales_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
        sales_data$生产日期 = paste("20", sales_data$生产日期, sep = "")

        sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,]
        sales_data$销售日期 = as.yearmon(as.character(sales_data$销售日期), "%Y%m")
        sales_data$生产日期 = as.yearmon(as.character(sales_data$生产日期), "%Y%m")
        sales_data$销售日期 = as.Date(sales_data$销售日期)
        sales_data$生产日期 = as.Date(sales_data$生产日期)
        sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                        sales_data$生产日期 <= enddate1 &
                                        sales_data$销售日期 >= startdate3 & 
                                        sales_data$销售日期 <= enddate3),]
        int = interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
        sales_data$经过月 = time_length(int, "month")
        m_tt = c("M")
        x_tt = grep(pattern = m_tt, x = claim_data$轮胎花纹, value = TRUE)
        y_tt = grep(pattern = m_tt, x = sales_data$轮胎花纹, value = TRUE)
        myvars1_tt = claim_data$轮胎花纹 %in% x_tt
        myvars2_tt = sales_data$轮胎花纹 %in% y_tt
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
        newdata_tt = newdata_tt[newdata_tt$经过月 >= 0 & newdata_tt$理赔率 >= 0 & newdata_tt$理赔率 <= 10,]
        newdata_tt = newdata_tt %>% group_by(newdata_tt$生产日期) %>% mutate(count=n())
        newdata_tt = newdata_tt[newdata_tt$count != 1,]
        newdata_tt = subset(newdata_tt, select = -count)
        newdata_tt = newdata_tt[,-6]
      })
    }))
  
  output$table7 = DT::renderDataTable(DT::datatable(options = list(
    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 5),{
      
      if (v13$doTable == FALSE) return()
      
      if (is.null(input$file1))     
        return(NULL)
      if (is.null(input$file2)) 
        return(NULL)
      isolate({
        claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
        sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
        startdate1 = input$dateInput1
        enddate1 = input$dateInput2
        startdate2 = input$dateInput3
        enddate2 = input$dateInput4
        startdate3 = input$dateInput5
        enddate3 = input$dateInput6
        
        claim_data = claim_data[,c('年', '月', '轮胎花纹', '生产月', 'DOT年号', '生产厂')]
        claim_data = unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
        claim_data = unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
        claim_data$生产日期 = paste("20", claim_data$生产日期, sep = "")
        
        claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,]
        claim_data$理赔日期 = as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
        claim_data$生产日期 = as.yearmon(as.character(claim_data$生产日期), "%Y%m")
        claim_data$理赔日期 = as.Date(claim_data$理赔日期)
        claim_data$生产日期 = as.Date(claim_data$生产日期)
        claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                        claim_data$生产日期 <= enddate1 & 
                                        claim_data$理赔日期 >= startdate2 & 
                                        claim_data$理赔日期 <= enddate2),]
        int = interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
        claim_data$经过月 = time_length(int, "month")
        
        sales_data = sales_data[,c('销售年', '销售月', '轮胎花纹', '生产厂', '生产月', 'DOT年号')]
        sales_data = unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
        sales_data = unite(sales_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
        sales_data$生产日期 = paste("20", sales_data$生产日期, sep = "")
        
        sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,]
        sales_data$销售日期 = as.yearmon(as.character(sales_data$销售日期), "%Y%m")
        sales_data$生产日期 = as.yearmon(as.character(sales_data$生产日期), "%Y%m")
        sales_data$销售日期 = as.Date(sales_data$销售日期)
        sales_data$生产日期 = as.Date(sales_data$生产日期)
        sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                        sales_data$生产日期 <= enddate1 &
                                        sales_data$销售日期 >= startdate3 & 
                                        sales_data$销售日期 <= enddate3),]
        int = interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
        sales_data$经过月 = time_length(int, "month")
        m_tt = c("R")
        x_tt = grep(pattern = m_tt, x = claim_data$轮胎花纹, value = TRUE)
        y_tt = grep(pattern = m_tt, x = sales_data$轮胎花纹, value = TRUE)
        myvars1_tt = claim_data$轮胎花纹 %in% x_tt
        myvars2_tt = sales_data$轮胎花纹 %in% y_tt
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
        newdata_tt = newdata_tt[newdata_tt$经过月 >= 0 & newdata_tt$理赔率 >= 0 & newdata_tt$理赔率 <= 10,]
        newdata_tt = newdata_tt %>% group_by(newdata_tt$生产日期) %>% mutate(count=n())
        newdata_tt = newdata_tt[newdata_tt$count != 1,]
        newdata_tt = subset(newdata_tt, select = -count)
        newdata_tt = newdata_tt[,-6]
      })
    }))
  
  output$table8 = DT::renderDataTable(DT::datatable(options = list(
    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),pageLength = 5),{
      
      if (v15$doTable == FALSE) return()
      
      if (is.null(input$file1))     
        return(NULL)
      if (is.null(input$file2)) 
        return(NULL)
      isolate({
        claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
        sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
        startdate1 = input$dateInput1
        enddate1 = input$dateInput2
        startdate2 = input$dateInput3
        enddate2 = input$dateInput4
        startdate3 = input$dateInput5
        enddate3 = input$dateInput6
        
        claim_data = claim_data[,c("年", "月", "轮胎品牌", "轮胎规格", "轮胎花纹", 
                                   "轮胎层级", "生产厂", "生产月", "DOT年号", '损坏原因')]
        claim_data = unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
        claim_data = unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
        claim_data <- unite(claim_data, 
                            轮胎规格,
                            轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                            sep = " ",remove = TRUE)
        claim_data$轮胎规格 <- paste(claim_data$轮胎规格, "PR", sep = "")
        claim_data$生产日期 = paste("20", claim_data$生产日期, sep = "")
        
        claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,]
        claim_data$理赔日期 = as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
        claim_data$生产日期 = as.yearmon(as.character(claim_data$生产日期), "%Y%m")
        claim_data$理赔日期 = as.Date(claim_data$理赔日期)
        claim_data$生产日期 = as.Date(claim_data$生产日期)
        claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                        claim_data$生产日期 <= enddate1 & 
                                        claim_data$理赔日期 >= startdate2 & 
                                        claim_data$理赔日期 <= enddate2),]
        int = interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
        claim_data$经过月 = time_length(int, "month")
        
        sales_data = sales_data[,c("销售年", "销售月", "轮胎品牌", "轮胎规格", "轮胎花纹", 
                                   "轮胎层级", "生产厂", "生产月", "DOT年号")]
        sales_data = unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
        sales_data = unite(sales_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
        
        sales_data <- unite(sales_data, 
                            轮胎规格, 
                            轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                            sep = " ",remove = TRUE)
        sales_data$生产日期 = paste("20", sales_data$生产日期, sep = "")
        sales_data$轮胎规格 <- paste(sales_data$轮胎规格, "PR", sep = "")
        sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,]
        sales_data$销售日期 = as.yearmon(as.character(sales_data$销售日期), "%Y%m")
        sales_data$生产日期 = as.yearmon(as.character(sales_data$生产日期), "%Y%m")
        sales_data$销售日期 = as.Date(sales_data$销售日期)
        sales_data$生产日期 = as.Date(sales_data$生产日期)
        sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                        sales_data$生产日期 <= enddate1 &
                                        sales_data$销售日期 >= startdate3 & 
                                        sales_data$销售日期 <= enddate3),]
        int = interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
        sales_data$经过月 = time_length(int, "month")
        m_tt = input$selectizeInput3
        x_tt = grep(pattern = m_tt, x = claim_data$损坏原因, value = TRUE)
        myvars1_tt = claim_data$损坏原因 %in% x_tt
        dat1_tt = claim_data[myvars1_tt, ]
        dat1_tt <- dat1_tt[dat1_tt$轮胎规格 == input$selectizeInput1,]
        sales_data <- sales_data[sales_data$轮胎规格 == input$selectizeInput1,]
        dat1_1_tt = dat1_tt[, c("生产日期", "经过月")]
        dat1_2_tt = plyr::count(dat1_1_tt, names(dat1_1_tt))
        names(dat1_2_tt)[3] = "理赔数量"
        dat1_3_tt = dat1_2_tt[order(dat1_2_tt$生产日期, dat1_2_tt$经过月),]
        dat2_1_tt = sales_data[, c("生产日期", "经过月")]
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
        newdata_tt = newdata_tt[newdata_tt$经过月 >= 0 & newdata_tt$理赔率 >= 0 & newdata_tt$理赔率 <= 10,]
        newdata_tt = newdata_tt %>% group_by(newdata_tt$生产日期) %>% mutate(count=n())
        newdata_tt = newdata_tt[newdata_tt$count != 1,]
        newdata_tt = subset(newdata_tt, select = -count)
        newdata_tt = newdata_tt[,-6]
      })
    }))
  

  
  output$plot1 = renderPlot(width = 'auto', height = 'auto', res = 100, {
    
    if (v2$doPlot == FALSE) return()
    
    if (is.null(input$file1))     
      return(NULL)
    if (is.null(input$file2)) 
      return(NULL)
    
    isolate({
      claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
      sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
      startdate1 = input$dateInput1
      enddate1 = input$dateInput2
      startdate2 = input$dateInput3
      enddate2 = input$dateInput4
      startdate3 = input$dateInput5
      enddate3 = input$dateInput6
      
      claim_data <- claim_data[,c("年", "月", "生产厂", "生产月", "DOT年号")]
      claim_data <- unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
      claim_data <- unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
      claim_data$生产日期 <- paste("20", claim_data$生产日期, sep = "")
      claim_data <- claim_data[claim_data$生产厂 == input$selectizeInput2,]
      claim_data$理赔日期 <- as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
      claim_data$生产日期 <- as.yearmon(as.character(claim_data$生产日期), "%Y%m")
      claim_data$理赔日期 = as.Date(claim_data$理赔日期)
      claim_data$生产日期 = as.Date(claim_data$生产日期)
      claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                      claim_data$生产日期 <= enddate1 &
                                      claim_data$理赔日期 >= startdate2 & 
                                      claim_data$理赔日期 <= enddate2),]
      
      int <- interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
      claim_data$经过月 <- time_length(int, "month")
      
      sales_data <- sales_data[,c("销售年", "销售月", "生产厂", "生产月", "DOT年号")]
      sales_data <- unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
      sales_data <- unite(sales_data, 生产日期, DOT年号, 生产月, sep = "",remove = TRUE)
      sales_data$生产日期 <- paste("20", sales_data$生产日期, sep = "")
      sales_data <- sales_data[sales_data$生产厂 == input$selectizeInput2,]
      sales_data$销售日期 <- as.yearmon(as.character(sales_data$销售日期), "%Y%m")
      sales_data$生产日期 <- as.yearmon(as.character(sales_data$生产日期), "%Y%m")
      sales_data$销售日期 = as.Date(sales_data$销售日期)
      sales_data$生产日期 = as.Date(sales_data$生产日期)
      sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                      sales_data$生产日期 <= enddate1 &
                                      sales_data$销售日期 >= startdate3 & 
                                      sales_data$销售日期 <= enddate3),]
      int <- interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
      sales_data$经过月 <- time_length(int, "month")
      
      dat1 <- claim_data[, c("生产日期", "经过月")]
      dat1_1 <- plyr::count(dat1, names(dat1))
      names(dat1_1)[3] <- "理赔数量"
      dat1_1 <- dat1_1[,c("生产日期", "经过月", "理赔数量")]
      dat1_1 <- dat1_1[order(dat1_1$生产日期,dat1_1$经过月),]
      
      dat2 <- sales_data[, c("生产日期", "经过月")]
      dat2_1 <- plyr::count(dat2, names(dat2))
      names(dat2_1)[3] <- "销售数量"
      dat2_1 <- dat2_1[,c("生产日期", "经过月", "销售数量")]
      dat2_1 <- dat2_1[order(dat2_1$生产日期, dat2_1$经过月),]
      
      a <- merge(dat1_1, dat2_1 ,by = c("经过月", "生产日期"), all = TRUE)
      a$理赔数量[is.na(a$理赔数量)] <- 0
      a$销售数量[is.na(a$销售数量)] <- 0
      data <- a[, c("生产日期", "经过月", "销售数量", "理赔数量")]
      data <- data[order(data$生产日期,data$经过月),]
      t = data.table(data)
      data1 <- t[,cumsum(销售数量), by = 生产日期]
      data2 <- t[,cumsum(理赔数量), by = 生产日期]
      names(data1)[2] <- "销售数量"
      names(data2)[2] <- "理赔数量"
      newdata <- cbind(data1, data2, data$经过月)
      names(newdata)[5] <- "经过月"
      newdata <- newdata[,c( "生产日期", "经过月", "销售数量", "理赔数量")]
      
      newdata <- transform(newdata, 理赔率 = (理赔数量 / 销售数量)*100)
      newdata$理赔率 <- round(newdata$理赔率, 2)
      newdata$生产日期 <- format(newdata$生产日期, format = "%Y%m")
      newdata$生产日期 = as.character(newdata$生产日期)
      newdata = newdata[newdata$经过月 >= 0 & newdata$理赔率 >= 0 & newdata$理赔率 < 10,]
      newdata = newdata %>% group_by(newdata$生产日期) %>% mutate(count=n())
      newdata = newdata[newdata$count != 1,]
      newdata = subset(newdata, select = -count)
      newdata = newdata[,-6]
      bg_col = rgb(218/255,230/255,237/255)
      blue1 = rgb(0/255,76/255,102/255)
      blue2 = rgb(0,156/255,215/255)
      blue3 = rgb(129/255,208/255,242/255)
      ggplot(newdata, aes(经过月, 理赔率, group = 生产日期)) +
        geom_line(size = 1, aes(color = 生产日期)) +
        geom_point(size = 2, aes(color = 生产日期)) +
        scale_y_continuous(breaks = seq(0,max(newdata$理赔率) + 0.1,0.1), 
                           limits = c(0,max(newdata$理赔率) + 0.1)) +
        scale_x_continuous(breaks = seq(0,max(newdata$经过月),1),
                           limits = c(0,max(newdata$经过月) + 1)) +
        labs(title = "Claim Ratios Trend of Total Tires",
             x = "Interval Month", y = "Claim Ratios/%") +
        scale_fill_manual(values = c(blue1, blue2 , blue3)) +
        theme(title = element_text(family = "GB1", size = rel(1.5), color = "black",
                                   lineheight = 0.2),                     
              plot.title = element_text(hjust = 0.5, size = rel(1), face = "bold"),
              legend.position = 'top',
              legend.title = element_blank(),
              legend.key = element_blank(),
              legend.key.height = unit(0.5,"cm"),
              legend.key.width = unit(1,"cm"),
              legend.text = element_text(size = rel(1.5)),
              panel.grid.major.y = element_line(size = 1),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.background = element_blank(),
              legend.background = element_blank(),
              plot.background = element_rect(fill = bg_col),
              axis.text.y = element_text(size = rel(1.5), hjust = 0 , colour = "black"),
              axis.text.x = element_text(size = rel(1.5), face = "bold",colour = "black")
        ) 
    })
  })
      
  output$plot2 = renderPlot(width = 'auto', height = 'auto', res = 100, {
    
    if (v4$doPlot == FALSE) return()
    
    if (is.null(input$file1))     
      return(NULL)
    if (is.null(input$file2)) 
      return(NULL)
    isolate({
    claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
    sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
    startdate1 = input$dateInput1
    enddate1 = input$dateInput2
    startdate2 = input$dateInput3
    enddate2 = input$dateInput4
    startdate3 = input$dateInput5
    enddate3 = input$dateInput6
    
    claim_data = claim_data[,c('年', '月', '轮胎品牌', '轮胎规格', '轮胎花纹',
                               '轮胎层级', '生产月', 'DOT年号', '生产厂')]
    claim_data = unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
    claim_data = unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
    claim_data$生产日期 = paste("20", claim_data$生产日期, sep = "")
    claim_data = unite(claim_data,
                       轮胎规格,
                       轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                       sep = " ",remove = TRUE)
    claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,]
    claim_data$轮胎规格 = paste(claim_data$轮胎规格, "PR", sep = "")
    claim_data$理赔日期 = as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
    claim_data$生产日期 = as.yearmon(as.character(claim_data$生产日期), "%Y%m")
    claim_data$理赔日期 = as.Date(claim_data$理赔日期)
    claim_data$生产日期 = as.Date(claim_data$生产日期)
    claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                    claim_data$生产日期 <= enddate1 & 
                                    claim_data$理赔日期 >= startdate2 & 
                                    claim_data$理赔日期 <= enddate2),]
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
    sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,]
    sales_data$轮胎规格 = paste(sales_data$轮胎规格, "PR", sep = "")
    sales_data$销售日期 = as.yearmon(as.character(sales_data$销售日期), "%Y%m")
    sales_data$生产日期 = as.yearmon(as.character(sales_data$生产日期), "%Y%m")
    sales_data$销售日期 = as.Date(sales_data$销售日期)
    sales_data$生产日期 = as.Date(sales_data$生产日期)
    sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                    sales_data$生产日期 <= enddate1 &
                                    sales_data$销售日期 >= startdate3 & 
                                    sales_data$销售日期 <= enddate3),]
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
    newdata_tt = newdata_tt[newdata_tt$经过月 >= 0 & newdata_tt$理赔率 >= 0 & newdata_tt$理赔率 <= 10,]
    newdata_tt = newdata_tt %>% group_by(newdata_tt$生产日期) %>% mutate(count=n())
    newdata_tt = newdata_tt[newdata_tt$count != 1,]
    newdata_tt = subset(newdata_tt, select = -count)
    newdata_tt = newdata_tt[,-6]
    bg_col = rgb(218/255,230/255,237/255)
    blue1 = rgb(0/255,76/255,102/255)
    blue2 = rgb(0,156/255,215/255)
    blue3 = rgb(129/255,208/255,242/255)
    ggplot(newdata_tt, aes(经过月, 理赔率, group = 生产日期)) +
      geom_line(size = 1, aes(color = 生产日期)) +
      geom_point(size = 2, aes(color = 生产日期)) +
      scale_y_continuous(breaks = seq(0,max(newdata_tt$理赔率) + 0.1,0.1), 
                         limits = c(0,max(newdata_tt$理赔率) + 0.1)) +
      scale_x_continuous(breaks = seq(0,max(newdata_tt$经过月),1),
                         limits = c(0,max(newdata_tt$经过月) + 1)) +
      labs(title = "Claim Ratios Trend of Tubetype Tires",
           x = "Interval Month", y = "Claim Ratios/%") +
      scale_fill_manual(values = c(blue1, blue2 , blue3)) +
      theme(title = element_text(family = "GB1", size = rel(1.5), color = "black",
                                 lineheight = 0.2),                     
            plot.title = element_text(hjust = 0.5, size = rel(1), face = "bold"),
            legend.position = 'top',
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.key.height = unit(0.5,"cm"),
            legend.key.width = unit(1,"cm"),
            legend.text = element_text(size = rel(1.5)),
            panel.grid.major.y = element_line(size = 1),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background = element_blank(),
            legend.background = element_blank(),
            plot.background = element_rect(fill = bg_col),
            axis.text.y = element_text(size = rel(1.5), hjust = 0 , colour = "black"),
            axis.text.x = element_text(size = rel(1.5), face = "bold",colour = "black")
      )
  })
  })
  
  output$plot3 = renderPlot(width = 'auto', height = 'auto', res = 100, {
    
    if (v6$doPlot == FALSE) return()
    
    if (is.null(input$file1))     
      return(NULL)
    if (is.null(input$file2)) 
      return(NULL)
    
    isolate({
    claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
    sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
    startdate1 = input$dateInput1
    enddate1 = input$dateInput2
    startdate2 = input$dateInput3
    enddate2 = input$dateInput4
    startdate3 = input$dateInput5
    enddate3 = input$dateInput6
    
    claim_data = claim_data[,c('年', '月', '轮胎品牌', '轮胎规格', '轮胎花纹',
                               '轮胎层级', '生产月', 'DOT年号', '生产厂')]
    claim_data = unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
    claim_data = unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
    claim_data$生产日期 = paste("20", claim_data$生产日期, sep = "")
    claim_data = unite(claim_data,
                       轮胎规格,
                       轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                       sep = " ",remove = TRUE)
    claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,]
    claim_data = claim_data[claim_data$生产厂 == '合肥',] 
    claim_data$轮胎规格 = paste(claim_data$轮胎规格, "PR", sep = "")
    claim_data$理赔日期 = as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
    claim_data$生产日期 = as.yearmon(as.character(claim_data$生产日期), "%Y%m")
    claim_data$理赔日期 = as.Date(claim_data$理赔日期)
    claim_data$生产日期 = as.Date(claim_data$生产日期)
    claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                    claim_data$生产日期 <= enddate1 & 
                                    claim_data$理赔日期 >= startdate2 & 
                                    claim_data$理赔日期 <= enddate2),]
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
    sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,]
    sales_data$轮胎规格 = paste(sales_data$轮胎规格, "PR", sep = "")
    sales_data$销售日期 = as.yearmon(as.character(sales_data$销售日期), "%Y%m")
    sales_data$生产日期 = as.yearmon(as.character(sales_data$生产日期), "%Y%m")
    sales_data$销售日期 = as.Date(sales_data$销售日期)
    sales_data$生产日期 = as.Date(sales_data$生产日期)
    sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                    sales_data$生产日期 <= enddate1 &
                                    sales_data$销售日期 >= startdate3 & 
                                    sales_data$销售日期 <= enddate3),]
    int = interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
    sales_data$经过月 = time_length(int, "month")
    m_tl = c("155|165|175|185|195|205|215|225|235|245|255|265|275|285|295|31|385|445|
       425|17.5|19.5|22.5|24.5")
    x_tl = grep(pattern = m_tl, x = claim_data$轮胎规格, value = TRUE)
    y_tl = grep(pattern = m_tl, x = sales_data$轮胎规格, value = TRUE)
    myvars1_tl = claim_data$轮胎规格 %in% x_tl
    myvars2_tl = sales_data$轮胎规格 %in% y_tl
    dat1_tl = claim_data[myvars1_tl, ]
    dat2_tl = sales_data[myvars2_tl, ]
    dat1_1_tl = dat1_tl[, c("生产日期","经过月")]
    dat1_2_tl = plyr::count(dat1_1_tl, names(dat1_1_tl))
    names(dat1_2_tl)[3] = "理赔数量"
    dat1_3_tl = dat1_2_tl[order(dat1_2_tl$生产日期, dat1_2_tl$经过月),]
    dat2_1_tl = dat2_tl[, c("生产日期","经过月")]
    dat2_2_tl = plyr::count(dat2_1_tl, names(dat2_1_tl))
    names(dat2_2_tl)[3] = "销售数量"
    dat2_3_tl = dat2_2_tl[order(dat2_2_tl$生产日期, dat2_2_tl$经过月),]
    merge_data_tl = merge(dat1_3_tl, dat2_3_tl, by = c("经过月", "生产日期"), all = TRUE)
    merge_data_tl$理赔数量[is.na(merge_data_tl$理赔数量)] = 0
    merge_data_tl$销售数量[is.na(merge_data_tl$销售数量)] = 0
    new_merge_data_tl = merge_data_tl[merge_data_tl$经过月 >= 0,]
    new_merge_data_tl = new_merge_data_tl[order(new_merge_data_tl$生产日期,new_merge_data_tl$经过月),]
    t_tl = data.table(new_merge_data_tl)
    new_merge_data1_tl = t_tl[,cumsum(销售数量), by = 生产日期]
    new_merge_data2_tl = t_tl[,cumsum(理赔数量), by = 生产日期]
    names(new_merge_data1_tl)[2] = "销售数量"
    names(new_merge_data2_tl)[2] = "理赔数量"
    newdata_tl = cbind(new_merge_data1_tl, new_merge_data2_tl,        
                       new_merge_data_tl$经过月)
    names(newdata_tl)[5] = "经过月"
    newdata_tl = newdata_tl[,c("生产日期", "经过月", "销售数量", "理赔数量")]
    newdata_tl = transform(newdata_tl, 理赔率 = 理赔数量 / 销售数量 * 100)
    newdata_tl$理赔率 = round(newdata_tl$理赔率, 2)
    newdata_tl$生产日期 = format(newdata_tl$生产日期, format = "%Y%m")
    newdata_tl$生产日期 = as.character(newdata_tl$生产日期)
    newdata_tl = newdata_tl[newdata_tl$经过月 >= 0 & newdata_tl$理赔率 >= 0 & newdata_tl$理赔率 <= 10,]
    newdata_tl = newdata_tl %>% group_by(newdata_tl$生产日期) %>% mutate(count=n())
    newdata_tl = newdata_tl[newdata_tl$count != 1,]
    newdata_tl = subset(newdata_tl, select = -count)
    newdata_tl = newdata_tl[,-6]
    bg_col = rgb(218/255,230/255,237/255)
    blue1 = rgb(0/255,76/255,102/255)
    blue2 = rgb(0,156/255,215/255)
    blue3 = rgb(129/255,208/255,242/255)
    ggplot(newdata_tl, aes(经过月, 理赔率, group = 生产日期)) +
      geom_line(size = 1, aes(color = 生产日期)) +
      geom_point(size = 2, aes(color = 生产日期)) +
      scale_y_continuous(breaks = seq(0,max(newdata_tl$理赔率) + 0.1,0.1), 
                         limits = c(0,max(newdata_tl$理赔率) + 0.1)) +
      scale_x_continuous(breaks = seq(0,max(newdata_tl$经过月),1),
                         limits = c(0,max(newdata_tl$经过月) + 1)) +
      labs(title = "Claim Ratios Trend of Tubeless Tires",
           x = "Interval Month", y = "Claim Ratios/%") +
      scale_fill_manual(values = c(blue1, blue2 , blue3)) +
      theme(title = element_text(family = "GB1", size = rel(1.5), color = "black",
                                 lineheight = 0.2),                     
            plot.title = element_text(hjust = 0.5, size = rel(1), face = "bold"),
            legend.position = 'top',
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.key.height = unit(0.5,"cm"),
            legend.key.width = unit(1,"cm"),
            legend.text = element_text(size = rel(1.5)),
            panel.grid.major.y = element_line(size = 1),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background = element_blank(),
            legend.background = element_blank(),
            plot.background = element_rect(fill = bg_col),
            axis.text.y = element_text(size = rel(1.5), hjust = 0 , colour = "black"),
            axis.text.x = element_text(size = rel(1.5), face = "bold",colour = "black")
      )
  })
  })
  
  output$plot4 = renderPlot(width = 'auto', height = 'auto', res = 100, {
    
    if (v8$doPlot == FALSE) return()
    
    if (is.null(input$file1))     
      return(NULL)
    if (is.null(input$file2)) 
      return(NULL)
    
    isolate({
    claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
    sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
    startdate1 = input$dateInput1
    enddate1 = input$dateInput2
    startdate2 = input$dateInput3
    enddate2 = input$dateInput4
    startdate3 = input$dateInput5
    enddate3 = input$dateInput6
  
    claim_data <- claim_data[,c("年", "月", "轮胎品牌", "轮胎规格", "轮胎花纹", 
                                "轮胎层级", "生产厂", "生产月", "DOT年号")]
    claim_data <- unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
    claim_data <- unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
    claim_data$生产日期 <- paste("20", claim_data$生产日期, sep = "")
    claim_data <- unite(claim_data, 
                        轮胎规格,
                        轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                        sep = " ",remove = TRUE)
    claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,]
    claim_data <- claim_data[claim_data$生产厂 == '合肥',] 
    claim_data$轮胎规格 <- paste(claim_data$轮胎规格, "PR", sep = "")
    claim_data$理赔日期 <- as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
    claim_data$生产日期 <- as.yearmon(as.character(claim_data$生产日期), "%Y%m")
    claim_data$理赔日期 <- as.Date(claim_data$理赔日期)
    claim_data$生产日期 <- as.Date(claim_data$生产日期)
    claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                    claim_data$生产日期 <= enddate1 & 
                                    claim_data$理赔日期 >= startdate2 & 
                                    claim_data$理赔日期 <= enddate2),]
    int <- interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
    claim_data$经过月 <- time_length(int, "month")
    
    sales_data <- sales_data[,c("销售年", "销售月", "轮胎品牌", "轮胎规格", "轮胎花纹", 
                                "轮胎层级", "生产厂", "生产月", "DOT年号")]
    sales_data <- unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
    sales_data <- unite(sales_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
    sales_data$生产日期 <- paste("20", sales_data$生产日期, sep = "")
    sales_data <- unite(sales_data, 
                        轮胎规格, 
                        轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                        sep = " ",remove = TRUE)
    sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,]
    sales_data$轮胎规格 <- paste(sales_data$轮胎规格, "PR", sep = "")
    sales_data$销售日期 <- as.yearmon(as.character(sales_data$销售日期), "%Y%m")
    sales_data$生产日期 <- as.yearmon(as.character(sales_data$生产日期), "%Y%m")
    sales_data$销售日期 <- as.Date(sales_data$销售日期)
    sales_data$生产日期 <- as.Date(sales_data$生产日期)
    sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                    sales_data$生产日期 <= enddate1 &
                                    sales_data$销售日期 >= startdate3 & 
                                    sales_data$销售日期 <= enddate3),]
    int <- interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
    sales_data$经过月 <- time_length(int, "month")
    
    dat1 <- claim_data[claim_data$轮胎规格 == input$selectizeInput1,]
    dat2 <- sales_data[sales_data$轮胎规格 == input$selectizeInput1,]
    dat1 <- dat1[, c("生产日期", "轮胎规格", "经过月")]
    dat1_1 <- plyr::count(dat1, names(dat1))
    names(dat1_1)[4] <- "理赔数量"
    dat1_1 <- dat1_1[,c("轮胎规格", "生产日期", "经过月", "理赔数量")]
    dat1_1 <- dat1_1[order(dat1_1$生产日期,dat1_1$经过月),]
    
    dat2 <- dat2[, c("生产日期", "轮胎规格", "经过月")]
    dat2_1 <- plyr::count(dat2, names(dat2))
    names(dat2_1)[4] <- "销售数量"
    dat2_1 <- dat2_1[,c("轮胎规格", "生产日期", "经过月", "销售数量")]
    dat2_1 <- dat2_1[order(dat2_1$生产日期, dat2_1$经过月),]
    
    a <- merge(dat1_1, dat2_1 ,by = c("经过月", "生产日期"), all = TRUE)
    a$理赔数量[is.na(a$理赔数量)] <- 0
    a$销售数量[is.na(a$销售数量)] <- 0
    a$轮胎规格.x[is.na(a$轮胎规格.x)] <- input$selectizeInput1
    a$轮胎规格.y[is.na(a$轮胎规格.y)] <- input$selectizeInput1
    data <- a[, c("轮胎规格.x", "生产日期", "经过月", "销售数量", "理赔数量")]
    data <- data[order(data$生产日期,data$经过月),]
    t = data.table(data)
    data1 <- t[,cumsum(销售数量), by = 生产日期]
    data2 <- t[,cumsum(理赔数量), by = 生产日期]
    names(data1)[2] <- "销售数量"
    names(data2)[2] <- "理赔数量"
    newdata <- cbind(data1, data2, data$经过月)
    names(newdata)[5] <- "经过月"
    newdata <- newdata[,c( "生产日期", "经过月", "销售数量", "理赔数量")]
    
    newdata <- transform(newdata, 理赔率 = (理赔数量 / 销售数量)*100)
    newdata$理赔率 <- round(newdata$理赔率, 2)
    newdata$生产日期 <- format(newdata$生产日期, format = "%Y%m")
    newdata$生产日期 = as.character(newdata$生产日期)
    newdata = newdata[newdata$经过月 >= 0 & newdata$理赔率 >= 0 & newdata$理赔率 <= 10,]
    newdata = newdata %>% group_by(newdata$生产日期) %>% mutate(count=n())
    newdata = newdata[newdata$count != 1,]
    newdata = subset(newdata, select = -count)
    newdata = newdata[,-6]
    bg_col = rgb(218/255,230/255,237/255)
    blue1 = rgb(0/255,76/255,102/255)
    blue2 = rgb(0,156/255,215/255)
    blue3 = rgb(129/255,208/255,242/255)
    ggplot(newdata, aes(经过月, 理赔率, group = 生产日期)) +
      geom_line(size = 1, aes(color = 生产日期)) +
      geom_point(size = 2, aes(color = 生产日期)) +
      scale_y_continuous(breaks = seq(0,max(newdata$理赔率) + 0.1,0.1), 
                         limits = c(0,max(newdata$理赔率) + 0.1)) +
      scale_x_continuous(breaks = seq(0,max(newdata$经过月),1),
                         limits = c(0,max(newdata$经过月) + 1)) +
      labs(title = "Claim Ratios Trend of Single Tire",
           x = "Interval Month", y = "Claim Ratios/%") +
      scale_fill_manual(values = c(blue1, blue2 , blue3)) +
      theme(title = element_text(family = "GB1", size = rel(1.5), color = "black",
                                 lineheight = 0.2),                     
            plot.title = element_text(hjust = 0.5, size = rel(1), face = "bold"),
            legend.position = 'top',
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.key.height = unit(0.5,"cm"),
            legend.key.width = unit(1,"cm"),
            legend.text = element_text(size = rel(1.5)),
            panel.grid.major.y = element_line(size = 1),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background = element_blank(),
            legend.background = element_blank(),
            plot.background = element_rect(fill = bg_col),
            axis.text.y = element_text(size = rel(1.5), hjust = 0 , colour = "black"),
            axis.text.x = element_text(size = rel(1.5), face = "bold",colour = "black")
      ) 
  })
})
  
  output$plot5 = renderPlot(width = 'auto', height = 'auto', res = 100, {
    
    if (v10$doPlot == FALSE) return()
    
    if (is.null(input$file1))     
      return(NULL)
    if (is.null(input$file2)) 
      return(NULL)
    isolate({
      claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
      sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
      startdate1 = input$dateInput1
      enddate1 = input$dateInput2
      startdate2 = input$dateInput3
      enddate2 = input$dateInput4
      startdate3 = input$dateInput5
      enddate3 = input$dateInput6
      
      claim_data = claim_data[,c('年', '月', '轮胎花纹', '生产月', 'DOT年号', '生产厂')]
      claim_data = unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
      claim_data = unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
      claim_data$生产日期 = paste("20", claim_data$生产日期, sep = "")

      claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,]
      claim_data$理赔日期 = as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
      claim_data$生产日期 = as.yearmon(as.character(claim_data$生产日期), "%Y%m")
      claim_data$理赔日期 = as.Date(claim_data$理赔日期)
      claim_data$生产日期 = as.Date(claim_data$生产日期)
      claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                      claim_data$生产日期 <= enddate1 & 
                                      claim_data$理赔日期 >= startdate2 & 
                                      claim_data$理赔日期 <= enddate2),]
      int = interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
      claim_data$经过月 = time_length(int, "month")
      
      sales_data = sales_data[,c('销售年', '销售月', '轮胎花纹', '生产厂', '生产月', 'DOT年号')]
      sales_data = unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
      sales_data = unite(sales_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
      sales_data$生产日期 = paste("20", sales_data$生产日期, sep = "")

      sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,]
      sales_data$销售日期 = as.yearmon(as.character(sales_data$销售日期), "%Y%m")
      sales_data$生产日期 = as.yearmon(as.character(sales_data$生产日期), "%Y%m")
      sales_data$销售日期 = as.Date(sales_data$销售日期)
      sales_data$生产日期 = as.Date(sales_data$生产日期)
      sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                      sales_data$生产日期 <= enddate1 &
                                      sales_data$销售日期 >= startdate3 & 
                                      sales_data$销售日期 <= enddate3),]
      int = interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
      sales_data$经过月 = time_length(int, "month")
      m_tt = c("P")
      x_tt = grep(pattern = m_tt, x = claim_data$轮胎花纹, value = TRUE)
      y_tt = grep(pattern = m_tt, x = sales_data$轮胎花纹, value = TRUE)
      myvars1_tt = claim_data$轮胎花纹 %in% x_tt
      myvars2_tt = sales_data$轮胎花纹 %in% y_tt
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
      newdata_tt = newdata_tt[newdata_tt$经过月 >= 0 & newdata_tt$理赔率 >= 0 & newdata_tt$理赔率 <= 10,]
      newdata_tt = newdata_tt %>% group_by(newdata_tt$生产日期) %>% mutate(count=n())
      newdata_tt = newdata_tt[newdata_tt$count != 1,]
      newdata_tt = subset(newdata_tt, select = -count)
      newdata_tt = newdata_tt[,-6]
      bg_col = rgb(218/255,230/255,237/255)
      blue1 = rgb(0/255,76/255,102/255)
      blue2 = rgb(0,156/255,215/255)
      blue3 = rgb(129/255,208/255,242/255)
      ggplot(newdata_tt, aes(经过月, 理赔率, group = 生产日期)) +
        geom_line(size = 1, aes(color = 生产日期)) +
        geom_point(size = 2, aes(color = 生产日期)) +
        scale_y_continuous(breaks = seq(0,max(newdata_tt$理赔率) + 0.1,0.1), 
                           limits = c(0,max(newdata_tt$理赔率) + 0.1)) +
        scale_x_continuous(breaks = seq(0,max(newdata_tt$经过月),1),
                           limits = c(0,max(newdata_tt$经过月) + 1)) +
        labs(title = "Claim Ratios Trend of Mine Tires",
             x = "Interval Month", y = "Claim Ratios/%") +
        scale_fill_manual(values = c(blue1, blue2 , blue3)) +
        theme(title = element_text(family = "GB1", size = rel(1.5), color = "black",
                                   lineheight = 0.2),                     
              plot.title = element_text(hjust = 0.5, size = rel(1), face = "bold"),
              legend.position = 'top',
              legend.title = element_blank(),
              legend.key = element_blank(),
              legend.key.height = unit(0.5,"cm"),
              legend.key.width = unit(1,"cm"),
              legend.text = element_text(size = rel(1.5)),
              panel.grid.major.y = element_line(size = 1),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.background = element_blank(),
              legend.background = element_blank(),
              plot.background = element_rect(fill = bg_col),
              axis.text.y = element_text(size = rel(1.5), hjust = 0 , colour = "black"),
              axis.text.x = element_text(size = rel(1.5), face = "bold",colour = "black")
        )
    })
  })
  
  output$plot6 = renderPlot(width = 'auto', height = 'auto', res = 100, {
    
    if (v12$doPlot == FALSE) return()
    
    if (is.null(input$file1))     
      return(NULL)
    if (is.null(input$file2)) 
      return(NULL)
    isolate({
      claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
      sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
      startdate1 = input$dateInput1
      enddate1 = input$dateInput2
      startdate2 = input$dateInput3
      enddate2 = input$dateInput4
      startdate3 = input$dateInput5
      enddate3 = input$dateInput6
      
      claim_data = claim_data[,c('年', '月','轮胎花纹', '生产月', 'DOT年号', '生产厂')]
      claim_data = unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
      claim_data = unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
      claim_data$生产日期 = paste("20", claim_data$生产日期, sep = "")
 
      claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,]
      claim_data$理赔日期 = as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
      claim_data$生产日期 = as.yearmon(as.character(claim_data$生产日期), "%Y%m")
      claim_data$理赔日期 = as.Date(claim_data$理赔日期)
      claim_data$生产日期 = as.Date(claim_data$生产日期)
      claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                      claim_data$生产日期 <= enddate1 & 
                                      claim_data$理赔日期 >= startdate2 & 
                                      claim_data$理赔日期 <= enddate2),]
      int = interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
      claim_data$经过月 = time_length(int, "month")
      
      sales_data = sales_data[,c('销售年', '销售月', '轮胎花纹', '生产厂', '生产月', 'DOT年号')]
      sales_data = unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
      sales_data = unite(sales_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
      sales_data$生产日期 = paste("20", sales_data$生产日期, sep = "")

      sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,]
      sales_data$销售日期 = as.yearmon(as.character(sales_data$销售日期), "%Y%m")
      sales_data$生产日期 = as.yearmon(as.character(sales_data$生产日期), "%Y%m")
      sales_data$销售日期 = as.Date(sales_data$销售日期)
      sales_data$生产日期 = as.Date(sales_data$生产日期)
      sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                      sales_data$生产日期 <= enddate1 &
                                      sales_data$销售日期 >= startdate3 & 
                                      sales_data$销售日期 <= enddate3),]
      int = interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
      sales_data$经过月 = time_length(int, "month")
      m_tt = c("M")
      x_tt = grep(pattern = m_tt, x = claim_data$轮胎花纹, value = TRUE)
      y_tt = grep(pattern = m_tt, x = sales_data$轮胎花纹, value = TRUE)
      myvars1_tt = claim_data$轮胎花纹 %in% x_tt
      myvars2_tt = sales_data$轮胎花纹 %in% y_tt
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
      newdata_tt = newdata_tt[newdata_tt$经过月 >= 0 & newdata_tt$理赔率 >= 0 & newdata_tt$理赔率 <= 10,]
      newdata_tt = newdata_tt %>% group_by(newdata_tt$生产日期) %>% mutate(count=n())
      newdata_tt = newdata_tt[newdata_tt$count != 1,]
      newdata_tt = subset(newdata_tt, select = -count)
      newdata_tt = newdata_tt[,-6]
      bg_col = rgb(218/255,230/255,237/255)
      blue1 = rgb(0/255,76/255,102/255)
      blue2 = rgb(0,156/255,215/255)
      blue3 = rgb(129/255,208/255,242/255)
      ggplot(newdata_tt, aes(经过月, 理赔率, group = 生产日期)) +
        geom_line(size = 1, aes(color = 生产日期)) +
        geom_point(size = 2, aes(color = 生产日期)) +
        scale_y_continuous(breaks = seq(0,max(newdata_tt$理赔率) + 0.1,0.1), 
                           limits = c(0,max(newdata_tt$理赔率) + 0.1)) +
        scale_x_continuous(breaks = seq(0,max(newdata_tt$经过月),1),
                           limits = c(0,max(newdata_tt$经过月) + 1)) +
        labs(title = "Claim Ratios Trend of Short-distance Tires",
             x = "Interval Month", y = "Claim Ratios/%") +
        scale_fill_manual(values = c(blue1, blue2 , blue3)) +
        theme(title = element_text(family = "GB1", size = rel(1.5), color = "black",
                                   lineheight = 0.2),                     
              plot.title = element_text(hjust = 0.5, size = rel(1), face = "bold"),
              legend.position = 'top',
              legend.title = element_blank(),
              legend.key = element_blank(),
              legend.key.height = unit(0.5,"cm"),
              legend.key.width = unit(1,"cm"),
              legend.text = element_text(size = rel(1.5)),
              panel.grid.major.y = element_line(size = 1),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.background = element_blank(),
              legend.background = element_blank(),
              plot.background = element_rect(fill = bg_col),
              axis.text.y = element_text(size = rel(1.5), hjust = 0 , colour = "black"),
              axis.text.x = element_text(size = rel(1.5), face = "bold",colour = "black")
        )
    })
  })
  
  output$plot7 = renderPlot(width = 'auto', height = 'auto', res = 100, {
    
    if (v14$doPlot == FALSE) return()
    
    if (is.null(input$file1))     
      return(NULL)
    if (is.null(input$file2)) 
      return(NULL)
    isolate({
      claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
      sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
      startdate1 = input$dateInput1
      enddate1 = input$dateInput2
      startdate2 = input$dateInput3
      enddate2 = input$dateInput4
      startdate3 = input$dateInput5
      enddate3 = input$dateInput6
      
      claim_data = claim_data[,c('年', '月','轮胎花纹','生产月', 'DOT年号', '生产厂')]
      claim_data = unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
      claim_data = unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
      claim_data$生产日期 = paste("20", claim_data$生产日期, sep = "")

      claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,]
      claim_data$理赔日期 = as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
      claim_data$生产日期 = as.yearmon(as.character(claim_data$生产日期), "%Y%m")
      claim_data$理赔日期 = as.Date(claim_data$理赔日期)
      claim_data$生产日期 = as.Date(claim_data$生产日期)
      claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                      claim_data$生产日期 <= enddate1 & 
                                      claim_data$理赔日期 >= startdate2 & 
                                      claim_data$理赔日期 <= enddate2),]
      int = interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
      claim_data$经过月 = time_length(int, "month")
      
      sales_data = sales_data[,c('销售年', '销售月', '轮胎花纹', '生产厂', '生产月', 'DOT年号')]
      sales_data = unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
      sales_data = unite(sales_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
      sales_data$生产日期 = paste("20", sales_data$生产日期, sep = "")
      
      sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,]
      sales_data$销售日期 = as.yearmon(as.character(sales_data$销售日期), "%Y%m")
      sales_data$生产日期 = as.yearmon(as.character(sales_data$生产日期), "%Y%m")
      sales_data$销售日期 = as.Date(sales_data$销售日期)
      sales_data$生产日期 = as.Date(sales_data$生产日期)
      sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                      sales_data$生产日期 <= enddate1 &
                                      sales_data$销售日期 >= startdate3 & 
                                      sales_data$销售日期 <= enddate3),]
      int = interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
      sales_data$经过月 = time_length(int, "month")
      m_tt = c("R")
      x_tt = grep(pattern = m_tt, x = claim_data$轮胎花纹, value = TRUE)
      y_tt = grep(pattern = m_tt, x = sales_data$轮胎花纹, value = TRUE)
      myvars1_tt = claim_data$轮胎花纹 %in% x_tt
      myvars2_tt = sales_data$轮胎花纹 %in% y_tt
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
      newdata_tt = newdata_tt[newdata_tt$经过月 >= 0 & newdata_tt$理赔率 >= 0 & newdata_tt$理赔率 <= 10,]
      newdata_tt = newdata_tt %>% group_by(newdata_tt$生产日期) %>% mutate(count=n())
      newdata_tt = newdata_tt[newdata_tt$count != 1,]
      newdata_tt = subset(newdata_tt, select = -count)
      newdata_tt = newdata_tt[,-6]
      bg_col = rgb(218/255,230/255,237/255)
      blue1 = rgb(0/255,76/255,102/255)
      blue2 = rgb(0,156/255,215/255)
      blue3 = rgb(129/255,208/255,242/255)
      ggplot(newdata_tt, aes(经过月, 理赔率, group = 生产日期)) +
        geom_line(size = 1, aes(color = 生产日期)) +
        geom_point(size = 2, aes(color = 生产日期)) +
        scale_y_continuous(breaks = seq(0,max(newdata_tt$理赔率) + 0.1,0.1), 
                           limits = c(0,max(newdata_tt$理赔率) + 0.1)) +
        scale_x_continuous(breaks = seq(0,max(newdata_tt$经过月),1),
                           limits = c(0,max(newdata_tt$经过月) + 1)) +
        labs(title = "Claim Ratios Trend of Long-distance Tires",
             x = "Interval Month", y = "Claim Ratios/%") +
        scale_fill_manual(values = c(blue1, blue2 , blue3)) +
        theme(title = element_text(family = "GB1", size = rel(1.5), color = "black",
                                   lineheight = 0.2),                     
              plot.title = element_text(hjust = 0.5, size = rel(1), face = "bold"),
              legend.position = 'top',
              legend.title = element_blank(),
              legend.key = element_blank(),
              legend.key.height = unit(0.5,"cm"),
              legend.key.width = unit(1,"cm"),
              legend.text = element_text(size = rel(1.5)),
              panel.grid.major.y = element_line(size = 1),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.background = element_blank(),
              legend.background = element_blank(),
              plot.background = element_rect(fill = bg_col),
              axis.text.y = element_text(size = rel(1.5), hjust = 0 , colour = "black"),
              axis.text.x = element_text(size = rel(1.5), face = "bold",colour = "black")
        )
    })
  })
  
  output$plot8 = renderPlot(width = 'auto', height = 'auto', res = 100, {
    
    if (v16$doPlot == FALSE) return()
    
    if (is.null(input$file1))     
      return(NULL)
    if (is.null(input$file2)) 
      return(NULL)
    isolate({
      claim_data = read.csv(input$file1$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
      sales_data = read.csv(input$file2$datapath, header = TRUE, sep = ",", fileEncoding = 'GB2312')
      startdate1 = input$dateInput1
      enddate1 = input$dateInput2
      startdate2 = input$dateInput3
      enddate2 = input$dateInput4
      startdate3 = input$dateInput5
      enddate3 = input$dateInput6
      
      claim_data = claim_data[,c("年", "月", "轮胎品牌", "轮胎规格", "轮胎花纹", 
                                 "轮胎层级", "生产厂", "生产月", "DOT年号", '损坏原因')]
      claim_data = unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
      claim_data = unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
      claim_data <- unite(claim_data, 
                          轮胎规格,
                          轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                          sep = " ",remove = TRUE)
      claim_data$轮胎规格 <- paste(claim_data$轮胎规格, "PR", sep = "")
      claim_data$生产日期 = paste("20", claim_data$生产日期, sep = "")
      
      claim_data = claim_data[claim_data$生产厂 == input$selectizeInput2,]
      claim_data$理赔日期 = as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
      claim_data$生产日期 = as.yearmon(as.character(claim_data$生产日期), "%Y%m")
      claim_data$理赔日期 = as.Date(claim_data$理赔日期)
      claim_data$生产日期 = as.Date(claim_data$生产日期)
      claim_data = claim_data[which(claim_data$生产日期 >= startdate1 & 
                                      claim_data$生产日期 <= enddate1 & 
                                      claim_data$理赔日期 >= startdate2 & 
                                      claim_data$理赔日期 <= enddate2),]
      int = interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
      claim_data$经过月 = time_length(int, "month")
      
      sales_data = sales_data[,c("销售年", "销售月", "轮胎品牌", "轮胎规格", "轮胎花纹", 
                                 "轮胎层级", "生产厂", "生产月", "DOT年号")]
      sales_data = unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
      sales_data = unite(sales_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
      
      sales_data <- unite(sales_data, 
                          轮胎规格, 
                          轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                          sep = " ",remove = TRUE)
      sales_data$生产日期 = paste("20", sales_data$生产日期, sep = "")
      sales_data$轮胎规格 <- paste(sales_data$轮胎规格, "PR", sep = "")
      sales_data = sales_data[sales_data$生产厂 == input$selectizeInput2,]
      sales_data$销售日期 = as.yearmon(as.character(sales_data$销售日期), "%Y%m")
      sales_data$生产日期 = as.yearmon(as.character(sales_data$生产日期), "%Y%m")
      sales_data$销售日期 = as.Date(sales_data$销售日期)
      sales_data$生产日期 = as.Date(sales_data$生产日期)
      sales_data = sales_data[which(sales_data$生产日期 >= startdate1 & 
                                      sales_data$生产日期 <= enddate1 &
                                      sales_data$销售日期 >= startdate3 & 
                                      sales_data$销售日期 <= enddate3),]
      int = interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
      sales_data$经过月 = time_length(int, "month")
      m_tt = input$selectizeInput3
      x_tt = grep(pattern = m_tt, x = claim_data$损坏原因, value = TRUE)
      myvars1_tt = claim_data$损坏原因 %in% x_tt
      dat1_tt = claim_data[myvars1_tt, ]
      dat1_tt <- dat1_tt[dat1_tt$轮胎规格 == input$selectizeInput1,]
      sales_data <- sales_data[sales_data$轮胎规格 == input$selectizeInput1,]
      dat1_1_tt = dat1_tt[, c("生产日期", "经过月")]
      dat1_2_tt = plyr::count(dat1_1_tt, names(dat1_1_tt))
      names(dat1_2_tt)[3] = "理赔数量"
      dat1_3_tt = dat1_2_tt[order(dat1_2_tt$生产日期, dat1_2_tt$经过月),]
      dat2_1_tt = sales_data[, c("生产日期", "经过月")]
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
      newdata_tt = newdata_tt[newdata_tt$经过月 >= 0 & newdata_tt$理赔率 >= 0 & newdata_tt$理赔率 <= 10,]
      newdata_tt = newdata_tt %>% group_by(newdata_tt$生产日期) %>% mutate(count=n())
      newdata_tt = newdata_tt[newdata_tt$count != 1,]
      newdata_tt = subset(newdata_tt, select = -count)
      newdata_tt = newdata_tt[,-6]
      bg_col = rgb(218/255,230/255,237/255)
      blue1 = rgb(0/255,76/255,102/255)
      blue2 = rgb(0,156/255,215/255)
      blue3 = rgb(129/255,208/255,242/255)
      ggplot(newdata_tt, aes(经过月, 理赔率, group = 生产日期)) +
        geom_line(size = 1, aes(color = 生产日期)) +
        geom_point(size = 2, aes(color = 生产日期)) +
        scale_y_continuous(breaks = seq(0,max(newdata_tt$理赔率) + 0.1,0.1), 
                           limits = c(0,max(newdata_tt$理赔率) + 0.1)) +
        scale_x_continuous(breaks = seq(0,max(newdata_tt$经过月),1),
                           limits = c(0,max(newdata_tt$经过月) + 1)) +
        labs(title = "Claim Ratios Trend of Different Cause of Sigle Tire",
             x = "Interval Month", y = "Claim Ratios/%") +
        scale_fill_manual(values = c(blue1, blue2 , blue3)) +
        theme(title = element_text(family = "GB1", size = rel(1.5), color = "black",
                                   lineheight = 0.2),                     
              plot.title = element_text(hjust = 0.5, size = rel(1), face = "bold"),
              legend.position = 'top',
              legend.title = element_blank(),
              legend.key = element_blank(),
              legend.key.height = unit(0.5,"cm"),
              legend.key.width = unit(1,"cm"),
              legend.text = element_text(size = rel(1.5)),
              panel.grid.major.y = element_line(size = 1),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.background = element_blank(),
              legend.background = element_blank(),
              plot.background = element_rect(fill = bg_col),
              axis.text.y = element_text(size = rel(1.5), hjust = 0 , colour = "black"),
              axis.text.x = element_text(size = rel(1.5), face = "bold",colour = "black")
        )
    })
  })
})