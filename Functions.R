#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20171217, by MaoY
# 
# Description: 存放各类分析函数
#------------------------------------------------------------------------------#


# 道路线形示意底图绘制----
PlotRoadLine <- function(kXStart, kXEnd, kYStart, kYEnd, kLineType = "solid", kSize = 1){
  # 绘制道路线形，组成行车轨迹图底图.
  #
  # 输入:
  #   kXStart: 描绘点的x坐标起点，对应annotate()函数的x变量.
  #   kXEnd: 描绘点的x坐标终点，对应annotate()函数的xend变量.
  #   kYStart: 描绘点的y坐标起点，对应annotate()函数的y变量.
  #   kYEnd: 描绘点的y坐标终点，对应annotate()函数的yend变量.
  #   kLineType: 描绘线的类型，对应annotate()函数的linetype变量.
  #   kSize：描绘线的类型，对应annotate()函数的size变量.
  #
  # 输出:
  #   对应的annotate()函数参数.
  
  return(annotate("segment",
                  x = kXStart,
                  xend = kXEnd,
                  y = kYStart,
                  yend = kYEnd,
                  colour = "black",
                  size = kSize,
                  linetype = kLineType))
}


# 主路进入匝道，计算行车轨迹----
CalcDrivingTrajectory <- function(data, is.main2ramp = TRUE){
  # 计算行车轨迹.
  #
  # 输入:
  #   data: 重命名后的数据框.
  #
  # 输出:
  #   含行车轨迹变量drivingTrajectory的数据框.
  
  get.roadname <- unique(data$roadName)
  
  get.tmpsubdata1 <- subset(data, data$roadName == get.roadname[1])
  get.tmpsubdata2 <- subset(data, data$roadName == get.roadname[2])
  
  kData1Length <- length(get.tmpsubdata1$disToLeftBorder)
  
  if (is.main2ramp) {
    
    get.tmpsubdata1$drivingTrajectory <- get.tmpsubdata1$disToLeftBorder
    
    kDeltaDrivingTrajectory <- get.tmpsubdata1[kData1Length,]$drivingTrajectory -
      get.tmpsubdata2[1,]$disToLeftBorder
    
    get.tmpsubdata2$drivingTrajectory <- get.tmpsubdata2$disToLeftBorder +
      kDeltaDrivingTrajectory
  } else {
    
    get.tmpsubdata2$drivingTrajectory <- get.tmpsubdata2$disToLeftBorder
    
    kDeltaDrivingTrajectory <- get.tmpsubdata2[1,]$drivingTrajectory -
      get.tmpsubdata1[kData1Length,]$disToLeftBorder
    
    get.tmpsubdata1$drivingTrajectory <- get.tmpsubdata1$disToLeftBorder +
      kDeltaDrivingTrajectory
  }
  
  results.data <- rbind(get.tmpsubdata1, get.tmpsubdata2)
  results.data <- results.data[order(results.data$disTravelled),]
  
  return(results.data)
}


# 新disFromRoadStart生成----
CalcNewDis <- function(data, is.main2ramp = TRUE, is.maindisraising = TRUE){
  # 计算新disFromRoadStart，解决不同道路桩号不一问题.
  # 计算的newDisFromRoadStart用于绘制行车轨迹图的横坐标.
  #
  # 输入:
  #   data: 重命名后的数据框.
  #   is.main2ramp：行驶方向是否为主线进入匝道.
  #
  # 输出:
  #   含新桩号newDisFromRoadStart的数据框.
  
  get.roadname <- unique(data$roadName)
  
  get.tmpsubdata1 <- subset(data, data$roadName == get.roadname[1])
  get.tmpsubdata2 <- subset(data, data$roadName == get.roadname[2])
  
  kData1Length <- length(get.tmpsubdata1$disFromRoadStart)
  
  if (is.main2ramp) {
    
    get.tmpsubdata1$newDisFromRoadStart <- get.tmpsubdata1$disFromRoadStart
    
    if (is.maindisraising) {
      
      get.tmpsubdata2$newDisFromRoadStart <- get.tmpsubdata2$disFromRoadStart +
        get.tmpsubdata1[kData1Length,]$disFromRoadStart
    } else {
      
      get.tmpsubdata2$newDisFromRoadStart <- get.tmpsubdata1[kData1Length,]$disFromRoadStart -
        get.tmpsubdata2$disFromRoadStart
    }
    
  } else {
    
    if (is.maindisraising) {
      
      get.tmpsubdata1$newDisFromRoadStart <- get.tmpsubdata1$disFromRoadStart -
        get.tmpsubdata1[kData1Length,]$disFromRoadStart +
        get.tmpsubdata2[1,]$disFromRoadStart
    } else {
      
      get.tmpsubdata1$newDisFromRoadStart <- get.tmpsubdata1[kData1Length,]$disFromRoadStart -
        get.tmpsubdata1$disFromRoadStart +
        get.tmpsubdata2[1,]$disFromRoadStart
    }
    
    get.tmpsubdata2$newDisFromRoadStart <- get.tmpsubdata2$disFromRoadStart
  }
  
  results.data <- rbind(get.tmpsubdata1, get.tmpsubdata2)
  results.data <- results.data[order(results.data$disTravelled),]
  
  return(results.data)
}


# newDisFromRoadStart和drivingTrajectory变量计算----
CalcNewDisDrivingTrajectory <- function(data, is.Main2Ramp = TRUE, is.MainDisRaising = TRUE){
  
  data <- CalcNewDis(data,
                     is.main2ramp = is.Main2Ramp,
                     is.maindisraising = is.MainDisRaising)
  
  data <- CalcDrivingTrajectory(data,
                                is.main2ramp = is.Main2Ramp)
  
  return(data)
}







