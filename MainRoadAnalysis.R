#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20171216, by MaoY
#
# Description: 分析妈湾主线的行驶速度，加速度，制动，加速等。
#------------------------------------------------------------------------------#


# 调用数据导入程序DataInput.R
source(file = "E:/R/MaWan/MawanDrivingSimulatorDataAnalysis/DataInput.R",
       encoding = "utf-8")


library(ggplot2)  # 导入绘图用数据包

# 数据切分
df.mainroadright <- subset(x = df.dsdata, Scen == "YX")  # 右线数据
df.mainroadleft <- subset(x = df.dsdata, Scen == "ZX")  # 左线数据

df.mainroadrightsedan <- subset(x = df.mainroadright,
                                dsVehicleType == "Sedan")  # 右线轿车数据
df.mainroadrighttruck <- subset(x = df.mainroadright,
                                dsVehicleType == "Truck")  # 右线卡车数据
df.mainroadleftsedan <- subset(x = df.mainroadleft,
                               dsVehicleType == "Sedan")  # 左线轿车数据
df.mainroadlefttruck <- subset(x = df.mainroadleft,
                               dsVehicleType == "Truck")  # 左线卡车数据


# 1.右线行驶速度----------------------------------------------------------------
# 1.1 右线，行驶速度分析----
# 1.1.1 轿车----
plot.MRRsedanspeed <- ggplot(df.mainroadrightsedan,
                             aes(x = disFromRoadStart, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 8750),
                     breaks = seq(969.9, 8750, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4", "RK5", "RK6", "RK7")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120)) +
  annotate(geom = "rect", xmin = 2719.9, xmax = 8473.9, ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 2719.9, y = 120, label = "地下道路起点") +
  annotate(geom = "text", x = 8473.9, y = 120, label = "地下道路终点") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRRsedanspeed


# 1.1.2 货车----
df.mainroadrighttruck <- df.mainroadrighttruck[df.mainroadrighttruck$driverID != "S0301" |
                                                 df.mainroadrighttruck$PW != "PW6.0",]

plot.MRRtruckspeed <- ggplot(df.mainroadrighttruck,
                                  aes(x = disFromRoadStart, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 8750),
                     breaks = seq(969.9, 8750, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4", "RK5", "RK6", "RK7")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  annotate(geom = "rect", xmin = 2719.9, xmax = 8473.9, ymin = 0, ymax = 100, alpha = 0.2) +
  annotate(geom = "text", x = 2719.9, y = 100, label = "地下道路起点") +
  annotate(geom = "text", x = 8473.9, y = 100, label = "地下道路终点") +
  theme(legend.position = "none", 
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12)) +
  facet_wrap(~PW, ncol = 1)

plot.MRRtruckspeed


# 1.2 右线，加速度分析----
# 1.2.1 轿车----
plot.MRRsedanacc <- ggplot(df.mainroadrightsedan,
                           aes(x = disFromRoadStart, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-3.5, 3.5), colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 8750),
                     breaks = seq(969.9, 8750, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4", "RK5", "RK6", "RK7")) +
  scale_y_continuous(name = "加速度(m/s2)）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 2719.9, xmax = 8473.9, ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 2719.9, y = 5, label = "地下道路起点") +
  annotate(geom = "text", x = 8473.9, y = 5, label = "地下道路终点") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRRsedanacc


# 1.2.2 货车----
plot.MRRtruckacc <- ggplot(df.mainroadrighttruck,
                           aes(x = disFromRoadStart, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-3.5, 3.5), colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 8750),
                     breaks = seq(969.9, 8750, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4", "RK5", "RK6", "RK7")) +
  scale_y_continuous(name = "加速度(m/s2)）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 2719.9, xmax = 8473.9, ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 2719.9, y = 5, label = "地下道路起点") +
  annotate(geom = "text", x = 8473.9, y = 5, label = "地下道路终点") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  facet_wrap(~PW, ncol = 1)

plot.MRRtruckacc


# 1.3 右线，制动踏板位移分析----
# 1.3.1 轿车----
plot.MRRsedanbrakepedal <- ggplot(df.mainroadrightsedan,
                                  aes(x = disFromRoadStart, y = brakePedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 8750),
                     breaks = seq(969.9, 8750, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4", "RK5", "RK6", "RK7")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 2719.9, xmax = 8473.9, ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 2719.9, y = 1, label = "地下道路起点") +
  annotate(geom = "text", x = 8473.9, y = 1, label = "地下道路终点") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRRsedanbrakepedal


# 1.3.2 货车----
plot.MRRtruckbrakepedal <- ggplot(df.mainroadrighttruck,
                                  aes(x = disFromRoadStart, y = appBrake)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 8750),
                     breaks = seq(969.9, 8750, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4", "RK5", "RK6", "RK7")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 2719.9, xmax = 8473.9, ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 2719.9, y = 1, label = "地下道路起点") +
  annotate(geom = "text", x = 8473.9, y = 1, label = "地下道路终点") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  facet_wrap(~PW, ncol = 1)

plot.MRRtruckbrakepedal


# 1.4 右线，油门踏板位移分析----
# 1.4.1 轿车----
plot.MRRsedangaspedal <- ggplot(df.mainroadrightsedan,
                                aes(x = disFromRoadStart, y = gasPedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 8750),
                     breaks = seq(969.9, 8750, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4", "RK5", "RK6", "RK7")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 2719.9, xmax = 8473.9, ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 2719.9, y = 1, label = "地下道路起点") +
  annotate(geom = "text", x = 8473.9, y = 1, label = "地下道路终点") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRRsedangaspedal


# 1.4.2 货车----
plot.MRRtruckgaspedal <- ggplot(df.mainroadrighttruck,
                                aes(x = disFromRoadStart, y = appGasPedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(969.9, 8750),
                     breaks = seq(969.9, 8750, by = 1000),
                     labels = c("RK0", "RK1", "RK2", "RK3", "RK4", "RK5", "RK6", "RK7")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 2719.9, xmax = 8473.9, ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 2719.9, y = 1, label = "地下道路起点") +
  annotate(geom = "text", x = 8473.9, y = 1, label = "地下道路终点") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  facet_wrap(~PW, ncol = 1)

plot.MRRtruckgaspedal


# 2.左线行驶速度----------------------------------------------------------------
# 2.1 左线，行驶速度分析----
# 2.1.1 轿车----
plot.MRLsedanspeed <- ggplot(df.mainroadleftsedan,
                             aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = "桩号", limits = c(52, 8052),
                     breaks = seq(52, 8052, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4", "LK3", "LK2", "LK1", "LK0")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 120)) +
  annotate(geom = "rect", xmin = 558, xmax = 6303, ymin = 0, ymax = 120, alpha = 0.2) +
  annotate(geom = "text", x = 558, y = 120, label = "地下道路起点") +
  annotate(geom = "text", x = 6303, y = 120, label = "地下道路终点") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRLsedanspeed

#####################################################################
# 货车数据disFromRoadStart修正
CalcTruckDisMain <- function(data,
                             kDisFromRoadStart1 = 1302,
                             kDisFromRoadStart2 = 7726){
  # 修正货车模拟器中difFromRoadStart输出错误
  #
  # 输入: 货车模拟器驾驶模拟数据框
  #
  # 输出: 修正后数据
  

  len <- length(data$Time)
  
  tmp.time <- as.numeric(data$simTime)
  
  tmp.timebefore <- as.numeric(data$simTime)
  tmp.timebefore <- append(x = tmp.timebefore,
                           values = 0,
                           after = 0)
  tmp.timebefore <- tmp.timebefore[-(len + 1)]
  
  tmp.timegap <- tmp.time - tmp.timebefore
  
  tmp.speed <- data$speedMS
  
  tmp.dis <- tmp.timegap * tmp.speed
  tmp.dis <- append(x = tmp.dis,
                    values = 0,
                    after = 0)

  tmp.distancefromroadstart <- c()
  tmp.dissum <- 0
    
  for(i in 1:length(tmp.dis)){
    if(i == 1){
      tmp.distancefromroadstart <- append(tmp.distancefromroadstart,
                                          kDisFromRoadStart1 - 10 + kDisFromRoadStart2)
      tmp.dissum <- tmp.dissum + tmp.dis[i]
    } else {
      tmp.dissum <- tmp.dissum + tmp.dis[i]
      tmp.distancefromroadstart <- append(tmp.distancefromroadstart,
                                          kDisFromRoadStart1 - 10 + kDisFromRoadStart2 - tmp.dissum)
    }
  }
  
  data$disFromRoadStart <- tmp.distancefromroadstart
 
  data$dis <- tmp.speed[-(length(tmp.speed) + 1)]
  data$timegap <- data$timegap[-(length(data$timegap) + 1)]
   
  return(data)
}

#####################################################################


# 2.1.2 货车----


# 2.2 左线，加速度分析----
# 2.2.1 轿车----
plot.MRLsedanacc <- ggplot(df.mainroadleftsedan,
                           aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-3.5, 3.5), colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(52, 8052),
                     breaks = seq(52, 8052, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4", "LK3", "LK2", "LK1", "LK0")) +
  scale_y_continuous(name = "加速度(m/s2)）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 558, xmax = 6303, ymin = -5, ymax = 5, alpha = 0.2) +
  annotate(geom = "text", x = 558, y = 5, label = "地下道路起点") +
  annotate(geom = "text", x = 6303, y = 5, label = "地下道路终点") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRLsedanacc


# 2.2.2 货车----



# 2.3 左线，制动踏板位移分析----
# 2.3.1 轿车----
plot.MRLsedanbrakepedal <- ggplot(df.mainroadleftsedan,
                                  aes(x = disTravelled, y = brakePedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(52, 8052),
                     breaks = seq(52, 8052, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4", "LK3", "LK2", "LK1", "LK0")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 558, xmax = 6303, ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 558, y = 1, label = "地下道路起点") +
  annotate(geom = "text", x = 6303, y = 1, label = "地下道路终点") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRLsedanbrakepedal


# 2.3.2 货车----



# 2.4 左线，油门踏板位移分析----
# 2.4.1 轿车----
plot.MRLsedangaspedal <- ggplot(df.mainroadleftsedan,
                                aes(x = disTravelled, y = gasPedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.75, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = "桩号", limits = c(52, 8052),
                     breaks = seq(52, 8052, by = 1000),
                     labels = c("LK8", "LK7", "LK6", "LK5", "LK4", "LK3", "LK2", "LK1", "LK0")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 558, xmax = 6303, ymin = 0, ymax = 1, alpha = 0.2) +
  annotate(geom = "text", x = 558, y = 1, label = "地下道路起点") +
  annotate(geom = "text", x = 6303, y = 1, label = "地下道路终点") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12))

plot.MRLsedangaspedal


# 2.4.2 货车----




















CalcSpeedDiff100 <- function(data, kGap = 100){
  # 计算百米速度差
  #
  # 输入: 对应驾驶模拟数据框
  #
  # 输出: 含百米速度差的数据框
  
  if(data$disTravelled <= min(data$disTravelled) + 100){
    
  }
  
}



