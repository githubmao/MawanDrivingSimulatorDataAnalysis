#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20171217, by MaoY
# ZD5匝道只通行小车
#
# Description: 分析妈湾ZD5的行驶轨迹，速度，加速度，制动，加速等。
#------------------------------------------------------------------------------#


# 调用数据导入程序DataInput.R
source(file = "E:/R/MaWan/MawanDrivingSimulatorDataAnalysis/DataInput.R", encoding = "utf-8")
# 调用数据导入程序Functions.R
source(file = "E:/R/MaWan/MawanDrivingSimulatorDataAnalysis/Functions.R", encoding = "utf-8")


library(ggplot2)  # 导入绘图用数据包

# 数据切分
df.zd5 <- subset(x = df.dsdata, Scen == "ZD5")  # ZD5数据


# 1. ZD5行驶速度----
plot.zd5speed <- ggplot(df.zd5, aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = NULL, limits = c(1200, 2360),
                     breaks = c(1800, 1880, 2000, 2360),
                     labels = c("", "", "S5K0+000", "S5K0+360")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  annotate(geom = "rect", xmin = 1200, xmax = 2115, ymin = 0, ymax = 100, alpha = 0.1) +
  annotate(geom = "text", x = 2115, y = 100, label = "敞开段起点") +
  annotate("segment", x= 1880, xend= 2000, y= 95, yend= 95,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1940, y = 100, label = "减速段") +
  annotate("segment", x= 1800, xend= 1880, y= 95, yend= 95,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1840, y = 100, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd5speed


# 2. ZD5行驶轨迹----
# 3. ZD5车道跨越点位置----


# 4. ZD5加速度----
plot.zd5acc <- ggplot(df.zd5, aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-3.5, 3.5), colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(1200, 2360),
                     breaks = c(1800, 1880, 2000, 2360),
                     labels = c("", "", "S5K0+000", "S5K0+360")) +
  scale_y_continuous(name = "加速度(m/s2)", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 1200, xmax = 2115, ymin = -5, ymax = 5, alpha = 0.1) +
  annotate(geom = "text", x = 2115, y = 4.5, label = "敞开段起点") +
  annotate("segment", x= 1880, xend= 2000, y= 5, yend= 5,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1940, y = 4.5, label = "减速段") +
  annotate("segment", x= 1800, xend= 1880, y= 5, yend= 5,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1840, y = 4.5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd5acc


# 5. ZD5制动踏板位移----
plot.zd5brakepedal <- ggplot(df.zd5, aes(x = disTravelled, y = brakePedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(1200, 2360),
                     breaks = c(1800, 1880, 2000, 2360),
                     labels = c("", "", "S5K0+000", "S5K0+360")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 1200, xmax = 2115, ymin = 0, ymax = 1, alpha = 0.1) +
  annotate(geom = "text", x = 2115, y = 0.95, label = "敞开段起点") +
  annotate("segment", x= 1880, xend= 2000, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1940, y = 0.95, label = "减速段") +
  annotate("segment", x= 1800, xend= 1880, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1840, y = 0.95, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd5brakepedal


# 6. ZD5油门踏板位移----
plot.zd5gaspedal <- ggplot(df.zd5, aes(x = disTravelled, y = gasPedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(1200, 2360),
                     breaks = c(1800, 1880, 2000, 2360),
                     labels = c("", "", "S5K0+000", "S5K0+360")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 1200, xmax = 2115, ymin = 0, ymax = 1, alpha = 0.1) +
  annotate(geom = "text", x = 2115, y = 0.95, label = "敞开段起点") +
  annotate("segment", x= 1880, xend= 2000, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1940, y = 0.95, label = "减速段") +
  annotate("segment", x= 1800, xend= 1880, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1840, y = 0.95, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd5gaspedal

