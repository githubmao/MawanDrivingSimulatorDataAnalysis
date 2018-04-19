#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20171217, by MaoY
# ZD6匝道只通行小车
#
# Description: 分析妈湾ZD6的行驶轨迹，速度，加速度，制动，加速等。
#------------------------------------------------------------------------------#


# 调用数据导入程序DataInput.R
source(file = "E:/R/MaWan/MawanDrivingSimulatorDataAnalysis/DataInput.R", encoding = "utf-8")
# 调用数据导入程序Functions.R
source(file = "E:/R/MaWan/MawanDrivingSimulatorDataAnalysis/Functions.R", encoding = "utf-8")


library(ggplot2)  # 导入绘图用数据包

# 数据切分
df.zd6 <- subset(x = df.dsdata, Scen == "ZD6")  # ZD6数据


# 1. ZD6行驶速度----
plot.zd6speed <- ggplot(df.zd6, aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = NULL, limits = c(0, 750),
                     breaks = c(0, 365, 585, 665),
                     labels = c("S6K0+000", "S6K0+365", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  annotate(geom = "rect", xmin = 218, xmax = 750, ymin = 0, ymax = 100, alpha = 0.1) +
  annotate(geom = "text", x = 218, y = 100, label = "敞开段终点") +
  annotate("segment", x= 365, xend= 585, y= 95, yend= 95,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 475, y = 100, label = "加速段") +
  annotate("segment", x= 585, xend= 665, y= 95, yend= 95,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 625, y = 100, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd6speed


# 2. ZD6行驶轨迹----
# 3. ZD6车道跨越点位置----



# 4. ZD6加速度----
plot.zd6acc <- ggplot(df.zd6, aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-3.5, 3.5), colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(0, 750),
                     breaks = c(0, 365, 585, 665),
                     labels = c("S6K0+000", "S6K0+365", "", "")) +
  scale_y_continuous(name = "加速度(m/s2)", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 218, xmax = 750, ymin = -5, ymax = 5, alpha = 0.1) +
  annotate(geom = "text", x = 218, y = 4.5, label = "敞开段终点") +
  annotate("segment", x= 365, xend= 585, y= 5, yend= 5,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 475, y = 4.5, label = "加速段") +
  annotate("segment", x= 585, xend= 665, y= 5, yend= 5,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 625, y = 4.5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd6acc


# 5. ZD6制动踏板位移----
plot.zd6brakepedal <- ggplot(df.zd6, aes(x = disTravelled, y = brakePedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(0, 750),
                     breaks = c(0, 365, 585, 665),
                     labels = c("S6K0+000", "S6K0+365", "", "")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 218, xmax = 750, ymin = 0, ymax = 1, alpha = 0.1) +
  annotate(geom = "text", x = 218, y = 0.95, label = "敞开段终点") +
  annotate("segment", x= 365, xend= 585, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 475, y = 0.95, label = "加速段") +
  annotate("segment", x= 585, xend= 665, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 625, y = 0.95, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd6brakepedal


# 6. ZD6油门踏板位移----
plot.zd6gaspedal <- ggplot(df.zd6, aes(x = disTravelled, y = gasPedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(0, 750),
                     breaks = c(0, 365, 585, 665),
                     labels = c("S6K0+000", "S6K0+365", "", "")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 218, xmax = 750, ymin = 0, ymax = 1, alpha = 0.1) +
  annotate(geom = "text", x = 218, y = 0.95, label = "敞开段终点") +
  annotate("segment", x= 365, xend= 585, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 475, y = 0.95, label = "加速段") +
  annotate("segment", x= 585, xend= 665, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 625, y = 0.95, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd6gaspedal

