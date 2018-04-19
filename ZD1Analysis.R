#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20171217, by MaoY
# ZD1匝道只通行小车
#
# Description: 分析妈湾ZD1的行驶轨迹，速度，加速度，制动，加速等。
#------------------------------------------------------------------------------#


# 调用数据导入程序DataInput.R
source(file = "E:/R/MaWan/MawanDrivingSimulatorDataAnalysis/DataInput.R", encoding = "utf-8")
# 调用数据导入程序Functions.R
source(file = "E:/R/MaWan/MawanDrivingSimulatorDataAnalysis/Functions.R", encoding = "utf-8")


library(ggplot2)  # 导入绘图用数据包

# 数据切分
df.zd1 <- subset(x = df.dsdata, Scen == "ZD1")  # ZD1数据


# 1. ZD1行驶速度----
plot.zd1speed <- ggplot(df.zd1, aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = NULL, limits = c(0, 900),
                     breaks = c(0, 596.54, 816.54, 866.54),
                     labels = c("S1K0+000", "S1K0+596.54", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  annotate(geom = "rect", xmin = 190, xmax = 900, ymin = 0, ymax = 100, alpha = 0.1) +
  annotate(geom = "text", x = 190, y = 100, label = "敞开段终点") +
  annotate("segment", x= 596.54, xend= 816.54, y= 95, yend= 95,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 705, y = 100, label = "加速段") +
  annotate("segment", x= 816.54, xend= 866.54, y= 95, yend= 95,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 842, y = 100, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd1speed


# 2. ZD1行驶轨迹----
# 3. ZD1车道跨越点位置----


# 4. ZD1加速度----
plot.zd1acc <- ggplot(df.zd1, aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-3.5, 3.5), colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(0, 900),
                     breaks = c(0, 596.54, 816.54, 866.54),
                     labels = c("S1K0+000", "S1K0+596.54", "", "")) +
  scale_y_continuous(name = "加速度(m/s2)）", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 190, xmax = 900, ymin = -5, ymax = 5, alpha = 0.1) +
  annotate(geom = "text", x = 190, y = 4.5, label = "敞开段终点") +
  annotate("segment", x= 596.54, xend= 816.54, y= 5, yend= 5,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 705, y = 4.5, label = "加速段") +
  annotate("segment", x= 816.54, xend= 866.54, y= 5, yend= 5,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 842, y = 4.5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd1acc


# 5. ZD1制动踏板位移----
plot.zd1brakepedal <- ggplot(df.zd1, aes(x = disTravelled, y = brakePedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(0, 900),
                     breaks = c(0, 596.54, 816.54, 866.54),
                     labels = c("S1K0+000", "S1K0+596.54", "", "")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 190, xmax = 900, ymin = 0, ymax = 1, alpha = 0.1) +
  annotate(geom = "text", x = 190, y = 0.95, label = "敞开段终点") +
  annotate("segment", x= 596.54, xend= 816.54, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 705, y = 0.95, label = "加速段") +
  annotate("segment", x= 816.54, xend= 866.54, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 842, y = 0.95, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd1brakepedal


# 6. ZD1油门踏板位移----
plot.zd1gaspedal <- ggplot(df.zd1, aes(x = disTravelled, y = gasPedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(0, 900),
                     breaks = c(0, 596.54, 816.54, 866.54),
                     labels = c("S1K0+000", "S1K0+596.54", "", "")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 190, xmax = 900, ymin = 0, ymax = 1, alpha = 0.1) +
  annotate(geom = "text", x = 190, y = 0.95, label = "敞开段终点") +
  annotate("segment", x= 596.54, xend= 816.54, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 705, y = 0.95, label = "加速段") +
  annotate("segment", x= 816.54, xend= 866.54, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 842, y = 0.95, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd1gaspedal














