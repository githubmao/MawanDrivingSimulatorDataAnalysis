#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20171217, by MaoY
# ZD2匝道只通行小车
#
# Description: 分析妈湾ZD2的行驶轨迹，速度，加速度，制动，加速等。
#------------------------------------------------------------------------------#


# 调用数据导入程序DataInput.R
source(file = "E:/R/MaWan/MawanDrivingSimulatorDataAnalysis/DataInput.R", encoding = "utf-8")
# 调用数据导入程序Functions.R
source(file = "E:/R/MaWan/MawanDrivingSimulatorDataAnalysis/Functions.R", encoding = "utf-8")


library(ggplot2)  # 导入绘图用数据包

# 数据切分
df.zd2 <- subset(x = df.dsdata, Scen == "ZD2")  # ZD2数据


# 1. ZD2行驶速度----
plot.zd2speed <- ggplot(df.zd2, aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = NULL, limits = c(259, 3051.8),
                     breaks = c(1258.2, 1308.2, 1918.2, 3051.8),
                     labels = c("", "", "S2K0+000", "S2K1+133.65")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  annotate(geom = "rect", xmin = 259, xmax = 2850.8, ymin = 0, ymax = 100, alpha = 0.1) +
  annotate(geom = "text", x = 2850.8, y = 100, label = "敞开段起点") +
  annotate("segment", x= 1308.2, xend= 1918.2, y= 95, yend= 95,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1613.2, y = 100, label = "减速段") +
  annotate("segment", x= 1258.2, xend= 1308.2, y= 95, yend= 95,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1283.2, y = 100, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd2speed


# 2. ZD2行驶轨迹----
# 3. ZD2车道跨越点位置----


# 4. ZD2加速度----
plot.zd2acc <- ggplot(df.zd2, aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-3.5, 3.5), colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(259, 3051.8),
                     breaks = c(1258.2, 1308.2, 1918.2, 3051.8),
                     labels = c("", "", "S2K0+000", "S2K1+133.65")) +
  scale_y_continuous(name = "加速度(m/s2)", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 259, xmax = 2850.8, ymin = -5, ymax = 5, alpha = 0.1) +
  annotate(geom = "text", x = 2850.8, y = 4.5, label = "敞开段起点") +
  annotate("segment", x= 1308.2, xend= 1918.2, y= 5, yend= 5,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1613.2, y = 4.5, label = "减速段") +
  annotate("segment", x= 1258.2, xend= 1308.2, y= 5, yend= 5,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1283.2, y = 4.5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd2acc


# 5. ZD2制动踏板位移----
plot.zd2brakepedal <- ggplot(df.zd2, aes(x = disTravelled, y = brakePedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(259, 3051.8),
                     breaks = c(1258.2, 1308.2, 1918.2, 3051.8),
                     labels = c("", "", "S2K0+000", "S2K1+133.65")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 259, xmax = 2850.8, ymin = 0, ymax = 1, alpha = 0.1) +
  annotate(geom = "text", x = 2850.8, y = 0.95, label = "敞开段起点") +
  annotate("segment", x= 1308.2, xend= 1918.2, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1613.2, y = 0.95, label = "减速段") +
  annotate("segment", x= 1258.2, xend= 1308.2, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1283.2, y = 0.95, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd2brakepedal


# 6. ZD2油门踏板位移----
plot.zd2gaspedal <- ggplot(df.zd2, aes(x = disTravelled, y = gasPedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(259, 3051.8),
                     breaks = c(1258.2, 1308.2, 1918.2, 3051.8),
                     labels = c("", "", "S2K0+000", "S2K1+133.65")) +
  scale_y_continuous(name = "油门踏板位移）", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 259, xmax = 2850.8, ymin = 0, ymax = 1, alpha = 0.1) +
  annotate(geom = "text", x = 2850.8, y = 0.95, label = "敞开段起点") +
  annotate("segment", x= 1308.2, xend= 1918.2, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1613.2, y = 0.95, label = "减速段") +
  annotate("segment", x= 1258.2, xend= 1308.2, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 1283.2, y = 0.95, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zd2gaspedal




