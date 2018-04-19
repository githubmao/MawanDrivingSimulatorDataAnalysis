#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20171217, by MaoY
#
# Description: 分析妈湾ZDA的行驶轨迹，速度，加速度，制动，加速等。
#------------------------------------------------------------------------------#


# 调用数据导入程序DataInput.R
source(file = "E:/R/MaWan/MawanDrivingSimulatorDataAnalysis/DataInput.R", encoding = "utf-8")
# 调用数据导入程序Functions.R
source(file = "E:/R/MaWan/MawanDrivingSimulatorDataAnalysis/Functions.R", encoding = "utf-8")

library(ggplot2)  # 导入绘图用数据包

# 数据切分
df.zdb <- subset(x = df.dsdata, Scen == "ZDB")  # ZDB数据

df.zdbsedan <- subset(x = df.zdb, dsVehicleType == "Sedan")  # ZDB轿车数据
df.zdbtruck <- subset(x = df.zdb, dsVehicleType == "Truck")  # ZDB货车数据


# 1. ZDB行驶速度----
# 1.1 轿车----
plot.zdbsedanspeed <- ggplot(df.zdbsedan, aes(x = disTravelled, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_continuous(name = NULL, limits = c(0, 750),
                     breaks = c(0, 350, 657.2, 737.2),
                     labels = c("BK0+000", "BK0+350", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  annotate(geom = "rect", xmin = 200, xmax = 750, ymin = 0, ymax = 100, alpha = 0.1) +
  annotate(geom = "text", x = 200, y = 100, label = "敞开段终点") +
  annotate("segment", x= 350, xend= 657.2, y= 95, yend= 95,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 503.6, y = 100, label = "加速段") +
  annotate("segment", x= 657.2, xend= 737.2, y= 95, yend= 95,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 697.2, y = 100, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zdbsedanspeed


# 1.2 货车----
df.zdbtruck <- df.zdbtruck[df.zdbtruck$PW != "PW9.3" | df.zdbtruck$driverID != "S0501",]

plot.zdbtruckspeed <- ggplot(df.zdbtruck, aes(x = disFromRoadStart, y = speedKMH)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  scale_x_reverse(name = NULL, limits = c(8070, 7250),
                  breaks = c(8070, 7720, 7412.8, 7332.8),
                  labels = c("BK0+000", "BK0+350", "", "")) +
  scale_y_continuous(name = "速度（km/h）", limits = c(0, 100)) +
  annotate(geom = "rect", xmin = 7250, xmax = 7870, ymin = 0, ymax = 100, alpha = 0.1) +
  annotate(geom = "text", x = 7870, y = 100, label = "敞开段终点") +
  annotate("segment", x= 7720, xend= 7412.8, y= 95, yend= 95,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 7566.4, y = 100, label = "加速段") +
  annotate("segment", x= 7412.8, xend= 7332.8, y= 95, yend= 95,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 7372.8, y = 100, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
       axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5)) +
  facet_wrap(~PW, ncol = 1)

plot.zdbtruckspeed


# 2. ZDB行驶轨迹----
# 2.1 轿车----
# 2.2 货车----

# 3. ZDB车道跨越点位置----
# 3.1 轿车----
# 3.2 货车----

# 4. ZDB加速度----
# 4.1 轿车----
plot.zdbsedanacc <- ggplot(df.zdbsedan, aes(x = disTravelled, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-3.5, 3.5), colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(0, 750),
                     breaks = c(0, 350, 657.2, 737.2),
                     labels = c("BK0+000", "BK0+350", "", "")) +
  scale_y_continuous(name = "加速度(m/s2)", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 200, xmax = 750, ymin = -5, ymax = 5, alpha = 0.1) +
  annotate(geom = "text", x = 200, y = 4.5, label = "敞开段终点") +
  annotate("segment", x= 350, xend= 657.2, y= 5, yend= 5,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 503.6, y = 4.5, label = "加速段") +
  annotate("segment", x= 657.2, xend= 737.2, y= 5, yend= 5,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 697.2, y = 4.5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zdbsedanacc


# 4.2 货车----
plot.zdbtruckacc <- ggplot(df.zdbtruck, aes(x = disFromRoadStart, y = accZMS2)) +
  geom_point(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = c(-3.5, 3.5), colour = "red", linetype = "dashed", size = 1) +
  scale_x_reverse(name = NULL, limits = c(8070, 7250),
                  breaks = c(8070, 7720, 7412.8, 7332.8),
                  labels = c("BK0+000", "BK0+350", "", "")) +
  scale_y_continuous(name = "加速度(m/s2)", limits = c(-5, 5)) +
  annotate(geom = "rect", xmin = 7250, xmax = 7870, ymin = -5, ymax = 5, alpha = 0.1) +
  annotate(geom = "text", x = 7870, y = 4.5, label = "敞开段终点") +
  annotate("segment", x= 7720, xend= 7412.8, y= 5, yend= 5,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 7566.4, y = 4.5, label = "加速段") +
  annotate("segment", x= 7412.8, xend= 7332.8, y= 5, yend= 5,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 7372.8, y = 4.5, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5)) +
  facet_wrap(~PW, ncol = 1)

plot.zdbtruckacc


# 5. ZDB制动踏板位移----
# 5.1 轿车----
plot.zdbsedanbrakepedal <- ggplot(df.zdbsedan, aes(x = disTravelled, y = appBrake)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(0, 750),
                     breaks = c(0, 350, 657.2, 737.2),
                     labels = c("BK0+000", "BK0+350", "", "")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 200, xmax = 750, ymin = 0, ymax = 1, alpha = 0.1) +
  annotate(geom = "text", x = 200, y = 0.95, label = "敞开段终点") +
  annotate("segment", x= 350, xend= 657.2, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 503.6, y = 0.95, label = "加速段") +
  annotate("segment", x= 657.2, xend= 737.2, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 697.2, y = 0.95, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zdbsedanbrakepedal


# 5.2 货车----
plot.zdbtruckbrakepedal <- ggplot(df.zdbtruck, aes(x = disFromRoadStart, y = appBrake)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_reverse(name = NULL, limits = c(8070, 7250),
                  breaks = c(8070, 7720, 7412.8, 7332.8),
                  labels = c("BK0+000", "BK0+350", "", "")) +
  scale_y_continuous(name = "制动踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 7250, xmax = 7870, ymin = 0, ymax = 1, alpha = 0.1) +
  annotate(geom = "text", x = 7870, y = 0.95, label = "敞开段终点") +
  annotate("segment", x= 7720, xend= 7412.8, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 7566.4, y = 0.95, label = "加速段") +
  annotate("segment", x= 7412.8, xend= 7332.8, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 7372.8, y = 0.95, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5)) +
  facet_wrap(~PW, ncol = 1)

plot.zdbtruckbrakepedal


# 6. ZDB油门踏板位移----
# 6.1 轿车----
plot.zdasedangaspedal <- ggplot(df.zdasedan, aes(x = disTravelled, y = gasPedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(name = NULL, limits = c(0, 750),
                     breaks = c(0, 350, 657.2, 737.2),
                     labels = c("BK0+000", "BK0+350", "", "")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 200, xmax = 750, ymin = 0, ymax = 1, alpha = 0.1) +
  annotate(geom = "text", x = 200, y = 0.95, label = "敞开段终点") +
  annotate("segment", x= 350, xend= 657.2, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 503.6, y = 0.95, label = "加速段") +
  annotate("segment", x= 657.2, xend= 737.2, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 697.2, y = 0.95, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5))

plot.zdasedangaspedal


# 6.2 货车----
plot.zdbtruckgaspedal <- ggplot(df.zdbtruck, aes(x = disFromRoadStart, y = appGasPedal)) +
  geom_line(aes(colour = factor(driverID)), size = 1) +
  geom_hline(yintercept = 0.5, colour = "red", linetype = "dashed", size = 1) +
  scale_x_reverse(name = NULL, limits = c(8070, 7250),
                  breaks = c(8070, 7720, 7412.8, 7332.8),
                  labels = c("BK0+000", "BK0+350", "", "")) +
  scale_y_continuous(name = "油门踏板位移", limits = c(0, 1)) +
  annotate(geom = "rect", xmin = 7250, xmax = 7870, ymin = 0, ymax = 1, alpha = 0.1) +
  annotate(geom = "text", x = 7870, y = 0.95, label = "敞开段终点") +
  annotate("segment", x= 7720, xend= 7412.8, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 7566.4, y = 0.95, label = "加速段") +
  annotate("segment", x= 7412.8, xend= 7332.8, y= 1, yend= 1,  arrow=arrow(ends="both", angle=45, length=unit(0.2, "cm"))) +
  annotate(geom = "text", x = 7372.8, y = 0.95, label = "渐变段") +
  theme(legend.position = "none", 
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_line(linetype = "dashed", colour = "black", size = 0.5)) +
  facet_wrap(~PW, ncol = 1)

plot.zdbtruckgaspedal

