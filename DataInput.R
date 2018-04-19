#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20171215, by MaoY
#
# Description: 导入驾驶模拟试验得到的所有数据，包括小客车和货车。
#------------------------------------------------------------------------------#


# UC-winRoad V12数据重命名----
RenameSimDataV12 <- function(data){
  # 重命名UC-winRoad V12版本输出数据变量名。
  #
  # 输入：UC-winRoad V12版本输出数据。
  # 输出：重命名后的数据框。
  
  kSimDataNewName <- c("simTime",         # Time
                       "logTime",         # TimeStamp
                       "type",            # Type
                       "carModel",        # Model
                       "logID",           # ID
                       "customID",        # customID
                       "logDescription",  # description
                       "positionX",       # position X
                       "positionY",       # position Y
                       "positionZ",       # position Z
                       "yawAngle",        # yawAngle
                       "pitchAngle",      # pitchAngle
                       "rollAngle",       # rollAngle
                       "directionX",      # direction X
                       "directionY",      # direction Y
                       "directionZ",      # direction Z
                       "bodyPitchAngle",  # bodyPitchAngle
                       "bodyRollAngle",   # bodyRollAngle
                       "RPM",             # RPM
                       "gearNumber",      # gearNumber
                       "speedXMS",        # speedVectInMetresPerSecond X
                       "speedYMS",        # speedVectInMetresPerSecond Y
                       "speedZMS",        # speedVectInMetresPerSecond Z
                       "speedKMH",        # speedInKmPerHour
                       "speedMS",         # speedInMetresPerSecond
                       "accXMS2",         # localAccelInMetresPerSecond2 X
                       "accYMS2",         # localAccelInMetresPerSecond2 Y
                       "accZMS2",         # localAccelInMetresPerSecond2 Z
                       "bodyRotSpeedYawRS",   # bodyRotSpeedInRadsPerSecond Yaw
                       "bodyRotSpeedPitchRS", # bodyRotSpeedInRadsPerSecond Pitch
                       "bodyRotSpeedRollRS",  # bodyRotSpeedInRadsPerSecond Roll
                       "bodyRotAccYawRS2",    # bodyRotAccelInRadsPerSecond Yaw
                       "bodyRotAccPitchRS2",  # bodyRotAccelInRadsPerSecond Pitch
                       "bodyRotAccRollRS2",   # bodyRotAccelInRadsPerSecond Roll
                       "rotSpeedYawRS",       # rotSpeedInRadsPerSecond Yaw
                       "rotSpeedPitchRS",     # rotSpeedInRadsPerSecond Pitch
                       "rotSpeedRollRS",      # rotSpeedInRadsPerSecond Roll
                       "rotAccYawRS2",        # rotAccelInRadsPerSecond Yaw
                       "rotAccPitchRS2",      # rotAccelInRadsPerSecond Pitch
                       "rotAccRollRS2",       # rotAccelInRadsPerSecond Roll
                       "disTravelled",        # distanceTravelled
                       "steeringValue",       # steering
                       "appSteering",         # appliedSteering
                       "steeringVelocity",    # steeringVelocity
                       "turningCurvature",    # turningCurvature
                       "gasPedal",            # throttle
                       "pedalTorque",         # pedalTorque
                       "appGasPedal",         # appliedThrottle
                       "brakePedal",          # brake
                       "appBrake",            # appliedBrake
                       "lightState",          # lightState
                       "automaticControl",    # automaticControl
                       "dragForce",           # dragForce
                       "carMass",             # mass
                       "carWheelBase",        # wheelBase
                       "centerOfGravityHeight",    # centerOfGravityHeight
                       "centerOfGravityPosition",  # centerOfGravityPosition
                       "rollAxisHeight",           # rollAxisHeight
                       "trailerState",             # trailer
                       "trailerAngle",             # trailerAngle
                       "trailerPitchAngle",        # trailerPitchAngle
                       "trailerWheelbase",         # trailerWheelbase
                       "isInIntersection",         # inIntersection
                       "roadName",                 # road
                       "disFromRoadStart",         # distanceAlongRoad
                       "latestRoad",               # latestRoad
                       "disFromLatestRoadStart",   # distanceAnlongLatestRoad
                       "disToLeftBorder",          # distanceToLeftBorder
                       "disToRightBorder",         # distanceToRightBorder
                       "carriagewayWidth",         # carriagewayWidth
                       "roadOffset",               # offsetFromRoadCenter
                       "laneOffset",               # offsetFromLaneCenter
                       "logitudinalSlope",         # roadLongitudinalSlop
                       "lateralSlope",             # roadLateralSlop
                       "laneNumber",               # laneNumber
                       "laneWidth",                # laneWidth
                       "laneDirectionX",           # laneDirection X
                       "laneDirectionY",           # laneDirection Y
                       "laneDirectionZ",           # laneDirection Z
                       "laneCurvature",            # laneCurvature
                       "isDrivingForward",         # drivingForwards
                       "speedLimit",               # speedLimit
                       "isSpeedOver",              # speedOver
                       "leftLaneOverlap",          # leftLaneOverLap
                       "rightLaneOverlap",         # rightLaneOverLap
                       "collisionWithUser",        # collicionWithUser
                       "pedestrianNumber",         # pedestriansNumber
                       "roadSurface",              # surface
                       "averageFlux")              # averageFlux
  
  names(data) <- kSimDataNewName
  return(data)
}


library(data.table)


# 设置数据所在目录为工作目录
setwd(dir = "E:/R/MaWan/MawanDrivingSimulatorDataAnalysis/MawanDrivingData")
kFileList <- list.files(pattern = "*.csv")  # 数据文件名
kDataName <- gsub(".csv", "", kFileList)  # 数据集命名
df.dsdata <- data.frame()  # 创建用于存放所有数据的数据框

for(i in 1:length(kDataName)){  # 导入数据
  
  get.file2data <- fread(input = kFileList[i],
                         header = TRUE,
                         sep = "auto",
                         stringsAsFactors = FALSE,
                         data.table = FALSE,
                         select = c(1:89))
  
  get.file2data <- RenameSimDataV12(get.file2data)  # 数据框变量重命名
  
  get.file2data$dsVehicleType <- strsplit(x = kDataName[i], split = "_")[[1]][2]  # 增加模拟车辆类型
  get.file2data$PW <- strsplit(x = kDataName[i], split = "_")[[1]][3]  # 增加模拟车辆比功率
  get.file2data$Scen <- strsplit(x = kDataName[i], split = "_")[[1]][4]  # 增加驾驶模拟场景
  get.file2data$driverID <- strsplit(x = kDataName[i], split = "_")[[1]][5]  # 增加被试驾驶人ID
  get.file2data$driverAge <- strsplit(x = kDataName[i], split = "_")[[1]][6]  # 增加被试驾驶人年龄
  get.file2data$driverSex <- strsplit(x = kDataName[i], split = "_")[[1]][7]  # 增加被试驾驶人性别
  get.file2data$drivingYears <- strsplit(x = kDataName[i], split = "_")[[1]][8]  # 增加被试驾驶人驾龄
  
  df.dsdata <- rbind(get.file2data, df.dsdata)  # 合并导入数据到预先生成的数据框
}


options(scipen = 200)

df.dsdata$speedXMS <- as.numeric(df.dsdata$speedXMS)
df.dsdata$speedYMS <- as.numeric(df.dsdata$speedYMS)
df.dsdata$speedZMS <- as.numeric(df.dsdata$speedZMS)
df.dsdata$speedMS <- as.numeric(df.dsdata$speedMS)
df.dsdata$speedKMH <- as.numeric(df.dsdata$speedKMH)

df.dsdata$accXMS2 <- as.numeric(df.dsdata$accXMS2)
df.dsdata$accYMS2 <- as.numeric(df.dsdata$accYMS2)
df.dsdata$accZMS2 <- as.numeric(df.dsdata$accZMS2)

df.dsdata$disTravelled <- as.numeric(df.dsdata$disTravelled)

df.dsdata$steeringValue <- as.numeric(df.dsdata$steeringValue)
df.dsdata$appSteering <- as.numeric(df.dsdata$appSteering)

df.dsdata$gasPedal <- as.numeric(df.dsdata$gasPedal)
df.dsdata$appGasPedal <- as.numeric(df.dsdata$appGasPedal)

df.dsdata$brakePedal <- as.numeric(df.dsdata$brakePedal)
df.dsdata$appBrake <- as.numeric(df.dsdata$appBrake)

df.dsdata$disFromRoadStart <- as.numeric(df.dsdata$disFromRoadStart)

df.dsdata$disToLeftBorder <- as.numeric(df.dsdata$disToLeftBorder)
df.dsdata$disToRightBorder <- as.numeric(df.dsdata$disToRightBorder)

df.dsdata$carriagewayWidth <- as.numeric(df.dsdata$carriagewayWidth)

df.dsdata$roadOffset <- as.numeric(df.dsdata$roadOffset)
df.dsdata$laneOffset <- as.numeric(df.dsdata$laneOffset)

df.dsdata$laneWidth <- as.numeric(df.dsdata$laneWidth)