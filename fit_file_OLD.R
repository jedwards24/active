
# Old version of FitFileR --------
dt$file_id #device info
dt$file_creator #version numbers
dt$event #start/stop
dt$device_info %>% glimpse #technical stuff
dt$record #position, HR, power, cadence
dt$lap # lap presses
dt$session # 1 row. session info
dt$activity # similar to session?


# fitdc package------------
library(fitdc)
dt2 <- read_fit(file) #errors
?read_fit
#example
fp <- system.file("extdata/example.fit", package = "fitdc")
msgs <- read_fit(fp)
names(msgs)
length(msgs)
msgs[[200]]
