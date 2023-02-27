circos.par(gap.degree = 0, cell.padding = c(0, 0, 0, 0), start.degree = 90)
circos.initialize("a", xlim = c(0, 24))
circos.track(ylim = c(0, 1), bg.border = NA)
circos.axis(major.at = 0:24, labels = NULL, direction = "inside", 
            major.tick.length = mm_y(2))
circos.text(1:24, rep(1, 24) - mm_y(6), 1:24, facing = "downward")

current.time = as.POSIXlt(Sys.time())
sec = ceiling(current.time$sec)
min1 = 34
hour1 = 6
min2 = 40
hour2 = 19

sec.degree = 90 - sec/60 * 360
# arrows(0, 0, cos(sec.degree/180*pi)*0.8, sin(sec.degree/180*pi)*0.8)

min.degree = 90 - min/60 * 360
# arrows(0, 0, cos(min.degree/180*pi)*0.7, sin(min.degree/180*pi)*0.7, lwd = 2)   

sunrise.degree = 90 - hour1/24 * 360 - min1/60 * 360/24
sunset.degree = 90 - hour2/24 * 360 - min2/60 * 360/24

# arrows(0, 0, cos(hour.degree/180*pi)*0.4, sin(hour.degree/180*pi)*0.4, lwd = 2)

draw.sector(start.degree = sunrise.degree,
            end.degree = sunset.degree,
            col = alpha("grey70", 0.75), border = NA, rou2 = 0.99, rou1 = 0.15, cl)
