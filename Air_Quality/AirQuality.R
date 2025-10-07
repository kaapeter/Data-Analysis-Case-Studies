data("airquality")
View(airquality)
airquality[,"Solar.R"] > 150 & airquality[,"Wind"] > 10
airquality[, "Solar.R"] > 150 & airquality[, "Wind"] > 10
airquality[, "Solar.R"] > 150 | airquality[, "Wind"] > 10
ggplot(data = airquality)

ggplot(airquality, mapping = aes(x = Temp, y = Month))

ggplot(airquality, aes(Temp, Month)) +
  geom_hex() +
  scale_colour_viridis_c() +
  theme_minimal()

