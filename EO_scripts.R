####Librerias
install.packages("bookdown")
install.packages("tidyverse")
install.packages("pie3D")
install.packages("plotrix")
install.packages("pie3D")
install.packages("lubridate",dependencies = TRUE)
install.packages('forecast')
install.packages('gganimate')
install.packages('gifski')
#install.packages('gapminder')

library(forecast)
library(lubridate)
library(tidyverse)
library(readr)
library(markdown)
library(plotrix)
library(gganimate)
library(gifski)
#library(gapminder)

##Cargando el origen de datos
Informe_de_virus <- read_delim("Informe de virus.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Informe_de_virus)


##Primera consulta
Pie_tipo=Informe_de_virus%>%select(`Object type`)%>%group_by(`Object type`)%>%summarise(n_incidencias = n())
pie3D(Pie_tipo$n_incidencias,labels = Pie_tipo$`Object type`,explode = 0.2,border = "white", main = "Proporcion de inc. por tipo de malware")

##Segunda consulta
(dis_infectados=Informe_de_virus%>%select(Device,`Object type`)%>%group_by(Device,`Object type`)%>%summarise(n_incidencias = n())%>%arrange(desc(n_incidencias)))
(dis_masinfectados=Informe_de_virus%>%select(Device,`Object type`)%>%group_by(Device,`Object type`)%>%summarise(n_incidencias = n())%>%arrange(desc(n_incidencias))%>%head(15))

ggplot(dis_masinfectados, aes(x=Device, y=n_incidencias,color=`Object type`)) + geom_count(size=7)

##Tercera consulta
(devuserinfectados=Informe_de_virus%>%select(Device,`Object type`,Action,Details,Account)%>%group_by(Device,`Object type`,Action,Account)%>%summarise(n_incidencias = n())%>%arrange(desc(n_incidencias))%>%head(10))

ggplot(devuserinfectados, aes(x=Account, y=n_incidencias,color=`Object type`)) + geom_count(size=7)


##Cuarta consulta
tiempo_detectado=Informe_de_virus%>%select(`Detected at`,Device,Account,`Object type`,`Path to file`,Details,Action)

glimpse(tiempo_detectado)

fecha_tiempo=parse_date_time(tiempo_detectado$`Detected at`,"dmy HM")

ft=as.data.frame(fecha_tiempo)
ft$fecha=date(ft$fecha_tiempo)

ft$tiempo=format(ft$fecha_tiempo,format = "%H:%M")

##qplot(data=ft, x=ft$fecha, y=ft$tiempo)
qplot(data=ft, x=ft$fecha)
#ggplot(data=ft,
#       aes(x=ft$fecha,
#           y=conteo)) +
#  geom_line()

##qplot(data=ft, x=ft$tiempo)

##Quinta pregunta

tseleccion=tiempo_detectado%>%mutate(fecha_hora=parse_date_time(tiempo_detectado$`Detected at`,"dmy HM"))%>%
  filter(fecha_hora>= as.Date('2022-04-27'),
         fecha_hora<= as.Date('2022-05-05'))%>%
  group_by(horas=floor_date(fecha_hora,unit = 'hour'))%>%
  summarise(conteo=n())

## rellenando lo ceros
horas_completas=data.frame(
  horas=seq(floor_date(min(tseleccion$horas),unit = 'hour'),
            floor_date(max(tseleccion$horas),unit = 'hour'),
            by='hour'))

##left join con horas
tseleccion_hora=horas_completas%>%group_by(horas_redondeadas=floor_date(horas,unit = 'hour'))%>%
  left_join(tseleccion)%>%
  mutate(conteo= ifelse(is.na(conteo),0,conteo))
##grafica inicial
ggplot(data=tseleccion_hora,
       aes(x=horas,
           y=conteo)) +
  geom_line()

##Creando el objeto ts para el modelo
conteo_ts=ts(tseleccion_hora$conteo,
             start = 1,
             frequency = 24)


ajuste=auto.arima(y=conteo_ts)

summary(ajuste)

predicciones=forecast(ajuste)

min(predicciones[['lower']])
max(predicciones[['upper']])

##grafica de predicciones
p_predict=autoplot(predicciones)

p_predict

#####comprobando la prediccion 

tseleccion2=tiempo_detectado%>%mutate(fecha_hora2=parse_date_time(tiempo_detectado$`Detected at`,"dmy HM"))%>%
  filter(fecha_hora2>= as.Date('2022-04-11'),
         fecha_hora2<= as.Date('2022-05-10'))%>%
  group_by(horas2=floor_date(fecha_hora2,unit = 'hour'))%>%
  summarise(conteo2=n())

## rellenando lo ceros
horas_completas2=data.frame(
  horas2=seq(floor_date(min(tseleccion2$horas2),unit = 'hour'),
            floor_date(max(tseleccion2$horas2),unit = 'hour'),
            by='hour'))

##left join con horas
tseleccion_hora2=horas_completas2%>%group_by(horas_redondeadas2=floor_date(horas2,unit = 'hour'))%>%
  left_join(tseleccion2)%>%
  mutate(conteo2= ifelse(is.na(conteo2),0,conteo2))
##grafica inicial
ggplot(data=tseleccion_hora2,
       aes(x=horas2,
           y=conteo2)) +
  geom_line()

###########################################

##Sexta consulta

tseleccion3=tiempo_detectado%>%
  mutate(fecha_hora3=parse_date_time(tiempo_detectado$`Detected at`,"dmy HM"))%>%
  group_by(horas3=floor_date(fecha_hora3,unit = 'hour'))%>%
  summarise(conteo3=n())

tseleccion4=tiempo_detectado%>%
  mutate(fecha_hora3=parse_date_time(tiempo_detectado$`Detected at`,"dmy HM"))%>%
  group_by(horas3=floor_date(fecha_hora3,unit = 'hour'))%>%
  summarise(conteo3=n())

tiempo_detectado
glimpse(tiempo_detectado)

fecha_momento=parse_date_time(tiempo_detectado$`Detected at`,"dmy HM")

df=as.data.frame(fecha_momento)
df$fecha=date(df$fecha_momento)
df$tiempo=format(df$fecha_momento,format = "%H:%M")

dt=as.character(df$tiempo)
class(dt)
#du=df%>%df$tiempo%>%group_by(df$tiempo)

du=dt%>%select(dt$tiempo)%>%group_by(dt$tiempo)%>%summarise(nnumeros=n())

qplot(data=df, x=df$fecha, y=df$tiempo)
qplot(data=df, x=df$fecha)
qplot(data=df, x=df$tiempo)

ggplot(df, aes(x=`fecha`, y=`tiempo`)) + geom_line()




#####comprobando la prediccion 

tseleccion10=tiempo_detectado%>%mutate(fecha_hora10=parse_date_time(tiempo_detectado$`Detected at`,"dmy HM"))%>%
  group_by(horas10=floor_date(fecha_hora10,unit = 'hour'))%>%
  summarise(conteo10=n())

## rellenando lo ceros
horas_completas10=data.frame(
  horas10=seq(floor_date(min(tseleccion10$horas10),unit = 'hour'),
             floor_date(max(tseleccion10$horas10),unit = 'hour'),
             by='hour'))

##left join con horas
tseleccion_hora10=horas_completas10%>%group_by(horas_redondeadas10=floor_date(horas10,unit = 'hour'))%>%
  left_join(tseleccion10)%>%
  mutate(conteo10= ifelse(is.na(conteo10),0,conteo10))
##grafica inicial
ggplot(data=tseleccion_hora10,
       aes(x=horas10,
           y=conteo10)) +
  geom_line() + transition_reveal(horas10)












