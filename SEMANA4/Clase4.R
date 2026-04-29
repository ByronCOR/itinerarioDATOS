getwd()
setwd("C:/Users/SALASC/Desktop/itinerarioDATOS/SEMANA4")

#CARGA DE DATOS DESDE FUENTE ESTRUCTURADAD

datos <- read.csv("Z:\\titanic.csv", sep = ",", quote = "\"", dec = ".")
 
head(datos)
 str(datos)
 summary(datos)
 dim(datos )
 #MANIPULACION DE DDATOS (SELCCION , PROYECCION , VARIABLES DERIVADAS)
 #obtener nombre, genero , edad ,estado de supervivencia , puerto de emvarque
 #de pasajeros de 3ra clase menores a 40a;os
 
 #opcion1
  resultado<- datos[datos$Pclass == 3 & datos$Age < 40 & !is.na(datos$Age), 
                    c("Name","Sex","Age","Embarked")]
#opcion2 
  resultado<- subset(datos,subset = Pclass==3 & Age < 40,
                     select = c(Name,Sex,Age,Survived,Embarked))
  
#opcion3 (Usando la libreria dplyr)
  install.packages("dplyr")
library("dplyr")  

  resultado3 <- datos %>% 
    filter(Pclass == 3 & Age < 40) %>% 
    select(Nombre=Name,Genero=Sex,Edad=Age,EstadoSupervivencia=Survived,PuertoEmbarque=Embarked)%>% 
    arrange(desc(EstadoSupervivencia),Nombre)%>% 
  mutate(RangoEdad = ifelse(Edad < 13, "INFANTE",
                            ifelse(Edad < 25, "JOVEN", "VIEJO")))%>% 
    mutate(NombrePuertoEmbarque = case_when(PuertoEmbarque== "C"~"Cherbourg"
                                            TRUE~"Desconocido"))
    
  
  
                             