getwd()
setwd("C:/Users/SALASC/Desktop/itinerarioDATOS/SEMANA4")

#CARGA DE DATOS 
datos <- read.csv("C:/Users/SALASC/Desktop/itinerarioDATOS/titanic.csv")

str(datos)
head(datos)
#CONSULTA Y MANIPULACION
library("dplyr")
#obtener id pasjero ,estado de supervidenci, clase, genero,
#edad, precio
resultado1 <- datos %>%
  filter((Pclass == 1 | Pclass == 2)  & Fare >0)%>%
  select(Id=PassengerId, EstadoSupervivencia = Survived,
         Clase= Pclass,Genero= Sex,Edad=Age,Tarifa=Fare,PuertoEmbarque = Embarked)%>%
  mutate(EdadvsPromedio = case_when(Edad>= mean(Edad,na.rm=TRUE)~"Arriba",
                                    Edad< mean(Edad,na.rm=TRUE)~"Debajo",
                                    TRUE ~"Identerminado"),
         Genero= if_else(Genero=="Male","Hombre","Mujer"))%>%
  arrange(Genero,Clase,desc(Tarifa))
  
#AGRUPAMIENTOS
#n sirve para contar observaciones en el data ser

datos%>%summarise(TotalPasajeros = n(),
            SumaTarifas = sum(Fare),
            TarifaPromedio = mean(Fare),
            EdadMaxima = max(Age,na.rm = TRUE),
            EdadMinima = min(Age,na.rm = TRUE),
            CantidadPuertos = n_distinct(Embarked[Embarked !=""]))


datos%>%group_by(Pclass)%>%summarise(EdadPromedio= mean(Age,na.rm = TRUE),
            TarifaMaxima= max(Fare),
            TarifaMinima= min(Fare[Fare>0]))

datos%>%
  group_by(Pclass)%>%
  summarise(TotaldePasajeros = n(),
            EdadPromedio=mean(Age,na.rm = TRUE),
            TarifaMaxima = max(Fare),
            TarifaMinima= min(Fare[Fare>0]),
            PasajerosMujeres = sum(Sex=="female"),
            PorcentajeMujeres = PasajerosMujeres/TotaldePasajeros*100)%>%
  filter(PorcentajeMujeres>30)

datos%>%
  filter(!is.na(Age))%>%
  group_by(Edad=floor(Age))%>%
  summarise(TotalPasajeros=n())%>%
  filter(TotalPasajeros>10)%>%
  arrange(desc(TotalPasajeros))

datos%>%
  filter(!is.na(Age))%>%
  group_by(Edad=floor(Age))%>%
  summarise(TotalPasajeros=n())%>%
  filter(TotalPasajeros>10)%>%
  arrange(desc(TotalPasajeros))%>%
  slice_head(n=5)

#mean(datos$Age,na.rm)
#combinaciones
#JOINS
estaturas = data.frame(id=c(1:891), estatura =sample(c(145:189),891,replace = TRUE))

r2<- datos%>%
  inner_join(estaturas,by= join_by(PassengerId==id))
TiposTarifa<- data.frame(desde= c(0,30,100),
                         hasta= c(29.99,99.99,9999.99),
                         TipoTarifa=c("Baja","Media","Alta"))
r3<- r2%>%
  inner_join(TiposTarifa,by=join_by(between(Fare,desde,hasta)))%>%
  select(-c(desde,hasta))

