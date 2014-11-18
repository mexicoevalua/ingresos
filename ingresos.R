# Treemaps para el ingresos del gob federal de 1994-2015
# El script organiza los datos y la visualización se hizo con Tableau Public

# Cargar datos armonizados que contienen dos series para 96, 97 y 00
data  <- read.csv("ingresos_1994-2015.csv", encoding="UTF8",
                  stringsAsFactors=F)
str(data)
names(data)
head(data)

# Cambiar formato de los datos a long
require(reshape2)
data  <- melt(data, id.vars= c("year","total"),
              variable.name="concepto", value.name="mdp")
head(data)

data$pctTot  <- with(data, (mdp / total) * 100)
head(data)
str(data$pctTot)

# Cargar datos PIB
source("gdp.R")
head(gdp)
gdp  <- gdp[,-3]

# Unir los dos archivos
ing  <- merge(data, gdp)
head(ing)

# Calcular gasto por ramo como % del pib
ing$conPctPib <- with(ing, (mdp/gdpCorriente)*100)
ing$totIngPctPib  <- with(ing, (total/gdpCorriente)*100)
head(ing)

# Eliminar variables
names(ing)
ing  <- ing[,-6]
head(ing)

# Agregar categorias
###################################
require(plyr)
unique(ing$concepto)
table(ing$concepto)
ing$tipo <- ing$concepto
unique(ing$tipo)

# Petroleros
petroleros  <- c("iepsGasDie","derechos","aprovechamientos","otrPet","pemex")

table(ing$tipo)

for(i in petroleros){
  ing$tipo  <- gsub(i,"Petroleros",ing$tipo)
}
table(ing$tipo)

# No Petroleros
noPetroleros  <- c("isr","ietu", "iva","ieps","importacion","ide","otros",
                   "NoTrib","entControlDir","entDesinc","cfe","imss","issste",
                   "entDesinc","lfc")
for(i in noPetroleros){
  ing$tipo  <- gsub(i,"No petroleros",ing$tipo)
}
table(ing$tipo)

# Cambiar las etiquetas de los valores
#####
table(ing$concepto)
unique(ing$concepto)
f  <- unique(ing$concepto)
f  <- as.character(f)
t  <- c("IEPS de gasolina y diesel","Aprovechamientos","Total Gobierno Federal petroleros","Total petroleros", 
        "CFE","Otros", "ISR","Derechos","Otros petroleros", "Pemex", "IEPS","IMSS","Total tributarios","IETU","ISSSTE",
        "IDE","IVA", "Importación", "Total Gobierno Federal no petroleros", "No tributarios", "Entidades desincorporadas",
        "LFC", "Total no petroleros", "Entidades de control directo")
ing$concepto  <- mapvalues(x = ing$concepto,from = f, to = t)
table(ing$concepto)
unique(ing$concepto)

# Eliminar Entidades de control directo para no duplicar cifras con CFE, LFC, etc.
ing <- subset(ing, ing$concepto != "Entidades de control directo")

# Mantener solo ing petroleros y no petroleros
petNoPet  <- subset(ing, ing$tipo == "Petroleros" | ing$tipo =="No petroleros")
table(petNoPet$tipo)
head(petNoPet)

#Verificar que todos los porcentajes sumen 100
ddply(petNoPet,.(year), summarise,
      sum = sum(pctTot,na.rm=T))
# Reemplazar NAs con 0 para mantener el tamaño del rectángulo constante
# Exportar datos como csv
write.csv(petNoPet, "ing_pet_noPet_1994_2013.csv", fileEncoding="utf8", row.names=F,na="0")
  