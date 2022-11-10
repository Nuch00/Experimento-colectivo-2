funcionEval =  function(datos){
  ref =  which(datos$Predicted==1)
  datos$resultado = 0
  datos$resultado[ref] = -2000
  ref2 = which(datos$clase_ternaria=='BAJA+2')
  datos$resultado[intersect(ref,ref2)] = datos$resultado[intersect(ref,ref2)] + 80000 
  return(sum(datos$resultado))
}

require(data.table)
setwd("C:/Users/herna/Desktop/Maestria/2022-02 DM-EyF/datasets")
dataset_input  <- paste0( "./competenciaFINAL_2022.csv.gz" )
dataset  <- fread( dataset_input )

dataset = dataset[foto_mes == 202107,]
dataset = dataset[,.(numero_de_cliente,clase_ternaria)] 

View(dataset)

setwd("C:/Users/herna/Desktop/Maestria/2022-02 DM-EyF/experimento colectivo")
folders = dir()
folders = folders[which(startsWith(prefix = "ZZ",x= folders ))]


resultados = data.frame()

for(f in 1:length(folders)){
  setwd(paste0("./",folders[f]))
  archivos = dir()
  archivos = archivos[which(startsWith(prefix = "ZZ9420",x= archivos ))]
  for(a in 1:length(archivos)){
    prediccion = read.csv(archivos[a])
    valor=funcionEval(merge.data.table(prediccion, dataset,by="numero_de_cliente"))
    corte = as.numeric(substr(archivos[a],18,22))
    resultados = rbind(resultados,data.frame("nombre" = archivos[a], "corte"=corte,"valor" = valor))
  }
    setwd("./..")
}




