data <- read.table(file.choose(),sep = ",",header = TRUE,dec = ".")


#fonction qui renvoie le pourcentage des prêts superieur à $900
pourcentage_sup_900 = function(data){
  compteur = 0
  for(i in 1:nrow(data)){
    if(data$Principal[i] > 900){
      compteur = compteur + 1
    }
  }
  
  frequence = compteur *100/nrow(data)
  return(paste(frequence,"%"))
  
}





library(lubridate)

#fonction qui donne la difference de jour entre 2 dates
# la methode mdy() appartient au package lubridate
diff_day = function(a,b){
  # mdy() specifie le format de la date (mois,jour,année)
  date1 = mdy(a) 
  date2 = mdy(b)
  result = difftime(date1,date2,units = "days")
  return(abs(result))     # retourne la valeur absolue
}

# modification de la fonction diff_day en tenant compte de l'heure
# la methode mdy_hm() appartient au package lubridate

modification_diff_day = function(a,b){
  
  #  mdy() specifie le format de la date (mois,jour,année,heure,minute) 
  date1 = mdy_hm(a) 
  date2 = mdy_hm(b)
  result = difftime(date1,date2,units = "days")
  
  # retourne la valeur absolue
  return(abs(result))
  
}

# cette fonction renvoir la frequence des personnes ayant remboursées à temps 
#prend en parametre un data.frame
payer_temps = function(data){
    compteur = 0
    for( i in 1:nrow(data)){
      if(data$paid_off_time[i]== ""){
        i = i+1
      }
      else{
        if( modification_diff_day(paste(data$effective_date[i],"00:00"),data$paid_off_time[i]) <= data$terms[i]){
        compteur = compteur + 1
      }
      }
    }
    frequence = compteur*100/nrow(data)
    return(paste(frequence,"%"))
}


# cette fonction renvoir la frequence des personnes ayant remboursées (même avec recouvrement) 
#prend en parametre un data.frame
loyal = function(data){
  compteur = 0
 
  for( i in 1:nrow(data)){
    if(data$loan_status[i] == "PAIDOFF" | data$loan_status[i] == "COLLECTION_PAIDOFF"){
      compteur = compteur + 1
    }
  }
  frequence = compteur*100/nrow(data)
  return(paste(frequence,"%"))
}


    plus_honnete = function(data){
      client = data$Loan_ID[1]
      j = 1
      for( i in 2:nrow(data)){
  
          if(modification_diff_day(paste(data$effective_date[i],"00:00"),data$paid_off_time[i]) <
             modification_diff_day(paste(data$effective_date[j],"00:00"),data$paid_off_time[j]) ){
                client = data$Loan_ID[i]
                j = i

          }
      }
      
      return (client)
    }
  
    
    #fonction pour obtenir le mode
    #prend en entrée un vecteur
    
    get_mode = function(v){
      uniqv = unique(v) # cree un vecteur sans rédondance
      uniqv[which.max(tabulate(match(v,uniqv)))]
      # which.max renvoi la  position du max, tabulate donne le nombre d'occurrence, match donne la position de la premiere occurrence dans un vecteur
    }
    
    #************************************************************
    
    #question pourcentage case vide 0.1.2
    df = subset(data,is.na(Loan_ID) | data$Loan_ID=="")
    freq = nrow(df)/nrow(data)
    freq
    
    # diagramme en bar   0.2.1
    tab = table(data$loan_status)
    barplot(tab, ylab = "Effectifs", xlab = "categories", main = "Diagramme en bar des effectif")
    
    # diagramme en secteur  0.2.1
    tab = table(data$loan_status)
    pie(tab)
      
    #boîte à moustaches question 0.2.2
    
    Liste = list("paidoff"=Principal[loan_status == "PAIDOFF"], "collection" = Principal[loan_status == "COLLECTION"], "collection_paidoff"= Principal[loan_status == "COLLECTION_PAIDOFF"])
    boxplot(Liste, main = "Boîtes à moustaches des principals en fonction des loan_status" )
    
    
    #diagramme en bar de status question 0.2.2
    
    library(ggplot2)
    ggplot(data = data, aes(x= Principal))+ geom_histogram(binwidth = 100, aes(y = ..density..))
    
    #tableau de données observation par status 0.2.2
    df <- subset(data, T,select = c(loan_status,Principal)) 
    table(df)
    
    #diagramme en bar des effectifs de terms  quest 0.2.3
    barplot(table(data$terms), main = "diagramme en bar des effectifs de terms")
    
    #diagramme en bar de terms en fonction de loan_status ques 0.2.3
    
    df <- subset(data,T,select = c(loan_status, terms))
    tab = table(df)
    barplot(tab, main = "Diagramme en bar de terms en fonction de loan_staus", legend =T)
    
    #diagramme en bar des effectifs par date effective 0.2.4
    
    df <- subset(data,T,select = c(loan_status, effective_date))
    tab = table(df)
    barplot(tab, main ="diagramme en bar des effectifs par date", legend = T, ylim = c(0,250))
    
    # histogramme des âge ques 0.2.5
    
    hist(data$age, main = "histogramme des âges", freq = F,xlab = "Âge", breaks = 5)
    
    
    #boîtes à moustaches des ages par status de prèt 0.2.5
    
    Liste = list("paidoff" = age[loan_status == "PAIDOFF"], "collection" = age[loan_status == "COLLECTION"], "collection_paidoff" = age[loan_status == "COLLECTION_PAIDOFF"])
    boxplot(Liste, main = "Boîtes à moustaches des ages en fonction des status", ylab = "age")
    
    #diagramme en bar de l'education question 0.2.6 
    barplot(table(education), main = "Diagramme en bar des effectifs", ylab = "effectifs", ylim = c(0,300))
    
    # diagramme en bar des effectifs de l'eduction en fonction du status de prèt 0.2.6
    tab = table(subset(data,select = c(loan_status, education)))
    barplot(tab, main ="Diagramme en bar de l'education en fonction de status", legend = T, ylim = c(0,250))
    
    
    #diagramme en bar des effectifs de gender question 0.2.7
    barplot(table(data$Gender), main = "Diagramme en bar du genre", ylim = c(0,550), legend = T)
    
    # diagramme en bar des effectifs de gender en fonction de status de prèt  0.2.7
    tab = table(subset(data, select = c(loan_status, Gender)))
    barplot(tab, main = "Diagramme en bar du genre en fonction des status", legend = T, ylim = c(0,550))
    
    
    #moyenne des montants de prèt  0.3.1
    
    moy_principal = sum(data$Principal)/nrow(data)
    moy_principal
    
    # variance        0.3.1
    
    v =c(data$Principal)
    variance = sum((v - mean(v))^2)/nrow(data)
    variance
    
    #pourcentage des prèts au dessus de $900 0.3.2
    df = subset(data, Principal > 900)
    frequence = 100*nrow(df)/nrow(data)
    frequence
    
    # moyenne des âges des clients ayant effectués un preèt  0.3.3
    v = c(data$age)
    moy_age = mean(v)
    moy_age
    
    
    # moyenne des âges des clients ayant effectués un prèt enn dessous de $600  0.3.4
    
    df = subset(data, Principal < 600)
    v = c(df$age)
    mean(v)
    
    #nombre de prèt avec term d'une semaine 0.3.5
    
    df = subset(data, terms == 7)
    nb = nrow(df)
    nb
    
    # niveau d' etude majoritaire des client ayant effectués un prèt superieur ou egal à $900  0.3.6
    
    df = subset(data, Principal >= 900, select = education)
    table(df)
    
    # plus loyal entre femme et homme       0.3.9
    homme = subset(data, Gender == "male")
    femme = subset(data, Gender == "female")
    loyal(femme)
    loyal(homme)
    
    # entre homme et femme pret superieur à $900    0.3.10
    
    homme = subset(data, Gender == "male")
    femme = subset(data, Gender == "female")
    pourcentage_sup_900(homme)
    pourcentage_sup_900(femme)
    
    # niveau d'etude majoritaire des moins fidèle     0.3.11
    
    moin_fidel = subset(data, !(loan_status == "PAIDOFF"), select = education)
    table(moin_fidel)
    
    
    # term possedant le plus grand nombre de remboursement    0.3.12
    
    df_rembours  = subset(data, loan_status == "PAIDOFF", select = terms)
    table(df_rembours)
    
    # moyenne d'âge des client qui ne rembourse pas   0.3.13
    
    df_rembour_pas = subset(data, loan_status == "COLLECTION")
    v = c(df_rembour_pas$age)
    mean(v)
    
    # question 14
    
    client_rembourser = subset(data, loan_status == "PAIDOFF")
    plus_honnete(client_rembourser)
    
    
    
    
    
    
    
    
    
        
    
        
    
        
