# datas come from https://data.oecd.org/interest/long-term-interest-rates.htm

library(tidyverse)
library(depmixS4)
library(gridExtra)
library(reshape2)

source("R/load_data.R")

# Préparation des données 
{
  path <- "data/"
	DATA <- load_csv("interest.csv", path)

	head(DATA)

	DATAFR = DATA[ DATA[,1]=="FRA",] 
	DATAFR$TIME
	# On change de rythme..
	#Il faut tronquer pour ne prendre en compte que les données trimestrielles
	 
	tokeep = grep("Q",DATAFR$TIME)
	DATAFRQ  = DATAFR[tokeep,]
	InterestRate = ts(DATAFRQ$Value,start=c(1960,1), frequency = 4)

	plot(InterestRate)
	
	save(InterestRate, file = paste0(path, "InterestRate.Rdata"))
	rm(list=setdiff(ls(), c("path", lsf.str())))
}

# Premier traitement des données : on considère les log-rendements  
{
	Z <- load_Rdata("InterestRate.Rdata", path)
	logZ 	= log(Z+5) #on décalle de 1 pour éviter log(0) 
	logZ.d 	= diff(logZ)
	
	plot(logZ.d)	
}

# Modélisation HMM à espace d'états discret des log-rendemens
{
	# Première vérification regardons, par curiosité, si le signe du log-rendement à l'instant "t" influe sur distribution du log-rendement à l'instant t+1  
	{
		n 		= length(logZ.d)
		
		# Identification des instants où les log rendements sont négatifs et où les log-rendements sont positifs
		{
			Tneg = which(logZ.d<0)
			Tpos = which(logZ.d>0)
			
			# L'objectif étant de comparer les lois des log-rendements suivant les instants de Tneg ou de  Tpos,
			# il ne faut pas que Tneg ou Tpos soient égaux à la taille de la série : n
			
			Tneg = Tneg[Tneg!=n]
			Tpos = Tpos[Tpos!=n]
		}
		
		# Comparaisons des deux distributions conditionnelles  Loi(logZ.d(t+1)|logZ.d(t)<0) et Loi(logZ.d(t+1)|logZ.d(t)>0) 
		{
			dev.new()
			hist(logZ.d[Tneg+1],breaks=10)
			dev.new()
			hist(logZ.d[Tpos+1],breaks=10)
			# D'aspect les deux lois sont différentes
			
			mean(logZ.d[Tneg+1]) 
			mean(logZ.d[Tpos+1]) 
			
			
			t.test(logZ.d[Tneg+1], logZ.d[Tpos+1])
			#Les deux distributions n'ont pas la même moyenne
			
			var.test(logZ.d[Tneg+1],logZ.d[Tpos+1])
			#Là la p-valeur n'est pas si petite (tout de même inférieur à 5%)
			
		}
		# Il semble judicieux d'utiliser un modélisation HMM de la série logZ.d
		
	}

	# Modélisation HMM 
	{
		# Commençons par ajuster un HMM à deux états cachés 
		{
			#On s'inspire du code de https://www.r-bloggers.com/2018/11/hidden-markov-model-example-in-r-with-the-depmixs4-package/
		
			logZ.d.DF = as.data.frame(logZ.d)
			names(logZ.d.DF) = 'logRendement'
			mod <- depmix(logRendement ~ 1, data = 	logZ.d.DF , nstates = 2, family = gaussian()) # use gaussian() for normally distributed data
			
			fit.mod <- fit(mod)
			 
			summary(fit.mod)
			# Commenter le modèle ajusté
			
			
			
			est.states <- posterior(fit.mod)
			table(est.states$state)
		  
			AIC(fit.mod)
			plot(est.states$state)
			
			fit.mod@response
			
			#Fonction d'extraction des parametres d'emission du modèle (pas de méthode dédiée 
			emissionDepmix <- function(fit.mod)
			{
				r 			<- fit.mod@nstates
				nPars 		<- length(getpars(getmodel(fit.mod,"response",1)))
				parsNames 	<- names(getpars(getmodel(fit.mod,"response",1)))
				emissionPars <- matrix(0,0,nPars)
				for(x in 1:r)
				{
					emissionPars <- rbind(emissionPars,getpars(getmodel(fit.mod,"response",x)))
				}
				emissionPars <- as.data.frame(emissionPars)
				names(emissionPars)<-parsNames
				return(emissionPars)
			}
			
			emissionPars <- emissionDepmix(fit.mod)
			
			MU 	= emissionPars[,1]
			SD  =  emissionPars[,2]
			
			par(mfrow=c(1,1))
			plot(MU,SD)
		 
			 
			MULogRendements = MU[est.states$state]
			MULogRendements = ts(MULogRendements,start = time(Z)[2], frequency =frequency(Z )  )
			SDLogRendements = SD[est.states$state]
			SDLogRendements = ts(SDLogRendements,start = time(Z)[2], frequency =frequency(Z )  )
			
		
			# The dualplot() function:
			source("https://gist.githubusercontent.com/ellisp/4002241def4e2b360189e58c3f461b4a/raw/e959562be9e7a4d919a9c454d8b1b70cde904ab0/dualplot.R")     

			#=================Different starting points==================
			# Load some example data from the stock exchange
			# Fonterra data only available from Yahoo for the past few years
		 
			# default - a cross over point happen at the earliest point of the shorter series
			dualplot(x1 = time(Z), y1 =Z,
					 x2 = time(MULogRendements), y2 = MULogRendements,
					 ylim1 = c(min(Z),max(Z)), ylim2 = c(min(MULogRendements),max(MULogRendements)),
					 
					 ylab1 = "InterestRate", ylab2 = "MULogRendements", 
					 legx = "je n'en veut pas", main = "LogRendements")
			dev.new()
			dualplot(x1 = time(Z), y1 =Z,
					 x2 = time(SDLogRendements), y2 = SDLogRendements,
					 ylim1 = c(min(Z),max(Z)), ylim2 = c(min(SDLogRendements),max(SDLogRendements)),
					 
					 ylab1 = "InterestRate", ylab2 = "SDLogRendements", 
					 legx = "je n'en veut pas", main = "LogRendements")
		
			#Commenter l'effet du mélange
			
					 	
		}
		
		# Créons une fonction HMMaic qui sélectionne le modèle avec le meilleur AIC
		{
			HMMaic <- function(DATA, VarName, M)
			{
				aicVec = rep(0,M) 
				logLikVec = rep(0,M) 
				bestAic = Inf 
				bestFit = NA
				bestr 	= 0
				for (r in 1:M)
				{
					eval(parse(text = paste("mod <- depmix(",VarName," ~ 1, data = 	DATA, nstates = ",r,", family = gaussian()) ")))
					fit.mod <- fit(mod)
					aicVec[r]<-  AIC(fit.mod)
					logLikVec[r]<-  logLik(fit.mod)
					if(aicVec[r]<bestAic)
					{
						bestr <- r
						bestAic <-aicVec[r]
						bestFit <-fit.mod
						
					}
					
				}
				
				 
				
				 return(list(r  = bestr , aic = bestAic,fit.mod = bestFit,aicVec=aicVec,logLikVec=logLikVec))
			}
			
			M=5
			List <- HMMaic(DATA=logZ.d.DF,VarName='logRendement',M=M)	
			List$r
			# List$r = M : la valeur max 
			# Augmentons la valeur de M
			M =10
			List <- HMMaic(DATA=logZ.d.DF,VarName='logRendement',M=M)	
			List$r
			# List$r < M 
			par(mfrow=c(2,1))
			plot(1:M,List$logLikVec,xlab = "r",ylab="LogLikelihood")
			plot(1:M,List$aicVec,xlab = "r",ylab="aic")
			
			
			r <- List$r
			r 
			
			fit.mod = List$fit.mod			
			summary(fit.mod)
			# Commenter le modèle ajusté 
			# Un oeil sutr les paramêtres
			{
				#Parametres d'emmission
				{
					emissionPars <- emissionDepmix(fit.mod)
					
					MU 	= emissionPars[,1]
					SD  =  emissionPars[,2]
					
					par(mfrow=c(1,1))
					plot(MU,SD,xlab="mu",ylab="sd")
				}
				
				# Paramètres de transition
				{
					#Fonction d'extraction de la matrice de transition d'un depmixS4 (pas de méthode dédiée)
					transitionDepmix <- function(fit.mod)
					{
						# on récupère le r
						r <- fit.mod@nstates
						
						transition_mat <- matrix(0,r,r)
						#état par état x on récupère la x^eme ligne de la matrice de transition
						for(x in 1:r)
						{
							transition_mat[x,] <-  getpars(getmodel(fit.mod,"transition",x))
						} 
						
						return(transition_mat)				
					}
					
					transition_mat <- transitionDepmix(fit.mod)
					plot(MU,SD,xlab="mu",ylab="sd")
					transition_mat
					library(lattice)
					levelplot(transition_mat) #mmhh il a transposé
					levelplot(t(transition_mat),xlab = "Xt+1",ylab="Xt") 
				}
				
				# Transition des MU et des SD
				{
					x11()
					factLim = 1.5
					levelplot(t(transition_mat),xlab = "MUt+1",ylab="MUt",row.values = MU,column.values = MU,xlim=c(min(MU)*factLim,max(MU)*factLim),ylim=c(min(MU)*factLim,max(MU)*factLim)) 
					x11()
					factLim = 1.1
					levelplot(t(transition_mat),xlab = "St+1",ylab="St",row.values = SD,column.values = SD,xlim=c(0,max(SD)*factLim),ylim=c(0,max(SD)*factLim)) 
					
				}
				
			}
			
			
			 

			# Loi a posteriori des etats cachés et estimateur de Viterbi
			{
				est.states <- posterior(fit.mod)
				table(est.states$state)
				
				
				MULogRendements = MU[est.states$state]
				MULogRendements = ts(MULogRendements,start = time(InterestRate)[2], frequency =frequency(InterestRate )  )
				
				SDLogRendements = SD[est.states$state]
				SDLogRendements = ts(SDLogRendements,start = time(InterestRate)[2], frequency =frequency(InterestRate )  )
				dev.new()
				dualplot(x1 = time(InterestRate), y1 =InterestRate,
						 x2 = time(MULogRendements), y2 = MULogRendements,
						 ylim1 = c(min(InterestRate),max(InterestRate)), ylim2 = c(min(MULogRendements),max(MULogRendements)),
						 
						 ylab1 = "InterestRate", ylab2 = "MULogRendements", 
						 legx = NULL, main = "Taux d'intêret et espérance des lois d'emmission des logs-rendements ")
				
				dev.new()
				dualplot(x1 = time(InterestRate), y1 =InterestRate,
						 x2 = time(SDLogRendements), y2 = SDLogRendements,
						 ylim1 = c(min(InterestRate),max(InterestRate)), ylim2 = c(min(SDLogRendements),max(SDLogRendements)),
						 ylab1 = "InterestRate", ylab2 = "SDLogRendements", 
						 legx = NULL, main = "Taux d'intêret et variances des lois d'emmission des logs-rendements ")
				
				
			
			}
			
		}
		
		# Prédiction des log-rendements 
		{
					
			
			# extraction de la matrice de transition 
			transition_mat <- transitionDepmix(fit.mod)
			
			# extraction de la loi de filtrage : P(Xn = x | Y1,...,Yn) pour tout x: loi initiale pour les observation futures : Yn+1,Yn+2,...
			prior_vec <- as.numeric(posterior(fit.mod)[n,-1])
			prior_vec
			
			# Espéranceq de Y conditionnellement X=x, pour tout x 
			MU

			# pour  n + 1
			# l'espérance de Yn+1 |Y1,...Yn) =doubleSomme(E(Yn+1|Xn+1=x1)*P(Xn+1 = x1| Xn = x0 ) (prior_vec %*% transition_mat)
		 
			sum(MU * (prior_vec %*% transition_mat))

			
			# pour  n + 2
			# l'espérance de Yn+2 |Y1,...Yn =doubleSomme(E(Yn+2|Xn+2=x2)*P(Xn+2 = x2| Xn = x0 ) (prior_vec %*% transition_mat %*% transition_mat)
			sum(MU * (prior_vec %*% transition_mat %*% transition_mat))

			# pour  n + 3
			# l'espérance de Yn+3 |Y1,...Yn) =doubleSomme(E(Yn+3|Xn+3=x3)*P(Xn+3 = x3| Xn = x0 ) (prior_vec %*% transition_mat %*% transition_mat %*% transition_mat)
		
			sum(MU * (prior_vec %*% transition_mat %*% transition_mat %*% transition_mat))

			library(expm) #pour faire des puissances de matrices 
			prior_vec 
			prior_vec %*% (transition_mat %^%1)
			prior_vec %*% (transition_mat %^%2)
			prior_vec %*% (transition_mat %^%3)
			prior_vec %*% (transition_mat %^%5)
			prior_vec %*% (transition_mat %^%10)
			prior_vec %*% (transition_mat %^%50)
			prior_vec %*% (transition_mat %^%100)
			rep(1/r,r) %*% (transition_mat %^%100)
			# Lorsque l'horizon est grand, la loi conditionnelle ne dépend plus de la loi initiale et s'approche de la loi stationnaire de la chaîne de Markov (propirété d'oubli de la condition initiale)
			
			#Pour la variance : 
			YsquareByState <- MU^2+SD^2
			
			# pour  n + 1
			# Variance de Yn+1 |Y1,...Yn =doubleSomme( E(Yn+1^2|Xn+1=x1)*P(Xn+1 = x1| Xn = x0) - [l'espérance de Yn+1 |Y1,...Yn]^2
			sqrt(sum(YsquareByState * (prior_vec %*% transition_mat)) - sum(MU * (prior_vec %*% transition_mat))^2)
			
			# pour  n + 2
			sqrt(sum(YsquareByState * (prior_vec %*% (transition_mat%^%2  ))) - sum(MU * (prior_vec %*%  (transition_mat%^%2  )))^2)
			
			# pour  n + 3
			sqrt(sum(YsquareByState * (prior_vec %*% (transition_mat%^%3 ))) - sum(MU * (prior_vec %*%  (transition_mat%^%3  )))^2)
			# L'écart-type de prédiction reste constant 
			
			Pred = matrix(0,4,2)
			
			for(h in 1:4)
			{
				Pred[h,1] = sum(MU *prior_vec %*% (transition_mat %^%h))
				Pred[h,2] = sqrt(sum(YsquareByState * (prior_vec %*% (transition_mat%^%h  ))) -  Pred[h,1]^2)
			}
			
			# prédiction des quatres prochain log rendements et de l'écart-type de prédiction
			Pred 
		}
		
		# Approche trajectorielle : méthode de Monte-Carlo pour la prédiction des taux d'intêret
		{
			nMC = 1000 # nombre de trajectoires 
			H  	= 4 #horizon : quatre trimestres
			
			# On commence par simuler des trajectoires de log-rendements
			# On part de la loi initiale correspondant à la loi de filtrage Loi de Xn | Y1,...Yn
			prior_vec 
			Yforecast = matrix(0,H+1,nMC)
			for(k in 1:nMC)
			{
				Yforecast[1,k] = logZ.d[n] # La valeur initiale prédiction à horizon 0 on connait : c'est la valeur finale de la série
				
				x0 				= sample(1:r,1,prob=prior_vec)
				Xforecast 		= rep(0,H+1) #attention au décalage Xpred[h+1] correspond à Xh 
				Xforecast[1] 	= x0 
				for(h in 1: H)
				{
					probPassage = transition_mat[Xforecast[h],]
					Xforecast[h+1] = sample(1:r,1,prob=probPassage)
					Xforecast[h+1] = sample(1:r,1,prob=probPassage)
					Yforecast[h+1,k] = rnorm(1,mean = MU[Xforecast[h+1]],sd=  SD[Xforecast[h+1]])
				}
			}
			par(mfrow=c(1,1))
			
			Yforecast = ts(Yforecast,start=time(logZ.d)[n],frequency = frequency(logZ.d))
			ts.plot(Yforecast,col=1:nMC)  # nMC trajectoires des log-rendements : la première valeur est inutile pour la suite
			
			# Maintenant on revient à logZ = log(Z+5) = log(InterestRate + 5)
			logZForecast	= matrix(0,H+1,nMC)
			logZForecast[1,] = logZ[n+1]
			for(h in 2: (H+1))
			{
				logZForecast[h,] <- logZForecast[h-1,] + Yforecast[h,]
			}
				
			logZForecast = ts(logZForecast,start=time(logZ)[n+1],frequency = frequency(logZ))
			ts.plot(logZForecast,col=1:nMC)  # nMC trajectoires des log-rendements : la première valeur est inutile pour la suite
		
			# Finalement on revient à InterestRate
			ZForecast = exp(logZForecast)- 5
			ts.plot(ZForecast,col=1:nMC)  # nMC trajectoires des log-rendements : la première valeur est inutile pour la suite
			ts.plot(Z,ZForecast,col=c(1,1:nMC))  # nMC trajectoires des log-rendements : la première valeur est inutile pour la suite
			ts.plot(Z,ZForecast,col=c(1,1:nMC),xlim = c(2020, time(ZForecast)[H+1]))  # nMC trajectoires des log-rendements : la première valeur est inutile pour la suite
			
			# Serons nous de ces trajectoires pour prédire le taux d'interêt moyen, médian, quantiles à 97.5%, 2.5%, 99.5%,0.5% 
			MeanForecast 	= ts(apply(ZForecast,1, mean),start=time(logZ)[n+1],frequency = frequency(logZ))
			MedianForecast 	= ts(apply(ZForecast,1, FUN = function(x){ quantile(x,0.5) }),start=time(logZ)[n+1],frequency = frequency(logZ))
			Q0975Forecast 	= ts(apply(ZForecast,1, FUN = function(x){ quantile(x,0.975) }),start=time(logZ)[n+1],frequency = frequency(logZ))
			Q0025Forecast 	= ts(apply(ZForecast,1, FUN = function(x){ quantile(x,0.025) }),start=time(logZ)[n+1],frequency = frequency(logZ))
			Q0995Forecast 	= ts(apply(ZForecast,1, FUN = function(x){ quantile(x,0.995) }),start=time(logZ)[n+1],frequency = frequency(logZ))
			Q0005Forecast 	= ts(apply(ZForecast,1, FUN = function(x){ quantile(x,0.005) }),start=time(logZ)[n+1],frequency = frequency(logZ))
		
			ts.plot(Z,MeanForecast,MedianForecast,Q0975Forecast,Q0025Forecast,Q0995Forecast,Q0005Forecast,col=c("black","blue","green","orange","orange","red","red"),lty=c(1,1,2,2,2,2,2),xlim = c(2020, time(ZForecast)[H+1]))  # nMC trajectoires des log-rendements : la première valeur est inutile pour la suite
			legend("topleft",c("Taux d'intêret","Préd. Moyenne ","Préd. Médiane","Préd. Quantile 0.975","Préd. Quantile 0.025","Préd. Quantile 0.995","Préd. Quantile 0.005"),col=c("black","blue","green","orange","orange","red","red"),lty=c(1,1,2,2,2,2,2))
		}
	}
}
	
# Autre méthode : Modélisation ARIMA 
{

	# Stationnarisation
	{
		
		dev.new()
		par(mfrow=c(3,1))
		plot(InterestRate,main = "Interest Rate")
		acf(InterestRate)
		pacf(InterestRate)
		 

		X = InterestRate 
		 
		
		X.d = diff(X) 	
		
		dev.new()
		par(mfrow=c(2,1))
		plot(X,main = "X")
		plot(X.d,main = "X.d")
		# Variation d'amplitude des variations : plus les valeurs de la série sont élevéds ,plus les amlitudes de variation sont fortes : passons au log
		
		logX = log(X+5) #on décalle de 1 pour éviter log(0) 
		logX.d = diff(logX) 	
		
		dev.new()
		par(mfrow=c(2,2))
		plot(X ,main = "X ")
		plot(logX ,main = "logX ")
		plot(X.d,main = "X.d")
		plot(logX.d ,main = "logX ")
		
		dev.new()
		par(mfrow=c(3,1))
		plot(logX.d ,main = "diff(logX) ")
		acf(logX.d)
		pacf(logX.d)

		# bon c'est déjà stationnaire. 
		}
		
	#Modélisation par AR(1) ? 
	{
		mean(logX.d)
		
		p = 1
		q = 0
		
		out.logX.d <- arima(logX.d, order=c(p,0,q) ,include.mean = FALSE)

		
		res =out.logX.d$residuals
			
		dev.new()
		par(mfrow=c(3,1))
		plot(res,main = "résidus")
		acf(res)
		pacf(res)
		
		Box.test(res,type = "Ljung-Box",fitdf = p + q+1 ,lag=20)
		# OK pour l'hypothèse d'indépendance 
		
		
	}

	#prédiction
	{
		out.logX <- arima(logX, order=c(p,1,q) ,include.mean = FALSE)

		h = 4
		 
		predList = predict(	out.logX,n.ahead=h)
		# Object de type list avec l'attribut pred : valeurs prédites 
		# et  l'attribut se : écart-types de prédictions qui nous permmettrons de construire des intervalles de prédiction
		
		predList

		PredLin 	= predList$pred
		#intervalles de prédiction à 95%
		PredLinUp 	= PredLin  + 1.96*predList$se     
		PredLinLow  = PredLin  - 1.96*predList$se  
		
		dev.new() 
		ts.plot(logX,PredLin,PredLinUp,PredLinLow,col = c("black","red","orange","orange"),lty  = c(1,1,2,2),main="Prédiction linéaire")
		#prédiction des log-rendements 
		
		
		# si logX = log(X+5)
		# X = exp(logX) - 5
		
		predX 			= exp(PredLin)-5
		predXUp 		= exp(PredLinUp)-5
		predXLow 		= exp(PredLinLow)-5
		
		dev.new() 
		ts.plot(X,predX,predXUp,predXLow,col = c("black","red","orange","orange"),lty  = c(1,1,2,2),main="Prédiction linéaire",xlim=c(2020,max(time(predX))))
		#

	}
		

}

