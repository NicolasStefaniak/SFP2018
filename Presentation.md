---
title: "Du p-hacking au paradis de l’approche bayesienne."
author: "Nicolas Stefaniak"
date: "5 septembre 2018"
output:
  ioslides_presentation:
    css: styles.css
    logo: logo.png
    incremental: true
    duration: 15
---

# Le biais de publication : "Publish or perish !"


## 


Fanelli :

- 2010 : 90% des études sont des résultats positifs.


- 2011 : augmentation de 22% de la publication des résultats positifs.



- 2009 : quelle est la proportion de chercheurs ayant des pratiques douteuses ? 


## 


Fanelli :

>- 2010 : 90% des études sont des résultats positifs.


>- 2011 : augmentation de 22% de la publication des résultats positifs.


>- 2009 : quelle est la proportion de chercheurs ayant des pratiques douteuses ? 


-------------------------------------------------------------------
                        Fabrication/modification     Autres pratiques
---------------------- ---------------------------- --------------------
J'ai pratiqué                    1,97%                 14,12%

Je connais quelqu'un               33,7%                 72%
-------------------------------------------------------------------




##


  ![](Prinz1.png)

 <div class="centered"> 
 ![](Prinz2.png)
</div>

##

  ![](Francis.png)

Le test d'excès de significativité dans Psychological Science entre 2009 et 2012 obtenu par la formule :

$$ puissance^n $$
où n est le nombre d'expérimentations

- 82% (34/44) des études sont problématiques


# Le problème de la puissance statistique

## 
  ![](ion.png)

$$ PPV = \frac{R (1- \beta)}{R+1-(1-\alpha)-R\beta} $$

Où R représente le ratio entre les études qui font l'hypothèse que le lien existe et celles qui font l'hypothèse contraire. 



## {.flexbox .vcenter}


Un exemple concret

Si la puissance = 0,80, $\alpha$ = 0,05 et que le ratio R = 1. 

```{r, echo = TRUE}
PPV = 1 * (1-0.2)/(1+1-(1-0.05)-1*0.20)
cat('PPV =', round(PPV,3))
```
\vspace{12pt}

-----------------------------------------------------
Test stat/Réalité         Lien       Pas de lien
---------------------- ---------- -----------------
Significatif              80            5

Non significatif          20            95
-----------------------------------------------------

```{r, echo = TRUE}
PPV = (80)/(80+5) 
cat('PPV =', round(PPV,3))
```


##

  ![](ion2.png)

-----------------------------------------------------
Taille d'effet      Puissance      
----------------- -----------------
Petite                0.12

Moyenne               0.44

Grande                0.73
-----------------------------------------------------


## {.flexbox .vcenter}

-----------------------------------------------------
Test stat/Réalité         Lien       Pas de lien
---------------------- ---------- -----------------
Significatif              44            5

Non significatif          56            95
-----------------------------------------------------

\vspace{12pt}

```{r, echo = TRUE}
cat('PPV =', round((44)/(44+5), 3))

```

##

  ![](Johnson.png)


R = 1/13 

-----------------------------------------------------
Test stat/Réalité         Lien       Pas de lien
---------------------- ---------- -----------------
Significatif              44             5*13

Non significatif          56             95*13
-----------------------------------------------------


```{r, echo = TRUE}
PPV = (44)/(44+5*13) 
cat('PPV =', round(PPV,3))
```

* Conclusion : la plupart des études publiées en psychologie sont fausses. 



## les solutions :



- S'assurer d'une puissance suffisante (Gervais et al., 2015) 

- La puissance devient acceptable à partir de 0.8 (Cohen, 1988)

- Une puissance de 0.95 est plus désirable (Lakens, 2013)

- Changer le seuil de significativité (Benjamen et al., 2018; Johnson, 2013)


##


```{r, echo = FALSE,  out.width = '100%', out.height='100%'}
library(pwr)
N<-pwr.t.test(n = NULL, d = 0.5, sig.level = 0.05, power = 0.44,
type = "two.sample",alternative = "greater")

par(mfrow=c(2,2))

Groupe.1<-c()
Groupe.2<-c()
for(i in 1:10000){
  Groupe.1a <- rnorm(19, 0, 1)
  Groupe.2a <- rnorm(19, 0.5, 1) # taille d'effet moyenne
  Groupe.1<-c(Groupe.1, mean(Groupe.1a))
  Groupe.2<-c(Groupe.2, mean(Groupe.2a))
}

## Histogrammes non-imprimés :
bp1 <- hist(Groupe.1, plot=FALSE, nclass=20)  # 20 classes...
bp2 <- hist(Groupe.2, plot=FALSE, nclass=20)  #

## Calcul de minima/maxima :
hlims <- range(c(bp1$breaks, bp2$breaks))
vlims <- range(c(bp1$counts, bp2$counts))


histcol <- sapply(apply(rbind(col2rgb(c("orangered2", "lightblue"))/255,
                              alpha = c(1, 1)), # Première couleur opaque, 50% de transparence pour la seconde.
                        2, as.list),
                  do.call, what = rgb)

## Couleurs de remplissage, dont une avec ajout de transparence :
histcol <- sapply(apply(rbind(col2rgb(c("orangered2", "lightblue"))/255,
                              alpha = c(1, 0.5)), # Première couleur opaque, 50% de transparence pour la seconde.
                        2, as.list),do.call, what = rgb)

## Création du graphique :
hist(Groupe.1, border="darkred", xlim=hlims, ylim=vlims,
     nclass=20,  col=histcol[1],xaxt='n', yaxt='n', ann=FALSE)

hist(Groupe.2, border="darkblue", add=TRUE,
     breaks=seq(from=min(hlims), to=max(hlims), by=diff(bp1$breaks[1:2])), 
     col=histcol[2]  )      # Couleur transparente (en sur-impression).
abline(v=1.96/(19^0.5), lwd=2)
legend("topleft", legend=c("Pop. 1","Pop. 2"),
       col=c("darkred","darkblue") ,bty="n", pch=rep(c(16,18),each=4),cex=0.7,pt.cex=0.7)
title("Puissance = 0.44, p = 0.05")



N<-pwr.t.test(n = NULL, d = 0.5, sig.level = 0.05, power = 0.80,
type = "two.sample",alternative = "greater")

Groupe.1<-c()
Groupe.2<-c()
for(i in 1:10000){
  Groupe.1a <- rnorm(round(N$n,0), 0, 1)
  Groupe.2a <- rnorm(round(N$n,0), 0.5, 1) # taille d'effet moyenne
  Groupe.1<-c(Groupe.1, mean(Groupe.1a))
  Groupe.2<-c(Groupe.2, mean(Groupe.2a))
}

## Histogrammes non-imprimés :
bp1 <- hist(Groupe.1, plot=FALSE, nclass=20)  # 20 classes...
bp2 <- hist(Groupe.2, plot=FALSE, nclass=20)  #

## Calcul de minima/maxima :
hlims <- range(c(bp1$breaks, bp2$breaks))
vlims <- range(c(bp1$counts, bp2$counts))


histcol <- sapply(apply(rbind(col2rgb(c("orangered2", "lightblue"))/255,
                              alpha = c(1, 1)), # Première couleur opaque, 50% de transparence pour la seconde.
                        2, as.list),
                  do.call, what = rgb)

## Couleurs de remplissage, dont une avec ajout de transparence :
histcol <- sapply(apply(rbind(col2rgb(c("orangered2", "lightblue"))/255,
                              alpha = c(1, 0.5)), # Première couleur opaque, 50% de transparence pour la seconde.
                        2, as.list),do.call, what = rgb)

## Création du graphique :
hist(Groupe.1, border="darkred", xlim=hlims, ylim=vlims,
     nclass=20,  col=histcol[1],xaxt='n', yaxt='n', ann=FALSE)

hist(Groupe.2, border="darkblue", add=TRUE,
     breaks=seq(from=min(hlims), to=max(hlims), by=diff(bp1$breaks[1:2])), 
     col=histcol[2]  )      # Couleur transparente (en sur-impression).
abline(v=1.96/(N$n^0.5), lwd=2)
title("Puissance = 0.80, p = 0.05")









N<-pwr.t.test(n = NULL, d = 0.5, sig.level = 0.05, power = 0.95,
type = "two.sample",alternative = "greater")

Groupe.1<-c()
Groupe.2<-c()
for(i in 1:10000){
  Groupe.1a <- rnorm(round(N$n,0), 0, 1)
  Groupe.2a <- rnorm(round(N$n,0), 0.5, 1) # taille d'effet moyenne
  Groupe.1<-c(Groupe.1, mean(Groupe.1a))
  Groupe.2<-c(Groupe.2, mean(Groupe.2a))
}

## Histogrammes non-imprimés :
bp1 <- hist(Groupe.1, plot=FALSE, nclass=20)  # 20 classes...
bp2 <- hist(Groupe.2, plot=FALSE, nclass=20)  #

## Calcul de minima/maxima :
hlims <- range(c(bp1$breaks, bp2$breaks))
vlims <- range(c(bp1$counts, bp2$counts))


histcol <- sapply(apply(rbind(col2rgb(c("orangered2", "lightblue"))/255,
                              alpha = c(1, 1)), # Première couleur opaque, 50% de transparence pour la seconde.
                        2, as.list),
                  do.call, what = rgb)

## Couleurs de remplissage, dont une avec ajout de transparence :
histcol <- sapply(apply(rbind(col2rgb(c("orangered2", "lightblue"))/255,
                              alpha = c(1, 0.5)), # Première couleur opaque, 50% de transparence pour la seconde.
                        2, as.list),do.call, what = rgb)

## Création du graphique :
hist(Groupe.1, border="darkred", xlim=hlims, ylim=vlims,
     nclass=20,  col=histcol[1],xaxt='n', yaxt='n', ann=FALSE)

hist(Groupe.2, border="darkblue", add=TRUE,
     breaks=seq(from=min(hlims), to=max(hlims), by=diff(bp1$breaks[1:2])), 
     col=histcol[2]  )      # Couleur transparente (en sur-impression).
abline(v=1.96/(N$n^0.5), lwd=2)
title("Puissance = 0.95, p = 0.05")





N<-pwr.t.test(n = NULL, d = 0.5, sig.level = 0.005, power = 0.95,
type = "two.sample",alternative = "greater")

Groupe.1<-c()
Groupe.2<-c()
for(i in 1:10000){
  Groupe.1a <- rnorm(round(N$n,0), 0, 1)
  Groupe.2a <- rnorm(round(N$n,0), 0.5, 1) # taille d'effet moyenne
  Groupe.1<-c(Groupe.1, mean(Groupe.1a))
  Groupe.2<-c(Groupe.2, mean(Groupe.2a))
}

## Histogrammes non-imprimés :
bp1 <- hist(Groupe.1, plot=FALSE, nclass=20)  # 20 classes...
bp2 <- hist(Groupe.2, plot=FALSE, nclass=20)  #

## Calcul de minima/maxima :
hlims <- range(c(bp1$breaks, bp2$breaks))
vlims <- range(c(bp1$counts, bp2$counts))


histcol <- sapply(apply(rbind(col2rgb(c("orangered2", "lightblue"))/255,
                              alpha = c(1, 1)), # Première couleur opaque, 50% de transparence pour la seconde.
                        2, as.list),
                  do.call, what = rgb)

## Couleurs de remplissage, dont une avec ajout de transparence :
histcol <- sapply(apply(rbind(col2rgb(c("orangered2", "lightblue"))/255,
                              alpha = c(1, 0.5)), # Première couleur opaque, 50% de transparence pour la seconde.
                        2, as.list),do.call, what = rgb)

## Création du graphique :
hist(Groupe.1, border="darkred", xlim=hlims, ylim=vlims,
     nclass=20,  col=histcol[1],xaxt='n', yaxt='n', ann=FALSE)

hist(Groupe.2, border="darkblue", add=TRUE,
     breaks=seq(from=min(hlims), to=max(hlims), by=diff(bp1$breaks[1:2])), 
     col=histcol[2]  )      # Couleur transparente (en sur-impression).
abline(v=2.57/(N$n^0.5), lwd=2)
title("Puissance = 0.80, p = 0.005")

```

##

Application à une corrélation estimée de 0.30. 

* Explique environ 10% de la variance.

* A une valeur supérieure à 66% des corrélations publiées (Hemphill, 2003).


##

Application à une corrélation estimée de 0.30. 

>- Explique environ 10% de la variance.

>- A une valeur supérieure à 66% des corrélations publiées (Hemphill, 2003).


```{r include=FALSE, cache=FALSE}
require(plotly)
require(ggplot2)
require(BayesFactor)
```


```{r, echo = FALSE, warning=FALSE}
p44<-pwr.r.test(n = NULL, r = 0.3, sig.level = 0.05, power = 0.44,
alternative = "two.sided")
p80<-pwr.r.test(n = NULL, r = 0.3, sig.level = 0.05, power = 0.80,
alternative = "two.sided")
p95<-pwr.r.test(n = NULL, r = 0.3, sig.level = 0.05, power = 0.95,
alternative = "two.sided")
p005<-pwr.r.test(n = NULL, r = 0.3, sig.level = 0.005, power = 0.95,
alternative = "two.sided")
r.2<-pwr.r.test(n = NULL, r = 0.15, sig.level = 0.005, power = 0.95,
alternative = "two.sided")
values<-c(p44$n , p80$n, p95$n,p005$n,r.2$n)
values<-round(values, 0)
values<-data.frame("N"=values,"condition"=c("puissance 0,44, p = 0,05, r = 0.3",
                                            "puissance 0,80, p = 0,05, r = 0.3",
                                            "puissance 0,95, p = 0,05, r = 0.3",
                                            "puissance 0,95, p = 0,005, r = 0.3",
                                            "puissance 0,95, p = 0,005, r = 0,15"))
values$condition<-factor(values$condition, levels=c("puissance 0,44, p = 0,05, r = 0.3","puissance 0,80, p = 0,05, r = 0.3" ,"puissance 0,95, p = 0,05, r = 0.3", "puissance 0,95, p = 0,005, r = 0.3" ,"puissance 0,95, p = 0,005, r = 0,15"))


p<-ggplot(data=values, aes(x=condition, y=N)) +
   geom_bar(stat="identity")+theme(axis.line.x = element_line(color="black", size = 0.6),
                         axis.line.y = element_line(color="black", size = 0.6))
p<-p+theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggplotly(p)

```


# Changeons de paradigme : l'approche bayesienne

## 
### Les facteurs bayesiens : un rapport entre deux vraisemblances 

Supposons un amphi pour lequel on se demande si la parité est respectée. Dans cet amphi, il y a 200 personnes, dont 160 femmes. La probabilité que la parité soit respectée s'obtient par une binomiale 

$$ \begin{pmatrix} 200 \\160  \end{pmatrix} 0,5 ^{160} (1-0,5)^{40}\ $$

et vaut 


```{r include=T, echo=F}
p.h0<-dbinom(160, 200, 0.5,FALSE)
p.h0
```

##

La probabilité que la parité ne soit pas respectée (le ratio h/f est n'importe quelle autre valeur que 0,5) s'obtient par :

$$\int_{0}^{1} \begin{pmatrix} 200 \\160  \end{pmatrix} p ^{160} (1-p)^{40}\, \mathrm{d}p $$
et vaut :

```{r include=T, echo=F}
fdbinom<-function(p=NULL){
  r<-dbinom(160, 200, p,FALSE)
  return(r)
}

p.h1<-integrate(fdbinom, 0,1)
p.h1$value
```

##
Le facteur bayesien est le rapport entre ces deux probabilités.

\vspace{12pt}

```{r include=T,, echo=F}
cat(p.h0, "/",p.h1$value , "=", p.h1$value/p.h0)
```

et s'interprète de la manière suivante  :
\vspace{12pt}

<div class="centered">

 ![](evidencefor.png)

</div>

<small> <small><div style="text-align: right">  (Wagenmakers et al., 2011)  </div> </small></small>

## 
### Tester une hypothèse nulle 

```{r include=T, echo=F}
groupe1<-rnorm(1000, 0, 1)
groupe2<-rnorm(1000, 0, 1)
list.t<-c()
list.bf<-c()
for(i in 5:1000){
  ttest<-t.test(groupe1[1:i], groupe2[1:i])
  list.t<-c(list.t,  ttest$p.value)
  bf<-ttestBF(x = groupe1[1:i], y = groupe2[1:i], rscale = "medium")
  list.bf<-c(list.bf, extractBF(bf)$bf)
}
simul<-data.frame(index=5:1000, "Bayes.Factor"=list.bf,"test.t.p"=list.t)

cutoff.bf <- data.frame( x = c(-Inf, Inf), y =0.33, cutoff = factor(50) )
cutoff.bf2<-data.frame( x = c(-Inf, Inf), y =3, cutoff = factor(50) )
p<-ggplot(simul, aes(x=index, y=Bayes.Factor))+geom_point()+
  ylim(0,3.5)+geom_line(aes( x, y, linetype = cutoff ), cutoff.bf)+
  geom_line(aes( x, y, linetype = cutoff ), cutoff.bf2)+
  xlab("Taille de l'échantillon") + ylab("Valeur du facteur bayesien")+
   theme(legend.position="none")+
  theme(axis.line.x = element_line(color="black", size = 0.6),
                         axis.line.y = element_line(color="black", size = 0.6))
cutoff <- data.frame( x = c(-Inf, Inf), y = 0.05, cutoff = factor(50) )
p2<-ggplot(simul, aes(x=index, y=test.t.p))+geom_point()+
   geom_line(aes( x, y, linetype = cutoff ), cutoff)+
   xlab("Taille de l'échantillon") + ylab("Valeur de la probabilité du test t")+
   theme(legend.position="none")+
  theme(axis.line.x = element_line(color="black", size = 0.6),
                         axis.line.y = element_line(color="black", size = 0.6))
print(p2)

```

##
```{r include=T, echo=F}
print(p)
```


## 
### Appliquer les stat bayesiennes avec JASP
<div class="centered">

 ![](jasp.png)

</div>

##
### La distribution cauchy vs. la distribution normale

```{r include=T,, echo=F}
p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 1)) + 
  ylab("densité") +
  scale_y_continuous(breaks = NULL)
p1<-p1+ stat_function(fun = dcauchy, n = 1e4, args = list(location = 0, scale = 1), aes(color = "b"), size = 1)+ theme(legend.position="none") +
  annotate("text", label="Normale", color="black", x = 1.5, y = 0.5)+
  annotate("text", label="Cauchy", color="red",x = 1.5, y = 0.45)
p1
```


## Le prior

* Il faut spécifier a priori les moyennes ainsi que la probabilité qui y est associée.

* Une alternative est de fixer le prior sur la taille de l'effet. 

* Il s'agit de r-scale. La valeur par défaut pour le test t est 0.707.

* Le r-scale correspond à la taille d'effet en valeur absolue que le chercheur estime pouvoir observer dans plus de 50% des situations. 

* Il est raisonnable de fixer le r-scale à la valeur du d de Cohen désiré.



# Merci de votre attention 

Cette présentation est disponible ici : 

https://github.com/NicolasStefaniak/SFP2018


