Projet de PF5 2020 L-Système
===========


##Identifiants

**NGO FELIX** :

*    numéro étudiant : 71702718
*    alias : @ngof
    
**WEI VINCENT** :

*    numéro étudiant : 71805701
*    alias : @weiv

##Fonctionnalitées

** Sujet mininal **
* charge les donnees a partir d'un fichier avec la touche `o` du clavier
* interpretation des chaines parenthesees en suites de commandes Turtle
* animation lors de l'affichage d'une iteration avec la touche `t` du clavier
* calcule automatiquement les bornes

** Extension 1 : Sauvegarde de fichier **
* ecriture d'un L-systeme dans un fichier

** Extension 2 : Couleurs **
* generation aleatoire de couleurs lors d'un trace


##Compilation et exécution
Pour compiler le projet tapez la commande:
`make`

Puis executer l'executable:
`./run`



##Découpage modulaire
** main.ml **
** turtle.ml **
** system.ml **
** read.ml **
Ce module permet de lire un fichier contenant un L-Systeme afin d'etre traite
et trasnformer en type system.

La fonction `fileName` va demander a l'utilsateur le nom du fichier a lire.
Dans le cas ou le fichier n'existe pas on affichera une erreur sur la sortie
standard.

La fonction read_file va se charger separer le fichier en 3 parties distinctes :
Axiome, Rules, Interpretation et les mettre sous formes de liste de string.

La recuperation de ces donnes passe par les fonctions getAxiome, getRules,
getInter.

la fonction nthIter demande a l'utilisateur quelle iteration il/elle aimerait
voir.

** save.ml **
Le module save permet à l’utilisateur de créer un fichier contenant un nouvel
L-Système.
Elle contient plusieurs références :
* fileName correspondant au nom du nouveau fichier (String)
* newFile correspondant au stream obtenu lors de l’ouverture du fichier
  (out_channel)
* symbolSet correspondant a l’ensemble des symboles du L-Système (List)

La fonction writeFile est la fonction principale qui va créer le fichier.

newFileName va demander a l’utilisateur le nom du fichier a créer. Si le nom donner existe déjà il écrira une erreur sur la sortie standard et va se répéter 

La fonction ensembleFini va se charger de retourner l’ensemble fini de symbole 

baseExpression de donner l’axiome de départ.

Rules va demander a l’utilisateur si il veut une règle pour chaque symbole dans symbolSet.

Interprétation se charge d’affecter une commande a chaque symbole. (Line,move ,turn) 


##Organisation du travail

** FELIX **
Lors de ce projet, il s'est occupe de :
* parser les fichier texte en L-Systeme
* de calculer la ieme iteration d'un L-Systeme
* sauvegarder les L-Systeme sous un format texte
* d'embelir les illustrations obtenu en ajoutant un peu de couleurs
* et de sauvegarder une image

** VINCENT **
Lors de ce projet, il s'est occupe de :
* tout
