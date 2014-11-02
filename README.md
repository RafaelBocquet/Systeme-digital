Simulateur de Netlist
=====================

Ce simulateur de Netlist se présente sous la forme d'un
programme prenant en entrée un fichier .net produit par 
le compilateur *mjc.byte*. Lorsqu'utilisé avec *cabal* 
il se lance ainsi :

    cabal run -- /chemin/vers/le/fichier.net <nbr de cycles>
    
La ROM de taille d'adressage *addrs* et de taille de 
mot *wds* est lue depuis un fichier `<addrs>x<wds>.rom`
contenant $2^addrs$ lignes de wds charactères '0' ou '1' 
Les variables marquées comme *Input* sont demandées sur 
la ligne de commande. Les adressages des cases de la mémoire
sont petit-boutistes (little-endian).

Notes techniques
----------------

Dans sa version actuelle, le code se présente malheureusement 
comme un assemblage de rustines et de hacks peu lisibles, ajoutés 
au fur et à mesure que les instructions otn été implémentées.

Concernant le fonctionement même du programme : un pré-traitement
transforme tout les identifiants de variables en entier afin de
pouvoir les conserver dans un tableau. Ce tableau est en fait un
tableau de listes chaînées de Booléens. Une valeur booléenne est 
représentée par une liste (nappe) à un seul élément.


Concernant des instructions particulières :

 - Les *Input* sont gérés en rajoutant une équation "var = INPUT" 
 dans la net list. Qui est ensuite traitée en demandant simplement
 la valeur à 'utilisateur. 
 - Les *registres* sont gérés grâce à un "hack" dans le Scheduler : 
 lorsqu'on rencontre une instruction "a = REG b" on ajoute une arrête b 
 dépend de a dans le Scheduler et non pas l'inverse ainsi, lors de la mise 
 à jour de a au cycle n, b aura encore la valeur qu'il avait au cycle (n-1)
 - Les `ROM` sont toutes pré-chargées en mémoire, afin d'économiser les accès 
 disques. Elles sont représentées par une map indéxée par des paires d'entiers 
 et contenant des tableau immuables.
 - Les `RAM` sont représentées de manière similaire, sauf que le tableau est mutable.
 Leur traitement est séparé en deux partie : L'instruction `o = RAM n m ra we wa c` 
 n'ajoute que "o dépend de ra" au Scheduler. La passe principale du simulateur
 ne va gérer que les lectures sur les RAM. Les écritures sont toutes réalisée en 
 fin de cycle. Ici un hack est également nécessaire : en effet si l'une des variables
 *wa*, *we* ou *c* est un Input utilisateur, il est possible qu'elle n'ait pas été
 demandée pendant tout le cycle. Afin d'éviter ceci, on ajoute une équation 
 "VARIABLEINUTILE = <var>" pour toute variable <var> marquée dans le champ input.

À faire
-------

 - Remplacer les listes chainées par des tableaux pour gagner en performances.


