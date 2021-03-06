Ces applications traitent des données du Coronavirus COVID-19.

Liens des applications
----------------------

-   [**COVID-19 Dashboard**](https://lrakotoson.shinyapps.io/Covid-19/)
-   [**Shiny App
    Analysis**](https://lena35.shinyapps.io/Application_Shiny_Coronavirus/)

**Les différents CDC mettent à jour leurs données la nuit entre 01:30AM
et 04:00AM (heure de Paris), ce qui peut provoquer des `NAN` pendant
cette période sur les résultats affichés par les applications. Dans ce
cas, limitez l’analyse pour le jour *j* − 1.**

Les données
-----------

### Tables principales

Les 3 tables de base (**Cas**, **Rétablis**, **Morts**) sur lequel nous
travaillons possèdent la même structure. L’évolution des statistiques
pour chaque pays pour chaque jour depuis le 22 Janvier 2020, en plus des
coordonées GPS.  
Une colonne s’ajoute chaque jour, une ligne s’ajoute pour un nouvel Etat
touché.  
Après netoyage, voici les premières lignes de la table **Cas**:

``` r
head(T_cas)
```

### Tables calculées

#### Latest

La fonction `latest(t)` renvoie les données pour le t-eme relevé en
aggrégeant les trois tables. Par défaut les données les plus récentes
sont renvoyées. La fonction `continent()` ajoute la colonne
*Continent*.  
`t` suit le numero de relevé de l’OMS.  
Les premières lignes:

``` r
head(latest())
```


    ## # A tibble: 6 x 8

|  | State            |Country          |Continent   |Lat     |Long    |Cas     |Retablis|Morts
|--|:----------------:|:---------------:|:----------:|:------:|:------:|:------:|:------:|-----:
|  | *\<chr>*         |*\<chr>*         |*\<fct>*    |*\<dbl>*|*\<dbl>*|*\<dbl>*|*\<dbl>*|*\<dbl>*
| 1| Andorra          |Andorra          |Europe      |42.5    |1.5     |738     |344     |40
| 2| United Arab Emi~ |United Arab Emi~ |Asia        |24      |54      |10839   |2090    |82
| 3| Afghanistan      |Afghanistan      |Asia        |33      |65      |1703    |220     |57
| 4| Antigua and Bar~ |Antigua and Bar~ |South Amer~ |17.0    |-61.8   |24      |11      |3
| 5| Anguilla         |Anguilla         |<NA>        |18.2    |-63.2   |3       |3       |0
| 6| Albania          |Albania          |Europe      |41      |20      |736     |422     |28

#### Brief

La fonction `brief(group, t)` renvoie les données du t-eme relevé pour
chaque groupe (Continent ou Pays). Par défaut les données les plus
récentes pour le Monde sont renvoyées.  
Les premières lignes:

``` r
brief()
```


    ## # A tibble: 1 x 4

|  | group |     Cas| Retablis|  Morts
|--|-------|--------|---------|-------
|  | <chr> |  <dbl> |   <dbl> | <dbl>
| 1| Monde |3 059 108|  919 152 |211 208

#### Actus

La fonction `actus(rows)` renvoie les rows-er infos les plus récentes
dans la presse française. Les données en entrée sont en XML et sortent
en tible d’une colonne formaté avec des liens de redirection.  
Les premières lignes:

``` r
head(actus())
```

<hr>

COVID-19 Dashboard
==================

Le Dashboard permet de visualiser l’évolution du Coronavirus en temps
réel dans l’espace et le temps.  
Il contient trois onglets pour visualiser la situation mondiale, pour
chaque région du globe et pour analyser par pays *(déplacé dans le Shiny
App)*

Nous l’avons mis en production directement le 25 Février 2020 face à
l’urgence de la situation afin qu’on puisse s’informer et suivre
l’évolution de la situation.

Situation Mondiale
------------------

Cette page est subdivisée en trois parties: l’évolution géographique,
les dernières données et les actualités à la Une.

<figure align="center">
<img src="assets/SituationMondiale.png">
<figcaption align="right">
Capture écran du 14 Mars 2020
</figcaption>
</figure>

<br>

### Evolution géographique

Nous voulons présenter la répartition géographique du nombre de cas dans
le Monde avec les données les plus récentes.  
En survolant, on peut obtenir pour chaque Etat, le nombre de cas, de
rétablissements et de décès depuis le début de la pandémie.

### Dernières données

Nous resumons dans cette partie:

-   Le nombre total de cas recencés, dont les cas toujours actifs
-   Le taux de rétablissement
-   Le taux de létalité

### Dernières infos

Comme nos données se mettent à jour quotidiennement, les données
évoluent entre temps.  
Cette section permet de suivre en temps réel les articles de presse et
les dépèches pour connaître les actualités à la Une.

Evolution Regionale
-------------------

Cette page permet d’analyser de façon plus précise chaque région du
globe en fonction du temps.

<figure align="center">
<img src="assets/EvolutionRegionale.png">
<figcaption align="right">
Capture écran du 14 Mars 2020
</figcaption>
</figure>

<br>

Elle est divisée en 4 parties:

-   **Situation géographique**: représentation de la répartition des Cas
    / Morts / Rétablis dans la région sélectionnée et au t-ème relevé de
    l’OMS.
-   **Nouvelles données**: évolution du nombre de nouveaux Cas / Morts /
    Rétablis dans la région sélectionnée.
-   **Evolution dans la région**: évolution des cumuls des Cas, Morts et
    Rétablis dans la région. Le curseur permet de zoomer sur une période
    précise.
-   **Taux**: évolution des taux de rétablissement et de létalité dans
    la région sélectionnée. Le curseur permet de zoomer pour se
    concentrer sur une période.

La barre de contrôle permet de piloter l’analyse en spécifiant:

-   La région à analyser pour les 4 subdivisions
-   La situation à représenter sur les 2 subdivisions de la première
    ligne
-   Les données du t-ème relevé de l’OMS à représenter sur la carte
-   La représentation à l’échelle logarithmique ou non de l’évolution
    dans la région

Analyse par pays
----------------

Cette page a été déplacée dans le Shiny App pour le rendu, toutefois
elle est toujours comprise dans le Dashboard en production.  
Aussi, après la date de rendu (16-03-2020), **le Dashboard sera rendu
OpenSource** et donc sera ouverte aux contributions. Le code original de
notre groupe sans contribution externe est aussi aacessible pour la
version avant rendu du Dashboard [en cliquant ici: version avant
rendu](https://github.com/Lrakotoson/Covid-19/tree/d3839851f5ebce09117565e9979ba1447a06440e).

<hr>

Shiny App Analysis
==================

L’application Shiny permet d’effectuer l’analyse d’une des 3 situations
dans une période donnée ainsi que la comparaison de la situation entre
deux pays en appliquant des modèles de régression.  
Elle contient alors 3 onglet en plus des données.

Evolution
---------

Cette page permet d’évaluer l’évolution de chaque situation (Cas, Décès
ou Rétablissements) pour une période donnée et la répartition
géographique de celle-ci pour une date précise.

<figure align="center">
<img src="assets/AppEvolution.png">
<figcaption align="right">
Capture écran du 16 Mars 2020
</figcaption>
</figure>

<br>

Le Sidebar permet de contrôler la période ainsi que la situation dont on
veut visualiser l’évolution.  
Le titre se met à jour automatiquement, toutefois il est aussi possible
d’éditer celui-ci pour personnaliser une exportation.  
La visualisation de la situation géographique à un jour précis est
contrôlée par le curseur de la date.

Comparaisons
------------

Cette page permet une analyse par pays mais aussi une comparaison de
l’évolution de la situation à une période donnée et une régression avec
différents modèles.

<figure align="center">
<img src="assets/AppComparaison.png">
<figcaption align="right">
Capture écran du 16 Mars 2020
</figcaption>
</figure>

<br>

Le **Control Panel** permet de diriger l’analyse.  
On y définit entre autres les deux pays à analyser (si on veut analyser
un seul, choisir “Monde” pour l’autre) et la période.  
Pour chaque situation, on peut:

-   Visualiser à l’échelle logarithmique. *(une épidemie a une évolution
    exponentielle)*
-   Effectuer un Régression sur la période d’analyse. (Linéaire,
    Polynomiale de degré 2 ou 3)
-   Choisir le nombre de jours à prédire après la période d’analyse.
    (Prédiction du modèle de régression sélectionné)

Dans les **Résultats**, on retrouve:

-   La repésentation géographique et le nombre de cas
-   L’analyse des cas
    -   L’évolution des cas recencés ( + régression, prédictions)
    -   L’évolution des cas actifs (ni rétablis, ni morts)
-   L’analyse des décès
    -   L’évolution du taux de mortalité, les morts sur les cas ( +
        régression, prédictions)
    -   L’évolution du taux de létalité, les morts sur les cas inactifs
        ( + régression, prédictions)
-   L’analyse des rétablissements
    -   L’évolution du taux de rétablissement ( + régression,
        prédictions)
    -   Le nombre de nouveaux cas par jour

<hr>

Sources des données
-------------------

Les données proviennent de divers sources dont
l’[OMS](https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports/),
les [rapports
quotidiens](http://www.nhc.gov.cn/yjb/pzhgli/new_list.shtml) du
gouvernement Chinois et les CDC de différents gouvernements
([Etats-Unis](https://www.cdc.gov/coronavirus/2019-ncov/index.html),
[Canada](https://www.canada.ca/en/public-health/services/diseases/coronavirus.html),
[EU](https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases),
[Italie](http://www.salute.gov.it/nuovocoronavirus),
[Australie](https://www.health.gov.au/news/coronavirus-update-at-a-glance),
[Taiwan](https://sites.google.com/cdc.gov.tw/2019ncov/taiwan?authuser=0),
[Chine](http://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm),
[Singapour](https://www.moh.gov.sg/covid-19)).

Les trois jeux de données en CSV, traduites en anglais et aggrégées pour
les Cas, les Rétablis et les Morts proviennent du CSSE de l’[Université
de Johns Hopkins](https://systems.jhu.edu/) qui les met à jour pour
*j* − 1.

Les flux RSS proviennent des *Google Actus* en XML.

<hr>
<center>

[Loïc Rakotoson](https://github.com/Lrakotoson/) \| [Lena
Coïc](https://github.com/lenacoic/) \| [Pierre-Yves
Colson](https://github.com/pycolson)

</center>
