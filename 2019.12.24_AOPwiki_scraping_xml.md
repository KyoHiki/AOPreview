AOP wiki web scraping
================
Kyoshiro HIKI

``` r
knitr::opts_chunk$set(echo = TRUE, message=FALSE) #echo=TRUE
```

# Get the xml file from AOP wiki through the following web page.

<https://aopwiki.org/info_pages/5>

# Make a dataset tidy

``` r
require(XML)
require(rlist)
require(tidyverse)
require(magrittr)
require(purrr)
require(ggplot2)


data.xml <- xmlInternalTreeParse("aop-wiki-xml-2019-07-01.xml") %>% xmlToList
length(data.xml)
```

    ## [1] 4083

``` r
table(rownames(summary(data.xml))) ## showing the summary of data.xml
```

    ## 
    ##                    aop      biological-action      biological-object 
    ##                    261                     11                    357 
    ##     biological-process               chemical              key-event 
    ##                    352                    283                   1022 
    ## key-event-relationship               stressor               taxonomy 
    ##                   1199                    430                    167 
    ##        vendor-specific 
    ##                      1

``` r
chemical <- data.xml %>% list.match("chemical")
aop <- data.xml %>% list.match("aop")
bioaction<- data.xml %>% list.match("biological-action")
bioprocess<- data.xml %>% list.match("biological-process")
bioobject<- data.xml %>% list.match("biological-object")
keyevent<- data.xml %>% list.match("key-event")
ker<- data.xml %>% list.match("key-event-relationship")
stressor<- data.xml %>% list.match("stressor")
taxonomy<- data.xml %>% list.match("taxonomy")
vendor<- data.xml %>% list.match("vendor-specific")

# Exclude Archived AOP 
length(aop)
```

    ## [1] 261

``` r
saaop <- aop %>% unlist %>% list.match("saaop-status")
table(saaop)
```

    ## saaop
    ##                   Archived Included in OECD Work Plan 
    ##                          6                         54 
    ##          Under Development 
    ##                        149

``` r
## Exclude empty AOP
empty_ke <- aop %>%
  map(~.$`key-events`, flatten_df)  %>%
  map_depth(.,2, unlist) %>%
  lapply(function(x) {names(x)}) %>%
  lapply( is.null ) %>% unlist
table(empty_ke)
```

    ## empty_ke
    ## FALSE  TRUE 
    ##   241    20

``` r
empty_ker <- aop %>%
  map(~.$`key-event-relationship`, flatten_df)  %>%
  map_depth(.,2, unlist) %>%
  lapply(function(x) {names(x)}) %>%
  lapply( is.null ) %>% unlist
table(empty_ker)
```

    ## empty_ker
    ## FALSE  TRUE 
    ##   225    36

``` r
aop <- aop [- c(which(empty_ker==TRUE),which(empty_ker==TRUE),which(saaop=="Archived") )  ]
names(aop) <- paste("aop",c(1:length(aop)))

#aop list check
length(aop)
```

    ## [1] 222

``` r
summary(aop[[1]])
```

    ##                             Length Class  Mode     
    ## title                       1      -none- character
    ## short-name                  1      -none- character
    ## authors                     1      -none- character
    ## status                      3      -none- list     
    ## oecd-project                1      -none- character
    ## abstract                    1      -none- character
    ## molecular-initiating-event  2      -none- list     
    ## adverse-outcome             2      -none- list     
    ## key-event-relationships     8      -none- list     
    ## essentiality-support        1      -none- character
    ## key-events                  5      -none- list     
    ## applicability               2      -none- list     
    ## overall-assessment          5      -none- list     
    ## potential-applications      1      -none- character
    ## aop-stressors               2      -none- list     
    ## references                  1      -none- character
    ## source                      1      -none- character
    ## creation-timestamp          1      -none- character
    ## last-modification-timestamp 1      -none- character
    ## .attrs                      1      -none- character

``` r
## KER as data.frame format
ker_df <-  ker %>% 
  purrr::map(magrittr::extract, c("title", ".attrs")) %>%
  purrr::map(., unlist) %>%
  do.call(rbind,.) 
rownames(ker_df) <- rep("relationship",nrow(ker_df) )

ker_df2 <- aop %>%
  map(~.$`key-event-relationships`, flatten_df)  %>%
  map_depth(.,2, unlist) %>%
  lapply( function(x) do.call (rbind, x)) %>%
  list.filter(., ncol(.) >= 2) %>% 
  do.call(rbind,.)

ker_all <- merge(ker_df, ker_df2)

# Quantitative understanding value
quv <- table(ker_all$'.attrs.id',ker_all$`quantitative-understanding-value`)
quv[quv==0]<-NA
quv[quv!=1]<-1
quv2 <- as.data.frame.matrix(quv)[which(  apply(as.data.frame.matrix(quv), 1,sum, na.rm=TRUE) ==1),]  #remove overlapped quv among multiple AOPs
quv3<- apply(as.data.frame.matrix(quv2), 2,sum, na.rm=TRUE)

# Evidence value
evi <- table(ker_all$'.attrs.id',ker_all$`evidence`)
evi[evi==0]<-NA
evi[evi!=1]<-1
evi2 <- as.data.frame.matrix(evi)[which(  apply(as.data.frame.matrix(evi), 1,sum, na.rm=TRUE) ==1),] #remove overlapped evi among multiple AOPs
evi3 <- apply(as.data.frame.matrix(evi2), 2,sum, na.rm=TRUE)

#Quantitative & Evidence
ker_level <-as.data.frame (t( rbind( evi3, quv3) ))
ker_level [,3] <- rownames(ker_level)
colnames(ker_level) <- c("Weight of evidence","Quantitative understanding","Level")
ker_level<-gather(ker_level,key=Type,value=value,'Weight of evidence','Quantitative understanding')
ker_level2 <- transform(ker_level, Level=factor(Level,levels=c("Low","Moderate","High","Not Specified")) )

ggplot(ker_level2) +geom_bar(aes(x=Level,y=value,fill=Type),stat = "identity", position = "dodge")+ylab("Number of KERs")+ theme_classic(base_size = 20)+ theme(axis.text=element_text(colour = "black"),legend.position = c(0.01, 1), legend.justification = c(0, 1))+labs(fill = "")
```

![](2019.12.24_AOPwiki_scraping_xml_files/figure-gfm/read.xml-1.png)<!-- -->

``` r
# KE as data.frame format
MIE <-  aop %>% 
   map(., function(x) c(x$"molecular-initiating-event"$'.attrs') ) %>% 
  unlist %>% 
  lapply (function(x) {data.frame(ID=x,Type="MIE")}) %>% 
  do.call(rbind,.)
AO <-  aop %>% 
   map(., function(x) c(x$"adverse-outcome"$'.attrs') ) %>% 
  unlist %>% 
  lapply (function(x) {data.frame(ID=x,Type="AO")}) %>% 
  do.call(rbind,.)
KE <- aop %>% 
   map(., function(x) c(x$"key-events") ) %>% 
  unlist %>% 
  lapply (function(x) {data.frame(ID=x,Type="KE")}) %>% 
  do.call(rbind,.)
ke_all <- rbind(MIE,KE,AO)
table(ke_all[,"Type"])
```

    ## 
    ##  MIE   KE   AO 
    ##  195 1089  216

``` r
data.frame(No_of_AOP= length(aop), No_of_KE= nrow(ke_all), No_of_KER = nrow(ker_df2))
```

    ##   No_of_AOP No_of_KE No_of_KER
    ## 1       222     1500      1540

</br> </br>

# Statistics of AOP wiki

``` r
oecd <- aop %>% unlist %>% list.match("oecd-status")
table(oecd)
```

    ## oecd
    ##     EAGMST Approved EAGMST Under Review   TFHA/WNT Endorsed 
    ##                   7                  14                   9 
    ##   Under Development 
    ##                  19

``` r
wikistatus<- aop %>% unlist %>% list.match("wiki-status")
table(wikistatus)
```

    ## wikistatus
    ##                          Open for citation & comment 
    ##                                                   21 
    ##                        Open for comment. Do not cite 
    ##                                                   22 
    ## Under development: Not open for comment. Do not cite 
    ##                                                  179

``` r
aop.ke<- aop %>% unlist %>% list.match("key-event")

taxon <- taxonomy %>% unlist %>% list.match("name") 
table(taxon)
```

    ## taxon
    ##                               F344 rat 
    ##                                      1 
    ##                   Acipenser fulvescens 
    ##                                      1 
    ##                Acipenser transmontanus 
    ##                                      1 
    ##                   Acyrthosiphon kondoi 
    ##                                      1 
    ##                      Aegypius monachus 
    ##                                      1 
    ##                    African clawed frog 
    ##                                      1 
    ##                            all species 
    ##                                      1 
    ##                    Ambystoma mexicanum 
    ##                                      1 
    ##                       American kestrel 
    ##                                      1 
    ##                        Anodonta cygnea 
    ##                                      1 
    ##                      Arctica islandica 
    ##                                      1 
    ##                         Ardea herodias 
    ##                                      1 
    ##                   Argopecten irradians 
    ##                                      1 
    ##                     Atlantic killifish 
    ##                                      1 
    ##                                  birds 
    ##                                      1 
    ##                               bivalves 
    ##                                      1 
    ##                         bobwhite quail 
    ##                                      1 
    ##                       Bombus impatiens 
    ##                                      1 
    ##                             Bos taurus 
    ##                                      1 
    ##                                 bovine 
    ##                                      1 
    ##                        Bubalus bubalis 
    ##                                      1 
    ##                 Caenorhabditis elegans 
    ##                                      1 
    ##                 Canis lupus familiaris 
    ##                                      1 
    ##                    Carassius carassius 
    ##                                      1 
    ##                                    cat 
    ##                                      1 
    ##                                   cats 
    ##                                      1 
    ##                             CD-1 mouse 
    ##                                      1 
    ##             chaetanaphothrips orchidii 
    ##                                      1 
    ##                                chicken 
    ##                                      1 
    ##                               chickens 
    ##                                      1 
    ##               chickens, ducks, turkeys 
    ##                                      1 
    ##                       Chinese hamsters 
    ##                                      1 
    ##                    Colinus virginianus 
    ##                                      1 
    ##                        Common Starling 
    ##                                      1 
    ##                      Coturnix japonica 
    ##                                      1 
    ##                                    cow 
    ##                                      1 
    ##                            crustaceans 
    ##                                      1 
    ##                    Cynops pyrrhogaster 
    ##                                      1 
    ##                            Danio rerio 
    ##                                      1 
    ##                          Daphnia magna 
    ##                                      1 
    ##                          Daphnia pulex 
    ##                                      1 
    ##                                    dog 
    ##                                      1 
    ##                                   dogs 
    ##                                      1 
    ##                        domestic cattle 
    ##                                      1 
    ##               Double-crested cormorant 
    ##                                      1 
    ##                   Dreissena polymorpha 
    ##                                      1 
    ##                Drosophila melanogaster 
    ##                                      1 
    ##                       Eastern bluebird 
    ##                                      1 
    ##                 Echinogammarus marinus 
    ##                                      1 
    ##                         eisenia fetida 
    ##                                      1 
    ##                    Erinaceus europaeus 
    ##                                      1 
    ##                         fathead minnow 
    ##                                      1 
    ##                                 ferret 
    ##                                      1 
    ##                                   fish 
    ##                                      1 
    ##                              fruit fly 
    ##                                      1 
    ##                  Fundulus heteroclitus 
    ##                                      1 
    ##                          Gallus gallus 
    ##                                      1 
    ##                         Gammarus pulex 
    ##                                      1 
    ##                             gastropods 
    ##                                      1 
    ##                               goldfish 
    ##                                      1 
    ##                             guinea pig 
    ##                                      1 
    ##                         Gyps africanus 
    ##                                      1 
    ##                       Gyps bengalensis 
    ##                                      1 
    ##                       Gyps coprotheres 
    ##                                      1 
    ##                            Gyps fulvus 
    ##                                      1 
    ##                      Gyps himalayensis 
    ##                                      1 
    ##                           Gyps indicus 
    ##                                      1 
    ##                         Gyps rueppelli 
    ##                                      1 
    ##               Haliaeetus leucocephalus 
    ##                                      1 
    ##                                Hamster 
    ##                                      1 
    ##                        Helicoverpa zea 
    ##                                      1 
    ##                          Helix lucorum 
    ##                                      1 
    ##                           herring gull 
    ##                                      1 
    ##                           Homo sapiens 
    ##                                      1 
    ##                              Honey bee 
    ##                                      1 
    ##                               honeybee 
    ##                                      1 
    ##                                  human 
    ##                                      1 
    ##       human and other cells in culture 
    ##                                      1 
    ##                      human, mouse, rat 
    ##                                      1 
    ##                      Human, rat, mouse 
    ##                                      1 
    ##                                 humans 
    ##                                      1 
    ##                                  hydra 
    ##                                      1 
    ##                        Hydra attenuata 
    ##                                      1 
    ##                            hymenoptera 
    ##                                      1 
    ##                                insects 
    ##                                      1 
    ##                          Invertebrates 
    ##                                      1 
    ##                         Japanese quail 
    ##                                      1 
    ##                     Lampsilis fasciola 
    ##                                      1 
    ##                            Lemna gibba 
    ##                                      1 
    ##                            Lemna minor 
    ##                                      1 
    ##                            lepidoptera 
    ##                                      1 
    ##                     Liriomyza trifolii 
    ##                                      1 
    ##                      Lymnaea stagnalis 
    ##                                      1 
    ##                    Macaca fascicularis 
    ##                                      1 
    ##                         Macaca mulatta 
    ##                                      1 
    ##                  Mammalia sp. AVB-2011 
    ##                                      1 
    ##                                mammals 
    ##                                      1 
    ##                                 medaka 
    ##                                      1 
    ##                    Meleagris gallopavo 
    ##                                      1 
    ##                         Melibe leonine 
    ##                                      1 
    ##                  Mercenaria mercenaria 
    ##                                      1 
    ##                                   mice 
    ##                                      1 
    ##                      Microgadus tomcod 
    ##                                      1 
    ##                                 Monkey 
    ##                                      1 
    ##                             Monkey sp. 
    ##                                      1 
    ##                  Monodelphis domestica 
    ##                                      1 
    ##                                  mouse 
    ##                                      1 
    ##                        Mus musculoides 
    ##                                      1 
    ##                           Mus musculus 
    ##                                      1 
    ##                        Mus sp. 2000082 
    ##                                      1 
    ##              Mytilus galloprovincialis 
    ##                                      1 
    ##                               nematode 
    ##                                      1 
    ##                  northern leopard frog 
    ##                                      1 
    ##                  Nycticorax nycticorax 
    ##                                      1 
    ##                      Oncorhynchus keta 
    ##                                      1 
    ##                    Oncorhynchus mykiss 
    ##                                      1 
    ##                       Orius insidiosus 
    ##                                      1 
    ##                        orius isidiosus 
    ##                                      1 
    ##                  Oryctolagus cuniculus 
    ##                                      1 
    ##                        Oryzias latipes 
    ##                                      1 
    ##                                 osprey 
    ##                                      1 
    ##                             Ovis aries 
    ##                                      1 
    ##                           Pagrus major 
    ##                                      1 
    ##                        Pan troglodytes 
    ##                                      1 
    ##                    Phasianus colchicus 
    ##                                      1 
    ##                                    Pig 
    ##                                      1 
    ##                                   pigs 
    ##                                      1 
    ##                    Pimephales promelas 
    ##                                      1 
    ##              Primates sp. BOLD:AAA0001 
    ##                                      1 
    ##                     Radopholus similis 
    ##                                      1 
    ##                          rainbow trout 
    ##                                      1 
    ##                                    rat 
    ##                                      1 
    ##                                   rats 
    ##                                      1 
    ##                      Rattus norvegicus 
    ##                                      1 
    ##                          Rattus rattus 
    ##                                      1 
    ##                             Rattus sp. 
    ##                                      1 
    ##                  Rattus sp. ABTC 42503 
    ##                                      1 
    ##                         rhesus monkeys 
    ##                                      1 
    ##                   Ring-necked pheasant 
    ##                                      1 
    ##                               rodentia 
    ##                                      1 
    ##                          Rodentia spp. 
    ##                                      1 
    ##                                rodents 
    ##                                      1 
    ##               Saccharomyces cerevisiae 
    ##                                      1 
    ##                            Salmo salar 
    ##                                      1 
    ##                          salmonid fish 
    ##                                      1 
    ##                                 SD rat 
    ##                                      1 
    ##                         Sea urchin sp. 
    ##                                      1 
    ##                    Spisula solidissima 
    ##                                      1 
    ##                         Sprague-Dawley 
    ##                                      1 
    ##                             Sus scrofa 
    ##                                      1 
    ##                                  swine 
    ##                                      1 
    ##                  Syrian golden hamster 
    ##                                      1 
    ##                        Syrian hamsters 
    ##                                      1 
    ##                             tree shrew 
    ##                                      1 
    ##                      tritonea diomedea 
    ##                                      1 
    ##                                 turkey 
    ##                                      1 
    ##                            Vertebrates 
    ##                                      1 
    ##       Xenopus (Silurana) epitropicalis 
    ##                                      1 
    ## Xenopus (Silurana) n. sp. tetraploid-1 
    ##                                      1 
    ##                         Xenopus laevis 
    ##                                      1 
    ##                  Xenopus laevis laevis 
    ##                                      1 
    ##                     Xenopus tropicalis 
    ##                                      1 
    ##                                  yeast 
    ##                                      1 
    ##                            zebra danio 
    ##                                      1 
    ##                             zebra fish 
    ##                                      1 
    ##                              zebrafish 
    ##                                      1 
    ##                      Zoarces viviparus 
    ##                                      1

``` r
year <- aop %>% unlist %>% list.match("creation-timestamp") %>% str_sub(1,4)
table(year)
```

    ## year
    ## 2016 2017 2018 2019 
    ##  146   41   14   21

``` r
mod.year <- aop %>% unlist %>% list.match("last-modification-timestamp") %>% str_sub(1,4)
table(mod.year)
```

    ## mod.year
    ## 2016 2017 2018 2019 
    ##   20   35  102   65

# AOP network analysis

``` r
require(igraph)
require(sna)
require(statnet)
library(RColorBrewer)

g<- graph_from_data_frame(ker_all[,2:3],directed=TRUE)

## Add Keytype information to the nodes of graph (g)
V(g)$KE_type <- as.character( ke_all[match(V(g)$name,ke_all$ID),"Type"] )
V(g)$ color <- ifelse (  V(g)$KE_type == "MIE", "lightgreen",   ifelse( V(g)$KE_type == "KE", "white",  ifelse ( V(g)$KE_type == "AO", "tomato", "black" ) )  )
ker_all[,6] <- table (ker_all[,1]) [ match (  ker_all[,1] , names(table (ker_all[,1] )) )]
# V(g)$size <-rep(2,   length( V(g) )   )
V(g)$freq <-   table ( c(as.character(ker_all[,2]), as.character(ker_all[,3]))  )
#V(g)$size <- ifelse (V(g)$freq <= 5, 2, ifelse (V(g)$freq <= 10, 4, ifelse (V(g)$freq <= 20, 6, ifelse (V(g)$freq > 20, 9, 0 ) ) ) )


# Add information to the edges 
E(g)$name <- as.character( ker_all[,4] )
E(g)$ adj <- as.character  (ker_all[,"adjacency"])
E(g)$ evidence <- as.numeric(ker_all[,"evidence"])
#E(g) $ color  <- ifelse (E(g)$adj == "adjacet", "grey",   ifelse (E(g)$adj == "non-adjacent", "orange", "grey" ) )
#E(g) $ width  <- ifelse ( is.na ( E(g)$evidence) , 1 ,  ifelse(E(g)$evidence=="5",1,  ifelse(E(g)$evidence=="1",1, ifelse( E(g)$evidence=="2",2,ifelse(E(g)$evidence=="3",3,0) ))) )
#E(g) $ lty  <- ifelse (is.na (E(g)$evidence) ,  2 , 1 )


#Draw graph 
l <- layout_with_fr(g)
plot(g, vertex.label=NA, layout = l, edge.vertex.size=0.4, vertex.size=3,edge.arrow.size=0.2)
```

![](2019.12.24_AOPwiki_scraping_xml_files/figure-gfm/aop.network.analysis-1.png)<!-- -->

``` r
# Distance
distance_table(g)
```

    ## $res
    ##  [1] 1193 1457 1648 1717 1625 1456 1341 1241 1145  982  761  611  400  189
    ## [15]  107   53   17   10    3    2
    ## 
    ## $unconnected
    ## [1] 857332

``` r
dist_g <- distances(g)

dist_g2 <- dist_g
dist_g2[!is.finite(dist_g2)] <- 0


# Weakly connected components
comp_g <- igraph::components(g,"weak")  # csize: size,no: No of components
comp_g$no
```

    ## [1] 39

``` r
comp_g$csize
```

    ##  [1]   9 666   7   8  14  17   8  11   4   8   8   6  10   6   8   8   7
    ## [18]   8   8   2   7   7  10   3   7   4   6   6   5   6   7   6   5   4
    ## [35]  10   5   5   5   4

``` r
m <- as_adjacency_matrix(g) %>% as.matrix
largest_g <-  sna::component.largest(m,result="graph",connected="weak")
largest_g<- graph_from_adjacency_matrix(largest_g)

# The largest weakly connected components
V(largest_g)$KE_type <- as.character( ke_all[match(V(largest_g)$name,ke_all$ID),"Type"] )
V(largest_g)$ color <- ifelse (  V(largest_g)$KE_type == "MIE", "lightgreen",   ifelse( V(largest_g)$KE_type == "KE", "white",  ifelse ( V(largest_g)$KE_type == "AO", "tomato", "black" ) )  )
# V(largest_g)$size <-rep(2,   length( V(largest_g) )   )

 l <- layout_with_fr(largest_g)
 plot(largest_g, vertex.label=NA, vertex.size=3, edge.arrow.size=0.1,  layout = l)
```

![](2019.12.24_AOPwiki_scraping_xml_files/figure-gfm/aop.network.analysis-2.png)<!-- -->

``` r
dist_lg <- distances(largest_g)
dist_lg2 <- dist_lg
dist_lg2[!is.finite(dist_lg2)] <- 0
hist(dist_lg2)
```

![](2019.12.24_AOPwiki_scraping_xml_files/figure-gfm/aop.network.analysis-3.png)<!-- -->

``` r
# Strongly connected components
comp_gs <- igraph::components(g,"strong") 
comp_gs$no 
```

    ## [1] 911

``` r
comp_gs$csize[which(comp_gs$csize!=1)]
```

    ## [1]  4  2  4  2  2 15  2

``` r
ms <- as_adjacency_matrix(g) %>% as.matrix
largest_gs <-  sna::component.largest(ms,result="graph",connected="strong")
```

    ## Node 1, Reach 8, Total 8
    ## Node 2, Reach 5, Total 13
    ## Node 3, Reach 2, Total 15
    ## Node 4, Reach 3, Total 18
    ## Node 5, Reach 102, Total 120
    ## Node 6, Reach 9, Total 129
    ## Node 7, Reach 7, Total 136
    ## Node 8, Reach 2, Total 138
    ## Node 9, Reach 13, Total 151
    ## Node 10, Reach 89, Total 240
    ## Node 11, Reach 3, Total 243
    ## Node 12, Reach 12, Total 255
    ## Node 13, Reach 2, Total 257
    ## Node 14, Reach 4, Total 261
    ## Node 15, Reach 5, Total 266
    ## Node 16, Reach 98, Total 364
    ## Node 17, Reach 26, Total 390
    ## Node 18, Reach 4, Total 394
    ## Node 19, Reach 102, Total 496
    ## Node 20, Reach 78, Total 574
    ## Node 21, Reach 3, Total 577
    ## Node 22, Reach 21, Total 598
    ## Node 23, Reach 15, Total 613
    ## Node 24, Reach 82, Total 695
    ## Node 25, Reach 3, Total 698
    ## Node 26, Reach 12, Total 710
    ## Node 27, Reach 92, Total 802
    ## Node 28, Reach 107, Total 909
    ## Node 29, Reach 3, Total 912
    ## Node 30, Reach 14, Total 926
    ## Node 31, Reach 3, Total 929
    ## Node 32, Reach 8, Total 937
    ## Node 33, Reach 5, Total 942
    ## Node 34, Reach 3, Total 945
    ## Node 35, Reach 97, Total 1042
    ## Node 36, Reach 8, Total 1050
    ## Node 37, Reach 78, Total 1128
    ## Node 38, Reach 86, Total 1214
    ## Node 39, Reach 104, Total 1318
    ## Node 40, Reach 7, Total 1325
    ## Node 41, Reach 25, Total 1350
    ## Node 42, Reach 83, Total 1433
    ## Node 43, Reach 7, Total 1440
    ## Node 44, Reach 8, Total 1448
    ## Node 45, Reach 2, Total 1450
    ## Node 46, Reach 3, Total 1453
    ## Node 47, Reach 2, Total 1455
    ## Node 48, Reach 2, Total 1457
    ## Node 49, Reach 15, Total 1472
    ## Node 50, Reach 24, Total 1496
    ## Node 51, Reach 90, Total 1586
    ## Node 52, Reach 5, Total 1591
    ## Node 53, Reach 11, Total 1602
    ## Node 54, Reach 78, Total 1680
    ## Node 55, Reach 4, Total 1684
    ## Node 56, Reach 13, Total 1697
    ## Node 57, Reach 4, Total 1701
    ## Node 58, Reach 3, Total 1704
    ## Node 59, Reach 4, Total 1708
    ## Node 60, Reach 115, Total 1823
    ## Node 61, Reach 3, Total 1826
    ## Node 62, Reach 6, Total 1832
    ## Node 63, Reach 17, Total 1849
    ## Node 64, Reach 11, Total 1860
    ## Node 65, Reach 6, Total 1866
    ## Node 66, Reach 2, Total 1868
    ## Node 67, Reach 6, Total 1874
    ## Node 68, Reach 6, Total 1880
    ## Node 69, Reach 5, Total 1885
    ## Node 70, Reach 12, Total 1897
    ## Node 71, Reach 2, Total 1899
    ## Node 72, Reach 83, Total 1982
    ## Node 73, Reach 10, Total 1992
    ## Node 74, Reach 97, Total 2089
    ## Node 75, Reach 3, Total 2092
    ## Node 76, Reach 29, Total 2121
    ## Node 77, Reach 3, Total 2124
    ## Node 78, Reach 12, Total 2136
    ## Node 79, Reach 79, Total 2215
    ## Node 80, Reach 6, Total 2221
    ## Node 81, Reach 8, Total 2229
    ## Node 82, Reach 5, Total 2234
    ## Node 83, Reach 6, Total 2240
    ## Node 84, Reach 5, Total 2245
    ## Node 85, Reach 10, Total 2255
    ## Node 86, Reach 106, Total 2361
    ## Node 87, Reach 2, Total 2363
    ## Node 88, Reach 9, Total 2372
    ## Node 89, Reach 21, Total 2393
    ## Node 90, Reach 5, Total 2398
    ## Node 91, Reach 3, Total 2401
    ## Node 92, Reach 30, Total 2431
    ## Node 93, Reach 94, Total 2525
    ## Node 94, Reach 3, Total 2528
    ## Node 95, Reach 5, Total 2533
    ## Node 96, Reach 86, Total 2619
    ## Node 97, Reach 3, Total 2622
    ## Node 98, Reach 79, Total 2701
    ## Node 99, Reach 87, Total 2788
    ## Node 100, Reach 7, Total 2795
    ## Node 101, Reach 78, Total 2873
    ## Node 102, Reach 13, Total 2886
    ## Node 103, Reach 9, Total 2895
    ## Node 104, Reach 2, Total 2897
    ## Node 105, Reach 5, Total 2902
    ## Node 106, Reach 5, Total 2907
    ## Node 107, Reach 20, Total 2927
    ## Node 108, Reach 3, Total 2930
    ## Node 109, Reach 87, Total 3017
    ## Node 110, Reach 80, Total 3097
    ## Node 111, Reach 3, Total 3100
    ## Node 112, Reach 18, Total 3118
    ## Node 113, Reach 6, Total 3124
    ## Node 114, Reach 7, Total 3131
    ## Node 115, Reach 8, Total 3139
    ## Node 116, Reach 7, Total 3146
    ## Node 117, Reach 87, Total 3233
    ## Node 118, Reach 20, Total 3253
    ## Node 119, Reach 100, Total 3353
    ## Node 120, Reach 5, Total 3358
    ## Node 121, Reach 9, Total 3367
    ## Node 122, Reach 2, Total 3369
    ## Node 123, Reach 8, Total 3377
    ## Node 124, Reach 11, Total 3388
    ## Node 125, Reach 7, Total 3395
    ## Node 126, Reach 87, Total 3482
    ## Node 127, Reach 2, Total 3484
    ## Node 128, Reach 2, Total 3486
    ## Node 129, Reach 9, Total 3495
    ## Node 130, Reach 10, Total 3505
    ## Node 131, Reach 6, Total 3511
    ## Node 132, Reach 2, Total 3513
    ## Node 133, Reach 5, Total 3518
    ## Node 134, Reach 3, Total 3521
    ## Node 135, Reach 6, Total 3527
    ## Node 136, Reach 3, Total 3530
    ## Node 137, Reach 5, Total 3535
    ## Node 138, Reach 5, Total 3540
    ## Node 139, Reach 83, Total 3623
    ## Node 140, Reach 4, Total 3627
    ## Node 141, Reach 85, Total 3712
    ## Node 142, Reach 9, Total 3721
    ## Node 143, Reach 12, Total 3733
    ## Node 144, Reach 2, Total 3735
    ## Node 145, Reach 7, Total 3742
    ## Node 146, Reach 81, Total 3823
    ## Node 147, Reach 3, Total 3826
    ## Node 148, Reach 81, Total 3907
    ## Node 149, Reach 6, Total 3913
    ## Node 150, Reach 86, Total 3999
    ## Node 151, Reach 5, Total 4004
    ## Node 152, Reach 5, Total 4009
    ## Node 153, Reach 117, Total 4126
    ## Node 154, Reach 100, Total 4226
    ## Node 155, Reach 103, Total 4329
    ## Node 156, Reach 7, Total 4336
    ## Node 157, Reach 4, Total 4340
    ## Node 158, Reach 2, Total 4342
    ## Node 159, Reach 89, Total 4431
    ## Node 160, Reach 15, Total 4446
    ## Node 161, Reach 4, Total 4450
    ## Node 162, Reach 4, Total 4454
    ## Node 163, Reach 2, Total 4456
    ## Node 164, Reach 86, Total 4542
    ## Node 165, Reach 2, Total 4544
    ## Node 166, Reach 4, Total 4548
    ## Node 167, Reach 24, Total 4572
    ## Node 168, Reach 5, Total 4577
    ## Node 169, Reach 7, Total 4584
    ## Node 170, Reach 8, Total 4592
    ## Node 171, Reach 2, Total 4594
    ## Node 172, Reach 10, Total 4604
    ## Node 173, Reach 23, Total 4627
    ## Node 174, Reach 5, Total 4632
    ## Node 175, Reach 9, Total 4641
    ## Node 176, Reach 2, Total 4643
    ## Node 177, Reach 5, Total 4648
    ## Node 178, Reach 6, Total 4654
    ## Node 179, Reach 78, Total 4732
    ## Node 180, Reach 3, Total 4735
    ## Node 181, Reach 9, Total 4744
    ## Node 182, Reach 99, Total 4843
    ## Node 183, Reach 3, Total 4846
    ## Node 184, Reach 14, Total 4860
    ## Node 185, Reach 5, Total 4865
    ## Node 186, Reach 108, Total 4973
    ## Node 187, Reach 95, Total 5068
    ## Node 188, Reach 9, Total 5077
    ## Node 189, Reach 86, Total 5163
    ## Node 190, Reach 6, Total 5169
    ## Node 191, Reach 33, Total 5202
    ## Node 192, Reach 5, Total 5207
    ## Node 193, Reach 90, Total 5297
    ## Node 194, Reach 95, Total 5392
    ## Node 195, Reach 11, Total 5403
    ## Node 196, Reach 9, Total 5412
    ## Node 197, Reach 7, Total 5419
    ## Node 198, Reach 12, Total 5431
    ## Node 199, Reach 5, Total 5436
    ## Node 200, Reach 5, Total 5441
    ## Node 201, Reach 5, Total 5446
    ## Node 202, Reach 9, Total 5455
    ## Node 203, Reach 5, Total 5460
    ## Node 204, Reach 4, Total 5464
    ## Node 205, Reach 10, Total 5474
    ## Node 206, Reach 3, Total 5477
    ## Node 207, Reach 2, Total 5479
    ## Node 208, Reach 5, Total 5484
    ## Node 209, Reach 7, Total 5491
    ## Node 210, Reach 4, Total 5495
    ## Node 211, Reach 11, Total 5506
    ## Node 212, Reach 90, Total 5596
    ## Node 213, Reach 6, Total 5602
    ## Node 214, Reach 82, Total 5684
    ## Node 215, Reach 26, Total 5710
    ## Node 216, Reach 104, Total 5814
    ## Node 217, Reach 6, Total 5820
    ## Node 218, Reach 2, Total 5822
    ## Node 219, Reach 5, Total 5827
    ## Node 220, Reach 2, Total 5829
    ## Node 221, Reach 98, Total 5927
    ## Node 222, Reach 4, Total 5931
    ## Node 223, Reach 80, Total 6011
    ## Node 224, Reach 88, Total 6099
    ## Node 225, Reach 88, Total 6187
    ## Node 226, Reach 3, Total 6190
    ## Node 227, Reach 3, Total 6193
    ## Node 228, Reach 96, Total 6289
    ## Node 229, Reach 3, Total 6292
    ## Node 230, Reach 78, Total 6370
    ## Node 231, Reach 8, Total 6378
    ## Node 232, Reach 14, Total 6392
    ## Node 233, Reach 4, Total 6396
    ## Node 234, Reach 87, Total 6483
    ## Node 235, Reach 9, Total 6492
    ## Node 236, Reach 2, Total 6494
    ## Node 237, Reach 86, Total 6580
    ## Node 238, Reach 5, Total 6585
    ## Node 239, Reach 3, Total 6588
    ## Node 240, Reach 9, Total 6597
    ## Node 241, Reach 114, Total 6711
    ## Node 242, Reach 84, Total 6795
    ## Node 243, Reach 5, Total 6800
    ## Node 244, Reach 2, Total 6802
    ## Node 245, Reach 2, Total 6804
    ## Node 246, Reach 28, Total 6832
    ## Node 247, Reach 9, Total 6841
    ## Node 248, Reach 85, Total 6926
    ## Node 249, Reach 7, Total 6933
    ## Node 250, Reach 3, Total 6936
    ## Node 251, Reach 7, Total 6943
    ## Node 252, Reach 4, Total 6947
    ## Node 253, Reach 16, Total 6963
    ## Node 254, Reach 6, Total 6969
    ## Node 255, Reach 4, Total 6973
    ## Node 256, Reach 4, Total 6977
    ## Node 257, Reach 7, Total 6984
    ## Node 258, Reach 6, Total 6990
    ## Node 259, Reach 2, Total 6992
    ## Node 260, Reach 14, Total 7006
    ## Node 261, Reach 2, Total 7008
    ## Node 262, Reach 2, Total 7010
    ## Node 263, Reach 4, Total 7014
    ## Node 264, Reach 9, Total 7023
    ## Node 265, Reach 8, Total 7031
    ## Node 266, Reach 13, Total 7044
    ## Node 267, Reach 3, Total 7047
    ## Node 268, Reach 7, Total 7054
    ## Node 269, Reach 9, Total 7063
    ## Node 270, Reach 4, Total 7067
    ## Node 271, Reach 4, Total 7071
    ## Node 272, Reach 8, Total 7079
    ## Node 273, Reach 4, Total 7083
    ## Node 274, Reach 5, Total 7088
    ## Node 275, Reach 7, Total 7095
    ## Node 276, Reach 4, Total 7099
    ## Node 277, Reach 12, Total 7111
    ## Node 278, Reach 3, Total 7114
    ## Node 279, Reach 7, Total 7121
    ## Node 280, Reach 3, Total 7124
    ## Node 281, Reach 7, Total 7131
    ## Node 282, Reach 18, Total 7149
    ## Node 283, Reach 5, Total 7154
    ## Node 284, Reach 6, Total 7160
    ## Node 285, Reach 8, Total 7168
    ## Node 286, Reach 3, Total 7171
    ## Node 287, Reach 89, Total 7260
    ## Node 288, Reach 2, Total 7262
    ## Node 289, Reach 6, Total 7268
    ## Node 290, Reach 24, Total 7292
    ## Node 291, Reach 4, Total 7296
    ## Node 292, Reach 4, Total 7300
    ## Node 293, Reach 3, Total 7303
    ## Node 294, Reach 4, Total 7307
    ## Node 295, Reach 89, Total 7396
    ## Node 296, Reach 2, Total 7398
    ## Node 297, Reach 4, Total 7402
    ## Node 298, Reach 5, Total 7407
    ## Node 299, Reach 10, Total 7417
    ## Node 300, Reach 3, Total 7420
    ## Node 301, Reach 6, Total 7426
    ## Node 302, Reach 5, Total 7431
    ## Node 303, Reach 10, Total 7441
    ## Node 304, Reach 3, Total 7444
    ## Node 305, Reach 21, Total 7465
    ## Node 306, Reach 7, Total 7472
    ## Node 307, Reach 5, Total 7477
    ## Node 308, Reach 5, Total 7482
    ## Node 309, Reach 4, Total 7486
    ## Node 310, Reach 10, Total 7496
    ## Node 311, Reach 7, Total 7503
    ## Node 312, Reach 4, Total 7507
    ## Node 313, Reach 84, Total 7591
    ## Node 314, Reach 2, Total 7593
    ## Node 315, Reach 17, Total 7610
    ## Node 316, Reach 13, Total 7623
    ## Node 317, Reach 12, Total 7635
    ## Node 318, Reach 2, Total 7637
    ## Node 319, Reach 16, Total 7653
    ## Node 320, Reach 10, Total 7663
    ## Node 321, Reach 25, Total 7688
    ## Node 322, Reach 78, Total 7766
    ## Node 323, Reach 8, Total 7774
    ## Node 324, Reach 115, Total 7889
    ## Node 325, Reach 5, Total 7894
    ## Node 326, Reach 4, Total 7898
    ## Node 327, Reach 6, Total 7904
    ## Node 328, Reach 87, Total 7991
    ## Node 329, Reach 2, Total 7993
    ## Node 330, Reach 3, Total 7996
    ## Node 331, Reach 97, Total 8093
    ## Node 332, Reach 2, Total 8095
    ## Node 333, Reach 3, Total 8098
    ## Node 334, Reach 4, Total 8102
    ## Node 335, Reach 10, Total 8112
    ## Node 336, Reach 2, Total 8114
    ## Node 337, Reach 10, Total 8124
    ## Node 338, Reach 78, Total 8202
    ## Node 339, Reach 4, Total 8206
    ## Node 340, Reach 3, Total 8209
    ## Node 341, Reach 5, Total 8214
    ## Node 342, Reach 7, Total 8221
    ## Node 343, Reach 18, Total 8239
    ## Node 344, Reach 3, Total 8242
    ## Node 345, Reach 8, Total 8250
    ## Node 346, Reach 3, Total 8253
    ## Node 347, Reach 3, Total 8256
    ## Node 348, Reach 2, Total 8258
    ## Node 349, Reach 4, Total 8262
    ## Node 350, Reach 2, Total 8264
    ## Node 351, Reach 4, Total 8268
    ## Node 352, Reach 8, Total 8276
    ## Node 353, Reach 2, Total 8278
    ## Node 354, Reach 15, Total 8293
    ## Node 355, Reach 8, Total 8301
    ## Node 356, Reach 79, Total 8380
    ## Node 357, Reach 7, Total 8387
    ## Node 358, Reach 4, Total 8391
    ## Node 359, Reach 2, Total 8393
    ## Node 360, Reach 6, Total 8399
    ## Node 361, Reach 2, Total 8401
    ## Node 362, Reach 6, Total 8407
    ## Node 363, Reach 3, Total 8410
    ## Node 364, Reach 6, Total 8416
    ## Node 365, Reach 9, Total 8425
    ## Node 366, Reach 3, Total 8428
    ## Node 367, Reach 78, Total 8506
    ## Node 368, Reach 4, Total 8510
    ## Node 369, Reach 3, Total 8513
    ## Node 370, Reach 14, Total 8527
    ## Node 371, Reach 11, Total 8538
    ## Node 372, Reach 2, Total 8540
    ## Node 373, Reach 87, Total 8627
    ## Node 374, Reach 5, Total 8632
    ## Node 375, Reach 7, Total 8639
    ## Node 376, Reach 5, Total 8644
    ## Node 377, Reach 2, Total 8646
    ## Node 378, Reach 10, Total 8656
    ## Node 379, Reach 10, Total 8666
    ## Node 380, Reach 4, Total 8670
    ## Node 381, Reach 8, Total 8678
    ## Node 382, Reach 8, Total 8686
    ## Node 383, Reach 79, Total 8765
    ## Node 384, Reach 5, Total 8770
    ## Node 385, Reach 87, Total 8857
    ## Node 386, Reach 13, Total 8870
    ## Node 387, Reach 2, Total 8872
    ## Node 388, Reach 6, Total 8878
    ## Node 389, Reach 8, Total 8886
    ## Node 390, Reach 7, Total 8893
    ## Node 391, Reach 97, Total 8990
    ## Node 392, Reach 7, Total 8997
    ## Node 393, Reach 84, Total 9081
    ## Node 394, Reach 98, Total 9179
    ## Node 395, Reach 8, Total 9187
    ## Node 396, Reach 85, Total 9272
    ## Node 397, Reach 95, Total 9367
    ## Node 398, Reach 5, Total 9372
    ## Node 399, Reach 2, Total 9374
    ## Node 400, Reach 12, Total 9386
    ## Node 401, Reach 4, Total 9390
    ## Node 402, Reach 3, Total 9393
    ## Node 403, Reach 84, Total 9477
    ## Node 404, Reach 8, Total 9485
    ## Node 405, Reach 5, Total 9490
    ## Node 406, Reach 105, Total 9595
    ## Node 407, Reach 2, Total 9597
    ## Node 408, Reach 2, Total 9599
    ## Node 409, Reach 4, Total 9603
    ## Node 410, Reach 4, Total 9607
    ## Node 411, Reach 2, Total 9609
    ## Node 412, Reach 2, Total 9611
    ## Node 413, Reach 13, Total 9624
    ## Node 414, Reach 3, Total 9627
    ## Node 415, Reach 87, Total 9714
    ## Node 416, Reach 88, Total 9802
    ## Node 417, Reach 94, Total 9896
    ## Node 418, Reach 78, Total 9974
    ## Node 419, Reach 4, Total 9978
    ## Node 420, Reach 5, Total 9983
    ## Node 421, Reach 2, Total 9985
    ## Node 422, Reach 90, Total 10075
    ## Node 423, Reach 8, Total 10083
    ## Node 424, Reach 4, Total 10087
    ## Node 425, Reach 81, Total 10168
    ## Node 426, Reach 2, Total 10170
    ## Node 427, Reach 26, Total 10196
    ## Node 428, Reach 6, Total 10202
    ## Node 429, Reach 6, Total 10208
    ## Node 430, Reach 85, Total 10293
    ## Node 431, Reach 4, Total 10297
    ## Node 432, Reach 7, Total 10304
    ## Node 433, Reach 23, Total 10327
    ## Node 434, Reach 18, Total 10345
    ## Node 435, Reach 4, Total 10349
    ## Node 436, Reach 5, Total 10354
    ## Node 437, Reach 102, Total 10456
    ## Node 438, Reach 4, Total 10460
    ## Node 439, Reach 5, Total 10465
    ## Node 440, Reach 4, Total 10469
    ## Node 441, Reach 5, Total 10474
    ## Node 442, Reach 3, Total 10477
    ## Node 443, Reach 7, Total 10484
    ## Node 444, Reach 89, Total 10573
    ## Node 445, Reach 95, Total 10668
    ## Node 446, Reach 6, Total 10674
    ## Node 447, Reach 5, Total 10679
    ## Node 448, Reach 9, Total 10688
    ## Node 449, Reach 8, Total 10696
    ## Node 450, Reach 5, Total 10701
    ## Node 451, Reach 3, Total 10704
    ## Node 452, Reach 2, Total 10706
    ## Node 453, Reach 4, Total 10710
    ## Node 454, Reach 92, Total 10802
    ## Node 455, Reach 80, Total 10882
    ## Node 456, Reach 2, Total 10884
    ## Node 457, Reach 13, Total 10897
    ## Node 458, Reach 6, Total 10903
    ## Node 459, Reach 93, Total 10996
    ## Node 460, Reach 2, Total 10998
    ## Node 461, Reach 25, Total 11023
    ## Node 462, Reach 8, Total 11031
    ## Node 463, Reach 3, Total 11034
    ## Node 464, Reach 40, Total 11074
    ## Node 465, Reach 11, Total 11085
    ## Node 466, Reach 4, Total 11089
    ## Node 467, Reach 103, Total 11192
    ## Node 468, Reach 78, Total 11270
    ## Node 469, Reach 121, Total 11391
    ## Node 470, Reach 2, Total 11393
    ## Node 471, Reach 3, Total 11396
    ## Node 472, Reach 7, Total 11403
    ## Node 473, Reach 7, Total 11410
    ## Node 474, Reach 2, Total 11412
    ## Node 475, Reach 5, Total 11417
    ## Node 476, Reach 91, Total 11508
    ## Node 477, Reach 2, Total 11510
    ## Node 478, Reach 7, Total 11517
    ## Node 479, Reach 8, Total 11525
    ## Node 480, Reach 2, Total 11527
    ## Node 481, Reach 10, Total 11537
    ## Node 482, Reach 14, Total 11551
    ## Node 483, Reach 2, Total 11553
    ## Node 484, Reach 6, Total 11559
    ## Node 485, Reach 2, Total 11561
    ## Node 486, Reach 2, Total 11563
    ## Node 487, Reach 5, Total 11568
    ## Node 488, Reach 6, Total 11574
    ## Node 489, Reach 2, Total 11576
    ## Node 490, Reach 4, Total 11580
    ## Node 491, Reach 2, Total 11582
    ## Node 492, Reach 10, Total 11592
    ## Node 493, Reach 101, Total 11693
    ## Node 494, Reach 7, Total 11700
    ## Node 495, Reach 4, Total 11704
    ## Node 496, Reach 3, Total 11707
    ## Node 497, Reach 4, Total 11711
    ## Node 498, Reach 3, Total 11714
    ## Node 499, Reach 3, Total 11717
    ## Node 500, Reach 2, Total 11719
    ## Node 501, Reach 4, Total 11723
    ## Node 502, Reach 10, Total 11733
    ## Node 503, Reach 2, Total 11735
    ## Node 504, Reach 2, Total 11737
    ## Node 505, Reach 5, Total 11742
    ## Node 506, Reach 3, Total 11745
    ## Node 507, Reach 86, Total 11831
    ## Node 508, Reach 5, Total 11836
    ## Node 509, Reach 7, Total 11843
    ## Node 510, Reach 2, Total 11845
    ## Node 511, Reach 8, Total 11853
    ## Node 512, Reach 8, Total 11861
    ## Node 513, Reach 7, Total 11868
    ## Node 514, Reach 80, Total 11948
    ## Node 515, Reach 11, Total 11959
    ## Node 516, Reach 6, Total 11965
    ## Node 517, Reach 108, Total 12073
    ## Node 518, Reach 5, Total 12078
    ## Node 519, Reach 5, Total 12083
    ## Node 520, Reach 12, Total 12095
    ## Node 521, Reach 4, Total 12099
    ## Node 522, Reach 3, Total 12102
    ## Node 523, Reach 4, Total 12106
    ## Node 524, Reach 11, Total 12117
    ## Node 525, Reach 4, Total 12121
    ## Node 526, Reach 6, Total 12127
    ## Node 527, Reach 4, Total 12131
    ## Node 528, Reach 5, Total 12136
    ## Node 529, Reach 12, Total 12148
    ## Node 530, Reach 4, Total 12152
    ## Node 531, Reach 2, Total 12154
    ## Node 532, Reach 6, Total 12160
    ## Node 533, Reach 4, Total 12164
    ## Node 534, Reach 6, Total 12170
    ## Node 535, Reach 5, Total 12175
    ## Node 536, Reach 14, Total 12189
    ## Node 537, Reach 5, Total 12194
    ## Node 538, Reach 5, Total 12199
    ## Node 539, Reach 95, Total 12294
    ## Node 540, Reach 78, Total 12372
    ## Node 541, Reach 4, Total 12376
    ## Node 542, Reach 5, Total 12381
    ## Node 543, Reach 4, Total 12385
    ## Node 544, Reach 14, Total 12399
    ## Node 545, Reach 3, Total 12402
    ## Node 546, Reach 79, Total 12481
    ## Node 547, Reach 5, Total 12486
    ## Node 548, Reach 96, Total 12582
    ## Node 549, Reach 6, Total 12588
    ## Node 550, Reach 13, Total 12601
    ## Node 551, Reach 2, Total 12603
    ## Node 552, Reach 4, Total 12607
    ## Node 553, Reach 8, Total 12615
    ## Node 554, Reach 2, Total 12617
    ## Node 555, Reach 101, Total 12718
    ## Node 556, Reach 5, Total 12723
    ## Node 557, Reach 3, Total 12726
    ## Node 558, Reach 4, Total 12730
    ## Node 559, Reach 3, Total 12733
    ## Node 560, Reach 88, Total 12821
    ## Node 561, Reach 5, Total 12826
    ## Node 562, Reach 2, Total 12828
    ## Node 563, Reach 5, Total 12833
    ## Node 564, Reach 4, Total 12837
    ## Node 565, Reach 6, Total 12843
    ## Node 566, Reach 2, Total 12845
    ## Node 567, Reach 3, Total 12848
    ## Node 568, Reach 2, Total 12850
    ## Node 569, Reach 3, Total 12853
    ## Node 570, Reach 11, Total 12864
    ## Node 571, Reach 6, Total 12870
    ## Node 572, Reach 7, Total 12877
    ## Node 573, Reach 10, Total 12887
    ## Node 574, Reach 9, Total 12896
    ## Node 575, Reach 3, Total 12899
    ## Node 576, Reach 2, Total 12901
    ## Node 577, Reach 6, Total 12907
    ## Node 578, Reach 5, Total 12912
    ## Node 579, Reach 7, Total 12919
    ## Node 580, Reach 102, Total 13021
    ## Node 581, Reach 6, Total 13027
    ## Node 582, Reach 10, Total 13037
    ## Node 583, Reach 116, Total 13153
    ## Node 584, Reach 8, Total 13161
    ## Node 585, Reach 9, Total 13170
    ## Node 586, Reach 5, Total 13175
    ## Node 587, Reach 8, Total 13183
    ## Node 588, Reach 6, Total 13189
    ## Node 589, Reach 14, Total 13203
    ## Node 590, Reach 98, Total 13301
    ## Node 591, Reach 3, Total 13304
    ## Node 592, Reach 4, Total 13308
    ## Node 593, Reach 6, Total 13314
    ## Node 594, Reach 3, Total 13317
    ## Node 595, Reach 2, Total 13319
    ## Node 596, Reach 82, Total 13401
    ## Node 597, Reach 7, Total 13408
    ## Node 598, Reach 7, Total 13415
    ## Node 599, Reach 3, Total 13418
    ## Node 600, Reach 3, Total 13421
    ## Node 601, Reach 4, Total 13425
    ## Node 602, Reach 7, Total 13432
    ## Node 603, Reach 2, Total 13434
    ## Node 604, Reach 2, Total 13436
    ## Node 605, Reach 2, Total 13438
    ## Node 606, Reach 83, Total 13521
    ## Node 607, Reach 2, Total 13523
    ## Node 608, Reach 3, Total 13526
    ## Node 609, Reach 12, Total 13538
    ## Node 610, Reach 3, Total 13541
    ## Node 611, Reach 3, Total 13544
    ## Node 612, Reach 4, Total 13548
    ## Node 613, Reach 3, Total 13551
    ## Node 614, Reach 3, Total 13554
    ## Node 615, Reach 11, Total 13565
    ## Node 616, Reach 6, Total 13571
    ## Node 617, Reach 2, Total 13573
    ## Node 618, Reach 11, Total 13584
    ## Node 619, Reach 3, Total 13587
    ## Node 620, Reach 3, Total 13590
    ## Node 621, Reach 2, Total 13592
    ## Node 622, Reach 2, Total 13594
    ## Node 623, Reach 6, Total 13600
    ## Node 624, Reach 6, Total 13606
    ## Node 625, Reach 7, Total 13613
    ## Node 626, Reach 2, Total 13615
    ## Node 627, Reach 83, Total 13698
    ## Node 628, Reach 9, Total 13707
    ## Node 629, Reach 3, Total 13710
    ## Node 630, Reach 3, Total 13713
    ## Node 631, Reach 2, Total 13715
    ## Node 632, Reach 2, Total 13717
    ## Node 633, Reach 3, Total 13720
    ## Node 634, Reach 4, Total 13724
    ## Node 635, Reach 3, Total 13727
    ## Node 636, Reach 9, Total 13736
    ## Node 637, Reach 2, Total 13738
    ## Node 638, Reach 116, Total 13854
    ## Node 639, Reach 7, Total 13861
    ## Node 640, Reach 11, Total 13872
    ## Node 641, Reach 7, Total 13879
    ## Node 642, Reach 9, Total 13888
    ## Node 643, Reach 78, Total 13966
    ## Node 644, Reach 16, Total 13982
    ## Node 645, Reach 2, Total 13984
    ## Node 646, Reach 5, Total 13989
    ## Node 647, Reach 4, Total 13993
    ## Node 648, Reach 2, Total 13995
    ## Node 649, Reach 5, Total 14000
    ## Node 650, Reach 6, Total 14006
    ## Node 651, Reach 13, Total 14019
    ## Node 652, Reach 7, Total 14026
    ## Node 653, Reach 4, Total 14030
    ## Node 654, Reach 10, Total 14040
    ## Node 655, Reach 79, Total 14119
    ## Node 656, Reach 4, Total 14123
    ## Node 657, Reach 3, Total 14126
    ## Node 658, Reach 9, Total 14135
    ## Node 659, Reach 5, Total 14140
    ## Node 660, Reach 3, Total 14143
    ## Node 661, Reach 2, Total 14145
    ## Node 662, Reach 4, Total 14149
    ## Node 663, Reach 6, Total 14155
    ## Node 664, Reach 2, Total 14157
    ## Node 665, Reach 16, Total 14173
    ## Node 666, Reach 2, Total 14175
    ## Node 667, Reach 94, Total 14269
    ## Node 668, Reach 98, Total 14367
    ## Node 669, Reach 3, Total 14370
    ## Node 670, Reach 3, Total 14373
    ## Node 671, Reach 87, Total 14460
    ## Node 672, Reach 2, Total 14462
    ## Node 673, Reach 114, Total 14576
    ## Node 674, Reach 2, Total 14578
    ## Node 675, Reach 86, Total 14664
    ## Node 676, Reach 5, Total 14669
    ## Node 677, Reach 9, Total 14678
    ## Node 678, Reach 4, Total 14682
    ## Node 679, Reach 3, Total 14685
    ## Node 680, Reach 4, Total 14689
    ## Node 681, Reach 4, Total 14693
    ## Node 682, Reach 4, Total 14697
    ## Node 683, Reach 2, Total 14699
    ## Node 684, Reach 6, Total 14705
    ## Node 685, Reach 2, Total 14707
    ## Node 686, Reach 10, Total 14717
    ## Node 687, Reach 10, Total 14727
    ## Node 688, Reach 2, Total 14729
    ## Node 689, Reach 89, Total 14818
    ## Node 690, Reach 11, Total 14829
    ## Node 691, Reach 4, Total 14833
    ## Node 692, Reach 7, Total 14840
    ## Node 693, Reach 8, Total 14848
    ## Node 694, Reach 6, Total 14854
    ## Node 695, Reach 2, Total 14856
    ## Node 696, Reach 4, Total 14860
    ## Node 697, Reach 2, Total 14862
    ## Node 698, Reach 6, Total 14868
    ## Node 699, Reach 5, Total 14873
    ## Node 700, Reach 27, Total 14900
    ## Node 701, Reach 16, Total 14916
    ## Node 702, Reach 2, Total 14918
    ## Node 703, Reach 2, Total 14920
    ## Node 704, Reach 87, Total 15007
    ## Node 705, Reach 4, Total 15011
    ## Node 706, Reach 9, Total 15020
    ## Node 707, Reach 2, Total 15022
    ## Node 708, Reach 24, Total 15046
    ## Node 709, Reach 5, Total 15051
    ## Node 710, Reach 86, Total 15137
    ## Node 711, Reach 4, Total 15141
    ## Node 712, Reach 79, Total 15220
    ## Node 713, Reach 2, Total 15222
    ## Node 714, Reach 7, Total 15229
    ## Node 715, Reach 4, Total 15233
    ## Node 716, Reach 78, Total 15311
    ## Node 717, Reach 2, Total 15313
    ## Node 718, Reach 5, Total 15318
    ## Node 719, Reach 5, Total 15323
    ## Node 720, Reach 2, Total 15325
    ## Node 721, Reach 19, Total 15344
    ## Node 722, Reach 2, Total 15346
    ## Node 723, Reach 3, Total 15349
    ## Node 724, Reach 3, Total 15352
    ## Node 725, Reach 6, Total 15358
    ## Node 726, Reach 4, Total 15362
    ## Node 727, Reach 11, Total 15373
    ## Node 728, Reach 3, Total 15376
    ## Node 729, Reach 99, Total 15475
    ## Node 730, Reach 3, Total 15478
    ## Node 731, Reach 4, Total 15482
    ## Node 732, Reach 3, Total 15485
    ## Node 733, Reach 13, Total 15498
    ## Node 734, Reach 7, Total 15505
    ## Node 735, Reach 6, Total 15511
    ## Node 736, Reach 78, Total 15589
    ## Node 737, Reach 97, Total 15686
    ## Node 738, Reach 3, Total 15689
    ## Node 739, Reach 4, Total 15693
    ## Node 740, Reach 105, Total 15798
    ## Node 741, Reach 2, Total 15800
    ## Node 742, Reach 4, Total 15804
    ## Node 743, Reach 87, Total 15891
    ## Node 744, Reach 8, Total 15899
    ## Node 745, Reach 6, Total 15905
    ## Node 746, Reach 3, Total 15908
    ## Node 747, Reach 3, Total 15911
    ## Node 748, Reach 3, Total 15914
    ## Node 749, Reach 5, Total 15919
    ## Node 750, Reach 4, Total 15923
    ## Node 751, Reach 5, Total 15928
    ## Node 752, Reach 7, Total 15935
    ## Node 753, Reach 87, Total 16022
    ## Node 754, Reach 2, Total 16024
    ## Node 755, Reach 2, Total 16026
    ## Node 756, Reach 8, Total 16034
    ## Node 757, Reach 2, Total 16036
    ## Node 758, Reach 4, Total 16040
    ## Node 759, Reach 4, Total 16044
    ## Node 760, Reach 3, Total 16047
    ## Node 761, Reach 5, Total 16052
    ## Node 762, Reach 4, Total 16056
    ## Node 763, Reach 39, Total 16095
    ## Node 764, Reach 3, Total 16098
    ## Node 765, Reach 3, Total 16101
    ## Node 766, Reach 5, Total 16106
    ## Node 767, Reach 3, Total 16109
    ## Node 768, Reach 5, Total 16114
    ## Node 769, Reach 2, Total 16116
    ## Node 770, Reach 2, Total 16118
    ## Node 771, Reach 24, Total 16142
    ## Node 772, Reach 18, Total 16160
    ## Node 773, Reach 5, Total 16165
    ## Node 774, Reach 7, Total 16172
    ## Node 775, Reach 2, Total 16174
    ## Node 776, Reach 4, Total 16178
    ## Node 777, Reach 14, Total 16192
    ## Node 778, Reach 4, Total 16196
    ## Node 779, Reach 4, Total 16200
    ## Node 780, Reach 4, Total 16204
    ## Node 781, Reach 3, Total 16207
    ## Node 782, Reach 5, Total 16212
    ## Node 783, Reach 115, Total 16327
    ## Node 784, Reach 6, Total 16333
    ## Node 785, Reach 3, Total 16336
    ## Node 786, Reach 2, Total 16338
    ## Node 787, Reach 2, Total 16340
    ## Node 788, Reach 2, Total 16342
    ## Node 789, Reach 2, Total 16344
    ## Node 790, Reach 2, Total 16346
    ## Node 791, Reach 6, Total 16352
    ## Node 792, Reach 16, Total 16368
    ## Node 793, Reach 3, Total 16371
    ## Node 794, Reach 22, Total 16393
    ## Node 795, Reach 99, Total 16492
    ## Node 796, Reach 14, Total 16506
    ## Node 797, Reach 10, Total 16516
    ## Node 798, Reach 6, Total 16522
    ## Node 799, Reach 5, Total 16527
    ## Node 800, Reach 9, Total 16536
    ## Node 801, Reach 7, Total 16543
    ## Node 802, Reach 2, Total 16545
    ## Node 803, Reach 3, Total 16548
    ## Node 804, Reach 2, Total 16550
    ## Node 805, Reach 5, Total 16555
    ## Node 806, Reach 9, Total 16564
    ## Node 807, Reach 3, Total 16567
    ## Node 808, Reach 85, Total 16652
    ## Node 809, Reach 18, Total 16670
    ## Node 810, Reach 6, Total 16676
    ## Node 811, Reach 93, Total 16769
    ## Node 812, Reach 1, Total 16770
    ## Node 813, Reach 1, Total 16771
    ## Node 814, Reach 1, Total 16772
    ## Node 815, Reach 1, Total 16773
    ## Node 816, Reach 1, Total 16774
    ## Node 817, Reach 1, Total 16775
    ## Node 818, Reach 1, Total 16776
    ## Node 819, Reach 1, Total 16777
    ## Node 820, Reach 1, Total 16778
    ## Node 821, Reach 1, Total 16779
    ## Node 822, Reach 1, Total 16780
    ## Node 823, Reach 1, Total 16781
    ## Node 824, Reach 1, Total 16782
    ## Node 825, Reach 1, Total 16783
    ## Node 826, Reach 1, Total 16784
    ## Node 827, Reach 1, Total 16785
    ## Node 828, Reach 1, Total 16786
    ## Node 829, Reach 1, Total 16787
    ## Node 830, Reach 1, Total 16788
    ## Node 831, Reach 1, Total 16789
    ## Node 832, Reach 1, Total 16790
    ## Node 833, Reach 1, Total 16791
    ## Node 834, Reach 1, Total 16792
    ## Node 835, Reach 1, Total 16793
    ## Node 836, Reach 1, Total 16794
    ## Node 837, Reach 1, Total 16795
    ## Node 838, Reach 1, Total 16796
    ## Node 839, Reach 1, Total 16797
    ## Node 840, Reach 1, Total 16798
    ## Node 841, Reach 1, Total 16799
    ## Node 842, Reach 1, Total 16800
    ## Node 843, Reach 1, Total 16801
    ## Node 844, Reach 1, Total 16802
    ## Node 845, Reach 1, Total 16803
    ## Node 846, Reach 1, Total 16804
    ## Node 847, Reach 1, Total 16805
    ## Node 848, Reach 1, Total 16806
    ## Node 849, Reach 1, Total 16807
    ## Node 850, Reach 1, Total 16808
    ## Node 851, Reach 1, Total 16809
    ## Node 852, Reach 1, Total 16810
    ## Node 853, Reach 1, Total 16811
    ## Node 854, Reach 1, Total 16812
    ## Node 855, Reach 1, Total 16813
    ## Node 856, Reach 1, Total 16814
    ## Node 857, Reach 1, Total 16815
    ## Node 858, Reach 1, Total 16816
    ## Node 859, Reach 1, Total 16817
    ## Node 860, Reach 1, Total 16818
    ## Node 861, Reach 1, Total 16819
    ## Node 862, Reach 1, Total 16820
    ## Node 863, Reach 1, Total 16821
    ## Node 864, Reach 1, Total 16822
    ## Node 865, Reach 1, Total 16823
    ## Node 866, Reach 1, Total 16824
    ## Node 867, Reach 1, Total 16825
    ## Node 868, Reach 1, Total 16826
    ## Node 869, Reach 1, Total 16827
    ## Node 870, Reach 1, Total 16828
    ## Node 871, Reach 1, Total 16829
    ## Node 872, Reach 1, Total 16830
    ## Node 873, Reach 1, Total 16831
    ## Node 874, Reach 1, Total 16832
    ## Node 875, Reach 1, Total 16833
    ## Node 876, Reach 1, Total 16834
    ## Node 877, Reach 1, Total 16835
    ## Node 878, Reach 1, Total 16836
    ## Node 879, Reach 1, Total 16837
    ## Node 880, Reach 1, Total 16838
    ## Node 881, Reach 1, Total 16839
    ## Node 882, Reach 1, Total 16840
    ## Node 883, Reach 1, Total 16841
    ## Node 884, Reach 1, Total 16842
    ## Node 885, Reach 1, Total 16843
    ## Node 886, Reach 1, Total 16844
    ## Node 887, Reach 1, Total 16845
    ## Node 888, Reach 1, Total 16846
    ## Node 889, Reach 1, Total 16847
    ## Node 890, Reach 1, Total 16848
    ## Node 891, Reach 1, Total 16849
    ## Node 892, Reach 1, Total 16850
    ## Node 893, Reach 1, Total 16851
    ## Node 894, Reach 1, Total 16852
    ## Node 895, Reach 1, Total 16853
    ## Node 896, Reach 1, Total 16854
    ## Node 897, Reach 1, Total 16855
    ## Node 898, Reach 1, Total 16856
    ## Node 899, Reach 1, Total 16857
    ## Node 900, Reach 1, Total 16858
    ## Node 901, Reach 1, Total 16859
    ## Node 902, Reach 1, Total 16860
    ## Node 903, Reach 1, Total 16861
    ## Node 904, Reach 1, Total 16862
    ## Node 905, Reach 1, Total 16863
    ## Node 906, Reach 1, Total 16864
    ## Node 907, Reach 1, Total 16865
    ## Node 908, Reach 1, Total 16866
    ## Node 909, Reach 1, Total 16867
    ## Node 910, Reach 1, Total 16868
    ## Node 911, Reach 1, Total 16869
    ## Node 912, Reach 1, Total 16870
    ## Node 913, Reach 1, Total 16871
    ## Node 914, Reach 1, Total 16872
    ## Node 915, Reach 1, Total 16873
    ## Node 916, Reach 1, Total 16874
    ## Node 917, Reach 1, Total 16875
    ## Node 918, Reach 1, Total 16876
    ## Node 919, Reach 1, Total 16877
    ## Node 920, Reach 1, Total 16878
    ## Node 921, Reach 1, Total 16879
    ## Node 922, Reach 1, Total 16880
    ## Node 923, Reach 1, Total 16881
    ## Node 924, Reach 1, Total 16882
    ## Node 925, Reach 1, Total 16883
    ## Node 926, Reach 1, Total 16884
    ## Node 927, Reach 1, Total 16885
    ## Node 928, Reach 1, Total 16886
    ## Node 929, Reach 1, Total 16887
    ## Node 930, Reach 1, Total 16888
    ## Node 931, Reach 1, Total 16889
    ## Node 932, Reach 1, Total 16890
    ## Node 933, Reach 1, Total 16891
    ## Node 934, Reach 1, Total 16892
    ## Node 935, Reach 1, Total 16893

``` r
largest_gs <- graph_from_adjacency_matrix(largest_gs)
middle_gs1 <- names(which(comp_gs$membership==which(comp_gs$csize!=1)[1]))
middle_gs1 <- induced_subgraph(g,middle_gs1)
  
# The largest strongly connected components
V(largest_gs)$KE_type <- as.character( ke_all[match(V(largest_gs)$name,ke_all$ID),"Type"] )
V(largest_gs)$ color <- ifelse (  V(largest_gs)$KE_type == "MIE", "lightgreen",   ifelse( V(largest_gs)$KE_type == "KE", "white",  ifelse ( V(largest_gs)$KE_type == "AO", "tomato", "black" ) )  )
# V(largest_gs)$size <-rep(2,   length( V(largest_gs) )   )

l <- layout_with_fr(largest_gs)
plot(largest_gs, vertex.label=NA, vertex.size=5, edge.arrow.size=0.2,  layout = l)
```

![](2019.12.24_AOPwiki_scraping_xml_files/figure-gfm/aop.network.analysis-4.png)<!-- -->

``` r
dist_lgs <- distances(largest_gs)
dist_lgs2 <- dist_lgs
dist_lgs2[!is.finite(dist_lgs2)] <- 0
hist(dist_lgs2)
```

![](2019.12.24_AOPwiki_scraping_xml_files/figure-gfm/aop.network.analysis-5.png)<!-- -->

``` r
# Draw graph again, but with the largest strongly connected components
# The following R code is based on the code by Pollesch et al. (2019)
g2 <- g
ntcomps<-which(comp_gs$csize>1) 
V(g2)$scc <- comp_gs$membership
V(g2)$scc_col<-rgb(1,1,1,alpha=0)
E(g2)$scc_col<-rgb(1,1,1,alpha=0)
sccPal<-hsv(0.59, 0.85, 0.95,alpha=0.6)

for(i in 1:length(ntcomps)){
  V(g2)$scc_col[V(g2)$scc==ntcomps[i]]<- sccPal
  subG<-induced_subgraph(g2, V(g2)[V(g2)$scc==ntcomps[i]])
  E<-as.character(as.vector(t(as_edgelist(subG))))
  E(g2, P=E)$scc_col<- sccPal
}
l2 <- layout_with_fr(g2)
plot(g2, vertex.label=NA, layout=l2, edge.vertex.size=0.4,edge.width=3, vertex.size=3, edge.arrow.size=0.2)
plot(g2, vertex.size=3, vertex.color=V(g2)$scc_col, vertex.frame.color=V(g2)$scc_col, vertex.label=NA,
     edge.width=5, edge.color=E(g2)$scc_col, edge.arrow.size=0, layout=l2, add=TRUE)
```

![](2019.12.24_AOPwiki_scraping_xml_files/figure-gfm/aop.network.analysis-6.png)<!-- -->

``` r
# Investigate KEs of the largest strongly connected components
na.exclude( ke_all[match(V(largest_gs)$name,ke_all$ID),] )
```

    ##                                                          ID Type
    ## aop 11.key-event.id6   56d03689-9f66-494c-af12-f63245f1a8a6   KE
    ## aop 216.key-event-id   7be434bc-3cba-48b4-99d6-61f99e0573fc  MIE
    ## aop 35.key-event.id2   61d97890-1b0e-4bd7-9b0a-bda1e2b3df57   KE
    ## aop 97.key-event.id    25cfcb9c-3378-4a99-8c4b-1f58d490702b   KE
    ## aop 144.key-event.id16 6f70f165-30f7-4a55-8856-822d44180d85   KE
    ## aop 1.key-event.id3    47adbded-a1f2-4ed9-8a51-d963190b3eb5   KE
    ## aop 1.key-event.id4    c8bfc3a8-5ed4-4692-bc7c-4bd5a9a5137c   KE
    ## aop 215.key-event-id   11b09029-3a99-445e-b5c4-2e91cc5f4173  MIE
    ## aop 6.key-event.id3    5bda2a92-86a7-4b96-8e0d-287e67c6ba71   KE
    ## aop 144.key-event.id17 c424f52c-88ef-4b15-b3c0-144aa916dd3d   KE
    ## aop 99.key-event.id4   9914f9a5-cf4b-4c35-9b86-02523903c16b   KE
    ## aop 97.key-event.id1   91056302-2808-444a-a42e-3e7c07dceb10   KE
    ## aop 97.key-event.id5   c8e0cf37-96bf-40cb-8635-ef2e93984519   KE
    ## aop 11.key-event.id5   d3c58429-dbc6-41bc-b9a6-f1de8239aeff   KE
    ## aop 97.key-event.id3   d4a13f11-fa40-4209-a858-e607d799af9c   KE

``` r
rownames( na.exclude( ke_all[match(V(largest_gs)$name,ke_all$ID),] ) )
```

    ##  [1] "aop 11.key-event.id6"   "aop 216.key-event-id"  
    ##  [3] "aop 35.key-event.id2"   "aop 97.key-event.id"   
    ##  [5] "aop 144.key-event.id16" "aop 1.key-event.id3"   
    ##  [7] "aop 1.key-event.id4"    "aop 215.key-event-id"  
    ##  [9] "aop 6.key-event.id3"    "aop 144.key-event.id17"
    ## [11] "aop 99.key-event.id4"   "aop 97.key-event.id1"  
    ## [13] "aop 97.key-event.id5"   "aop 11.key-event.id5"  
    ## [15] "aop 97.key-event.id3"
