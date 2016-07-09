library(networkD3)
library(dplyr)
library(xlsx)

## Remove scientific notation
options(scipen=999)

## Read data in
a <- read.xlsx2("ALL_Origincountry-Tocountry_circosfinal_2016_7_7_avannualwgenebank_share.xlsx",1)
str(a)
a[,1] <- as.character(a[,1])
a[,2] <- as.character(a[,2])
a[,3] <- as.character(a[,3])
a[,4] <- as.numeric(a[,4])

## We are interested in international collaboration, 
## so let's take out all the self-contributions
a %>%
  filter(Origin!=Genebank_country & 
           Genebank_country != Recipient) -> a

## Step 0, make 3 levels
# a[,1] <- paste(a[,1]," >",sep="")
# a[,3] <- paste("> ",a[,3],sep="")

## or just 2 levels? To Genebank and back to see how ti works:
a[,2] <- paste(a[,2],"GB",sep="")

## Step 1, get SOURCE -> Genebank relationships
a %>%
  group_by(Origin,Genebank_country) %>%
  summarize(Val=sum(Average.no.samples.per.year)) -> Source2GB

names(Source2GB)[1:2] <- c("FROM","TO") 

## Step 2, get GB -> Sink
a %>%
  group_by(Genebank_country,Recipient) %>%
  summarize(Val=sum(Average.no.samples.per.year)) -> GB2Sink

names(GB2Sink)[1:2] <- c("FROM","TO") 


## Combine
Boff <- bind_rows(Source2GB,GB2Sink)

## And now see what the biggest players are:
Boff %>% filter(Val>50000) -> Boff


## get nodes
nodes <- data.frame(name=Boff[,1:2] %>% unlist %>% as.character() %>% unique())

## and match to IDs to make edges
Boff$From1 <- match(Boff$FROM,nodes$name)
Boff$To1 <- match(Boff$TO,nodes$name)

## Clean up into Edges df
Edges <- Boff %>% select(source=From1,target=To1,value=Val)

EdgesPlot <- Edges

## Edges are 0 indexed
EdgesPlot$source <- EdgesPlot$source -1
EdgesPlot$target <- EdgesPlot$target-1


# Create graph
sankeyNetwork(Links = EdgesPlot, Nodes = nodes,
              Value = 'value', NodeID = 'name',
              fontSize = 12, nodeWidth = 30)
