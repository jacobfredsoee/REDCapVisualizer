library(magrittr)
library(XML)

metaData = xmlParse(file = "U:/Projects/gitlabMOMA/REDCapVisualizer/PRIMA_REDCap.xml", useInternalNodes = TRUE, encoding = "UTF-8")
metaData = xmlParse(file = "U:/Projects/gitlabMOMA/REDCapVisualizer/PCProjektPAUH_REDCap.xml", useInternalNodes = TRUE, encoding = "UTF-8")
metaData = xmlParse(file = "U:/Projects/gitlabMOMA/REDCapVisualizer/MDM_.REDCap.xml", useInternalNodes = TRUE, encoding = "UTF-8")

xmltop = metaData %>% xmlRoot

fields = sapply(xmlElementsByTagName(xmltop[["Study"]][["MetaDataVersion"]], "ItemGroupDef"), function(x) {
  name = xmlAttrs(x)["OID"] %>% 
    substr(1, (regexpr("\\.", .) - 1))
  
  
  
  sapply(x %>% xmlChildren, function(y) return(c(name, xmlAttrs(y)["Variable"])))
}) %>% 
  unlist %>% 
  matrix(ncol = 2, byrow = TRUE) %>% 
  as.data.frame(stringsAsFactors = FALSE)

branches = sapply(xmlElementsByTagName(xmltop[["Study"]][["MetaDataVersion"]], "ItemDef"), function(x) {
  if((xmlAttrs(x) %>% names %in% "BranchingLogic" %>% sum) > 0) {
    attr = xmlAttrs(x)
    
    branches = data.frame(start = (attr["BranchingLogic"] %>% as.character %>% gregexpr(pattern = "\\["))[[1]],
                          stop = (attr["BranchingLogic"] %>% as.character %>% gregexpr(pattern = "\\]"))[[1]]) %>% 
      apply(1, function(pos) {
        c(from = substr(attr["BranchingLogic"] %>% as.character, (pos[1]+1), (pos[2]-1)),
          to = attr["Variable"] %>% as.character)
      })
    return(branches)
  } else return(c(NA, NA))
}) %>%
  unlist %>% 
  matrix(ncol = 2, byrow = TRUE) %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  drop_na %>% 
  apply(2, function(col) {
    gsub("\\s*\\([^\\)]+\\)","",as.character(col)) #remove everything between two parentheses
  })  %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  set_colnames(c("from", "to"))


#For PC
#fields = fields %>% subset(V1 != "pc_backup")
#branches = branches %>% subset(from != "pc_backup" & to != "pc_backup")

branches %>% head(10)

nodes = data.frame(nodeName = unique(fields %>% unlist %>% as.character), stringsAsFactors = FALSE)
nodes$number = 1:nrow(nodes)
nodes$type = "origin"
nodes$from = NA
nodes$to = NA

for(i in 1:nrow(nodes)) {
  parentNode = match(nodes[i,"nodeName"], fields$V2)
  
  if(!is.na(parentNode)) {
    nodes[i,"from"] = which(nodes$nodeName == fields$V1[parentNode])
    nodes[i,"to"] = nodes[i,"number"]
  } else {
    nodes[i,"from"] = nodes[i,"number"]
    nodes[i,"to"] = nodes[i,"number"]
  }
}

nodes %>% head(10)

branchNodes = data.frame(nodeName = branches$to,
                         number = nodes[branches$from %>% match(nodes$nodeName), "number"],
                         from = nodes[branches$to %>% match(nodes$nodeName), "number"],
                         to = nodes[branches$from %>% match(nodes$nodeName), "number"],
                         type = rep("BranchingLogic", nrow(branches)))

nodes = rbind(nodes, branchNodes)

nodes$main = ifelse(nodes$nodeName %in% fields$V1, 4, 3)

nodes$mainNode = sapply(nodes$nodeName, function(n) {
  if(n %in% fields$V1) return(n) #a main
  fields$V1[match(n, fields$V2)]
})

nodes$nCon = sapply(nodes$number, function(i) sum(nodes$from == i) + sum(nodes$to == i))

#either main, or popular, while still in origin
topPercentage = 0.5
textLimit = (((nodes %>% subset(main != 4))$nCon %>% max) * topPercentage) %>% ceiling

nodes = nodes %>% cbind(popNodes = ifelse(.[,"type"] == "origin" & (.[,"main"] == 4 | .[,"nCon"] >= textLimit), .[,"nodeName"], ""))

nodes = nodes %>% cbind(nodeNameMains = ifelse(.[,"main"] == 4, .[,"nodeName"], ""))

redcapStructure <- tbl_graph(
  nodes = nodes %>% subset(type == "origin") %>%  select(number, nodeName, main, nodeNameMains, popNodes, mainNode), 
  edges = nodes %>% select(from, to, type, nCon),
  directed = TRUE
) %>% 
  mutate(Popularity = centrality_degree(mode = 'in'))

redcapStructure

ggraph(redcapStructure, layout = "graphopt") + 
  geom_edge_link(aes(color = factor(type)), width = 2) +
  scale_edge_color_manual(values = c("green", "grey70"), 
                          name = "Connection type", 
                          breaks = c("BranchingLogic", "origin"), 
                          labels = c("Branching", "To instrument")) +
  geom_node_point(aes(size = main, color = factor(main))) +
  scale_size_continuous(guide = FALSE) +
  scale_color_manual(values = c("black", "firebrick"), 
                     name = "Node type", 
                     breaks = c(3, 4), 
                     labels = c("Variable", "Instrument")) + 
  geom_node_text(aes(label = popNodes), repel = TRUE) +
  theme_graph() +
  guides(color = guide_legend(override.aes = list(size = c(2,5)))) +
  ggtitle((xmltop[["Study"]][["GlobalVariables"]] %>% xmlChildren)[["StudyName"]] %>% xmlValue,
          subtitle = (xmltop[["Study"]][["GlobalVariables"]] %>% xmlChildren)[["ProjectNotes"]] %>% xmlValue)

c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
  'randomly', 'fr', 'kk', 'drl', 'lgl')





