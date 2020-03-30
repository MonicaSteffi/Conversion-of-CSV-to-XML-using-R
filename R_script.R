library(parallel)
all_files_1<-list.files("file_path", full.names = TRUE)
apply_fun <- function(df) {
 ##initializing the newXMLDocument
  Fin_Doc = newXMLDoc()
  root = newXMLNode("gxl", doc = Fin_Doc)
  graph = newXMLNode("graph", parent = root,
                     attrs = c(id="Co-occurance Network", edgeids="true", edgemode="undirected"))
    ##########adding the node id and attribute_name###############################
  foreach(w=as.vector(unique(sort('column_name'))),y = as.vector(unique('column_name')), s=as.vector(unique('column_name')),d=as.vector(unique('column_name'))) %do%{
    (grp_node = newXMLNode("node", parent = graph, attrs=c(id=paste0("_",s,sep=""))))
    (attr_name = newXMLNode("attr",parent = grp_node, text="", attrs=c(name="OTU")))
    (otu_id=newXMLNode("int",parent = attr_name, text="",d ))
    (bacteria=newXMLNode("attr",parent = grp_node, text="", attrs=c(name="Bacteria")))
    (string_name=newXMLNode("string",parent = bacteria, text="",w))
  }
  ####################edge from otuids##########################
  foreach(s=as.vector('column_name'),q=as.vector('column_name'), z=as.vector('column_name')) %do% {
    (edge_node1 = newXMLNode("edge", parent=graph, attrs=c(from=paste0("_", q,sep=""), to=paste0("_", s,sep=""))))
    (attrs_node1=newXMLNode("attr", parent=edge_node1, text=" ", attrs = c(name="nlogratio")))
    (weight_node1= newXMLNode("float", as.character(z), parent=attrs_node1, text=" "))
  }
  return(Fin_Doc)
}

mclapply(seq_along(all_files_1), mc.cores=5, function(x) saveXML(apply_fun(read.csv(all_files_1[x])), all_files_1[x], row.names = FALSE))

