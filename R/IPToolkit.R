library(stringr)

nslookup <- function (name,server=NA,type="A",debug=F) {
  cmdline<-"nslookup"
  typeArg<-"-type"
  
  type<-toupper(type)
  
  parsers <-c(A=function(text){
    out<-NULL
    text<-text[-1:-4]
    cn_fields<-grepl("canonical name =",text)
    cnames<-text[cn_fields]
    addrs<-text[!cn_fields]
    tm<-matrix(addrs,ncol=2,byrow=T)
    tm[,1]<-sapply(strsplit(tm[,1],split="\\t"),FUN=function(x){x[2]})
    tm[,2]<-sapply(strsplit(tm[,2],split=" "),FUN=function(x){x[2]})
    out_addrs<-data.frame(tm,stringsAsFactors=F)
    out_addrs<-out_addrs[1:nrow(out_addrs)-1,]
    colnames(out_addrs)<-c("Name","Address")
    out$Address<-out_addrs
    
    if(sum(cn_fields)>0){
      cn<-strsplit(cnames,split="\tcanonical name = ")
      cn<-simplify2array(cn)
      out$CName<-data.frame(cn,stringsAsFactors=F)
      colnames(out$cname)<-c("Name","Alias")
    }
    
    out
#    tm<-matrix(text,ncol=2,byrow=T)
#    tm<-tm[grepl("Name:",tm[,1]),]
#    sapply(strsplit(tm[,2],split=" "),FUN=function(l){l[2]})
  },
    MX=function(text){
      text<-text[grepl("mail exchanger",text)]
      text<-simplify2array(strsplit(text,split="="))
      mx=strsplit(str_trim(text[2,]),split=" ")
      mx<-data.frame(t(simplify2array(mx)),stringsAsFactors=F)
      colnames(mx)<-c("Priority","Server")
      mx$Priority=as.integer(mx$Priority)
      mx<-mx[order(mx$Priority),]
      mx
  },
    SOA=function(text){
      fields<-text[which(grepl("=",text))]
      fields<-gsub("\t","",fields)
      fields<-strsplit(fields,split=" = ")
      out=NULL
      lapply(fields,FUN=function(fieldrow){out[fieldrow[1]]<<-fieldrow[2]})
      out<-data.frame(out)
      colnames(out)<-c("value")
      out
    },
   TXT=function(text){
     fields<-text[which(grepl("=",text))]
     fields<-gsub("\t","",fields)
     fields<-strsplit(fields,"\\\"")
     fields<-sapply(fields,FUN=function(txt){txt[2]})
     fields
  },
  NS=function(text){
    text
  }.
  SPF=function(text){
   text
  },
  AAAA=function(text){
    fields<-text[which(grepl("has AAAA address",text))]
    fields<-strsplit(fields,"has AAAA address ")
    fields<-sapply(fields,FUN=function(txt){txt[2]})
    fields

  }
  
);

  if(is.null(parsers[type][[1]])){
    stop(paste("Unknown lookup type: ",type))
  }
  
  typeArg<-paste(typeArg,type,sep="=")
  cmdline<-paste(cmdline,typeArg)  
    
  cmdline<-paste(cmdline,name)
  
  if(!is.na(server)){
    cmdline<-paste(cmdline,server)
  }
  
  resp<-system(cmdline,intern=T)
  if(debug) d_out<<-resp
  if(grepl(pattern="no servers could be reached",x=paste0(resp,collapse=""))){
    stop("No servers could be reached, lookup failed")
  }
  
  #padding
  if(length(resp)%%2==1) resp[length(resp)+1]<-""

  func<-parsers[type][[1]]
  func(resp)
}


ipSort<-function(ipAddrs,...){
# NOT TESTED
  ipNums<-iptoint(ipAddrs)
  ipAddrs[order(ipNums,...)]
}

loadIPLocationData<-function(filename,format="ip2l"){
  locData<-read.csv(filename,header=F,stringsAsFactors=F)
  cols<-c("IP_Start","IP_End","CountryCode","CountryName","State","City","Latitude","Longitude","ZipCode","Timezone")
  ncols<-ncol(locData);
  if (ncols> length(cols))ncols=length(cols)
  
  colnames(locData)<- cols[1:ncols]
  locData
}

geoIP<-function(IPAddress,dataSet=NA,dsFormat="ip2l"){
  if(!dsFormat %in% c("ip2l")) stop ("Unknown dataset, see help for more information")
  ipNum<-ip2int(IPAddress)
  out<-sapply(ipNum,FUN=function(ip){ dataSet[ip>=dataSet$IP_Start & ip<=dataSet$IP_End,-1:-2]})
  colnames(out)<-IPAddress;
  t(out)
}

ip2int<-function(ipAddress){
    ipStr<-strsplit(ipAddress,split="\\.")
    sapply(ipStr,FUN=function(ip){ip<-as.numeric(ip);16777216*ip[1]+65536*ip[2]+256*ip[3]+ip[4]})
}

int2ip<-function(int){
  a<-floor((int/16777216)%% 256)
  b<-floor((int/65536)%%256)
  c<-floor((int/256)%%256)
  d<-floor(int %% 256)
  paste(a,b,c,d,sep=".")
}
