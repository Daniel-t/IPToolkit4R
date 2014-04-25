library(stringr)

nslookup <- function (name,type="A",server=NA) {
  cmdline<-"nslookup"
  typeArg<-"-type"
  
  type<-toupper(type)
  
  parsers <-c(A=function(text){
    addrs<-text[grepl("Address:",text)]
    addrs<-addrs[-1]
    addrs<-sapply(strsplit(addrs,split=" "),FUN=function(addr){addr[2]})
    data.frame(addrs,stringsAsFactors=F)
  },
    MX=function(text){
      text<-text[grepl("mail exchanger",text)]
      text<-simplify2array(strsplit(text,split="="))
      mx<-strsplit(str_trim(text[2,]),split=" ")
      mx<-data.frame(t(simplify2array(mx)),stringsAsFactors=F)
      colnames(mx)<-c("Priority","Server")
      mx$Priority=as.integer(mx$Priority)
      mx<-data.frame(mx[order(mx$Priority),2],stringsAsFactors=F)
      mx
  },
    SOA=function(text){
      fields<-text[which(grepl("=",text))]
      fields<-gsub("\t","",fields)
      fields<-strsplit(fields,split=" = ")
      out=NULL
      lapply(fields,FUN=function(fieldrow){out[fieldrow[1]]<<-fieldrow[2]})
      out<-data.frame(out,stringsAsFactors=F)
      colnames(out)<-c("value")
      out
    },
   TXT=function(text){
     fields<-text[which(grepl("=",text))]
     fields<-gsub("\t","",fields)
     fields<-strsplit(fields,"\\\"")
     fields<-sapply(fields,FUN=function(txt){txt[2]})
     data.frame(text=fields,stringsAsFactors=F)
  },
  NS=function(text){
    text<-text[grepl("nameserver = ",text)]
    ns<-simplify2array(strsplit(text,split="="))[2,]
    ns<-substr(ns,start=2,stop=nchar(ns)-1)
    data.frame(ns,stringsAsFactors=F)
  },
  SPF=function(text){
   res<-sapply(regexec("(v=spf.*)",text),FUN=function(res){if (res[1]>1){stop<-attributes(res)$match.length[2];start<-res[2];c(start,stop)}else{c(0,0)}})
   out<-substr(text,res[1,],res[2,]+res[1,]-2)
   out<-out[which(nchar(out)>0)]
   data.frame(spf=out,stringsAsFactors=F);
  },
  AAAA=function(text){
    fields<-text[which(grepl("has AAAA address",text))]
    fields<-strsplit(fields,"has AAAA address ")
    fields<-sapply(fields,FUN=function(txt){txt[2]})
    fields

  }
  
  )

  if(is.null(parsers[type][[1]])){
    stop(paste("Unknown lookup type: ",type))
  }
  out<-lapply(name,FUN=function(name){
    typeArg<-paste(typeArg,type,sep="=")
    cmdline<-paste(cmdline,typeArg)  
      
    cmdline<-paste(cmdline,name)
    
    if(!is.na(server)){
      cmdline<-paste(cmdline,server)
    }
    
    resp<-system(cmdline,intern=T)
    if(grepl(pattern="no servers could be reached",x=paste0(resp,collapse=""))){
      stop("No servers could be reached, lookup failed")
    }
    
    #padding
    if(length(resp)%%2==1) resp[length(resp)+1]<-""
  
    func<-parsers[type][[1]]
    func(resp)
  })
  names(out)<-name
  if (length(name)==1){
    out<-out[[1]]
  }

  out
}


ipSort<-function(ipAddrs,...){
  ipNums<-ip2int(ipAddrs)
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
