library(TextWiller)
source("Slang.r")

##toglie gli spazi in prima e ultima posizione
.togliSpaziEsterni <- function(testo){ 
  testo <- gsub("^[[:blank:]]*","",testo)
  testo <- gsub("[[:blank:]]*$","",testo)
  testo
}

##########################

.contaStringhe  <- function(testo,
                            stringhe=c("\\?","\\!","#","@","(???|euro)","(\\$|dollar)")                            
){
  conteggi=sapply(stringhe,function(stringa) str_count(testo,stringa))
  colnames(conteggi)=paste("Conteggi.",colnames(conteggi),sep="")
  conteggi
}

normalizzare <- function(testo, tolower=TRUE,normalizzahtml=TRUE,
                         normalizzacaratteri=TRUE,
                         normalizzaemote=TRUE,
                         normalizzaEmoticons=TRUE,
                         normalizzapunteggiatura=TRUE,
                         normalizzaslang=TRUE,
                         fixed=TRUE,perl=TRUE,
                         preprocessingEncoding=TRUE, encoding="UTF-8", sub="",
                         contaStringhe=c("\\?","\\!","@","#",
                                         "(\u20AC|euro)","(\\$|dollar)",
                                         "SUPPRESSEDTEXT"),
                         suppressInvalidTexts=TRUE,
                         verbatim=TRUE, remove=TRUE,removeUnderscore=FALSE, stopwords = stopwords("it")){
  Sys.setlocale("LC_ALL", "")
  if(preprocessingEncoding) testo<-preprocessingEncoding(testo,
                                                       encoding=encoding,
                                                        sub=sub,
                                                         suppressInvalidTexts=suppressInvalidTexts,
                                                       verbatim=verbatim)
  #######################
  # PREPROCESSING #
  #######################
  # aggiunta spazi per preprocessing
  testo <- paste(" ",testo," ",sep="")
  # normalizza encoding
  if(normalizzacaratteri) testo <- normalizzacaratteri(testo,fixed=fixed)
  # pulizia testo preliminare (html)
  if(normalizzahtml) testo <- normalizzahtml(testo)
  if(!is.null(contaStringhe))
    conteggiStringhe=.contaStringhe(testo,contaStringhe) else
      conteggiStringhe=NULL
    # identifica emote
    #source(paste(functiondir,"/normalizzaemote.R",sep=""), .GlobalEnv)
    
    if(normalizzaemote) testo <- normalizzaemote(testo,perl=perl)
    # pulizia punteggiatura
    #source(paste(functiondir,"/normalizzapunteggiatura.R",sep=""), .GlobalEnv)
    if(normalizzapunteggiatura) testo <- normalizzapunteggiatura(testo,perl=perl,fixed=fixed,removeUnderscore=removeUnderscore)
    if(normalizzaEmoticons) testo<-normalizzaEmoticons(testo)
    # normalizza slang
    #source(paste(functiondir,"/normalizzaslang.R",sep=""), .GlobalEnv)
    if(normalizzaslang) testo <- normalizzareslang(testo,perl=perl)
    # tolower
    if(tolower) testo <- tryTolower(testo,ifErrorReturnText=TRUE)
    if(is.null(remove)) remove=TRUE
    if(is.character(remove)) {
      testo <- removeStopwords(testo,remove)
    } else {
      if(remove) {
        testo <- removeStopwords(testo,stopwords)
      }
    }
    testo <- gsub("\\s+", " ", testo, perl=perl)
    testo <- .togliSpaziEsterni(testo)
    attr(testo,"counts")=conteggiStringhe
    testo
}
