# ODDS

library(shiny)

options(scipen = 999)

VALORAR <- c("Carta alta",
             "Um par",
             "Dois pares",
             "Trinca",
             "Sequência",
             "Flush",
             "Full House",
             "Quadra",
             "Street Flush",
             "Royal Street Flush")

deck <-
  c("2c", "2d", "2h", "2s", "3c", "3d", "3h", "3s", "4c", "4d", "4h", "4s",
    "5c", "5d", "5h", "5s", "6c", "6d", "6h", "6s", "7c", "7d", "7h", "7s",
    "8c", "8d", "8h", "8s", "9c", "9d", "9h", "9s", "Tc", "Td", "Th", "Ts",
    "Jc", "Jd", "Jh", "Js", "Qc", "Qd", "Qh", "Qs", "Kc", "Kd", "Kh", "Ks",
    "Ac", "Ad", "Ah", "As")

DECK.SHINY <-
  c(`2♣` = "2c", `2♦` = "2d", `2♠` = "2s", `2♥` = "2h", 
    `3♣` = "3c", `3♦` = "3d", `3♠` = "3s", `3♥` = "3h",
    `4♣` = "4c", `4♦` = "4d", `4♠` = "4s", `4♥` = "4h",
    `5♣` = "5c", `5♦` = "5d", `5♠` = "5s", `5♥` = "5h",
    `6♣` = "6c", `6♦` = "6d", `6♠` = "6s", `6♥` = "6h",
    `7♣` = "7c", `7♦` = "7d", `7♠` = "7s", `7♥` = "7h", 
    `8♣` = "8c", `8♦` = "8d", `8♠` = "8s", `8♥` = "8h",
    `9♣` = "9c", `9♦` = "9d", `9♠` = "9s", `9♥` = "9h",
    `T♣` = "Tc", `T♦` = "Td", `T♠` = "Ts", `T♥` = "Th",
    `J♣` = "Jc", `J♦` = "Jd", `J♠` = "Js", `J♥` = "Jh",
    `Q♣` = "Qc", `Q♦` = "Qd", `Q♠` = "Qs", `Q♥` = "Qh",
    `K♣` = "Kc", `K♦` = "Kd", `K♠` = "Ks", `K♥` = "Kh",
    `A♣` = "Ac", `A♦` = "Ad", `A♠` = "As", `A♥` = "Ah")

ranking.range <-
  c("AA.", "KK.", "QQ.", "JJ.", "TT.", "99.", "AKs", "AQs", "88.", 
    "AJs", "AKo", "KQs", "ATs", "AQo", "77.", "KJs", "A9s", "KTs", 
    "AJo", "KQo", "QJs", "A8s", "66.", "QTs", "ATo", "K9s", "A7s", 
    "KJo", "JTs", "A6s", "A5s", "KTo", "55.", "Q9s", "A9o", "QJo", 
    "A4s", "K8s", "A3s", "A8o", "K7s", "J9s", "A2s", "QTo", "Q8s", 
    "K6s", "K9o", "A7o", "T9s", "44.", "JTo", "K5s", "J8s", "A5o", 
    "Q7s", "A6o", "Q9o", "K4s", "T8s", "K3s", "K8o", "A4o", "98s", 
    "Q6s", "K7o", "K2s", "J7s", "A3o", "33.", "J9o", "Q5s", "T7s", 
    "Q8o", "T9o", "A2o", "K6o", "Q4s", "J6s", "97s", "Q3s", "J5s", 
    "K5o", "87s", "J8o", "22.", "T6s", "Q2s", "Q7o", "J4s", "96s", 
    "K4o", "T8o", "86s", "J3s", "76s", "98o", "Q6o", "J7o", "K3o", 
    "T5s", "T4s", "K2o", "J2s", "95s", "Q5o", "T3s", "T7o", "85s", 
    "65s", "Q4o", "75s", "J6o", "T2s", "97o", "94s", "87o", "Q3o", 
    "93s", "54s", "84s", "J5o", "64s", "T6o", "74s", "Q2o", "92s", 
    "J4o", "53s", "96o", "86o", "76o", "73s", "83s", "63s", "J3o", 
    "T5o", "82s", "43s", "J2o", "T4o", "95o", "52s", "62s", "72s", 
    "85o", "65o", "75o", "T3o", "42s", "32s", "T2o", "94o", "54o", 
    "84o", "64o", "93o", "74o", "92o", "53o", "83o", "63o", "73o", 
    "43o", "82o", "52o", "62o", "42o", "32o", "72o")

equidade.shiny <-
  function (N,RANGE,CCC,b1,b2,b3,b4,b5) {
    
    vic <- NULL
    tie <- NULL
    tamanho.tie <- NULL
    
    VEZES <- 300
    
    for (i in 1:VEZES) {
      vencedores<-nova.rodada(N,RANGE,CCC,b1,b2,b3,b4,b5)
      
      if (length(vencedores) == 1) {
        vic <- append(vic,as.numeric(vencedores)) }
      if (length(vencedores) != 1) {
        tie <- append(tie,as.numeric(vencedores))
        tamanho.tie <- grep(1,tie)
      }
    }
    
    CCC[,1][CCC[,1] == ""] <- "range"
    
    alfabeto <- c("A","B","C","D","E","F","G","H","I")
    
    for (i in 1:N){
      percentual <- paste0(
        round(
          length(grep(i,vic))/300
          ,2)*100,"%"
      )
      percentual.tie <- paste0(
        round(
          length(grep(i,tie))/300
          ,2)*100,"%")
      if (CCC[i,1] != "range"){
        cat(" /","Player",alfabeto[i],"/",naipes(paste(c(CCC[i,1],CCC[i,2]))),"/ Win:",percentual,"/ Tie:",percentual.tie,"/",'\n')
      }
      else {
        cat(" /","Player",alfabeto[i],"/",naipes(paste(c(CCC[i,1]))),"/ Win:",percentual,"/ Tie:",percentual.tie,"/",'\n')
      }
    }
  }

naipes <-
  function(x) {
    novo<-gsub("c","♣", x)
    novo<-gsub("s","♠",novo)
    novo<-gsub("d","♦",novo)
    novo<-gsub("h","♥",novo)
    novo<-gsub("T","10",novo)
    return(novo)
  }

formar.deck <- function() {
  suits <- c('c','d','h','s')
  ranks <- c(2:9,"T","J","Q","K","A")
  ranks <- factor(2:14,2:14,ranks)
  
  deck <- data.frame(card=0)
  
  for (i in ranks) {
    for (s in suits) {
      deck <- rbind(deck,paste0(i,s))
    }
  }
  deck<-deck[-1,]
}

full.deck <- formar.deck()

nova.mao <-
  function(x1,x2,y1,y2,y3,y4,y5) {
    
    suits <- c('c','d','h','s')
    ranks <- c(2:9,"T","J","Q","K","A")
    ranks <- factor(2:14,2:14,ranks)
    
    deck <- full.deck
    
    sua.mao<- c(x1,x2)
    
    board <- c(y1,y2,y3,y4,y5)
    
    pontos <- data.frame(par=1,trinca=2,sequencia=3,flush=4,full.house=5,quadra=6,street=7,royal.street=8)
    
    board.mais.cartas <- c(substring(sua.mao,1,1),substring(board,1,1))
    
    for (i in unique(board.mais.cartas[duplicated(board.mais.cartas)]) ) {
      if (sum(board.mais.cartas %in% i) == 2) { pontos <- rbind(pontos,c(i,0,0,0,0,0,0,0)) }
      if (sum(board.mais.cartas %in% i) == 3) { pontos <- rbind(pontos,c(0,i,0,0,0,0,0,0)) }
      if (sum(board.mais.cartas %in% i) == 4) { pontos <- rbind(pontos,c(0,0,0,0,0,i,0,0)) }
    }

    # Armazena frequências de naipes para ver se bateu flush
    h <- sum(c(substring(sua.mao,2),substring(board,2)) %in% "h")
    c <- sum(c(substring(sua.mao,2),substring(board,2)) %in% "c")
    s<-  sum(c(substring(sua.mao,2),substring(board,2)) %in% "s")
    d<-  sum(c(substring(sua.mao,2),substring(board,2)) %in% "d")
    
    i <- c("h","c","s","d")[c(h,c,s,d) >= 5]
    
    flush.7 <- NULL
    if (length(i) != 0) {
      pontos <- rbind(pontos,c(0,0,0,i,0,0,0,0))
      # mostra flush
      flush.hand <- board.mais.cartas[grep(i,c(sua.mao,board),fixed = T)]
      flush.hand <- flush.hand[order(match(flush.hand, ranks))] # organizar as cartas em ordem de acordo com o ranking estabelecido
      flush.7 <- flush.hand
      if (length(flush.hand) == 6) flush.hand <- flush.hand[-1] # remove excedentes
      if (length(flush.hand) == 7) flush.hand <- flush.hand[c(-1,-2)] # remove excedentes
      meu.flush <- paste0(flush.hand)
    }
    
    # prepara para verificar se bateu sequência
    sequencia <- NULL
    
    # coloca as cartas em ordem
    em.ordem <- board.mais.cartas[order(match(board.mais.cartas, ranks))]
    
    # remove cartas repetidas
    em.ordem <- unique(em.ordem)
    
    # verifica os elementos que estão em sequencia
    checagem.sequencia <- cumsum(c(1, diff(match(em.ordem,ranks)) != 1))
    u.seq <- unique(checagem.sequencia)
    u.max <- which.max(tabulate(match(checagem.sequencia, u.seq)))
    
    if(sum(checagem.sequencia %in% u.max) >= 5) {
      sequencia <- em.ordem[checagem.sequencia %in% u.max] # armazena a sequência
      if (length(sequencia) == 6) sequencia <- sequencia[-1]
      if (length(sequencia) == 7) sequencia <- sequencia[c(-1,-2)] # remove excedentes
      pontos <- rbind(pontos,c(0,0,"SIM",0,0,0,0,0)) # registra que há sequência
      # print(sequencia)
    }
    
    # identifica se é street flush
    street <- NULL
    if ( length(flush.7) > 0 ) {
      
      ver.street <- cumsum( c(1,diff(match(flush.7,ranks)) != 1) )
      ux <- unique(ver.street)
      gr <- which.max(tabulate(match(ver.street, ux)))
      ver.street <- grep(gr,ver.street,fixed = T)
      street <- flush.7[ver.street]
      if (length(street) == 7) street <- street[c(-1,-2)] # remove excedentes
      if (length(street) == 6) street <- street[-1]
      if (length(street) == 5) {
        pontos <- rbind(pontos,c(0,0,0,0,0,0,"SIM",0))
        # identifica se é royal
        if ("A" %in% street) pontos <- rbind(pontos,c(0,0,0,0,0,0,0,"SIM"))
      }
      
    }
    
    pontos <- pontos[!duplicated(pontos),]
    pontos <- pontos[-1,]
    rownames(pontos) <- NULL
    
    # verifica se há três pares e exclui o mais fraco
    if (sum(pontos$par != 0)==3) {
      os.tres <- pontos$par
      os.tres.em.ordem <- os.tres[order(match(os.tres, ranks))]
      os.dois.maiores <- os.tres.em.ordem[-1]
      
      pontos$par[1] <- 0
      pontos$par[2] <- 0
      pontos$par[3] <- 0
      pontos <- rbind(pontos,c(os.dois.maiores[1],0,0,0,0,0,0,0,0))
      pontos <- rbind(pontos,c(os.dois.maiores[2],0,0,0,0,0,0,0,0))
      pontos <- pontos[apply(pontos, 1, function(x) !all(x==0|is.na(x))),]
    }
    # verifica se há dois pares e uma trinca e exclui o par mais fraco
    else if (sum( pontos$par != 0 )==2 && sum(pontos$trinca != 0)==1) {
      os.tres <- pontos$par
      os.tres.em.ordem <- os.tres[order(match(os.tres, ranks))]
      os.dois.maiores <- os.tres.em.ordem[-3]
      
      pontos$par[1] <- 0
      pontos$par[2] <- 0
      pontos$par[3] <- 0
      pontos <- rbind(pontos,c(os.dois.maiores[2],0,0,0,0,0,0,0,0))
      pontos <- rbind(pontos,c(os.dois.maiores[3],0,0,0,0,0,0,0,0))
      pontos <- pontos[apply(pontos, 1, function(x) !all(x==0|is.na(x))),]
    }
    # verifica se há duas trincas e exclui a mais fraca
    else if (sum(pontos$trinca != 0)==2) {
      as.duas <- c(pontos$trinca[1],pontos$trinca[2])
      as.duas.em.ordem <- as.duas[order(match(as.duas, ranks))]
      
      pontos$trinca[1] <- 0
      pontos$trinca[2] <- 0
      pontos <- rbind(pontos,c(0,as.duas.em.ordem[2],0,0,0,0,0,0))
      pontos <- rbind(pontos,c(as.duas.em.ordem[1],0,0,0,0,0,0,0))
      pontos <- pontos[apply(pontos, 1, function(x) !all(x==0|is.na(x))),]
    }
    
    # FULL HOUSE
    if(nrow(pontos)>1) {
      if (pontos$par[1] != 0 && pontos$trinca[2] != 0) { 
        pontos$full.house[1] <- "SIM" }} # confirma full house
    if(nrow(pontos)>1) {
      if (pontos$par[2] != 0 && pontos$trinca[1] != 0) { 
        pontos$full.house[1] <- "SIM" }} # confirma full house
    
    if (nrow(pontos) == 0) pontos <- data.frame(par=0,trinca=0,sequencia=0,flush=0,full.house=0,quadra=0,street=0,royal.street=0)
    
    # ROYAL
    if ("SIM" %in% pontos$royal) { 
      mao.final <- NULL
      mao.final <- c("Royal Street Flush",sequencia[5:1]) }
    
    # STREET FLUSH
    else if ("SIM" %in% pontos$street) { 
      mao.final <- NULL
      mao.final <- c("Street Flush",sequencia[5:1]) }
    
    # QUADRA
    else if (sum(pontos$quadra != 0)==1) { 
      mao.final <- NULL
      quadra <- pontos$quadra[pontos$quadra != 0]
      sobra <- board.mais.cartas[!board.mais.cartas %in% quadra]
      sobra.em.ordem <- sobra[order(match(sobra, ranks))]
      carta.alta <- sobra.em.ordem[length(sobra.em.ordem)]
      hand.final <- c(carta.alta,quadra[1],quadra[1],quadra[1],quadra[1])
      mao.final <- c("Quadra",hand.final[5:1]) }
    
    # VERIFICA FULL HOUSE
    else if ("SIM" %in% pontos$full.house) { 
      mao.final <- NULL
      meu.par <- pontos$par[pontos$par !=0]
      minha.trinca<- pontos$trinca[pontos$trinca !=0]
      hand.final <- c(minha.trinca[1],minha.trinca[1],minha.trinca[1],meu.par[1],meu.par[1])
      mao.final <- c("Full House",hand.final) }
    
    # FLUSH
    else if ( sum(pontos$flush !=0 )==1)  { 
      mao.final <- NULL
      mao.final <- c("Flush",meu.flush[5:1]) }
    
    # SEQUENCIA
    else if ("SIM" %in% pontos$sequencia) { 
      mao.final <- NULL
      mao.final <- c("Sequência",sequencia[5:1]) }
    
    # TRINCA FUNCIONANDO
    else if (sum(pontos$trinca !=0 )==1) { 
      mao.final <- NULL
      
      trinca <- pontos$trinca[pontos$trinca != 0]
      sobra <- board.mais.cartas[!board.mais.cartas %in% trinca]
      sobra.em.ordem <- sobra[order(match(sobra, ranks))]
      cartas.altas <- sobra.em.ordem[length(sobra.em.ordem):1]
      cartas.altas <- cartas.altas[1:2]
      hand.final <- c(cartas.altas[2],cartas.altas[1],trinca[1],trinca[1],trinca[1])
      
      mao.final <- c("Trinca",hand.final[5:1]) }
    
    # DOIS PARES
    else if (sum(pontos$par != 0)==2) { 
      mao.final <- NULL
      
      dois.pares <- pontos$par[pontos$par != 0]
      dois.pares.em.ordem <- dois.pares[order(match(dois.pares, ranks))]
      sobra <- board.mais.cartas[!board.mais.cartas %in% dois.pares]
      sobra.em.ordem <- sobra[order(match(sobra, ranks))]
      carta.alta <- sobra.em.ordem[length(sobra.em.ordem)]
      hand.final <- c(carta.alta,dois.pares.em.ordem[1],dois.pares.em.ordem[1],dois.pares.em.ordem[2],dois.pares.em.ordem[2])
      
      mao.final <- c("Dois pares",hand.final[5:1]) }
    
    else if (sum(pontos$par !=0)==1) {
      mao.final <- NULL
      meu.par <- pontos$par[pontos$par != 0]
      sobra <- board.mais.cartas[!board.mais.cartas %in% meu.par]
      sobra.em.ordem <- sobra[order(match(sobra, ranks))]
      cartas.altas <- sobra.em.ordem[length(sobra.em.ordem):1]
      cartas.altas <- cartas.altas[1:3]
      hand.final <- c(cartas.altas[3],cartas.altas[2],cartas.altas[1],meu.par[1],meu.par[1])
      mao.final <- c("Um par",hand.final[5:1]) }
    
    else {
      sobra.em.ordem <- board.mais.cartas[order(match(board.mais.cartas, ranks))]
      hand.final <- sobra.em.ordem[3:7]
      mao.final <- c("Carta alta",hand.final[5:1]) }
    
    mao.final <- c(mao.final[1],gsub("A","14",mao.final[2:6])) # substituição para permitir o cálculo das mãos em 
    mao.final <- c(mao.final[1],gsub("K","13",mao.final[2:6])) # substituição para permitir o cálculo das mãos em 
    mao.final <- c(mao.final[1],gsub("Q","12",mao.final[2:6])) # substituição para permitir o cálculo das mãos em 
    mao.final <- c(mao.final[1],gsub("J","11",mao.final[2:6])) # substituição para permitir o cálculo das mãos em 
    mao.final <- c(mao.final[1],gsub("T","10",mao.final[2:6])) # substituição para permitir o cálculo das mãos em nova.rodada
    
    return(mao.final)
    
  }

nova.rodada <-
  function(N,RANGE,ccc,b1,b2,b3,b4,b5) {
    
    jogadores<-data.frame(card1=0,card2=0)
    
    for (i in 1:N) {
      if (ccc[i,1] != 0 & ccc[i,1] != "") {
        jogadores[i,] <- ccc[i,]
        for (i in paste(jogadores[i,])) {
          deck <- deck[-grep(i,deck)] }
      }
      else {
        jogadores[i,]<-sortear.cartas.range(RANGE)
        while (sum( jogadores[i,] %in% c(c(b1,b2,b3,b4,b5),jogadores$card1[1:i-1],jogadores$card2[1:i-1]) ) > 0) { jogadores[i,]<-sortear.cartas.range(RANGE) } # ESSE LOOP EVITA CARTA REPETIDA
        for (i in paste(jogadores[i,])) {
          deck <- deck[-grep(i,deck)] }
      }
    }
    
    if (b1 == 0) { 
      b1 <- sample(deck,1) }
    deck <- deck[-grep(b1,deck)]
    
    if (b2 == 0) { 
      b2 <- sample(deck,1) }
    deck <- deck[-grep(b2,deck)]
    
    if (b3 == 0) { 
      b3 <- sample(deck,1) }
    deck <- deck[-grep(b3,deck)]
    
    if (b4 == 0) { 
      b4 <- sample(deck,1) }
    deck <- deck[-grep(b4,deck)]
    
    if (b5 == 0) { 
      b5 <- sample(deck,1) }
    deck <- deck[-grep(b5,deck)]
    
    board <- c(b1,b2,b3,b4,b5)
    
    # organizar players
    players<-NULL
    for (i in 1:N) {
      players <- rbind(players,nova.mao(jogadores[i,1],jogadores[i,2],board[1],board[2],board[3],board[4],board[5])) # registra a melhor combinação do jogador 
    }
    
    #cat('\n','\n',"Board:",naipes(board),'\n','\n')
    
    #exibir players
    #for (i in 1:N) {
    #  cat(paste(" Jogador",i,""))
    #  cat("",naipes(paste0("",jogadores[i,],"")),"")
    #  cat(paste(players[i,1]),'\n','\n')
    #}
    
    # COMANDO PARA COMPARAR CARTAS
    comparar<-NULL
    for (i in 1:N) {
      comparar<- append(comparar,match(players[i,1],VALORAR))
    }
    venceu <- which(comparar == max(comparar))
    
    if (length(venceu)==1) {
      
      #cat(paste0(">>> Jogador ",venceu," venceu",'\n'))
      
      VENCEDOR <- venceu
    }
    
    
    
    if (length(venceu)!=1) {
      comparar.cards <- data.frame()
      for (i in venceu) {
        comparar.cards <- rbind(comparar.cards,as.numeric(players[i,][2:6])) }
      rownames(comparar.cards) <- venceu
      colnames(comparar.cards) <- c("[,1]","[,2]","[,3]","[,4]","[,5]")
      
      #até aqui está ok
      
      n<-1
      
      while (length(which(comparar.cards[,n] == max(comparar.cards[,n]))) > 1) {
        comparar.cards[-which(comparar.cards[,n] == max(comparar.cards[,n])),] <- 0
        
        n<-n+1
        if (n==6) { break }
      }
      if (n < 6) comparar.cards[-which(comparar.cards[,n] == max(comparar.cards[,n])),] <- 0
      
      
      comparar.cards <- comparar.cards[as.logical(rowSums(comparar.cards != 0)), ]
      
      #cat(paste0(">>> Jogador ",rownames(comparar.cards)," venceu",'\n'))
      
      VENCEDOR <- rownames(comparar.cards)
    }
    return(VENCEDOR)
  }

tabela.ok <-
  structure(list(c("AA.", "AKo", "AQo", "AJo", "ATo", "A9o", "A8o", "A7o", "A6o", "A5o", "A4o", "A3o", "A2o"),
                 c("AKs", "KK.", "KQo", "KJo", "KTo", "K9o", "K8o", "K7o", "K6o", "K5o", "K4o", "K3o", "K2o"),
                 c("AQs", "KQs", "QQ.", "QJo", "QTo", "Q9o", "Q8o", "Q7o", "Q6o", "Q5o", "Q4o", "Q3o", "Q2o"),
                 c("AJs", "KJs", "QJs", "JJ.", "JTo", "J9o", "J8o", "J7o", "J6o", "J5o", "J4o", "J3o", "J2o"),
                 c("ATs", "KTs", "QTs", "JTs", "TT.", "T9o", "T8o", "T7o", "T6o", "T5o", "T4o", "T3o", "T2o"),
                 c("A9s", "K9s", "Q9s", "J9s", "T9s", "99.", "98o", "97o", "96o", "95o", "94o", "93o", "92o"),
                 c("A8s", "K8s", "Q8s", "J8s", "T8s", "98s", "88.", "87o", "86o", "85o", "84o", "83o", "82o"),
                 c("A7s", "K7s", "Q7s", "J7s", "T7s", "97s", "87s", "77.", "76o", "75o", "74o", "73o", "72o"),
                 c("A6s", "K6s", "Q6s", "J6s", "T6s", "96s", "86s", "76s", "66.", "65o", "64o", "63o", "62o"),
                 c("A5s", "K5s", "Q5s", "J5s", "T5s", "95s", "85s", "75s", "65s", "55.", "54o", "53o", "52o"),
                 c("A4s", "K4s", "Q4s", "J4s", "T4s", "94s", "84s", "74s", "64s", "54s", "44.", "43o", "42o"),
                 c("A3s", "K3s", "Q3s", "J3s", "T3s", "93s", "83s", "73s", "63s", "53s", "43s", "33.", "32o"),
                 c("A2s", "K2s", "Q2s", "J2s", "T2s", "92s", "82s", "72s", "62s", "52s", "42s", "32s", "22.")
                 ), row.names = c(NA, -13L), class = "data.frame")

range.shiny <-
  function(x) {
    
    pfr <- ceiling(169*x/100)
    
    filtrar.p <- ranking.range[1:pfr]
    
    cortar <- ranking.range[!(ranking.range %in% filtrar.p)]
    
    for (i in 1:13) {
      for (j in 1:13){
        if (tabela.ok[i,j] %in% cortar){
          tabela.ok[i,j] <- "---"
        }
        if (j == 1) {
          cat(" ")
        }
        cat(paste(tabela.ok[i,j]),"")
      }
      cat('\n')
    }
  }

sortear.cartas.range <-
  function(RANGE) {
    
    range <- RANGE/100
    
    ranking.filtrado <- ranking.range[1:round(length(ranking.range)*range)]
    
    cartas.split <- strsplit(sample(ranking.filtrado,1),"")[[1]]
    
    C1 <- cartas.split[1]
    C2 <- cartas.split[2]
    SUIT <- cartas.split[3]
    
    if (SUIT == "s") {
      
      naipe <- sample(c("c","h","s","d"),1)
      
      carta1 <- paste0(C1,naipe)
      
      deck <- deck[-grep(carta1,deck)]
      
      carta2 <- paste0(C2,naipe)
      
      deck <- deck[-grep(carta2,deck)]
      
      cartas <- c(carta1,carta2) }
    
    if (SUIT == "o"| SUIT == ".") {
      lista.de.naipes <- c("c","h","s","d")
      naipe <- sample(lista.de.naipes,1)
      lista.de.naipes <- lista.de.naipes[-grep(naipe,lista.de.naipes)]
      
      carta1 <- paste0(C1,naipe)
      deck <- deck[-grep(carta1,deck)]
      
      naipe <- sample(lista.de.naipes,1)
      
      carta2 <- paste0(C2,naipe)
      deck <- deck[-grep(carta2,deck)]
      
      cartas <- c(carta1,carta2)
      
    }
    
    return(cartas)
    
  }

rednaipes <- grep("h|d",deck,value = T)

texto.naipes <- function(){
  
  reds <- paste0("[data-value=","","\"",rednaipes,"","\"]")
  
  completo <-   paste(
    ".selectize-input",reds,
    "{ color: red; }
   .selectize-dropdown", reds,
    "{ color: red; }"
  )
  
  reds.completo <- capture.output(cat(completo))
  reds.completo
  
}

reds.completo <- texto.naipes()

lista.alfabetica <- c("input.A_2","input.B_2","input.C_2","input.D_2","input.E_2","input.F_2","input.G_2","input.H_2","input.I_2")

lista.inputs <- c(sort(c(paste0(LETTERS[1:9],"_1"),paste0(LETTERS[1:9],"_2"))),"flop1","flop2","flop3","turn","river","nplayers")

ampliar.fonte <- function(){
  texto.todo <- NULL
  for (i in lista.inputs){
    texto.todo <-
      append(texto.todo,
             paste(
               paste0("#",i),"+ div>.selectize-input {
                                                      font-size: 24px !important;
                                                      line-height: 24px !important;
                                                      }"
             ))
    texto.todo <- 
      append(texto.todo,
             paste(
               paste0("#",i),"+ div>.selectize-dropdown {
                                                    font-size: 24px;
                                                    line-height: 24px;
                                                    }"
             ))
  }
  texto.final <- capture.output(cat(texto.todo))
  texto.final
}

texto.final <- ampliar.fonte()

BACKGROUND <- function(){
  tags$head(
    tags$style(paste0(
      "body { ",
      "  content: ''; ",
      "  background-color: WhiteSmoke }",
      "
.irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
  background: black;
  border-color: black;
}

*:focus:not(.focus-visible) {
  outline: 0 !important;
  box-shadow: none !important;
}

.btn {
border-color:black;
background-color: transparent;}

.h5, h5 {
    font-size: 20px;
    width: 500px;
    color: #34495E;
    text-align: center;
}

footer {
background: black;
width: 100%;
height: 30px;
position: fixed;
z-index: 9999;
margin: auto;
bottom: 0;
left: 0;
text-align: right;
color: darkgray;
line-height: 30px;
font-size: 11px;
}

")))
}

ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  tags$style(HTML(reds.completo)), #♣♠♦♥
  BACKGROUND(),
  HTML("<br>"),
  
  mainPanel(
    tags$head(tags$style(
      HTML(".selectize-input {height: 60px; width: 130%;}"))),
    tags$style(HTML(texto.final)),
    HTML("<br>"),
    div(style="display:inline-block;",selectInput("nplayers","Players",c(2:3),width = 60,selected = 2)),
    HTML("&ensp; &ensp;&ensp; &ensp;&ensp;&ensp;"),
    
    div(style="display:inline-block;",uiOutput("flop1")),
    div(style="display:inline-block;",uiOutput("flop2")),
    div(style="display:inline-block;",uiOutput("flop3")),
    HTML("&ensp; &ensp;&ensp;"),
    div(style="display:inline-block;",uiOutput("turn")),
    HTML("&ensp; &ensp;&ensp;"),
    div(style="display:inline-block;",uiOutput("river")),
    HTML("<br>"),
    
    div(style="display:inline-block;",selectInput("A_1","A",c("",DECK.SHINY),width = 60)),
    div(style="display:inline-block;",uiOutput("A_2")),
    HTML("&ensp; &ensp;&ensp; &ensp;&ensp;&ensp;"),
    
    div(style="display:inline-block;",uiOutput("B_1")),
    div(style="display:inline-block;",uiOutput("B_2")),
    div(style="display:inline-block",uiOutput("B_range")),
    HTML("&ensp; &ensp;&ensp; &ensp;&ensp;&ensp;"),
    
    div(style="display:inline-block;",uiOutput("C_1")),
    div(style="display:inline-block;",uiOutput("C_2")),
    div(style="display:inline-block",uiOutput("C_range")),

    div(style="display:inline-block",uiOutput("A_range")),
    HTML("<br><br>"),
    
    tags$head(tags$style("
    #range{text-align: center;font-size:14px;font-weight: bold;
    background-color: transparent; border-color:black; height:85px; width:509px;
    display: table-cell; vertical-align: middle }
    .btn {font-weight: bold;}")),
    div(verbatimTextOutput("range",placeholder = T)),
    HTML("<br>"),
    div(style="display:inline-block;",sliderInput("range"," Range:",min=5,max=100,value=100,step = 5,ticks = F,width = 345)),
    HTML("&ensp; &ensp;"),
    actionButton("enviar","CALCULATE",width = 140,style="margin-top:0px;height:120px;"),
    actionButton("limpar","CLEAN",width = 140,style="margin-top:0px;height:120px;"),
    tags$head(tags$style("
                         #enviar { position:absolute;left:384px;top:379px; }
                         #limpar { position:absolute;left:384px;top:519px; }
                         ")),
    HTML("<br>"),
    tags$head(tags$style(HTML('.selectize-input {white-space: nowrap}
    #choice+ div>.selectize-dropdown{width: 660px !important}
    #choices+ div>.selectize-dropdown{width: 300px !important}'))),
    tags$head(tags$style("#novo{color:black; background-color: transparent;border-color:black;
                         font-size:10px; text-align: center;}")),
    div(style="display:inline-block;margin-bottom:25px;width:345px;",verbatimTextOutput("novo")),

    fluid = F,width = 12,style='width:600px;left:25px;')
)

server <-
  function(input, output,session) {
    
    output$range <- renderPrint({ cat("- ODDS CALCULATOR -\nwe test 300 hands and results may vary a bit") })
    
    observeEvent(input$enviar, {
      if (input$A_2 != "") {
        if ((input$flop1 != "" & input$flop2 == "") | (input$flop2 != "" & input$flop3 == "")){
          output$range <- renderPrint({cat("Please select three cards for the flop (or none)")})
        }
        else if (input$B_1 != "" & input$B_2 == "") {
          output$range <- renderPrint({cat("Please select two cards (or none) for Player B")})
        }
        else if (input$C_1 != "" & input$C_2 == "") {
          output$range <- renderPrint({cat("Please select two cards (or none) for Player C")})
        }
        else {
          cartas <- NULL
          
          for (i in c("A","B","C")){
            cartas <- rbind(cartas,
                            c(
                              eval(
                                parse(
                                  text=paste0("input$",i,"_1"))),
                              eval(
                                parse(
                                  text=paste0("input$",i,"_2")))
                            )
            )
          }
          
          CCC <- cartas[1:input$nplayers,]
          
          output$range <- 
            renderPrint({
              isolate(equidade.shiny(input$nplayers,input$range,CCC,max(0,input$flop1),max(0,input$flop2),max(0,input$flop3),max(0,input$turn),max(0,input$river)))
              
            }) }
      }
      else {
        output$range <- renderPrint({cat("Please select two cards for Player A")})
        
      }
    } )
    
    observeEvent(input$limpar, {
      
      output$range <- renderPrint({cat("- ODDS CALCULATOR -\nwe test 300 hands and results may vary a bit")})
      
      updateSliderInput(session,"range"," Range:",min=5,max=100,value=100,step = 5)

      updateSelectInput(session,"A_1","A",c("",DECK.SHINY))
      
      output$A_2 <- renderUI({
        conditionalPanel(
          condition = "input.A_1 != 0",
          selectInput("A_2","",c("",DECK.SHINY[!DECK.SHINY %in% input$A_1]),width = 55,selected = ""))
      })
      
      output$B_1 <- renderUI({
        conditionalPanel(
          condition = "input.A_2 != 0",
          selectInput("B_1","B",c("",DECK.SHINY[!DECK.SHINY %in% c(input$A_2,input$A_1)]),width = 55,selected = ""))
      })
      
      output$B_2 <- renderUI({
        conditionalPanel(
          condition = "input.B_1 != 0",
          selectInput("B_2","",c("",DECK.SHINY[!DECK.SHINY %in% c(input$B_1,input$A_2,input$A_1)]),width = 55,selected = ""))
      })
      
      output$C_1 <- renderUI({
        conditionalPanel(
          condition = "input.B_2 != 0 & input.nplayers > 2",
          selectInput("C_1","C",c("",DECK.SHINY[!DECK.SHINY %in% c(input$B_2,input$B_1,input$A_2,input$A_1)]),width = 55,selected = ""))
      })
      
      output$C_2 <- renderUI({
        conditionalPanel(
          condition = "input.C_1 != 0 & input.nplayers > 2",
          selectInput("C_2","",c("",DECK.SHINY[!DECK.SHINY %in% c(input$C_1,input$B_2,input$B_1,input$A_2,input$A_1)]),width = 55,selected = ""))
      })
      
      output$flop1 <- renderUI({
        conditionalPanel(
          condition = paste0(lista.alfabetica[as.numeric(input$nplayers)]," != 0 | ( input.A_2 != 0 & input.range > 0 )"),
          selectInput("flop1","Flop",c("",DECK.SHINY[!DECK.SHINY %in% c(input$C_2,input$C_1,input$B_2,input$B_1,input$A_2,input$A_1)]),width = 55, selected = ""))
      })
      
      output$flop2 <- renderUI({
        conditionalPanel(
          condition = "input.flop1 != 0",
          selectInput("flop2","",c("",DECK.SHINY[!DECK.SHINY %in% c(input$flop1,input$C_2,input$C_1,input$B_2,input$B_1,input$A_2,input$A_1)]),width = 55, selected = ""))
      })
      
      output$flop3 <- renderUI({
        conditionalPanel(
          condition = "input.flop2 != 0",
          selectInput("flop3","",c("",DECK.SHINY[!DECK.SHINY %in% c(input$flop2,input$flop1,input$C_2,input$C_1,input$B_2,input$B_1,input$A_2,input$A_1)]),width = 55, selected = ""))
      })
      output$turn <- renderUI({
        conditionalPanel(
          condition = "input.flop3 != 0",
          selectInput("turn","Turn",c("",DECK.SHINY[!DECK.SHINY %in% c(input$flop3,input$flop2,input$flop1,input$C_2,input$C_1,input$B_2,input$B_1,input$A_2,input$A_1)]),width = 55, selected = ""))
      })
      output$river <- renderUI({
        conditionalPanel(
          condition = "input.turn != 0",
          selectInput("river","River",c("",DECK.SHINY[!DECK.SHINY %in% c(input$turn,input$flop3,input$flop2,input$flop1,input$C_2,input$C_1,input$B_2,input$B_1,input$A_2,input$A_1)]),width = 55, selected = ""))
      })
      
    })
    
    observeEvent(input$range,{
      output$novo <- renderPrint({ 
        isolate(range.shiny(input$range))
        cat("")})
    }) 
    
    output$A_2 <- renderUI({
      conditionalPanel(
        condition = "input.A_1 != 0",
        selectInput("A_2","",c("",DECK.SHINY[!DECK.SHINY %in% input$A_1]),width = 60,selected = ""))
    })
    
    output$B_1 <- renderUI({
      conditionalPanel(
        condition = "input.A_2 != 0",
        selectInput("B_1","B",c("",DECK.SHINY[!DECK.SHINY %in% c(input$A_2,input$A_1)]),width = 60,selected = ""))
    })
    
    output$B_2 <- renderUI({
      conditionalPanel(
        condition = "input.B_1 != 0",
        selectInput("B_2","",c("",DECK.SHINY[!DECK.SHINY %in% c(input$B_1,input$A_2,input$A_1)]),width = 60,selected = ""))
    })
    
    output$C_1 <- renderUI({
      conditionalPanel(
        condition = "input.B_2 != 0 & input.nplayers > 2",
        selectInput("C_1","C",c("",DECK.SHINY[!DECK.SHINY %in% c(input$B_2,input$B_1,input$A_2,input$A_1)]),width = 60,selected = ""))
    })
    
    output$C_2 <- renderUI({
      conditionalPanel(
        condition = "input.C_1 != 0 & input.nplayers > 2",
        selectInput("C_2","",c("",DECK.SHINY[!DECK.SHINY %in% c(input$C_1,input$B_2,input$B_1,input$A_2,input$A_1)]),width = 60,selected = ""))
    })
    
    output$flop1 <- renderUI({
      conditionalPanel(
        condition = paste0(lista.alfabetica[as.numeric(input$nplayers)]," != 0 | ( input.A_2 != 0 & input.range > 0 )"),
        selectInput("flop1","Flop",c("",DECK.SHINY[!DECK.SHINY %in% c(input$C_2,input$C_1,input$B_2,input$B_1,input$A_2,input$A_1)]),width = 60, selected = ""))
    })
    
    output$flop2 <- renderUI({
      conditionalPanel(
        condition = "input.flop1 != 0",
        selectInput("flop2","",c("",DECK.SHINY[!DECK.SHINY %in% c(input$flop1,input$C_2,input$C_1,input$B_2,input$B_1,input$A_2,input$A_1)]),width = 60, selected = ""))
    })
    
    output$flop3 <- renderUI({
      conditionalPanel(
        condition = "input.flop2 != 0",
        selectInput("flop3","",c("",DECK.SHINY[!DECK.SHINY %in% c(input$flop2,input$flop1,input$C_2,input$C_1,input$B_2,input$B_1,input$A_2,input$A_1)]),width = 60, selected = ""))
    })
    output$turn <- renderUI({
      conditionalPanel(
        condition = "input.flop3 != 0",
        selectInput("turn","Turn",c("",DECK.SHINY[!DECK.SHINY %in% c(input$flop3,input$flop2,input$flop1,input$C_2,input$C_1,input$B_2,input$B_1,input$A_2,input$A_1)]),width = 60, selected = ""))
    })
    output$river <- renderUI({
      conditionalPanel(
        condition = "input.turn != 0",
        selectInput("river","River",c("",DECK.SHINY[!DECK.SHINY %in% c(input$turn,input$flop3,input$flop2,input$flop1,input$C_2,input$C_1,input$B_2,input$B_1,input$A_2,input$A_1)]),width = 60, selected = ""))
    })
    
    # Define os intervalos do range a depender do numero de jogadores, para evitar travamento
    observeEvent(input$nplayers,{
      
      nplayers <- as.numeric(input$nplayers)
      
      if (nplayers == 1) {
        output$B_1 <- renderUI({
          conditionalPanel(
            condition = "input.A_2 != 0",
            selectInput("B_1","B",c("",DECK.SHINY[!DECK.SHINY %in% c(input$A_2,input$A_1)]),width = 60,selected = ""))
        })
      }
      
      if (nplayers == 1) {
        output$B_2 <- renderUI({
          conditionalPanel(
            condition = "input.B_1 != 0",
            selectInput("B_2","",c("",DECK.SHINY[!DECK.SHINY %in% c(input$B_1,input$A_2,input$A_1)]),width = 60,selected = ""))
        })
      }
      
      if (nplayers == 2) {
        output$C_1 <- renderUI({
          conditionalPanel(
            condition = "input.B_2 != 0 & input.nplayers > 2",
            selectInput("C_1","C",c("",DECK.SHINY[!DECK.SHINY %in% c(input$B_2,input$B_1,input$A_2,input$A_1)]),width = 60,selected = ""))
        })
      }
      
      if (nplayers == 2) {
        output$C_2 <- renderUI({
          conditionalPanel(
            condition = "input.C_1 != 0 & input.nplayers > 2",
            selectInput("C_2","",c("",DECK.SHINY[!DECK.SHINY %in% c(input$C_1,input$B_2,input$B_1,input$A_2,input$A_1)]),width = 60,selected = ""))
        })
      }
      
    })
    
  }

shinyApp(ui, server)