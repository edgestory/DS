library(plotly)
library(plotrix)




relu<-function(x){
  
  ifelse(x>0,x,0)
  
}






nn.ff2<-function (nn, batch_x) 
  
{
  
  m <- nrow(batch_x)
  
  if (nn$visible_dropout > 0) {
    
    nn$dropout_mask[[1]] <- dropout.mask(ncol(batch_x), nn$visible_dropout)
    
    batch_x <- t(t(batch_x) * nn$dropout_mask[[1]])
    
  }
  
  nn$post[[1]] <- batch_x
  
  
  
  for (i in 2:(length(nn$size) - 1)) {
    
    nn$pre[[i]] <- t(nn$W[[i - 1]] %*% t(nn$post[[(i - 1)]]) + 
                       
                       nn$B[[i - 1]])
    
    if (nn$activationfun == "sigm") {
      
      nn$post[[i]] <- sigm(nn$pre[[i]])
      
    }
    
    else if (nn$activationfun == "tanh") {
      
      nn$post[[i]] <- tanh(nn$pre[[i]])
      
    }
    
    else if (nn$activationfun == "relu") {
      
      nn$post[[i]] <- relu(nn$pre[[i]])
      
    }
    
    else if (nn$activationfun == "linear") {
      
      nn$post[[i]] <- (nn$pre[[i]])
      
    }
    
    else {
      
      stop("unsupport activation function!")
      
    }
    
    if (nn$hidden_dropout > 0) {
      
      nn$dropout_mask[[i]] <- dropout.mask(ncol(nn$post[[i]]), 
                                           
                                           nn$hidden_dropout)
      
      nn$post[[i]] <- t(t(nn$post[[i]]) * nn$dropout_mask[[i]])
      
    }
    
  }
  
  dim(nn$W[[i - 1]])
  
  dim(t(nn$post[[(i - 1)]]))
  
  
  
  i <- length(nn$size)
  
  nn$pre[[i]] <- t(nn$W[[i - 1]] %*% t(nn$post[[(i - 1)]]) + 
                     
                     nn$B[[i - 1]])
  
  nn$pre[[i]] %in% nn$pre[[i]][1]
  
  
  
  if (nn$output == "sigm") {
    
    nn$post[[i]] <- sigm(nn$pre[[i]])
    
    
    
    
    
  } else if (nn$output == "linear") {
    
    nn$post[[i]] <-  nn$pre[[i]] 
    
    
    
  } else if (nn$output == "softmax") {
    
    nn$post[[i]] <- exp(nn$pre[[i]])
    
    
    
    nn$post[[i]] <- nn$post[[i]]/rowSums(nn$post[[i]])
    
    nn$post[[i]][is.na(nn$post[[i]] )]<-1
    
  }
  
  
  
  nn
  
}



nn.bp<-function (nn) 
  
{
  
  n <- length(nn$size)
  
  d <- list()
  
  if (nn$output == "sigm") {
    
    d[[n]] <- -nn$e * (nn$post[[n]] * (1 - nn$post[[n]]))
    
  }
  
  else if (nn$output == "linear" || nn$output == "softmax") {
    
    d[[n]] <- -nn$e
    
  }
  
  for (i in (n - 1):2) {
    
    if (nn$activationfun == "sigm") {
      
      d_act <- nn$post[[i]] * (1 - nn$post[[i]])
      
    }
    
    else if (nn$activationfun == "tanh") {
      
      d_act <- 1.7159 * 2/3 * (1 - 1/(1.7159)^2 * nn$post[[i]]^2)
      
    }
    
    else if (nn$activationfun == "relu") {
      
      d_act <-  ifelse(nn$post[[i]]>=0,1,0)
      
    }
    
    d[[i]] <- (d[[i + 1]] %*% nn$W[[i]]) * d_act
    
    if (nn$hidden_dropout > 0) {
      
      d[[i]] <- t(t(d[[i]]) * nn$dropout_mask[[i]])
      
    }
    
  }
  
  for (i in 1:(n - 1)) {
    
    dw <- t(d[[i + 1]]) %*% nn$post[[i]]/nrow(d[[i + 1]])
    
    dw <- dw * nn$learningrate
    
    if (nn$momentum > 0) {
      
      nn$vW[[i]] <- nn$momentum * nn$vW[[i]] + dw
      
      dw <- nn$vW[[i]]
      
    }
    
    nn$W[[i]] <- nn$W[[i]] - dw
    
    db <- colMeans(d[[i + 1]])
    
    db <- db * nn$learningrate
    
    if (nn$momentum > 0) {
      
      nn$vB[[i]] <- nn$momentum * nn$vB[[i]] + db
      
      db <- nn$vB[[i]]
      
    }
    
    nn$B[[i]] <- nn$B[[i]] - db
    
  }
  
  nn
  
}



sigm<-function (x) 
  
{
  
  1/(1 + exp(-x))
  
}





relu<-function(x){
  
  ifelse(x>0,x,0)
  
}






stm<-matrix(1:10000,ncol=100,nrow=200,byrow=T)



cal_coord<-function(x){
  wv<-rep(0,ncol(stm)/10)
  yv<-rep(0,nrow(stm)/10)
  
  wv2<-wv
  yv2<-yv
  x<-x/10
  
  if(x[1] %% 1 == 0){
    
    wv2[x[1]] <-1
  }else{
    wv2[ceiling(x[1])]<-abs(trunc(x[1])-x[1])
    wv2[trunc(x[1])]<- ceiling(x[1])-x[1]
    
  }
  if(x[2] %% 1 == 0){
    
    yv2[x[2]] <-1
  }else{
    yv2[ceiling(x[2])]<-abs(trunc(x[2])-x[2])
    yv2[trunc(x[2])]<- ceiling(x[2])-x[2]
    
  }
  
  
  c(wv2,yv2)
  
}



return_reward<-function(st){
  

  re<-0
  done<-F

  
  c(re,done)
  
}


move<-function(x,action_index){
  bfx<-x
  if(action_index==1){
    x[1]<-x[1]-0.5
  }
  if(action_index==2){
    x[1]<-x[1]+0.5
  }
  if(action_index==3){
    x[2]<- x[2]-0.5
  }
  if(action_index==4){
    x[2]<- x[2]+0.5
  }
  if(x[2] > (nrow(stm)-1) |x[1] < 1 | x[2] < 1 | x[1] >(ncol(stm)-1)){
    
    x<-bfx
    
  }
  x
}

action<-c(1:4)
library(plotly)

  par(mfrow=c(2,3)) 
  
  start_point<-c(50,50)
  
  
  maxv<-0.323*2
  thr<-0.25
  mis_info<-list()
  init_data<-cal_coord(start_point)
  init_feat<-cal_coord(start_point)
  
  {
    
    
    
    
    
    
    input_dim<-length(init_data)
    
    hidden<-c(100)
    
    output_dim<-length(action)
    
    size <- c(input_dim, hidden, output_dim)
    
    activationfun<-"tanh"
    
    # activationfun<-"relu"
    
    # output<-"linear"
    
    output<-"softmax"
    
    
    
    batchsize<-30
    
    momentum<-0
    
    learningrate_scale<-0.9999
    
    hidden_dropout = 0
    
    visible_dropout = 0
    
    numepochs = 10
    
    learningrate<-0.005
    
    
    
    
    
    vW <- list()
    
    vB <- list()
    
    W <- list()
    
    B <- list()
    
    
    
    
    
    
    
    
    
    for (i in 2:length(size)) {
      
      W[[i - 1]] <- matrix(rnorm(size[i] * size[i - 1],0,2/input_dim),
                           
                           c(size[i], size[i - 1]))
      
      B[[i - 1]] <- runif(size[i], min = -0.1, max = 0.1)
      
      vW[[i - 1]] <- matrix(rep(0, size[i] * size[i - 1]), 
                            
                            c(size[i], size[i - 1]))
      
      vB[[i - 1]] <- rep(0, size[i])
      
    }
    
    qn1<- list(input_dim = input_dim, output_dim = output_dim, 
               
               hidden = hidden, size = size, activationfun = activationfun, 
               
               learningrate = learningrate, momentum = momentum, learningrate_scale = learningrate_scale, 
               
               hidden_dropout = hidden_dropout, visible_dropout = visible_dropout, 
               
               output = output, W = W, vW = vW, B = B, vB = vB)
    
    
    
    
    
    
    
  }
  
  
  {
    
    
    
    
    
    # init_data<-convert_coord(start_point,start_point,v,1,90,1,90,0,0,0,thr,0)
    
    input_dim<-length(init_data)
    
    hidden<-c(100)
    
    output_dim<-1
    
    size <- c(input_dim, hidden, output_dim)
    
    activationfun<-"tanh"
    
    # activationfun<-"relu"
    
    output<-"linear"
    
    
    
    batchsize<-30
    
    momentum<-0
    
    learningrate_scale<-0.9999
    
    hidden_dropout = 0
    
    visible_dropout = 0
    
    numepochs = 10
    
    learningrate<-0.005
    
    
    
    
    
    vW <- list()
    
    vB <- list()
    
    W <- list()
    
    B <- list()
    
    
    
    
    
    
    
    
    
    for (i in 2:length(size)) {
      
      W[[i - 1]] <- matrix(rnorm(size[i] * size[i - 1],0,2/input_dim),
                           
                           c(size[i], size[i - 1]))
      
      B[[i - 1]] <- runif(size[i], min = -0.1, max = 0.1)
      
      vW[[i - 1]] <- matrix(rep(0, size[i] * size[i - 1]), 
                            
                            c(size[i], size[i - 1]))
      
      vB[[i - 1]] <- rep(0, size[i])
      
    }
    
    vg<- list(input_dim = input_dim, output_dim = output_dim, 
              
              hidden = hidden, size = size, activationfun = activationfun, 
              
              learningrate = learningrate, momentum = momentum, learningrate_scale = learningrate_scale, 
              
              hidden_dropout = hidden_dropout, visible_dropout = visible_dropout, 
              
              output = output, W = W, vW = vW, B = B, vB = vB)
    
    
    
    
    
    
    
    
    
  }
  
  
  #pn
  
  {
    
    
    
    
    
    
    input_dim<-length(init_data)
    
    hidden<-c(100)
    
    output_dim<-20
    
    size2 <- c(input_dim, hidden, output_dim)
    
    activationfun<-"tanh"
    
    activationfun<-"relu"
    
    output<-"linear"
    
    # output<-"softmax"
    
    
    
    batchsize<-30
    
    momentum<-0
    
    learningrate_scale<-0.9999
    
    hidden_dropout = 0
    
    visible_dropout = 0
    
    numepochs = 10
    
    learningrate<-0.005
    
    
    
    
    
    vW <- list()
    
    vB <- list()
    
    W <- list()
    
    B <- list()
    
    
    
    
    
    
    
    
    
    for (i in 2:length(size2)) {
      
      W[[i - 1]] <- matrix(rnorm(size2[i] * size2[i - 1],0,2/input_dim),
                           
                           c(size2[i], size2[i - 1]))
      
      B[[i - 1]] <- runif(size2[i], min = -0.1, max = 0.1)
      
      vW[[i - 1]] <- matrix(rep(0, size2[i] * size2[i - 1]), 
                            
                            c(size2[i], size2[i - 1]))
      
      vB[[i - 1]] <- rep(0, size2[i])
      
    }
    
    pn<- list(input_dim = input_dim, output_dim = output_dim, 
              
              hidden = hidden, size = size2, activationfun = activationfun, 
              
              learningrate = learningrate, momentum = momentum, learningrate_scale = learningrate_scale, 
              
              hidden_dropout = hidden_dropout, visible_dropout = visible_dropout, 
              
              output = output, W = W, vW = vW, B = B, vB = vB)
    
    
    
    
    
    
    
    
  }
  
  # tn
  {
    
    
    
    
    
    
    input_dim<-length(init_data)
    
    hidden<-c(100)
    
    output_dim<-20
    
    size2 <- c(input_dim, hidden, output_dim)
    
    activationfun<-"tanh"
    
    activationfun<-"relu"
    
    output<-"linear"
    
    # output<-"softmax"
    
    
    
    batchsize<-30
    
    momentum<-0
    
    learningrate_scale<-0.9999
    
    hidden_dropout = 0
    
    visible_dropout = 0
    
    numepochs = 10
    
    learningrate<-0.005
    
    
    
    
    
    vW <- list()
    
    vB <- list()
    
    W <- list()
    
    B <- list()
    
    
    
    
    
    
    
    
    for (i in 2:length(size2)) {
      
      W[[i - 1]] <- matrix(rnorm(size2[i] * size2[i - 1],0,2/input_dim),
                           
                           c(size2[i], size2[i - 1]))
      
      B[[i - 1]] <- runif(size2[i], min = -0.1, max = 0.1)
      
      vW[[i - 1]] <- matrix(rep(0, size2[i] * size2[i - 1]), 
                            
                            c(size2[i], size2[i - 1]))
      
      vB[[i - 1]] <- rep(0, size2[i])
      
    }
    tn<- list(input_dim = input_dim, output_dim = output_dim, 
              
              hidden = hidden, size = size2, activationfun = activationfun, 
              
              learningrate = learningrate, momentum = momentum, learningrate_scale = learningrate_scale, 
              
              hidden_dropout = hidden_dropout, visible_dropout = visible_dropout, 
              
              output = output, W = W, vW = vW, B = B, vB = vB)
    
    
    
    
    
    
    
    
    
    
  }

  
  
  thr<-0.25
  
  dis_f<-0.9
  
  reward_list<-c()
  in_reward_list<-c()
  final_total_reward_list<-c()
  
  final_action_list<-list()
  
  step_list<-c()
  
  q_table<-list()
  
  
  rad_list<-list()
  
  st_list<-list()
  
  tot_att<-0
  
  bi<-1
  
  r<-1
  
  attack_cnt<-c()
  
  st_mis_list<-list()
  
  avoid_action_list<-list()
  
  plot_att_rat<-c()
  
  die_action_list<-list()
  
  reward_total_list<-list()
  
  
  in_reward_list2<-list()
  v_list<-list()
  
  p_list<-list()
  
  h_list<-list()
  
  th_list<-list()
  
  bbc<-NULL
  
  ilikettt<-NULL
  
  th<-0.4
  
  epoch<-100
  
  dis_f<-0.999
  
  th2<-0.2
  
  th<-0.2
  
  TF_at<-F
  attack_cnt<-c()
  tot_att<-0
  i
  vloss<-c()
  
  
  pred_memory<-list()
  start_point_list<-list()
  goal_point_list<-list()
  replay_buffer<-list()

  M<-20
  bi2<-1
  bi3<-1
  bi4<-1
  i<-1
  
  inrth<-0.01
  for(i in 1:40000){
    

    start_point<-c(50,100)
    goal_point<-c(20,180)
    init_data<-cal_coord(start_point)
    init_feat<-cal_coord(start_point)
    st<-start_point
  
 
    total_inr<-0
    total_r<-0
    final_total_r<-0
    
    
    qn1<-nn.ff2(qn1,t(init_data))
  

    imsi_memory<-list()
    
    
    ap<-NULL

    tr_c<-0
    mb<-0
    bi<-1
    inr_memory<-c()
    inr_memory2<-c()

    episode_done<-F
    step<-1
    action_list<-c()
    max_step<-200
    reward_memory<-c()
    memory<-list()
    epoch_index<-1
    st_2<-NULL
    while(episode_done==0){
      
      
      if(step >1){
        qn1<-nn.ff2(qn1,t(cov_next_state))
        action_index<-which.max(qn1$post[[length(size)]])
        current_state<-cov_next_state
        action_prob<-qn1$post[[length(size)]]
    
      
      }else{
        
        cov_next_state<- init_data
        current_state<-cov_next_state
        action_index<-which.max(qn1$post[[length(size)]])
        action_prob<-qn1$post[[length(size)]]
        
      }
    
      avoid<-F
      action_index<-sample(1:4,1,prob=action_prob)
          next_action<-  action[action_index]
      
      
      
      
      action_list<-c(action_list,action_index)
      (st<-move(st,action_index))
      st_2<-rbind(st_2,st)
      
      
      
      re_ep<-return_reward(st) 
      
      
      
      
      cov_next_state<-cal_coord(st)
      cov_feat<-cal_coord(st)
      
      pn<-nn.ff2(pn,t(cov_feat))
      pf<-pn$post[[length(size2)]] ## feature
      
      tn<-nn.ff2(tn,t(cov_feat))
      tf<-tn$post[[length(size2)]] ## feature
      
      inr<- sum(abs(tf-pf));inr;action_index
   
      
      inr_memory<-c(inr_memory,inr)

      
      inr_memory2<-c(inr_memory2,inr)
      tn$e<-as.matrix(pf)-as.matrix(tf)
      tn<-nn.bp(tn)
      
      
      
      if(step ==max_step){
        episode_done<-T
        re_ep[1]<-re_ep[1]-5
      }
      reward_memory<-c(reward_memory,re_ep[1])
      total_r<-total_r+re_ep[1]
      total_inr<-total_inr+inr
      final_total_r<-final_total_r+re_ep[1]+inr
      bf_reward<-re_ep[1]
     
      episode_done<-re_ep[2]
      
      
      
      memory[[epoch_index]]<-  list(input=current_state,action=next_action,reward=re_ep[1]+inr,done=re_ep[2],next_state=cov_next_state,step=step)
      imsi_memory[[bi]]<-  list(input=current_state,action=next_action,reward=re_ep[1]+inr,done=re_ep[2],next_state=cov_next_state,step=step)
      bi<-bi+1
      

        if(bi3>1000000){
          bi3<-1
        }
        
      if(episode_done | step %% epoch == 0) {
       
   
        rz<-1
        buffer_v<-c()
        if(episode_done){
          vs<-0
        }else{
          vs<-  nn.ff2(vg,t(cov_next_state))$post[[length(size)]]
        }
        
        
        for(rz in (length(memory)):1){
          
          vs<-memory[[rz ]]$reward +dis_f*vs
          buffer_v<-c(buffer_v,vs)
          
        }
        x_stack<-t(sapply(memory,function(x){x$input}))
        done_stack<-t(sapply(memory,function(x){x$done}))
        action_stack<-t(sapply(memory,function(x){x$action}))

        
        vg<-nn.ff2(vg,(x_stack))
        buffer_v<-buffer_v[length(buffer_v):1]
        td_error<-buffer_v-vg$post[[length(size)]]
        
        
        vloss<-c(vloss,mean((td_error)^2))
        
        vg$e<-td_error
        vg<-nn.bp(vg)
        
        
        
        
        qn1<- nn.ff2(qn1,(x_stack))
        
        yy<-qn1$post[[length(size)]]
        yy2<-yy
        for(ml in 1:nrow(yy2)){
          yy[ml,action %in%action_stack[ml]]<-(yy[ml,action %in%action_stack[ml]]+0.0001)-1
          yy2[ml,action %in%action_stack[ml]]<-log(yy2[ml,action %in%action_stack[ml]]+0.00001)+1
          
        }
        
        exp_v<- -yy * as.numeric(td_error)
        
        entropy<-yy2
   
        qn1$e<- (-0.001 * entropy + exp_v)
       
        qn1<-nn.bp(qn1)
        memory<-list()
        epoch_index<-0
      }
      
      
      epoch_index<-epoch_index+1
      
      step<-step+1
      if(step ==max_step+1){
        cat("\n",i,"번째 episode-",step)
        step_list[i]<-step
        final_action_list[[i]]<-action_list
        reward_list[i]<-total_r
        in_reward_list[i]<-total_inr
        final_total_reward_list[i]<-final_total_r
        reward_total_list[[i]]<-reward_memory
        
        break;
      }
      
      
      
      if(episode_done==1){
     
        
        cat("\n",i,"번째 episode-",step)
        
        print(st)
        step_list[i]<-step
        final_action_list[[i]]<-action_list
        reward_list[i]<-total_r
        in_reward_list[i]<-total_inr
        final_total_reward_list[i]<-final_total_r
        reward_total_list[[i]]<-reward_memory
        
        break;
      }
      
      
      
      
    }    
   
    

    
    getwd()
    if(i %% 5000 ==0){

      save(qn1,file="rndQN4.RData")
      save(vg,file="rndvg.RData")
      save(st_list,file="rnd_st_list_4.RData")
      save(step_list,file="rnd_step_list_2.RData")
      save(reward_list,file="rnd_reward_list2.RData")
      save(in_reward_list,file="rnd_in_reward_list.RData")
      save(reward_total_list,file="final_total_reward_list.RData")
      save(tn,file=paste0("tn",i,".RData"))
      save(pn,file=paste0("pn",i,".RData"))

   
      zzz<-do.call("rbind",st_list)
      zzz2<-zzz[!duplicated(zzz),]
      
      
      xr<-seq(0,100,by=0.5)
      yr<-seq(0,200,by=0.5)
      
      
      qulst<-list()
      nm<-1
      for(t in xr){
        
        for(q in yr){
          qulst[[nm]]<-  c(t,q)
          nm<-nm+1
          
        }
      }
      
      qut<-do.call("rbind",qulst)
      zzz2<- rbind(zzz2,qut)
      head(qut)
      head(qulst)
      dim(zzz2)
      
      total_inp<-t(apply(zzz2,1,cal_coord))
      tn<-nn.ff2(tn,total_inp)
      a1<-tn$post[[length(size2)]]
      pn<-nn.ff2(pn,total_inp)
      a2<-pn$post[[length(size2)]]
      loss<-apply(a1-a2,1,function(x){sum(abs(x))})
      
      
      loss
      
      aq<-c(0.1,0.2,0.3)
      scr<-  function(x){(x-min(x))/(max(x)-min(x))}
      
      # loss<-scr(loss)
      
      zzz3<-(cbind(zzz2,loss))
      
      # max(loss)
      library(plotly)
      # install.packages("orca")
      # library(orca)
      
      p = plot_ly(x = zzz3[,1],
                  y = zzz3[,2],
                  z = zzz3[,3],
                  type = "contour", 
                  contours = list(
                    start = 0,
                    end = 1,
                    size = 0.1
                  ))
      
      print(p)
      
      
      htmlwidgets::saveWidget(p, paste0("plot",i,".html"))

    }
    
    

    
    in_reward_list2[[bi2]]<-(inr_memory)
    
    
    if(i>1){

      inrth<- quantile(unlist(in_reward_list2),0.05)
      
      dl<-do.call("rbind",in_reward_list2)
      head(dl)
      thlist<-apply(dl,2,quantile,0.05)
      
      # inrth<-quantile(quantile(in_reward_list2),0.05)
      
    }
    
    bi2<-bi2+1
    if(bi2 >1000){
      bi2<-1
    }
    # t(sapply(imsi_memory,function(x){x$reward}))
    
    rz<-1
    buffer_v<-c()
    if(episode_done){
      vs<-0
    }else{
      vs<-  nn.ff2(vg,t(cov_next_state))$post[[length(size)]]
    }
    
    
    for(rz in (length(imsi_memory)):1){
      
      vs<-imsi_memory[[rz ]]$reward +dis_f*vs
      buffer_v<-c(buffer_v,vs)
      
    }
    # x_stack<-t(sapply(imsi_memory,function(x){x$input}))
    done_stack<-t(sapply(imsi_memory,function(x){x$done}))
    action_stack<-t(sapply(imsi_memory,function(x){x$action}))
    # action_stack<-t(sapply(memory,function(x){x$action}))
    # memory[[1]]
    
    x_stack<-lapply(imsi_memory,function(x){x$input})
    x_stack2<-do.call("rbind",x_stack)
    
    vg<-nn.ff2(vg,(x_stack2))
    buffer_v<-buffer_v[length(buffer_v):1]
    td_error<-buffer_v-vg$post[[length(size)]]
    
    # dim(  replay_buffer$input)
    
    
    # length(  replay_buffer$input)
    replay_buffer$input<-append(replay_buffer$input,x_stack)
    replay_buffer$action<-c(replay_buffer$action,action_stack)
    replay_buffer$reward<-c(replay_buffer$reward,buffer_v)
    replay_buffer$td_error<-c(replay_buffer$td_error,td_error)
    qn1$learningrate<-0.005
    vg$learningrate<-0.005
    
    # qn1$learningrate<-qn1$learningrate*0.9999
    # vg$learningrate<-vg$learningrate*0.9999
    
    
    
    if(i %% M ==0){
      
      
      batch_sample<-30
      batch_num<-110
      
      prio_prob<-abs(replay_buffer$td_error)
      prio_prob<-prio_prob/sum(prio_prob)
      replay_buffer$input[[1]]
      
      length(abs(replay_buffer$td_error))
      length(replay_buffer$input)
      length(replay_buffer$reward)
      
      
      # prob2<-abs(unlist(sapply(imsi_memory,function(x){x$prob})))
      # prob2<-prob2/sum(probs)
      # for(t in 1:batch_num){
      #   
      #   sam<-sample(1:length(replay_buffer$reward),batch_sample,prob = prio_prob)
      #   # x_stack<-t(sapply(replay_buffer,function(x){x$input}))
      #   
      #   x_stack<-do.call("rbind",replay_buffer$input[sam])
      #   
      #   # x_stack<-x_stack[sam,]
      #   
      #   reward_stack<-replay_buffer$reward[sam]
      #   
      #   vg<-nn.ff2(vg,(x_stack))
      #   sil_td<-reward_stack-vg$post[[length(size)]]
      #   sil_index<-  sil_td> 0
      #   # sil_td[!sil_index]<-0
      # 
      #   vg$e<-as.matrix(sil_td)
      #   vg<-nn.bp(vg)
      #   
      #   
      #   qn1<- nn.ff2(qn1,(x_stack))
      #   action_stack<-replay_buffer$action[sam]
      #   yy<-qn1$post[[length(size)]]
      #   yy2<-yy
      #   for(ml in 1:nrow(yy2)){
      #     yy[ml,action %in% action_stack[ml]]<-(yy[ml,action %in%action_stack[ml]]+0.0001)-1
      #     yy2[ml,action %in%action_stack[ml]]<-log(yy2[ml,action %in%action_stack[ml]]+0.00001)+1      
      #   }
      #   
      #   
      #   entropy<-yy2
      #   exp_v<- -yy * as.numeric(sil_td)
      #   qn1$e<- (-0.001 * entropy + exp_v)
      #   qn1<-nn.bp(qn1)
      #   
      #   
      #   for(b in 1:5){
      #     sam<-sample(1:length(pred_memory),batch_sample*3.5)
      #     
      #     pred_x_stack<-t(sapply(pred_memory[sam],function(x){x$input}))
      #     pred_y_stack<-t(sapply(pred_memory[sam],function(x){x$output}))
      #     tn<-nn.ff2(tn,(pred_x_stack))
      #     
      #     
      #     tn$e<-pred_y_stack-tn$post[[length(size2)]]
      #     tn<-nn.bp(tn)
      #     # 
      #   }
      #   
      # }
      # 
      # 
      # if(length(replay_buffer$input)  >100000){
      #   ere<-length(replay_buffer$input)-100000     
      #   
      #   replay_buffer$input<-replay_buffer$input[-c(1: ere)]
      #   replay_buffer$action<-replay_buffer$action[-c(1: ere)]
      #   replay_buffer$reward<- replay_buffer$reward[-c(1: ere)]
      #   replay_buffer$td_error<- replay_buffer$td_error[-c(1: ere)]
      #   
      # }
      # 
      
      
      
      
      
    }
    
    length(    replay_buffer$input)  
    
    
    # ts.plot(bac)
    # rad_list[[i]]<-rad_2
    st_list[[i]]<-st_2
    # dev.off()
    # plot(st_2[,-3],pch=24,main=paste0(round(reward_list)[length(reward_list)],"-",step,"-",min(step_list)),type="l",xlim=c(0,100),ylim=c(0,100),cex=0.3)
    # points(rbind(init_point,goal_point),pch=16,cex=0.8,col="blue",bg="blue")
    # points(st_mis,pch=17,cex=0.5,col="red",bg="blue")
    # plot(st_2[,-1],pch=24,main=paste0(round(reward_list)[length(reward_list)],"-",step,"-",min(step_list)),type="l",xlim=c(0,100),ylim=c(0,100),cex=0.3)
    # points(rbind(init_point,goal_point)[,-1],pch=16,cex=0.8,col="blue",bg="blue")
    # points(st_mis[,-1],pch=17,cex=0.5,col="red",bg="blue")
    
    if(i %% 200==0){
      library(plotrix)
      
      # dev.off()
      rownames(st_2)<-1:nrow(st_2)
      # par(mfrow=c(1,2))
      # st_2<-st_list[[1021]]
      # grid(200,400,lwd=3)
      plot(st_2[,-3],pch=24,main=paste0(round(reward_list)[length(reward_list)],"-",step,"-",min(step_list)),type="l",xlim=c(0,100),ylim=c(0,nrow(stm)),cex=0.3)
      points(rbind(start_point,goal_point),pch=16,cex=0.8,col="blue",bg="blue")
      
      
      # plot(st_2[,-3],pch=24,main=paste0(round(reward_list)[length(reward_list)],"-",step,"-",min(step_list)),type="l",xlim=c(0,40),ylim=c(0,50),cex=0.3)
      # plot(st_2[,-1],pch=24,main=paste0(round(reward_list)[length(reward_list)],"-",step,"-",min(step_list)),type="l",xlim=c(0,50),ylim=c(0,40),cex=0.3)
      # ts.plot(ilikettt,col=c("#ff1200","#ff4800","#ff7800","#00ff18","#00ff66","#00ff90","#0036ff","#2400ff","#6000ff"))
      ts.plot(reward_list,main=paste0((reward_list)[length(reward_list)],"-",step,"-",min(step_list)))
      # qn1<-nn.ff2(qn1,t(init_data))
      # barplot(qn1$post[[length(size)]][nrow(qn1$post[[length(size)]]),])
      # bbc<-sapply(st_list,function(x){x[nrow(x),1]})
      # sum(bbc> 20)/length(bbc)
      # ts.plot(bbc)
      # ts.plot(step_list)
      # ts.plot(vloss)
      ts.plot(in_reward_list)
      # ts.plot(reward_memory)
      
      ts.plot(inr_memory2)
      ts.plot(apply(ap,1,max))
   
    }
    
    if(i %% 1000==0){
      slg<-500
      sz<-length(reward_list)/slg
      
      for(p in 1:sz){
        
        
        zzz<-do.call("rbind",st_list[((p-1)*slg+1):(p*slg)])
        dim(zzz)
        
        zzz2<-zzz[!duplicated(zzz),]
        dim(zzz2)
        plot(zzz2,pch=24,type="l",xlim=c(0,100),ylim=c(0,nrow(stm)),cex=0.3)
        points(rbind(start_point,goal_point),pch=16,cex=0.8,col="blue",bg="blue")
      }
      
    }
    
    # rek<-function(state,v,p,h,thr,bank_angle,action_index,time){
    #   state<-start_point
    #   bank_angle<-0
    #   v<-0.289
    #   maxv<-0.353
    #   p<-0
    #   thr<-0.25
    #   max_thr<-0.4
    #   min_thr<-0.05
    #   h<-90
    #   load_factor<-1
    #   stz<-move(state,thr,action_index,v,p,h,bank_angle,load_factor)
    #   load_factor<-stz[9]
    #   bank_angle<-stz[8]
    #   v<-stz[5]
    #   p<-stz[6]
    #   h<-stz[7]
    #   thr<-stz[4]
    #   stz<-stz[1:3]
    #   return_reward(stz,action_index,F,time,v,p,h,thr,bank_angle,avoid,load_factor)
    # }
    # # v
    # time<-1
    # act_bar<-list()
    # for(k in 1:length(action)){
    # za<-rek(start_point,v,p,h,thr,bank_angle,k,time)[1]
    # act_bar[[k]]<-za
    # }
    # 
    # barplot( unlist(act_bar))
    # 
    # which.max(c(za,za2,za3,za4,za5,za6,za7,za8,za9))
    # # 
    
    
    
    
    
  }   
  
  zzz<-do.call("rbind",st_list)
  zzz2<-zzz[!duplicated(zzz),]
  dim(zzz2)
  
  png("total.png")
  plot(zzz2,pch=24,type="l",xlim=c(0,100),ylim=c(0,nrow(stm)),cex=0.3)
  points(rbind(start_point,goal_point),pch=16,cex=0.8,col="blue",bg="blue")
  dev.off()
  graphics.off()
  
  


