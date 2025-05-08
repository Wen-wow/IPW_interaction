
library(tidyr)
library(dplyr)
library(lmtest)
library(sandwich)
library(haven)
library(survey)
library(jtools)
library(remotes)
library(svrepmisc)
library(rsimsum)
library(knitr)
library(ggplot2)
library(xlsx)

#######-----------The first DAG--------#######

#Standard expit 
expit<-function(x) exp(x)/(1+exp(x))

#Function to calculate the delta0 based on P(S=1)=0.5=sel.prob.
compute.d0<-function(sel.prob=0.5, delta1=0.3, delta2=0.3, delta3=0.1, prob.X=0.3, n.d0=1e5, s.model="logit",sd.error=1,alpha.XC=1,beta.XC=1){
  X<-rbinom(n.d0, 1, prob.X)
  C<-alpha.XC + beta.XC*X + rnorm(n.d0, 0, sd.error)  #C~X
  if (s.model=="logit"){
    find.d0<-function(d0) mean(expit(d0 + delta1*X + delta2 *C + delta3*X*C))-sel.prob
    d0<-uniroot(find.d0, interval = c(-10,10))$root
  }else if (s.model=="logadd"){
    d0 <- log(sel.prob) - log( mean( exp(delta1 * X + delta2 * C + delta3 * X * C) ) )
  }else if (s.model=="probit"){
    S.pr <- delta1 * X + delta2 * C + delta3 * X * C + rnorm(n.d0, 0, 1.6)
    d0 <- - sort(S.pr)[floor((1 - sel.prob) * n.d0)]
  }
  d0
}

# Function to calculate alpha.bin based on p(Y=1)=0.5
compute.alpha<-function(n.alpha=1e5, prob.X=0.3, alpha.XC=1, beta.XC=1, sd.error=1, beta=1,gamma=1, prob.Y=0.5){
  X<-rbinom(n.alpha, 1, prob.X)
  C<-alpha.XC + beta.XC*X + rnorm(n.alpha, 0, sd.error)  #C~X
  find.alpha<-function(alpha) mean(expit(alpha + beta*X + gamma*C)) - prob.Y
  alpha<-uniroot(find.alpha, interval = c(-10,10))$root
  alpha
}


#Function to simulate the data
#one simulation
sim<-function(n.sim=1,prob.X=0.3, mu.C=0, alpha.cts=1, beta=1, gamma=1,sd.error=1, sel.prob=0.5, delta1=0.3, delta2=0.3, delta3=0.1, n=5,seed=1356,k=1, s.model="logit",y.type="continuous",alpha.XC=1, beta.XC=1,prob.Y=0.5){
  data.out<-data.frame(delta3=rep(delta3,n),n.sim=rep(n.sim,n),seed=rep(NA,n),X=rep(NA,n), C=rep(NA,n), Y=rep(NA,n),Strue.prob=rep(NA,n),Strue=rep(NA,n),Yobs=rep(NA,n))
  seed<-seed+1000*n.sim*k
  set.seed(seed)
  X<-rbinom(n, 1, prob.X)
  C<-alpha.XC + beta.XC*X + rnorm(n, 0, sd.error)  #C~X
  
  #simulate Y when Y is continuous or binary
  if (y.type=="continuous"){
    Y<-alpha.cts + beta*X + gamma*C + rnorm(n, 0, sd.error)
  }else if (y.type=="binary"){
    #alpha.bin<--1.5
    alpha.bin<-compute.alpha(n.alpha=1e5, prob.X=prob.X, alpha.XC=alpha.XC, beta.XC=beta.XC, sd.error=sd.error, beta=beta,gamma=gamma, prob.Y=prob.Y)
    Y.pr<-expit(alpha.bin + beta*X + gamma*C)
    Y<-rbinom(n, 1, prob = Y.pr)
  }
  
  delta0<-compute.d0(sel.prob=sel.prob, delta1=delta1, delta2=delta2, delta3=delta3, prob.X=prob.X, n.d0=1e5, s.model=s.model,sd.error=sd.error,alpha.XC=alpha.XC,beta.XC=beta.XC)
  
  #simulate selection based on whether selection model is logit/logadd/probit
  if (s.model=="logit"){
    Strue.prob<-expit(delta0 + delta1*X + delta2*C + delta3*X*C)
    Strue<-rbinom(n, 1, Strue.prob)
  }else if (s.model=="logadd"){
    Strue.prob<-exp(delta0 + delta1*X + delta2*C + delta3*X*C)
    Strue.prob[Strue.prob>1]<- 1
    Strue<-rbinom(n, 1, Strue.prob)
  }else if (s.model=="probit"){
    Strue.prob <- delta0 + delta1*X + delta2*C + delta3*X*C +rnorm(n,0,1.6)
    Strue<-as.numeric(Strue.prob>0)
  }
  
  Yobs<-Y
  Yobs[Strue==0]<-NA
  data.out[,c(-1,-2)]<-c(rep(seed,n),X, C, Y, Strue.prob, Strue, Yobs)
  data.out
}


#######--------analyse the data---------########
#Function for each model based on the type of Y.
model<-function(model="True model", data=data.for.analysis, y.type="continuous"){
  data.out<-data.frame(alpha.est=NA, alpha.se=NA, alpha.ci=NA, beta.est=NA, beta.se=NA, beta.ci=NA)
  if (y.type=="continuous"){
    if(model=="True model"){
      fit.Y<-lm(Y~X, data = data)
    }else if (model=="CCA"){
      fit.Y<-lm(Yobs~X, data = data)
    }else{
      if(model=="IPW logit without interaction"){
        ipw.fit <-glm(Strue ~ X+C,data = data,family = binomial(link = "logit"))
      }else if(model=="IPW logit with interaction"){
        ipw.fit <-glm(Strue ~ X+C+X:C,data = data,family = binomial(link = "logit"))
      }else if(model=="IPW logadd without interaction"){
        ipw.fit <-glm(Strue ~ X+C,data = data,family = poisson(link = "log"))
      }else if(model=="IPW logadd with interaction"){
        ipw.fit <-glm(Strue ~ X+C+X:C,data = data,family = poisson(link = "log"))
      }
      ipw.probs <- ipw.fit$fitted.values
      ipw.weights <-data$Strue/ipw.probs #just to have weights for Strue=1, it does not matter if ipw.weights=0 when Strue=0.
      design1<-svydesign(id = ~1,weights = ~ipw.weights,data = data)
      fit.Y<-svyglm(Yobs~X,data=data,design = design1)  
    }
  }else if (y.type=="binary"){
    if(model=="True model"){
      fit.Y<-glm(Y~X, data = data, family = binomial(link = "logit"))
    }else if (model=="CCA"){
      fit.Y<- glm(Yobs~X, data = data, family = binomial(link = "logit"))
    }else{
      if(model=="IPW logit without interaction"){
        ipw.fit <-glm(Strue ~ X+C,data = data,family = binomial(link = "logit"))
      }else if(model=="IPW logit with interaction"){
        ipw.fit <-glm(Strue ~ X+C+X:C,data = data,family = binomial(link = "logit"))
      }else if(model=="IPW logadd without interaction"){
        ipw.fit <-glm(Strue ~ X+C,data = data,family = poisson(link = "log"))
      }else if(model=="IPW logadd with interaction"){
        ipw.fit <-glm(Strue ~ X+C+X:C,data = data,family = poisson(link = "log"))
      }
      ipw.probs <- ipw.fit$fitted.values
      ipw.weights <-data$Strue/ipw.probs 
      design1<-svydesign(id = ~1,weights = ~ipw.weights,data = data)
      fit.Y<-svyglm(Yobs~X,data=data,family = quasibinomial(link = "logit"), design = design1) 
    }
  }
  
  fit.Y2<-summary(fit.Y)$coefficients
  ci<-suppressMessages(confint(fit.Y, level=0.95))
  data.out$alpha.est<-fit.Y2[1,1]
  data.out$alpha.se<-fit.Y2[1,2]
  data.out$alpha.ci<-paste(round(ci[1,1],2),round(ci[1,2],2), sep="-") 
  data.out$beta.est<-fit.Y2[2,1]
  data.out$beta.se<-fit.Y2[2,2]
  data.out$beta.ci<-paste(round(ci[2,1],2),round(ci[2,2],2), sep="-")
  data.out
}

#function for analyse the data
analyse<-function(m=10,prob.X=0.3, mu.C=0, alpha.cts=1, beta=1, gamma=1,sd.error=1, sel.prob=0.5, delta1=0.3, delta2=0.3, delta3=0.1, n=1000,seed=1356,k=1,s.model="logit",y.type="continuous", alpha.XC=1, beta.XC=1, prob.Y=0.5){
  
  #the vectors below to store the analysis results
  full<-vector('list',m)
  obs<-vector('list',m)
  logit.noi<-vector('list',m)
  logit.i<-vector('list',m)
  logadd.noi<-vector('list',m)
  logadd.i<-vector('list',m)
  
  for (i in 1:m){
    #simulate one data set, then model this data set
    data.one.simulation<-as.data.frame(sim(n.sim=i,prob.X=prob.X, mu.C=mu.C, alpha.cts = alpha.cts, beta=beta, gamma=gamma,sd.error=sd.error, sel.prob=sel.prob, delta1=delta1, delta2=delta2, delta3=delta3, n=n,seed=seed,k=k, s.model = s.model, y.type = y.type,alpha.XC = alpha.XC, beta.XC = beta.XC, prob.Y = prob.Y))
    full[[i]]<-tibble(model("True model", data=data.one.simulation, y.type = y.type))
    obs[[i]]<-tibble(model("CCA",data = data.one.simulation, y.type = y.type))
    logit.noi[[i]]<-tibble(model("IPW logit without interaction",data = data.one.simulation, y.type = y.type))
    logit.i[[i]]<-tibble(model("IPW logit with interaction",data = data.one.simulation, y.type = y.type))
    logadd.noi[[i]]=tibble(model("IPW logadd without interaction",data = data.one.simulation, y.type = y.type))
    logadd.i[[i]]=tibble(model("IPW logadd with interaction",data = data.one.simulation, y.type = y.type))
  }
  
  #convert the list to dataframe
  full.df<-full %>% bind_rows(.id="n.sim")  %>% as.data.frame()
  obs.df<-obs %>% bind_rows(.id="n.sim")  %>% as.data.frame()
  logit.noi.df<-logit.noi%>% bind_rows(.id="n.sim")  %>% as.data.frame()
  logit.i.df<-logit.i%>% bind_rows(.id="n.sim")  %>% as.data.frame()
  logadd.noi.df<-logadd.noi%>% bind_rows(.id="n.sim")  %>% as.data.frame()
  logadd.i.df<-logadd.i%>% bind_rows(.id="n.sim")  %>% as.data.frame()
  #bind the results of all methods
  method<-rep(c("True model","CCA","ipw logit noi","ipw logit i", "ipw logadd noi","ipw logadd i"),each=m)
  delta3<-rep(delta3,6*m)
  sel.prob<-rep(sel.prob,6*m)
  data.out1<-rbind(full.df,obs.df,logit.noi.df,logit.i.df,logadd.noi.df,logadd.i.df)
  data.out<-cbind(delta3,sel.prob,method,data.out1)
  data.out
}

#function for performance measures
#vary one parameter at a time
compare<-function(m=10,prob.X=0.3, mu.C=0, alpha.cts=1, beta=1, gamma=1,sd.error=1, sel.prob.range=0.5, delta1=0.3, delta2=0.3, delta3.range=0.1, n=1e5,seed=1356,k=1,parameter.vary=0.1,s.model="logit",y.type="continuous", alpha.XC=1, beta.XC=1,prob.Y=0.5){
  k=length(parameter.vary)
  data.compare.list<-vector('list',k)
  
  if (all(parameter.vary==sel.prob.range)){ #vary sel.prob.
    for (j in 1:k){
      sel.prob=sel.prob.range[j]
      data.compare.list[[j]]<-tibble(analyse(m=m,prob.X=prob.X, mu.C=mu.C, alpha.cts=alpha.ctx, beta=beta, gamma=gamma,sd.error=sd.error, sel.prob=sel.prob, delta1=delta1, delta2=delta2, delta3=delta3.range, n=n,seed=seed,k=j,s.model = s.model, y.type = y.type, alpha.XC = alpha.XC, beta.XC = beta.XC, prob.Y = prob.Y))
      print(j)
    }
    
    #For DAG3,to have the true value for each value of delta3, no matter whether Y is continuous or binary.
    data.compare1<-data.compare.list %>% bind_rows(.id="DGM")  %>% as.data.frame()
    #to have the true value for each value of sel.prob
    mean.beta.full<-data.compare1%>% filter(method=="True model") %>% group_by(sel.prob) %>% summarise(each.true=mean(beta.est)) %>% as.data.frame() 
    data.compare<-merge(data.compare1,mean.beta.full,by="sel.prob")
    
    performance<-simsum(data=data.compare,estvarname = "beta.est", true = "each.true", se="beta.se", methodvar = "method", ref = "True model", by="sel.prob",x = TRUE)
    
    per.sum<-summary(performance)
    per.sum.df1<-tidy(per.sum) %>% mutate(across(where(is.numeric), ~ round(., 6)))
    per.sum.df1$est <- paste(per.sum.df1$est," (",per.sum.df1$mcse,")")
    per.sum.df2<-subset(per.sum.df1, select =  -c(mcse,lower, upper))
    colnames(per.sum.df2)[2]<-"estimate_MCSE"
    per.sum.df3<-per.sum.df2 %>% relocate(method,.before = stat) %>% relocate(sel.prob,.before = method)
    per.sum.df4<-reshape(per.sum.df3, idvar = c("sel.prob","method"), timevar = "stat", direction = "wide")
    head(per.sum.df4)
    per.sum.df<-subset(per.sum.df4, select = c(sel.prob,method,estimate_MCSE.bias,estimate_MCSE.cover,estimate_MCSE.empse,estimate_MCSE.modelse,estimate_MCSE.relprec,estimate_MCSE.relerror,estimate_MCSE.mse,estimate_MCSE.power))
    col.names<-c("sel.prob", "method",  "bias", "coverage","EmpSE", "ModSE", "relative_precision", "relative_error_ModSE", "MSE", "power")
    colnames(per.sum.df)<-col.names
    head(per.sum.df)
    write.csv(per.sum.df,paste("v1.0_dag3_",y.type,s.model,"_perform_sel_d3_",delta3.range,".csv"),row.names = F)
    
    
    ##plots for beta
    #a bias plot by myself
    bias.for.plot1<-separate(data = per.sum.df, col = bias, into = c("bias", "mcse1"), sep = "\\(")
    bias.for.plot2<-separate(data = bias.for.plot1,col = mcse1,into=c("mcse","unwanted"),sep="\\)")
    bias.for.plot3<-subset(bias.for.plot2,select=-c(unwanted))
    head(bias.for.plot3)
    bias.for.plot3[,c("bias","mcse")] <- sapply(bias.for.plot3[,c("bias","mcse")], as.numeric)
    ci.lower<-bias.for.plot3$bias - qnorm(0.975)*bias.for.plot3$mcse
    ci.upper<-bias.for.plot3$bias + qnorm(0.975)*bias.for.plot3$mcse
    bias.for.plot<-cbind(bias.for.plot3,ci.lower,ci.upper)
    
    bias.no0.9<-bias.for.plot[bias.for.plot$sel.prob!=0.9,]
    
    y.min<-as.numeric(min(bias.for.plot$bias))-0.01
    y.max<-as.numeric(max(bias.for.plot$bias))+0.01
    #including sel.prob=0.9
    ggplot(data = bias.for.plot, mapping = aes(x = sel.prob, y = as.numeric(bias), group = method,colour=method))+geom_point(size=1)+geom_errorbar(aes(ymax = ci.upper, ymin = ci.lower),width=0.2)+geom_line(linetype="dashed")+ggtitle(paste("Bias vs sel.prob (Strue=",s.model,", delta3=",delta3.range,")"))+ylab("Bias of beta")+theme_bw()
    ggsave(filename =paste("v1.0_dag3_",y.type,s.model,"_sel_d3_",delta3.range,"_bias_with_ci_with0.9.png"),units="in", width=7, height=4, bg = "transparent")
    
    ggplot(data = bias.for.plot, mapping = aes(x = sel.prob, y = as.numeric(bias), group = method,colour=method))+geom_point()+geom_line(linetype="dashed")+ggtitle(paste("Bias vs sel.prob (Strue=",s.model,", delta3=",delta3.range,")"))+ylab("Bias of beta")+theme_bw()
    ggsave(filename = paste("v1.0_dag3_",y.type,s.model,"_sel_d3_",delta3.range,"_bias_no_ci_with0.9.png"),units="in", width=7, height=4)
    
    if (s.model=="logadd"){
      #excluding se.prob=0.9
      ggplot(data = bias.no0.9, mapping = aes(x = sel.prob, y = as.numeric(bias), group = method,colour=method))+geom_point(size=1)+geom_errorbar(aes(ymax = ci.upper, ymin = ci.lower),width=0.2)+geom_line(linetype="dashed")+ggtitle(paste("Bias vs sel.prob (Strue=",s.model,", delta3=",delta3.range,")"))+ylab("Bias of beta")+theme_bw()
      ggsave(filename =paste("v1.0_dag3_",y.type,s.model,"_sel_d3_",delta3.range,"_bias_with_ci_no0.9.png"),units="in", width=7, height=4, bg = "transparent")
      
      ggplot(data = bias.no0.9, mapping = aes(x = sel.prob, y = as.numeric(bias), group = method,colour=method))+geom_point()+geom_line(linetype="dashed")+ggtitle(paste("Bias vs sel.prob (Strue=",s.model,", delta3=",delta3.range,")"))+ylab("Bias of beta")+theme_bw()
      ggsave(filename = paste("v1.0_dag3_",y.type,s.model,"_sel_d3_",delta3.range,"_bias_no_ci_no0.9.png"),units="in", width=7, height=4)
    }
    
  }else if (all(parameter.vary==delta3.range)){ #vary delta3.
    for (j in 1:k){
      delta3<-delta3.range[j]
      data.compare.list[[j]]<-tibble(analyse(m=m,prob.X=prob.X, mu.C=mu.C, alpha.cts=alpha.cts, beta=beta, gamma=gamma,sd.error=sd.error, sel.prob=sel.prob.range, delta1=delta1, delta2=delta2, delta3=delta3, n=n,seed=seed,k=j,s.model = s.model, y.type = y.type, alpha.XC = alpha.XC, beta.XC = beta.XC, prob.Y = prob.Y))
      print(j)
    }
    data.compare1<-data.compare.list %>% bind_rows(.id="DGM")  %>% as.data.frame()
    #to have the true value for each value of delta3, no matter whether Y is continuous or binary.
    mean.beta.full<-data.compare1%>% filter(method=="True model") %>% group_by(delta3) %>% summarise(each.true=mean(beta.est)) %>% as.data.frame() 
    data.compare<-merge(data.compare1,mean.beta.full,by="delta3")
    
    performance<-simsum(data=data.compare,estvarname = "beta.est", true = "each.true", se="beta.se", methodvar = "method", ref = "True model", by="delta3",x = TRUE)
    
    per.sum<-summary(performance)
    per.sum.df1<-tidy(per.sum) %>% mutate(across(where(is.numeric), ~ round(., 6)))
    per.sum.df1$est <- paste(per.sum.df1$est," (",per.sum.df1$mcse,")")
    per.sum.df2<-subset(per.sum.df1, select =  -c(mcse,lower, upper))
    colnames(per.sum.df2)[2]<-"estimate_MCSE"
    per.sum.df3<-per.sum.df2 %>% relocate(method,.before = stat) %>% relocate(delta3,.before = method)
    per.sum.df4<-reshape(per.sum.df3, idvar = c("delta3","method"), timevar = "stat", direction = "wide")
    head(per.sum.df4)
    per.sum.df<-subset(per.sum.df4, select = c(delta3,method,estimate_MCSE.bias,estimate_MCSE.cover,estimate_MCSE.empse,estimate_MCSE.modelse,estimate_MCSE.relprec,estimate_MCSE.relerror,estimate_MCSE.mse,estimate_MCSE.power))
    col.names<-c("delta3", "method",  "bias","coverage", "EmpSE", "ModSE", "relative_precision", "relative_error_ModSE", "MSE", "power")
    colnames(per.sum.df)<-col.names
    write.csv(per.sum.df, paste("v1.0_dag3_",y.type,s.model,"_perform_delta3_sel_",sel.prob.range,".csv"),row.names = F)
    
    ##plots for beta
    #a bias plot by myself
    bias.for.plot1<-separate(data = per.sum.df, col = bias, into = c("bias", "mcse1"), sep = "\\(")
    bias.for.plot2<-separate(data = bias.for.plot1,col = mcse1,into=c("mcse","unwanted"),sep="\\)")
    bias.for.plot3<-subset(bias.for.plot2,select=-c(unwanted))
    head(bias.for.plot3)
    bias.for.plot3[,c("bias","mcse")] <- sapply(bias.for.plot3[,c("bias","mcse")], as.numeric)
    ci.lower<-bias.for.plot3$bias - qnorm(0.975)*bias.for.plot3$mcse
    ci.upper<-bias.for.plot3$bias + qnorm(0.975)*bias.for.plot3$mcse
    bias.for.plot<-cbind(bias.for.plot3,ci.lower,ci.upper)
    
    y.min<-as.numeric(min(bias.for.plot$bias))-0.01
    y.max<-as.numeric(max(bias.for.plot$bias))+0.01
    ggplot(data = bias.for.plot, mapping = aes(x = delta3, y = as.numeric(bias), group = method,colour=method))+geom_point(size=1)+geom_errorbar(aes(ymax = ci.upper, ymin = ci.lower),width=0.2)+geom_line(linetype="dashed")+ggtitle(paste("Bias vs delta3 (Strue=",s.model,")"))+ylab("Bias of beta")+theme_bw()
    ggsave(filename = paste("v1.0_dag3_",y.type,s.model,"_d3_bias_with_ci.png"),units="in", width=7, height=4, bg = "transparent")
    
    ggplot(data = bias.for.plot, mapping = aes(x = delta3, y = as.numeric(bias), group = method,colour=method))+geom_point()+geom_line(linetype="dashed")+ggtitle(paste("Bias vs delta3 (Strue=",s.model,")"))+ylab("Bias of beta")+theme_bw()
    ggsave(filename = paste("v1.0_dag3_",y.type,s.model,"_d3_bias_no_ci.png"),units="in", width=7, height=4, bg = "transparent")
  }
  
  list(per.sum,per.sum.df)
}


####----y continuous

#####-----Strue=logit
# com.delta3.1<-compare(m=1000,n=1e5,prob.X=0.3, mu.C=0, alpha.cts=1, beta=1, gamma=1,sd.error=1, sel.prob.range=0.5, delta1=0.3, delta2=0.3, delta3.range=seq(-0.3,0.3,length=7), seed=1356,k=1,parameter.vary=seq(-0.3,0.3,length=7),s.model="logit",y.type="continuous", alpha.XC=1, beta.XC=1,prob.Y=0.5)
# 

#####-----Strue=logadd

com.delta3.2<-compare(m=1000,n=1e5,prob.X=0.3, mu.C=0, alpha.cts=1, beta=1, gamma=1,sd.error=1, sel.prob.range=0.5, delta1=-0.4, delta2=-0.1, delta3.range=seq(-0.3,0.3,length=7), seed=1356,k=1,parameter.vary=seq(-0.3,0.3,length=7),s.model="logadd",y.type="continuous", alpha.XC=1, beta.XC=1,prob.Y=0.5)



#####-----------y binary
#vary delta3

####---Strue=logit

# com.delta3.3<-compare(m=1000,n=1e5,prob.X=0.3, mu.C=0, alpha.cts=1, beta=1, gamma=1,sd.error=1, sel.prob.range=0.5, delta1=0.3, delta2=0.3, delta3.range=seq(-0.3,0.3,length=7),seed=1356,k=1,parameter.vary=seq(-0.3,0.3,length=7),s.model="logit",y.type="binary", alpha.XC=1, beta.XC=1,prob.Y=0.5)

#####-----Strue=logadd
com.delta3.4<-compare(m=1000,n=1e5,prob.X=0.3, mu.C=0, alpha.cts=1, beta=1, gamma=1,sd.error=1, sel.prob.range=0.5, delta1=-0.4, delta2=-0.1, delta3.range=seq(-0.3,0.3,length=7),seed=1356,k=1,parameter.vary=seq(-0.3,0.3,length=7),s.model="logadd",y.type="binary", alpha.XC=1, beta.XC=1,prob.Y=0.5)

