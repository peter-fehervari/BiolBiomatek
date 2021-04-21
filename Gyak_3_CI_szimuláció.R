#### Ciklusok ####
# for, while, repeat 

# for ciklus
pelda <- c()

for (i in 1:10)
{
pelda[i] <- 4*i
print(pelda)
}

pelda

# while ciklus
i <- 1
pelda2 <- c()
while (i<11)
{
  pelda2[i] <- 4*i
  i <- i+1
}
pelda2
pelda

# végtelen cikus
i <- 1
pelda2 <- c()
while (i<11)
{
  pelda2[i] <- 4*i  
}


## egymásba ágyazott ciklusok

for (k in 1:3) {
  for (l in 1:4) {
    print(paste("k =", k, "l= ",l))
  }
}

# logikai elágazások
# páros számokat 2vel páratlanokat 3-al szorozni

i <- 1
pelda3 <- c()

for (i in 1:10){
  if (i %% 2 ==0)
    {
pelda3[i] <- i*2    
    } 
  else { 
    pelda3[i] <- i*3  
       }
}

pelda3

# függvényekbe ágyazott ciklusok

szorzo_pelda <- function(n){
  pelda3 <- c()
  
  for (i in 1:n){
    if (i %% 2 ==0)
    {
      pelda3[i] <- i*2    
    } 
    else { 
      pelda3[i] <- i*3  
    }
  }
  return(pelda3)
}

szorzo_pelda(20)









###Inits
n <- 10
mintak_szama <- 100
exp_val <- 1
sigma <- 2
alpha <- 0.05

minta <- c()
atlag <- c()
ci_lower <- c()
ci_upper <- c() 

for (i in 1:mintak_szama){
  minta <- rnorm(n,
                 mean = exp_val,
                 sd = sigma)
  atlag[i] <- mean(minta)  
ci_lower[i] <- mean(minta)+qt(alpha/2, n-1)*sd(minta)/sqrt(n)  
ci_upper[i] <- mean(minta)+qt(1-(alpha/2), n-1)*sd(minta)/sqrt(n)
}

sim_res <- bind_cols(id=1:mintak_szama,atlag=atlag,ci_lower=ci_lower,ci_upper=ci_upper)
sim_res$CI_cover <- ifelse(sim_res$ci_lower < exp_val & exp_val < sim_res$ci_upper,"1","0")

sim_res %>% ggplot(aes(y=id,
                       x=atlag,
                       color=CI_cover))+
  geom_pointrange(xmin=ci_lower,
                  xmax=ci_upper,
                  fatten=0.6)+
  scale_x_continuous(limits=c(exp_val-(sigma*2),exp_val+(sigma*2)))+
  scale_color_manual(values = c("red","grey70"))+
  geom_line(aes(x=exp_val),
            colour="black",
            alpha=0.7)+
  labs(x = "Várható érték",
       y = "")+
  theme_minimal()+
  theme(legend.position = "",
        axis.text.y=element_blank())



CI_cover_sims <- function(n=10,
                          mintak_szama=10,
                          exp_val=0,
                          sigma=1,
                          alpha=0.05)
{
  minta <- c()
  atlag <- c()
  ci_lower <- c()
  ci_upper <- c() 
  
  for (i in 1:mintak_szama){
    minta <- rnorm(n,
                   mean = exp_val,
                   sd = sigma)
    atlag[i] <- mean(minta)  
    ci_lower[i] <- mean(minta)+qt(alpha/2, n-1)*sd(minta)/sqrt(n)  
    ci_upper[i] <- mean(minta)+qt(1-(alpha/2), n-1)*sd(minta)/sqrt(n)
  }
  
  sim_res <- bind_cols(id=1:mintak_szama,atlag=atlag,ci_lower=ci_lower,ci_upper=ci_upper)
  sim_res$CI_cover <- ifelse(sim_res$ci_lower < exp_val & exp_val < sim_res$ci_upper,"0","1")
  
  sim_res %>% ggplot(aes(y=id,
                         x=atlag,
                         color=CI_cover))+
    geom_pointrange(xmin=ci_lower,
                    xmax=ci_upper,
                    fatten=0.6)+
    scale_x_continuous(limits=c(exp_val-(sigma*2),exp_val+(sigma*2)))+
    scale_color_manual(values = c("grey70","red"))+
    geom_line(aes(x=exp_val),
              colour="black",
              alpha=0.7)+
    labs(x = "Várható érték",
         y = "")+
    theme_minimal()+
    theme(legend.position = "",
          axis.text.y=element_blank())
  
}

CI_cover_sims(n=10,
              mintak_szama = 200,
              sigma=10)
