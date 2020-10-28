
#### Parameters

* C = predator population ("Consumer")  
* V = prey population ("Victim")  
* R = prey per capita population growth rate  
* q = predator starvation rate  
* a = predator attack rate  
* f = conversion efficiency of consumed prey into predator offspring  
* Kc = predator carrying capacity  
* Kv = prey carrying capacity  
  
#### Density-independent growth  
  
##### Population growth  
  
  
Prey: 
$V_{t+1} = V_t + RV_t - aC_tV_t$  
  
Predator: 
$$C_{t+1} = C_t + afV_tC_t - qC_t$$   
  
  
##### Equilibrium solutions  
  
  
Prey: 
$$C_t = \frac{R}{a}$$  
  
Predator: 
$$V_t =  \frac{q}{af}$$   
  
  
#### Density-dependent growth   
  
##### Population growth  
   
   
Prey: 
$$V_{t+1} = V_t + RV_t\frac{K_V-V_t}{K_V} - aC_tV_t$$  
  
Predator: 
$$C_{t+1} = C_t + afV_tC_t\frac{K_C-C_t}{K_C} - qC_t$$
   
   
##### Equilibrium solutions  
   
   
Prey: 
$$C_t = \frac{R}{a}-\frac{R}{aK_V}V_t$$  
  
Predator: 
$$V_t = \frac{qK_C}{af(K_C-C_t)}$$
  
  

