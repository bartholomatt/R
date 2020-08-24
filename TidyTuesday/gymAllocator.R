
capacity = 20
nSlots = 8
nDays = 7
nMembers = 250


# floor(nSlots*nDays*capacity/nMembers)
# 
# nDays*nSlots*capacity / nBucks /nMembers
# 
supply = nDays*nSlots*capacity
# moneyPool = nBucks*nMembers
# 
# moneyPool/supply
# 
slotsPerMember = supply/nMembers

###how do we allocate based on demand 

#start with a simple 2 tier model with ALL days of the week in same trache 
tiers = c(1:2)
slots = rep(0,8)

monday = c(1,1,2,2,1,1,2,2)
#monday = c(2,2,1,1,2,2,1,1)
tuesday = monday
wednesday = monday
thursday  = monday
friday =  monday
saturday = rep(2,8)
sunday = rep(2,8)

weekDF = data.frame(monday,tuesday,wednesday,thursday,friday,saturday,sunday)

primeSlots = sum(weekDF)-dim(weekDF)[1]*dim(weekDF)[2]
nonPrimeSlots = dim(weekDF)[1]*dim(weekDF)[2] - primeSlots


d = 25000/(nonPrimeSlots *capacity + primeSlots*capacity*2)

13.5 * 20 * 20 + 27 * 36 * 20





