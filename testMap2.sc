val map = Map(1->2,2->3)
val mx = map.values.max
map.filter({case(k,v)=>v==mx})