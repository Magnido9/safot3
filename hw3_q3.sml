datatype 'a HashTableEntry = Empty
    | Deleted
    | Value of int * 'a;
type 'a HashTable = 'a HashTableEntry list;
fun create (len:int) = if(len=1) then [Empty] else Empty::(create (len-1));

exception Fail;

fun insert h (t:'a HashTable) (key,v):'a HashTable=
let
fun replace_i i counter x []=raise Fail
|replace_i i  counter x (l::ll) =if(counter=i) then x::ll else l::(replace_i i (counter+1) x ll);  

fun get_i i counter []=raise Fail
|get_i i counter (x::xs:'a HashTable)=if(counter=i) then x else get_i i (counter+1) xs;

fun insertHelp (h, t:'a HashTable, index, (Value (key,v)))=
case (get_i index 0 t) of
Value(x,_)=>if(x=key) then replace_i index 0 (Value (key,v)) t else insertHelp(h,t,h(index),Value(key,v))
|_=>replace_i index 0 (Value (key,v)) t;

in 
insertHelp (h,t,h(key),(Value(key,v)))
 
end;
create 9;
fun get hash (table:'a HashTable) (key:int)= (List.nth (table, hash(key)));

(*fun get hash table key=if(key=(hd (hd table)) then  (tl (hd table)) else (get hash (tl table) key);*)
