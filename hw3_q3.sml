exception Fail;
datatype 'a HashTableEntry = Empty
    | Deleted
    | Value of int * 'a;
type 'a HashTable = 'a HashTableEntry list;

fun create (len:int) = if(len=1) then [Empty] else Empty::(create (len-1));

fun insert h (t:'a HashTable) (key,v):'a HashTable=
let
fun replace_i i counter x []=raise Div
|replace_i i  counter x (l::ll) =if(counter=i) then x::ll else l::(replace_i i (counter+1) x ll);  

fun insertHelp (h, t:'a HashTable, index, (Value (key,v)))=
case List.nth (t,index) of
Value(x,_)=>if(x=key) then replace_i index 0 (Value (key,v)) t else insertHelp(h,t,h(index),Value(key,v))
|_=>replace_i index 0 (Value (key,v)) t;

in 
insertHelp (h,t,h(key),(Value(key,v)))
 
end;

fun getkeyval (entry:'a HashTableEntry ):int*'a=
case entry of
Value(x,y)=>(x,y);

fun getrec hash (table:'a HashTable) (key:int) (ogkey:int)=if(#1 (getkeyval (List.nth (table, (hash key))))=ogkey) then #2 (getkeyval (List.nth (table, (hash key)))) else getrec hash table (hash key) ogkey;

fun get hash (table:'a HashTable) (key:int)=getrec hash table key key;
val table=create 9;
fun h x:int= (x mod 9)+1;
val table=insert h table (2,"test");
(get h table 2);
val table=insert h table (1,"test2");
(get h table 1);
val table=insert h table (10,"test3");
(get h table 10);

fun remove hash (table:'a HashTable) key:'a HashTable=
let
fun replace_i i counter x []=raise Div
|replace_i i  counter x (l::ll) =if(counter=i) then x::ll else l::(replace_i i (counter+1) x ll); 
fun removeHelp hash table key index=
    case List.nth (table,index) of
    Value(x,_):'a HashTableEntry => if (x=key) then replace_i (index) 0 Deleted table else removeHelp hash table key (hash(index));
in
removeHelp hash table key (hash(key))
end;
