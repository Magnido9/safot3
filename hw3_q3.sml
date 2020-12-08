datatype 'a HashTableEntry = Empty
    | Deleted
    | Value of int * 'a;
type 'a HashTable = 'a HashTableEntry list;
fun create (len:int) = if(len=1) then [Empty] else Empty::(create (len-1));
create 9;
fun get hash (table:'a HashTable) (key:int)= (List.nth (table, hash(key)));

(*fun get hash table key=if(key=(hd (hd table)) then  (tl (hd table)) else (get hash (tl table) key);*)
