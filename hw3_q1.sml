
datatype ('a,'b) heterolist =
Nil
| cons of 'a * ('b,'a) heterolist;
cons(1,cons("x",Nil));
fun build4 (x, one, y, two) =cons(x,cons(one,cons(y,cons(two,Nil))));
val a = build4 ("x",1,"y",2);
fun unzipa (lst:('a,'b) heterolist): 'a list =
  case lst of
  Nil=>nil
  | cons(a,cons(b,a2)) =>a::(unzipa a2 );
val alist=unzipa(a);
fun unzipb (lst:('a,'b) heterolist): 'b list =
  case lst of
  Nil=>nil
  | cons(a,cons(b,a2)) =>b::(unzipb a2 );
val blist=unzipb(a)
fun unzip (lst:('a,'b) heterolist)=
  (unzipa(lst),unzipb(lst));
unzip(a);

exception Empty
fun zip (alist,blist)=
  if(blist=nil) then ( if(alist=nil) then Nil else raise Empty ) else if(alist=nil) then raise Empty else cons((hd alist),cons((hd blist),zip((tl alist),(tl blist)))) ;
zip(alist,blist);
zip ([1, 2,3], ["a", "b","c"]);
