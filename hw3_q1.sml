
datatype ('a,'b) heterolist =
Nil
| ::: of 'a * ('b,'a) heterolist;
infixr 5 ::: ;

1:::"x":::Nil;
fun build4 (x, one, y, two) =x:::one:::y:::two:::Nil;
val a = build4 ("x",1,"y",2);
fun unzipa (lst:('a,'b) heterolist): 'a list =
  case lst of
  Nil=>nil
  | a:::b:::a2 =>a::(unzipa a2 );
val alist=unzipa(a);
fun unzipb (lst:('a,'b) heterolist): 'b list =
  case lst of
  Nil=>nil
  | a:::b:::a2 =>b::(unzipb a2 );
val blist=unzipb(a)
fun unzip (lst:('a,'b) heterolist)=
  (unzipa(lst),unzipb(lst));
unzip(a);

exception Empty;
fun zip (alist,blist)=
  if(blist=nil) then ( if(alist=nil) then Nil else raise Empty ) else if(alist=nil) then raise Empty else (hd alist):::(hd blist):::zip((tl alist),(tl blist)) ;
zip(alist,blist);
zip ([1, 2, 3, 4, 5], [#"a", #"b", #"c", #"d", #"e"]);
