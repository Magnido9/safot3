fun to_binary  0 =[]
    | to_binary x = if (x mod 2=0) then 0::to_binary(x div 2) else 1::to_binary(x div 2) ;

fun encode lis=
let
  fun count1 lis= List.foldl op+ 0 lis;
  fun count0 lis= List.length(lis)- count1(lis); 
  fun encode_help one zero []=[]
    | encode_help one zero (0::xs) = if((count1(xs)+one)=(count0(xs)+1+zero)) then (0::xs)@(to_binary(one+zero)) else 1::(encode_help (one+1) zero xs)
    |encode_help one zero (1::xs)= if ((count1(xs)+1+one)=(count0(xs)+zero))then (1::xs)@(to_binary(one+zero)) else 0::(encode_help one (zero+1) xs)
    |encode_help _ _ _ =[];
in
 encode_help 0 0 lis
end;

fun decode (lis,x)=
let
  fun getIndex lis x i= if( List.drop( lis,x)=to_binary(i)) then i else getIndex lis x (i+1);
  fun decodeHelp lis size 0= List.take(lis,size)
    |decodeHelp (x::xs) size index= (1-x)::decodeHelp(xs) (size-1) (index-1)
    |decodeHelp _ _ _= [];
in
  case lis of
     [] => []
   | something => decodeHelp lis x (getIndex lis x 0)
  
end;
