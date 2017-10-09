open List;

(* ——————————————————————————————————*)

(* Ejercicio 1 *)
(* A *)
datatype 'a arbol1 = Vacio | Nodo1 of 'a arbol1 * 'a * 'a arbol1;
val t1= Nodo1(Vacio,2,Vacio);
val t2= Nodo1(Nodo1(Vacio,4,Vacio),3,Nodo1(Vacio, 5,Vacio));
val arbol1 = Nodo1(t1,1,t2);

(* B *)
datatype 'b arbol2 = Nodo2 of 'b * 'b arbol2 list;
val t3 = Nodo2(1, [ Nodo2(2,nil), Nodo2(3,[Nodo2(4,nil),Nodo2(5,nil)])]);

val t4 = Nodo2(1.0, [ Nodo2(2.0,nil), Nodo2(3.0,[Nodo2(4.0,nil),Nodo2(5.0,nil)])]);

(* ——————————————————————————————————*)

(* Ejercicio 2 *)
(* A *)

fun sumaParesArbol(Vacio:(int * int) arbol1)=Vacio:(int arbol1)
| sumaParesArbol(Nodo1(izq,(x:int,y:int),der)) = Nodo1(sumaParesArbol(izq),x+y, sumaParesArbol(der));

(* B *)

fun apareceEnLista(nil: char list, c:char)= false
| apareceEnLista(x::xs,c) = (x=c) orelse (apareceEnLista(xs,c)); 

fun apareceEnArbol(Nodo2(cadena,nil),c:char)= apareceEnLista(explode(cadena),c)
| apareceEnArbol(Nodo2(cadena, x::xs),c)= apareceEnLista(explode(cadena),c) orelse apareceEnArbol(x,c) orelse (apareceEnListaDeArboles(xs,c))
and
 apareceEnListaDeArboles(nil:(string arbol2) list,c:char) = false
|apareceEnListaDeArboles(x::xs,c) = apareceEnArbol(x,c) orelse apareceEnListaDeArboles(xs,c);


(* C *)

fun cuad(x)=x*x;

fun cuadrados(Nodo2(x:int,nil))= Nodo2(cuad(x),nil)
| cuadrados(Nodo2(x, XS)) = Nodo2(cuad(x),(map cuadrados XS));

(* ——————————————————————————————————*)

(* Ejercicio 3 *)
(* A *)

fun listrunc(l:real list) = map trunc l;

(* ejemplo de uso *)
listrunc([4.7,~5.4,~3.7,9.8]);

(* B *)

fun pos(x:real)= (x>0.0);
fun sumareal (x: real, y:real) = x+y;
fun sumpos(l:real list)= (foldr sumareal 0.0 (filter pos l));

(* ejemplo de uso *)
sumpos([4.0,~5.0,~3.0,9.8]);

(* C *)

fun filtro(x:int,y:int) = (y>5);

fun filtrar2daComponente(l:(int * int) list) = filter filtro l;

(* ejemplo de uso *)
filtrar2daComponente([(1,2),(1,5),(1,8),(3,9)]);

(* D *)
fun suma1eraComponente((x,y),z) = x+z;

fun sumarfiltrados(l:(int * int) list) = (foldr suma1eraComponente 0 (filtrar2daComponente l));


(* ejemplo de uso *)
sumarfiltrados([(1,2),(1,5),(1,8),(3,9)]);

(* ——————————————————————————————————*)

(* Ejercicio 4 *)

datatype 'etiqueta arbolbin = Vacio | Nodo of 'etiqueta arbolbin * 'etiqueta * 'etiqueta arbolbin; 

(* A *)

fun inorden(Vacio) = nil
| inorden(Nodo(izq,n,der))= inorden(izq)@[n]@inorden(der);

fun listar_en_orden_creciente(ar:(string*int) arbolbin) = inorden(ar);

(* B *)

fun f((cadena:string,x:int)) = (size(cadena)>=5);
fun filtrar_palabras_cortas(ar:(string*int) arbolbin) = filter f (listar_en_orden_creciente(ar));
