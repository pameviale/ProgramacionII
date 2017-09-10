open List;

(* ——————————————————————————————————*)

(* Ejercicio 1 *)
(* A *)
datatype 'a arbol1 = Vacio | Nodo of 'a arbol1 * 'a * 'a arbol1;
val t1= Nodo(Vacio,5,Nodo(Vacio,7,Vacio));
val t2= Nodo(Nodo(Vacio,12,Vacio),14,Vacio);
val tree1 = Nodo(t1,10,t2);

(* B *)
 datatype 'b arbol2 = Nodo2 of 'b * 'b arbol2 list;
val t3 = Nodo2(10, [ Nodo2(5,[Nodo2(7,nil)]), Nodo2(14,[Nodo2(12,nil)])]);

(* ——————————————————————————————————*)

(* Ejercicio 2 *)
(* A *)

fun conver(Vacio:(string arbol1))=Vacio:(int arbol1)
| conver(Nodo(izq,cadena:string,der)) = Nodo(conver(izq),size(cadena),conver(der));

(* B *)

fun contar2(Nodo2(a:real,nil),x:real) = if (x<=a andalso a<=x) then 1 else 0
| contar2(Nodo2(a,t::ts), x) = contar2(t,x) + contar2(Nodo2(a,ts),x);

(* C *)

fun prod2(Nodo2(a:real, nil)) = a
| prod2(Nodo2(a,x::xs)) = prod2(x)*prod2(Nodo2(a,xs));

(* ——————————————————————————————————*)

(* Ejercicio 3 *)
(* A *)

fun listabs(l:real list) = map abs l;

(* ejemplo de uso *)
listabs([4.0,~5.0,~3.0,9.8]);

(* B *)

fun neg(x:real)= (x<0.0);
fun sumareal (x: real, y:real) = x+y;
fun sumneg(l:real list)= (foldr sumareal 0.0 (filter neg l));

(* ejemplo de uso *)
sumneg([4.0,~5.0,~3.0,9.8]);

(* C *)

fun longmen(s:string)=(size(s)<5);
fun transformar(s:string) = 1;
fun sumaint(x:int,y:int)=x+y;
fun contarpalabras(l:string list) = (foldr sumaint 0 (map transformar (filter longmen l)));

(* ejemplo de uso *)
contarpalabras(["casa","perro","auto","verano"]);

(* D *)
fun filtrarlargas(l:string list) = filter longmen l;

(* ejemplo de uso *)
filtrarlargas(["casa","perro","auto","verano"]);

(* ——————————————————————————————————*)

(* Ejercicio 4 *)

datatype 'etiqueta arbolbin = Vacio | Nodo3 of 'etiqueta arbolbin * 'etiqueta * 'etiqueta arbolbin; 

(* A *)

fun inorden(Vacio) = nil
| inorden(Nodo3(izq,n,der))= inorden(izq)@[n]@inorden(der);

fun listar_en_orden_creciente(ar:(int*int) arbolbin) = inorden(ar);

(* B *)

fun f(x:int,y:int) = (y=2);
fun seg_comp_dos(ar:(int*int) arbolbin) = filter f (listar_en_orden_creciente(ar));
