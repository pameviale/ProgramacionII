
open List;

(* PRUEBA *)
(* TEMA 1*)

(*———————————————*)

(* EJERCICIO 1 *)

type tiempo = {hs: int, min: int, segs: int};

(* A *)

(* val tiempoCorrecto = fn : tiempo -> bool *)
fun tiempoCorrecto(t:tiempo)=(#hs(t)>=0 andalso #hs(t)<=23 andalso #min(t)>=0 andalso #min(t)<=59 andalso #segs(t)>=0 andalso #segs(t)<=59);

(* val andLogico = fn : bool * bool -> bool *)
fun andLogico(a:bool, b: bool) = (a andalso b);


(* val tiemposCorrectos = fn : tiempo list -> bool*)
fun tiemposCorrectos(tlist:tiempo list) = 
		let 
			val boollist= (map tiempoCorrecto tlist);
		in
			foldr andLogico true boollist 
		end;


(* B *)
(* val segsMedianoche = fn : tiempo -> int *)
fun segsMedianoche(t:tiempo) = 3600 * #hs(t) + 60 * #min(t) + #segs(t);


(* val listaSegsMedianoche = fn : tiempo list -> int list *)
fun listaSegsMedianoche(tlist: tiempo list) = if(tlist = nil ) then nil else (if (tiempoCorrecto(hd(tlist))) then segsMedianoche(hd(tlist))::listaSegsMedianoche(tl(tlist)) else (~1)::listaSegsMedianoche(tl(tlist)));


(* C *)
(* val filterTiemposCorrectos = fn : tiempo list -> tiempo list *)
fun filterTiemposCorrectos(tlist:tiempo list) = filter tiempoCorrecto tlist;

(* val segsSupera75000 = fn : tiempo -> bool *)
fun segsSupera75000(t:tiempo) = (segsMedianoche(t)>75000);

(* val listaSupera75000 = fn : tiempo list -> tiempo list *)
fun listaSupera75000(tlist:tiempo list) = filter segsSupera75000 (filterTiemposCorrectos tlist);


(* PRUEBA *)
(* TEMA 1*)

(*———————————————*)

(* EJERCICIO 2 *)

datatype 'a arbolbin = Vacio | Nodo of 'a  * 'a arbolbin * 'a arbolbin;

type auto = {Marca: string, Modelo: string, Colores: string list, PrecioBase: real};

(* A *)
(* val crear_auto = fn : string * string * string list * real -> auto *)
fun crear_auto(mar:string,mo:string, col:string list, prec: real) = {Marca=mar,Modelo=mo, Colores=col,PrecioBase=prec}:auto;

(* val ingresar_auto = fn : auto arbolbin * string * string * string list * real -> auto arbolbin *)
fun ingresar_auto(Vacio: auto arbolbin, mar:string,mo:string, col:string list, prec: real)=
	Nodo(crear_auto(mar,mo,col,prec),Vacio,Vacio)
| ingresar_auto(Nodo(n, izq,der): auto arbolbin, mar:string,mo:string, col:string list, prec: real)= 
        if(#Modelo(n) = mo) then Nodo(crear_auto(mar,mo,col,prec),izq,der)
		            else (if (#Modelo(n)> mo) then Nodo(n,ingresar_auto(izq, mar,mo,col,prec),der) 
                                                      else Nodo(n,izq,ingresar_auto(der, mar,mo,col,prec)));

(* B *)

(* val cambiar_precio = fn : auto arbolbin * string * real -> auto arbolbin *)
fun cambiar_precio(Vacio: auto arbolbin, mo:string, prec: real)= Vacio
| cambiar_precio(Nodo(n, izq,der): auto arbolbin, mo:string, prec: real)= 
        if(#Modelo(n) = mo) then Nodo(crear_auto(#Marca(n),mo,#Colores(n),prec),izq,der)
		            else (if (#Modelo(n)> mo) then Nodo(n,cambiar_precio(izq, mo,prec),der) 
                                                      else Nodo(n,izq,cambiar_precio(der, mo,prec)));

(* C *)

(* val buscar_colores = fn : auto arbolbin * string -> string list *)
fun buscar_colores(Vacio: auto arbolbin, mo:string)= nil: string list
| buscar_colores(Nodo(n, izq,der): auto arbolbin, mo:string)= 
        if(#Modelo(n) = mo) then #Colores(n)
		            else (if (#Modelo(n)> mo) then buscar_colores(izq, mo) 
                                                      else buscar_colores(der, mo));

(* D *)

(* val inOrden = fn : auto arbolbin -> auto list *)
fun inOrden(Vacio: auto arbolbin) = nil:auto list
| inOrden(Nodo(n,izq,der)) = inOrden(izq) @ [n] @ inOrden(der);

(* val colorEnLista = fn : string * string list -> bool *)
fun colorEnLista(c:string,nil: string list) = false
| colorEnLista(c:string,cl:string list) = (hd(cl) = c) orelse (colorEnLista(c,tl(cl)));

(* val autoDelColor = fn : string -> auto -> bool *)
fun autoDelColor (color:string) (aut:auto) = colorEnLista(color,#Colores(aut)); 

(* val buscar_autos = fn : auto arbolbin * string -> auto list *)
fun buscar_autos(autosarbol: auto arbolbin, color:string) = 
	let
		val autoslista = inOrden(autosarbol);
	in
		filter (autoDelColor(color)) autoslista
	end;	
			
(* E *)

(* val modelo = fn : auto -> string *)
fun modelo(au:auto) = #Modelo(au);

(* val buscar_modelos = fn : auto arbolbin * string -> string list *)
fun buscar_modelos(autosarbol: auto arbolbin, color:string) = 
	let
		val autos = buscar_autos(autosarbol,color);
	in 
		map modelo autos
	end;

(* F *)

(* val cantidad_disponibles = fn : auto arbolbin * string -> int *)
fun cantidad_disponibles(autosarbol: auto arbolbin, color:string) = length(buscar_autos(autosarbol,color));


