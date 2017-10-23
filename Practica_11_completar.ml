
open List;

(* PRÁCTICA 11 *)

(*———————————————*)

(* EJERCICIO 1 *)

type estudiante = {Nombre:string, Notas: real list};

type estudiante_prom = {Nombre:string, Promedio: real};

(* A *)


(*val promedio = fn : real list -> real*)
fun promedio(………………………………………) = ………………………………………………………………………………………………


(* val promedio_estudiante = fn : estudiante -> estudiante_prom *)
fun promedio_estudiante(est: estudiante) = {Nombre = #Nombre(est), Promedio=promedio(#Notas(est))}:estudiante_prom;                        

(* val promedios = fn : estudiante list -> estudiante_prom list*)
fun promedios(lista: estudiante list) = map ……………………………………………………………………………………………


(* B *)
(* val aprobado = fn : estudiante_prom -> bool *)
fun aprobado(est: estudiante_prom) = …………………………………………………………………………………………………


(* val aprobados = fn : estudiante list -> int *)
fun aprobados(lista: estudiante list) = 
	let
		aprob= (filter aprobado (…………………………))
	in
		foldr ………………………… aprob
	end;


(*———————————————*)

(* EJERCICIO 2 *)

datatype 'a arbolbin = Vacio | Nodo of 'a  * 'a arbolbin * 'a arbolbin;
type libro = {Titulo:string, Autor:string, Disponible:bool};

(* A *)

(* val crear_libro = fn : string * string * bool -> libro *)
fun crear_libro (tit: string, aut:string, disp:bool) = …………………………………………………………………………

(* B *)

(* val ingresar libro = fn : libro arbolbin * string * string -> libro arbolbin *)
fun ingresar libro(Vacio: libro arbolbin, tit:string, aut:string) = …………………………………………………………………………
| ingresar_libro(Nodo(lib,izq,der), tit: string, aut:string) = …………………………………………………………………………


(* C *)

(* val prestar_libro = fn : libro arbolbin * string -> libro arbolbin *)
fun prestar_libro(Vacio:libro arbolbin,tit:string) = …………………………………………………………………………
| prestar_libro(Nodo(lib,izq,der), tit: string) = …………………………………………………………………………


(* D *)

(* val buscar_autor = fn : libro arbolbin * string -> string *)
fun buscar_autor(Vacio:libro arbolbin, tit:string) = …………………………………………………………………………
| buscar_autor(Nodo(lib,izq,der), tit: string) = …………………………………………………………………………

(* E *)

(* val convertir_arbol_a_lista = fn : libro arbolbin -> libro list *)
fun convertir_arbol_a_lista(Vacio: libro arbolbin) = nil
|  convertir_arbol_a_lista(Nodo(lib, izq,der)) = …………………………………………………………………………


(* val libros_prestados = fn : libro arbolbin -> libro list *)
fun libros_prestados(registro: libro arbolbin) = 
	let 
		l=convertir_arbol_a_lista(registro)
	in
		filter ……………………… l
	end;


(* F *)
(* val cantidad_disponibles = fn : libro arbolbin -> int *)
fun cantidad_disponibles(registro: libro arbolbin) =  
	let
		lista= convertir_arbol_a_lista(registro);
		disponibles= filter ……………………… lista
	in
		foldr ………………………(map …………………… disponibles)
	end;


