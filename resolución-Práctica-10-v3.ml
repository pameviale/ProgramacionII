
open List;

(* PRÁCTICA 10 *)

(*———————————————*)

(* EJERCICIO 1 *)

(* A *)

type dino = { Nombre: string, Peso: real, Altura: real}; 

(* B *)

val tyranno = { Nombre= "Tyrannosaurius", Peso= 7.0, Altura= 20.0}:dino;

(* C *)

val  bracchio = { Nombre= "Bracchiosaurius", Peso= 50.0, Altura= 40.0}:dino;

(* D *)

#Altura(tyranno);

(* E *)

#Peso(bracchio);

(*———————————————*)

(* EJERCICIO 2 *)

exception ListaVacia;

(* A *)

fun masAlto(nil: dino list) = raise ListaVacia
| masAlto([x:dino]) = x:dino
| masAlto(x::XS) = 
	let 
		val masAltoResto = masAlto(XS);
		val alt = #Altura ( masAltoResto)
	in
		if  (#Altura(x)>alt) then x else masAltoResto
	end; 

(* Ejemplo de uso *)
masAlto([tyranno,bracchio]);

(* B *)

fun masPesado(nil:dino list) = raise ListaVacia
  | masPesado([x:dino]) = x:dino 
  | masPesado((x as {Peso=p,...})::XS) =
   		let
			val masPesadoResto = masPesado(XS);
			val peso = #Peso(masPesadoResto)
		in
			if (p>peso) then x else masPesadoResto
		end; 

(* Ejemplo de uso *)
masPesado([tyranno,bracchio]);

(*———————————————*)

(* EJERCICIO 3 *)

type estudiante = {ID:int, Cursos: string list, Nombre:string};

(* A *)

(* 1era versión *)
fun listaTocayos(nil:estudiante list, nombre:string)= nil: estudiante list
| listaTocayos(x::XS,nombre) = 
	if (#Nombre(x)=nombre) then x::listaTocayos(XS,nombre)
			       else listaTocayos(XS,nombre);

(* 2da versión *)

fun nombreCoincide (nom:string) (x:estudiante) = (#Nombre(x) = nom);
fun listaTocayosv2(l:estudiante list, nombre:string) = filter (nombreCoincide nombre) l; 

(* B *)

fun listaCursos(nil:estudiante list, id: int) = raise ListaVacia
| listaCursos([x],id) = if (#ID(x) = id) then #Cursos(x) else nil
| listaCursos(x::XS,id) = if (#ID(x) = id) then #Cursos(x) else listaCursos(XS,id);

(* C *) 

(* 1era versión *)
fun inLista(c:string, nil:string list) = false
| inLista(c,x::XS) = (x=c) orelse inLista(c,XS);  

fun listaNombres(nil:estudiante list, c: string) = nil:string list
| listaNombres(x::XS,c) = if(inLista(c,#Cursos(x))) then ((#Nombre x)::listaNombres(XS,c)) else listaNombres(XS,c);

(* 2da versión *)
fun inCursos (c:string) (e:estudiante) = inLista(c,#Cursos(e));
fun listaEstudiantes(l: estudiante list, c: string) = filter (inCursos c) l;
fun nom(e:estudiante) = #Nombre(e);
fun listaNombrev2(l:estudiante list, c: string) = map nom (listaEstudiantes(l,c)); 


(*———————————————*)

(* EJERCICIO 4 *)

datatype 'a arbolbin = Vacio | Nodo of 'a  * 'a arbolbin * 'a arbolbin;

(* A *)
fun crearContacto(iden:int, cur:string list, nom:string) = {ID=iden, Cursos= cur, Nombre=nom}:estudiante;

fun ingresar_contacto(iden:int, cur:string list, nom:string, Vacio: estudiante arbolbin)=Nodo(crearContacto(iden, cur, nom),Vacio, Vacio)
| ingresar_contacto(iden:int, cur:string list, nom:string, Nodo(est:estudiante,izq,der)) = 
if (nom = #Nombre(est)) then Nodo(est, izq,der) else (if (nom < #Nombre (est)) then Nodo(est,ingresar_contacto(iden, cur, nom,izq),der) else Nodo(est,izq,ingresar_contacto(iden, cur, nom,der)));


(* B *)

fun modificar_contacto(iden:int, cur:string list, nom:string, Vacio: estudiante arbolbin)=Vacio
| modificar_contacto(iden:int, cur:string list, nom:string, Nodo(est:estudiante,izq,der)) = 
if (nom = #Nombre(est)) then Nodo(crearContacto(iden, cur, nom), izq,der) else (if (nom < #Nombre (est)) then Nodo(est,modificar_contacto(iden, cur, nom,izq),der) else Nodo(est,izq,modificar_contacto(iden, cur, nom,der)));

(* C *)

fun buscar_cursos(Vacio: estudiante arbolbin, nom: string) = nil
| buscar_cursos(Nodo(est,izq,der),nom) = if (nom = #Nombre(est)) then #Cursos(est) else
(if (nom < #Nombre(est)) then buscar_cursos(izq,nom) else buscar_cursos(der,nom));

(* D *)

fun buscar_ID(Vacio: estudiante arbolbin, nom: string) = ~1
| buscar_ID(Nodo(est,izq,der),nom) = if (nom = #Nombre(est)) then #ID(est) else
(if (nom < #Nombre(est)) then buscar_ID(izq,nom) else buscar_ID(der,nom));

(*———————————————*)

(* EJERCICIO 5 *)

(* A *)

(* funciones “inLista” y “nom” definidas más arriba *)
fun no_inscripto (c:string) (est: estudiante) = not(inLista(c,#Cursos(est)));
fun estudiantes_no_inscriptos(lista: estudiante list, c:string) = (map nom (filter (no_inscripto c) lista)); 

(* B *)
fun f x = 1
fun suma(x,y) = x+y
fun cant_estudiantes_no_inscriptos(lista, c) = (foldr suma 0 (map f (estudiantes_no_inscriptos(lista,c))));

(* C *)

fun preorden(Vacio:estudiante arbolbin) = nil:estudiante list
| preorden(Nodo(a,izq,der)) = [a] @ preorden(izq) @ preorden(der);

fun estudiantes_no_inscriptosv2(arbol: estudiante arbolbin, c:string) =
let 
	val l = filter (no_inscripto c) (preorden arbol)
in
	map nom l
end;


(* D *)
fun cant_estudiantes_no_inscriptosv2(arbol, c) = 
let 
	fun f x = 1;
	fun suma(x,y) = x+y;
	val lista_est = estudiantes_no_inscriptosv2(arbol,c)
in 
	foldr suma 0 (map f lista_est)
end;
